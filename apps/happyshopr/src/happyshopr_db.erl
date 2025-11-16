-module(happyshopr_db).
-include("happyshopr.hrl").

-export([
    init/0,
    create_tables/0,
    % List operations
    create_list/2,
    get_list/1,
    get_all_lists/1,
    update_list/2,
    delete_list/1,
    % Item operations
    add_items/2,
    get_item/1,
    get_items_by_list/1,
    get_items_by_recipe/2,
    update_item/2,
    delete_item/1,
    mark_completed/2,
    clear_completed/1,
    toggle_required/2,
    % Recipe operations
    get_recipes_summary/1
]).

-define(TIMEOUT, 5000).

%% Initialize Mnesia and create schema
init() ->
    logger:info("Initializing Mnesia database"),

    %% Get Mnesia directory from config
    {ok, MnesiaDir} = application:get_env(happyshopr, mnesia_dir),
    filelib:ensure_dir(MnesiaDir ++ "/"),

    %% Create schema if it doesn't exist
    case mnesia:create_schema([node()]) of
        ok ->
            logger:info("Mnesia schema created");
        {error, {_, {already_exists, _}}} ->
            logger:info("Mnesia schema already exists");
        {error, Reason} ->
            logger:warning("Failed to create Mnesia schema: ~p", [Reason])
    end,

    %% Start Mnesia
    ok = mnesia:start(),
    logger:info("Mnesia started"),

    %% Create tables
    create_tables(),

    %% Wait for tables
    ok = mnesia:wait_for_tables([
        shopping_list,
        shopping_item,
        api_key,
        recipe,
        user
    ], ?TIMEOUT),

    logger:info("Mnesia database initialized successfully"),
    ok.

%% Create all tables
create_tables() ->
    %% Shopping lists table
    create_table(shopping_list, [
        {attributes, record_info(fields, shopping_list)},
        {disc_copies, [node()]},
        {index, [user_id]},
        {type, set}
    ]),

    %% Shopping items table
    create_table(shopping_item, [
        {attributes, record_info(fields, shopping_item)},
        {disc_copies, [node()]},
        {index, [list_id, recipe_id]},
        {type, set}
    ]),

    %% API keys table
    create_table(api_key, [
        {attributes, record_info(fields, api_key)},
        {disc_copies, [node()]},
        {type, set}
    ]),

    %% Recipes table (optional)
    create_table(recipe, [
        {attributes, record_info(fields, recipe)},
        {disc_copies, [node()]},
        {index, [user_id]},
        {type, set}
    ]),

    %% Users table (future use)
    create_table(user, [
        {attributes, record_info(fields, user)},
        {disc_copies, [node()]},
        {type, set}
    ]),

    ok.

%% Helper to create table
create_table(Name, Opts) ->
    case mnesia:create_table(Name, Opts) of
        {atomic, ok} ->
            logger:info("Created table: ~p", [Name]),
            ok;
        {aborted, {already_exists, Name}} ->
            logger:info("Table already exists: ~p", [Name]),
            ok;
        {aborted, Reason} ->
            logger:error("Failed to create table ~p: ~p", [Name, Reason]),
            {error, Reason}
    end.

%% ============================================================================
%% List operations
%% ============================================================================

create_list(Name, UserId) ->
    List = #shopping_list{
        id = generate_uuid(),
        user_id = UserId,
        name = Name,
        created_at = erlang:system_time(second),
        updated_at = erlang:system_time(second)
    },

    F = fun() ->
        mnesia:write(List)
    end,

    case mnesia:transaction(F) of
        {atomic, ok} ->
            {ok, List};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_list(ListId) ->
    F = fun() ->
        case mnesia:read(shopping_list, ListId) of
            [List] -> {ok, List};
            [] -> {error, not_found}
        end
    end,

    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_all_lists(UserId) ->
    F = fun() ->
        mnesia:match_object(#shopping_list{
            id = '_',
            user_id = UserId,
            name = '_',
            created_at = '_',
            updated_at = '_'
        })
    end,

    case mnesia:transaction(F) of
        {atomic, Lists} -> {ok, Lists};
        {aborted, Reason} -> {error, Reason}
    end.

update_list(ListId, Updates) ->
    F = fun() ->
        case mnesia:read(shopping_list, ListId) of
            [List] ->
                UpdatedList = List#shopping_list{
                    name = maps:get(name, Updates, List#shopping_list.name),
                    updated_at = erlang:system_time(second)
                },
                mnesia:write(UpdatedList),
                {ok, UpdatedList};
            [] ->
                {error, not_found}
        end
    end,

    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

delete_list(ListId) ->
    F = fun() ->
        %% Delete all items in the list first
        Items = mnesia:index_read(shopping_item, ListId, #shopping_item.list_id),
        lists:foreach(fun(Item) ->
            mnesia:delete_object(Item)
        end, Items),

        %% Delete the list
        case mnesia:read(shopping_list, ListId) of
            [List] ->
                mnesia:delete_object(List),
                ok;
            [] ->
                {error, not_found}
        end
    end,

    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% ============================================================================
%% Item operations
%% ============================================================================

add_items(ListId, ItemsData) ->
    %% ItemsData is a list of maps with keys: name, quantity, notes, recipe_id, recipe_name
    F = fun() ->
        %% Verify list exists
        case mnesia:read(shopping_list, ListId) of
            [] ->
                {error, list_not_found};
            [List] ->
                Timestamp = erlang:system_time(second),
                Items = lists:map(fun(ItemData) ->
                    #shopping_item{
                        id = generate_uuid(),
                        list_id = ListId,
                        name = maps:get(name, ItemData),
                        quantity = maps:get(quantity, ItemData, undefined),
                        notes = maps:get(notes, ItemData, undefined),
                        category = maps:get(category, ItemData, undefined),
                        recipe_id = maps:get(recipe_id, ItemData, undefined),
                        recipe_name = maps:get(recipe_name, ItemData, undefined),
                        required = maps:get(required, ItemData, true),
                        completed = false,
                        created_at = Timestamp,
                        added_by = List#shopping_list.user_id
                    }
                end, ItemsData),

                lists:foreach(fun(Item) ->
                    mnesia:write(Item)
                end, Items),

                %% Update list timestamp
                UpdatedList = List#shopping_list{updated_at = Timestamp},
                mnesia:write(UpdatedList),

                {ok, Items}
        end
    end,

    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_item(ItemId) ->
    F = fun() ->
        case mnesia:read(shopping_item, ItemId) of
            [Item] -> {ok, Item};
            [] -> {error, not_found}
        end
    end,

    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_items_by_list(ListId) ->
    F = fun() ->
        mnesia:index_read(shopping_item, ListId, #shopping_item.list_id)
    end,

    case mnesia:transaction(F) of
        {atomic, Items} -> {ok, Items};
        {aborted, Reason} -> {error, Reason}
    end.

get_items_by_recipe(ListId, RecipeId) ->
    F = fun() ->
        AllItems = mnesia:index_read(shopping_item, ListId, #shopping_item.list_id),
        lists:filter(fun(Item) ->
            Item#shopping_item.recipe_id =:= RecipeId
        end, AllItems)
    end,

    case mnesia:transaction(F) of
        {atomic, Items} -> {ok, Items};
        {aborted, Reason} -> {error, Reason}
    end.

update_item(ItemId, Updates) ->
    F = fun() ->
        case mnesia:read(shopping_item, ItemId) of
            [Item] ->
                UpdatedItem = Item#shopping_item{
                    name = maps:get(name, Updates, Item#shopping_item.name),
                    quantity = maps:get(quantity, Updates, Item#shopping_item.quantity),
                    notes = maps:get(notes, Updates, Item#shopping_item.notes),
                    completed = maps:get(completed, Updates, Item#shopping_item.completed),
                    required = maps:get(required, Updates, Item#shopping_item.required)
                },
                mnesia:write(UpdatedItem),
                {ok, UpdatedItem};
            [] ->
                {error, not_found}
        end
    end,

    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

delete_item(ItemId) ->
    F = fun() ->
        case mnesia:read(shopping_item, ItemId) of
            [Item] ->
                mnesia:delete_object(Item),
                ok;
            [] ->
                {error, not_found}
        end
    end,

    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

mark_completed(ItemId, Completed) ->
    update_item(ItemId, #{completed => Completed}).

toggle_required(ItemId, Required) ->
    update_item(ItemId, #{required => Required}).

clear_completed(ListId) ->
    F = fun() ->
        Items = mnesia:index_read(shopping_item, ListId, #shopping_item.list_id),
        CompletedItems = lists:filter(fun(Item) ->
            Item#shopping_item.completed =:= true
        end, Items),

        lists:foreach(fun(Item) ->
            mnesia:delete_object(Item)
        end, CompletedItems),

        {ok, length(CompletedItems)}
    end,

    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% ============================================================================
%% Recipe operations
%% ============================================================================

get_recipes_summary(ListId) ->
    F = fun() ->
        Items = mnesia:index_read(shopping_item, ListId, #shopping_item.list_id),

        %% Group items by recipe_id
        RecipeMap = lists:foldl(fun(Item, Acc) ->
            case Item#shopping_item.recipe_id of
                undefined -> Acc;
                RecipeId ->
                    Existing = maps:get(RecipeId, Acc, []),
                    maps:put(RecipeId, [Item | Existing], Acc)
            end
        end, #{}, Items),

        %% Calculate summary for each recipe
        Summaries = maps:fold(fun(RecipeId, RecipeItems, Acc) ->
            FirstItem = hd(RecipeItems),
            RequiredItems = lists:filter(fun(I) -> I#shopping_item.required end, RecipeItems),
            CompletedItems = lists:filter(fun(I) ->
                I#shopping_item.required andalso I#shopping_item.completed
            end, RecipeItems),

            Summary = #{
                recipe_id => RecipeId,
                recipe_name => FirstItem#shopping_item.recipe_name,
                total_items => length(RequiredItems),
                completed_items => length(CompletedItems),
                all_completed => length(RequiredItems) =:= length(CompletedItems) andalso length(RequiredItems) > 0
            },
            [Summary | Acc]
        end, [], RecipeMap),

        {ok, Summaries}
    end,

    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% ============================================================================
%% Helper functions
%% ============================================================================

generate_uuid() ->
    %% Simple UUID v4 generation
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    list_to_binary(io_lib:format(
        "~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
        [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]
    )).
