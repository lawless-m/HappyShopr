-module(items_handler).
-behaviour(cowboy_handler).
-include("happyshopr.hrl").

-export([init/2]).

init(Req, [Action]) ->
    %% Require authentication for all operations
    case happyshopr_handler:require_auth(Req) of
        {ok, _UserId} ->
            handle_request(Req, Action);
        {error, unauthorized} ->
            Req2 = happyshopr_handler:error_response(Req, 401, unauthorized),
            {ok, Req2, Action}
    end.

handle_request(Req, items) ->
    Method = cowboy_req:method(Req),
    ListId = cowboy_req:binding(list_id, Req),
    handle_items(Req, Method, ListId);

handle_request(Req, item) ->
    Method = cowboy_req:method(Req),
    ListId = cowboy_req:binding(list_id, Req),
    ItemId = cowboy_req:binding(item_id, Req),
    handle_single_item(Req, Method, ListId, ItemId);

handle_request(Req, complete) ->
    ListId = cowboy_req:binding(list_id, Req),
    ItemId = cowboy_req:binding(item_id, Req),
    handle_complete(Req, ListId, ItemId);

handle_request(Req, required) ->
    ListId = cowboy_req:binding(list_id, Req),
    ItemId = cowboy_req:binding(item_id, Req),
    handle_required(Req, ListId, ItemId);

handle_request(Req, clear_completed) ->
    ListId = cowboy_req:binding(list_id, Req),
    handle_clear_completed(Req, ListId);

handle_request(Req, recipes_summary) ->
    ListId = cowboy_req:binding(list_id, Req),
    handle_recipes_summary(Req, ListId);

handle_request(Req, recipe_items) ->
    ListId = cowboy_req:binding(list_id, Req),
    RecipeId = cowboy_req:binding(recipe_id, Req),
    handle_recipe_items(Req, ListId, RecipeId).

%% Handle /api/v1/lists/:list_id/items
handle_items(Req, <<"POST">>, ListId) ->
    case happyshopr_handler:parse_json_body(Req) of
        {ok, Data} ->
            %% Extract items array and optional recipe info
            Items = maps:get(<<"items">>, Data, []),
            RecipeId = maps:get(<<"recipe_id">>, Data, undefined),
            RecipeName = maps:get(<<"recipe_name">>, Data, undefined),

            %% Convert JSON items to maps
            ItemsData = lists:map(fun(ItemJson) ->
                BaseMap = #{
                    name => maps:get(<<"name">>, ItemJson),
                    quantity => maps:get(<<"quantity">>, ItemJson, undefined),
                    notes => maps:get(<<"notes">>, ItemJson, undefined)
                },

                %% Add recipe info if provided
                case RecipeId of
                    undefined -> BaseMap;
                    _ ->
                        maps:merge(BaseMap, #{
                            recipe_id => RecipeId,
                            recipe_name => RecipeName
                        })
                end
            end, Items),

            case happyshopr_db:add_items(ListId, ItemsData) of
                {ok, AddedItems} ->
                    Response = #{
                        <<"added">> => lists:map(fun item_to_json/1, AddedItems)
                    },
                    Req2 = happyshopr_handler:json_response(Req, 201, Response),
                    {ok, Req2, items};
                {error, list_not_found} ->
                    Req2 = happyshopr_handler:error_response(Req, 404, not_found),
                    {ok, Req2, items};
                {error, _} ->
                    Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
                    {ok, Req2, items}
            end;
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 400, bad_request),
            {ok, Req2, items}
    end;

handle_items(Req, _, _ListId) ->
    Req2 = cowboy_req:reply(405, Req),
    {ok, Req2, items}.

%% Handle /api/v1/lists/:list_id/items/:item_id
handle_single_item(Req, <<"PUT">>, _ListId, ItemId) ->
    case happyshopr_handler:parse_json_body(Req) of
        {ok, Data} ->
            Updates = #{
                name => maps:get(<<"name">>, Data, undefined),
                quantity => maps:get(<<"quantity">>, Data, undefined),
                notes => maps:get(<<"notes">>, Data, undefined),
                completed => maps:get(<<"completed">>, Data, undefined)
            },
            %% Remove undefined values
            CleanUpdates = maps:filter(fun(_, V) -> V =/= undefined end, Updates),

            case happyshopr_db:update_item(ItemId, CleanUpdates) of
                {ok, Item} ->
                    Response = item_to_json(Item),
                    Req2 = happyshopr_handler:json_response(Req, 200, Response),
                    {ok, Req2, item};
                {error, not_found} ->
                    Req2 = happyshopr_handler:error_response(Req, 404, not_found),
                    {ok, Req2, item};
                {error, _} ->
                    Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
                    {ok, Req2, item}
            end;
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 400, bad_request),
            {ok, Req2, item}
    end;

handle_single_item(Req, <<"DELETE">>, _ListId, ItemId) ->
    case happyshopr_db:delete_item(ItemId) of
        ok ->
            Response = #{<<"message">> => <<"Item deleted successfully">>},
            Req2 = happyshopr_handler:json_response(Req, 200, Response),
            {ok, Req2, item};
        {error, not_found} ->
            Req2 = happyshopr_handler:error_response(Req, 404, not_found),
            {ok, Req2, item};
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
            {ok, Req2, item}
    end;

handle_single_item(Req, _, _ListId, _ItemId) ->
    Req2 = cowboy_req:reply(405, Req),
    {ok, Req2, item}.

%% Handle /api/v1/lists/:list_id/items/:item_id/complete
handle_complete(Req, _ListId, ItemId) ->
    case happyshopr_handler:parse_json_body(Req) of
        {ok, Data} ->
            Completed = maps:get(<<"completed">>, Data, true),

            case happyshopr_db:mark_completed(ItemId, Completed) of
                {ok, Item} ->
                    Response = #{
                        <<"id">> => Item#shopping_item.id,
                        <<"completed">> => Item#shopping_item.completed
                    },
                    Req2 = happyshopr_handler:json_response(Req, 200, Response),
                    {ok, Req2, complete};
                {error, not_found} ->
                    Req2 = happyshopr_handler:error_response(Req, 404, not_found),
                    {ok, Req2, complete};
                {error, _} ->
                    Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
                    {ok, Req2, complete}
            end;
        {error, _} ->
            %% Default to marking as completed if no body provided
            case happyshopr_db:mark_completed(ItemId, true) of
                {ok, Item} ->
                    Response = #{
                        <<"id">> => Item#shopping_item.id,
                        <<"completed">> => Item#shopping_item.completed
                    },
                    Req2 = happyshopr_handler:json_response(Req, 200, Response),
                    {ok, Req2, complete};
                {error, not_found} ->
                    Req2 = happyshopr_handler:error_response(Req, 404, not_found),
                    {ok, Req2, complete};
                {error, _} ->
                    Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
                    {ok, Req2, complete}
            end
    end.

%% Handle /api/v1/lists/:list_id/items/:item_id/required
handle_required(Req, _ListId, ItemId) ->
    case happyshopr_handler:parse_json_body(Req) of
        {ok, Data} ->
            Required = maps:get(<<"required">>, Data, true),

            case happyshopr_db:toggle_required(ItemId, Required) of
                {ok, Item} ->
                    Response = #{
                        <<"id">> => Item#shopping_item.id,
                        <<"name">> => Item#shopping_item.name,
                        <<"recipe_name">> => Item#shopping_item.recipe_name,
                        <<"required">> => Item#shopping_item.required,
                        <<"completed">> => Item#shopping_item.completed
                    },
                    Req2 = happyshopr_handler:json_response(Req, 200, Response),
                    {ok, Req2, required};
                {error, not_found} ->
                    Req2 = happyshopr_handler:error_response(Req, 404, not_found),
                    {ok, Req2, required};
                {error, _} ->
                    Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
                    {ok, Req2, required}
            end;
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 400, bad_request),
            {ok, Req2, required}
    end.

%% Handle /api/v1/lists/:list_id/items/completed
handle_clear_completed(Req, ListId) ->
    case happyshopr_db:clear_completed(ListId) of
        {ok, Count} ->
            Response = #{
                <<"message">> => iolist_to_binary(io_lib:format("Cleared ~p completed items", [Count])),
                <<"deleted_count">> => Count
            },
            Req2 = happyshopr_handler:json_response(Req, 200, Response),
            {ok, Req2, clear_completed};
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
            {ok, Req2, clear_completed}
    end.

%% Handle /api/v1/lists/:list_id/recipes
handle_recipes_summary(Req, ListId) ->
    case happyshopr_db:get_recipes_summary(ListId) of
        {ok, Summaries} ->
            Response = #{<<"recipes">> => Summaries},
            Req2 = happyshopr_handler:json_response(Req, 200, Response),
            {ok, Req2, recipes_summary};
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
            {ok, Req2, recipes_summary}
    end.

%% Handle /api/v1/lists/:list_id/recipes/:recipe_id/items
handle_recipe_items(Req, ListId, RecipeId) ->
    case happyshopr_db:get_items_by_recipe(ListId, RecipeId) of
        {ok, Items} ->
            RequiredItems = lists:filter(fun(I) -> I#shopping_item.required end, Items),
            CompletedItems = lists:filter(fun(I) ->
                I#shopping_item.required andalso I#shopping_item.completed
            end, Items),

            RecipeName = case Items of
                [First | _] -> First#shopping_item.recipe_name;
                [] -> null
            end,

            Response = #{
                <<"recipe_id">> => RecipeId,
                <<"recipe_name">> => RecipeName,
                <<"items">> => lists:map(fun item_to_json/1, Items),
                <<"total_items">> => length(Items),
                <<"required_items">> => length(RequiredItems),
                <<"completed_items">> => length(CompletedItems)
            },
            Req2 = happyshopr_handler:json_response(Req, 200, Response),
            {ok, Req2, recipe_items};
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
            {ok, Req2, recipe_items}
    end.

%% Convert item to JSON
item_to_json(Item) ->
    Base = #{
        <<"id">> => Item#shopping_item.id,
        <<"name">> => Item#shopping_item.name,
        <<"required">> => Item#shopping_item.required,
        <<"completed">> => Item#shopping_item.completed,
        <<"created_at">> => Item#shopping_item.created_at
    },

    %% Add optional fields
    maps:merge(Base, optional_fields(Item)).

optional_fields(Item) ->
    Fields = #{
        <<"quantity">> => Item#shopping_item.quantity,
        <<"notes">> => Item#shopping_item.notes,
        <<"recipe_id">> => Item#shopping_item.recipe_id,
        <<"recipe_name">> => Item#shopping_item.recipe_name
    },
    %% Filter out undefined/null values
    maps:filter(fun(_, V) -> V =/= undefined andalso V =/= null end, Fields).
