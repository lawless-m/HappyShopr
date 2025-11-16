-module(lists_handler).
-behaviour(cowboy_handler).
-include("happyshopr.hrl").

-export([init/2]).

init(Req, [Action]) ->
    %% Require authentication for all operations
    case happyshopr_handler:require_auth(Req) of
        {ok, UserId} ->
            handle_request(Req, Action, UserId);
        {error, unauthorized} ->
            Req2 = happyshopr_handler:error_response(Req, 401, unauthorized),
            {ok, Req2, Action}
    end.

handle_request(Req, list, UserId) ->
    Method = cowboy_req:method(Req),
    handle_lists(Req, Method, UserId);

handle_request(Req, get_list, UserId) ->
    Method = cowboy_req:method(Req),
    ListId = cowboy_req:binding(list_id, Req),
    handle_single_list(Req, Method, ListId, UserId).

%% Handle /api/v1/lists
handle_lists(Req, <<"GET">>, UserId) ->
    %% Get all lists for user
    case happyshopr_db:get_all_lists(UserId) of
        {ok, Lists} ->
            %% Convert to JSON format with item counts
            JsonLists = lists:map(fun(List) ->
                {ok, Items} = happyshopr_db:get_items_by_list(List#shopping_list.id),
                list_to_json(List, length(Items))
            end, Lists),

            Response = #{<<"lists">> => JsonLists},
            Req2 = happyshopr_handler:json_response(Req, 200, Response),
            {ok, Req2, list};
        {error, _Reason} ->
            Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
            {ok, Req2, list}
    end;

handle_lists(Req, <<"POST">>, UserId) ->
    %% Create new list
    case happyshopr_handler:parse_json_body(Req) of
        {ok, Data} ->
            Name = maps:get(<<"name">>, Data, <<"Unnamed List">>),

            case happyshopr_db:create_list(Name, UserId) of
                {ok, List} ->
                    Response = list_to_json_full(List, []),
                    Req2 = happyshopr_handler:json_response(Req, 201, Response),
                    {ok, Req2, list};
                {error, _Reason} ->
                    Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
                    {ok, Req2, list}
            end;
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 400, bad_request),
            {ok, Req2, list}
    end;

handle_lists(Req, _, _UserId) ->
    Req2 = cowboy_req:reply(405, Req),
    {ok, Req2, list}.

%% Handle /api/v1/lists/:list_id
handle_single_list(Req, <<"GET">>, ListId, _UserId) ->
    %% Get single list with items
    case happyshopr_db:get_list(ListId) of
        {ok, List} ->
            case happyshopr_db:get_items_by_list(ListId) of
                {ok, Items} ->
                    Response = list_to_json_full(List, Items),
                    Req2 = happyshopr_handler:json_response(Req, 200, Response),
                    {ok, Req2, get_list};
                {error, _} ->
                    Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
                    {ok, Req2, get_list}
            end;
        {error, not_found} ->
            Req2 = happyshopr_handler:error_response(Req, 404, not_found),
            {ok, Req2, get_list};
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
            {ok, Req2, get_list}
    end;

handle_single_list(Req, <<"PUT">>, ListId, _UserId) ->
    %% Update list
    case happyshopr_handler:parse_json_body(Req) of
        {ok, Data} ->
            Updates = #{
                name => maps:get(<<"name">>, Data, undefined)
            },
            %% Remove undefined values
            CleanUpdates = maps:filter(fun(_, V) -> V =/= undefined end, Updates),

            case happyshopr_db:update_list(ListId, CleanUpdates) of
                {ok, List} ->
                    Response = #{
                        <<"id">> => List#shopping_list.id,
                        <<"name">> => List#shopping_list.name,
                        <<"updated_at">> => List#shopping_list.updated_at
                    },
                    Req2 = happyshopr_handler:json_response(Req, 200, Response),
                    {ok, Req2, get_list};
                {error, not_found} ->
                    Req2 = happyshopr_handler:error_response(Req, 404, not_found),
                    {ok, Req2, get_list};
                {error, _} ->
                    Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
                    {ok, Req2, get_list}
            end;
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 400, bad_request),
            {ok, Req2, get_list}
    end;

handle_single_list(Req, <<"DELETE">>, ListId, _UserId) ->
    %% Delete list
    case happyshopr_db:delete_list(ListId) of
        ok ->
            Response = #{<<"message">> => <<"List deleted successfully">>},
            Req2 = happyshopr_handler:json_response(Req, 200, Response),
            {ok, Req2, get_list};
        {error, not_found} ->
            Req2 = happyshopr_handler:error_response(Req, 404, not_found),
            {ok, Req2, get_list};
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 500, internal_error),
            {ok, Req2, get_list}
    end;

handle_single_list(Req, _, _ListId, _UserId) ->
    Req2 = cowboy_req:reply(405, Req),
    {ok, Req2, get_list}.

%% Convert list to JSON (summary)
list_to_json(List, ItemCount) ->
    #{
        <<"id">> => List#shopping_list.id,
        <<"name">> => List#shopping_list.name,
        <<"created_at">> => List#shopping_list.created_at,
        <<"updated_at">> => List#shopping_list.updated_at,
        <<"item_count">> => ItemCount
    }.

%% Convert list to JSON (full with items)
list_to_json_full(List, Items) ->
    #{
        <<"id">> => List#shopping_list.id,
        <<"user_id">> => List#shopping_list.user_id,
        <<"name">> => List#shopping_list.name,
        <<"created_at">> => List#shopping_list.created_at,
        <<"updated_at">> => List#shopping_list.updated_at,
        <<"items">> => lists:map(fun item_to_json/1, Items)
    }.

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
