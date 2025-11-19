-module(recipe_import_handler).
-behaviour(cowboy_handler).
-include("happyshopr.hrl").

-export([init/2]).

init(Req, State) ->
    %% Require authentication
    case happyshopr_handler:require_auth(Req) of
        {ok, UserId} ->
            handle_import(Req, UserId);
        {error, unauthorized} ->
            Req2 = happyshopr_handler:error_response(Req, 401, unauthorized),
            {ok, Req2, State}
    end.

handle_import(Req, UserId) ->
    Method = cowboy_req:method(Req),
    ListId = cowboy_req:binding(list_id, Req),
    handle_method(Req, Method, ListId, UserId).

handle_method(Req, <<"POST">>, ListId, UserId) ->
    case happyshopr_handler:parse_json_body(Req) of
        {ok, Data} ->
            Url = maps:get(<<"url">>, Data, undefined),
            case Url of
                undefined ->
                    Req2 = happyshopr_handler:error_response(Req, 400,
                        #{<<"error">> => <<"url field is required">>}),
                    {ok, Req2, []};
                _ ->
                    import_recipe_from_url(Req, ListId, UserId, Url)
            end;
        {error, _} ->
            Req2 = happyshopr_handler:error_response(Req, 400, bad_request),
            {ok, Req2, []}
    end;

handle_method(Req, _, _ListId, _UserId) ->
    Req2 = cowboy_req:reply(405, Req),
    {ok, Req2, []}.

import_recipe_from_url(Req, ListId, UserId, Url) ->
    logger:info("Importing recipe from URL: ~s", [Url]),

    %% Download content from URL
    case download_content(Url) of
        {ok, Content} ->
            logger:info("Downloaded content, parsing JSON from markdown"),
            %% Extract JSON from markdown
            case extract_json_from_markdown(Content) of
                {ok, RecipeData} ->
                    logger:info("Parsed recipe data, creating recipe and items"),
                    %% Create recipe and items
                    case create_recipe_and_items(ListId, UserId, RecipeData, Url) of
                        {ok, RecipeId, ItemCount} ->
                            Response = #{
                                <<"success">> => true,
                                <<"recipe_id">> => RecipeId,
                                <<"items_added">> => ItemCount,
                                <<"message">> => <<"Recipe imported successfully">>
                            },
                            Req2 = happyshopr_handler:json_response(Req, 201, Response),
                            {ok, Req2, []};
                        {error, Reason} ->
                            logger:error("Failed to create recipe and items: ~p", [Reason]),
                            Req2 = happyshopr_handler:error_response(Req, 500,
                                #{<<"error">> => <<"Failed to save recipe">>}),
                            {ok, Req2, []}
                    end;
                {error, parse_error} ->
                    Req2 = happyshopr_handler:error_response(Req, 400,
                        #{<<"error">> => <<"Could not parse JSON from markdown">>}),
                    {ok, Req2, []};
                {error, Reason} ->
                    logger:error("Failed to parse JSON: ~p", [Reason]),
                    Req2 = happyshopr_handler:error_response(Req, 400,
                        #{<<"error">> => <<"Invalid recipe format">>}),
                    {ok, Req2, []}
            end;
        {error, download_failed} ->
            Req2 = happyshopr_handler:error_response(Req, 400,
                #{<<"error">> => <<"Failed to download content from URL">>}),
            {ok, Req2, []};
        {error, Reason} ->
            logger:error("Download failed: ~p", [Reason]),
            Req2 = happyshopr_handler:error_response(Req, 500,
                #{<<"error">> => <<"Failed to fetch URL">>}),
            {ok, Req2, []}
    end.

%% Download content from URL using httpc
download_content(Url) ->
    UrlStr = binary_to_list(Url),

    %% Ensure inets is started
    case application:ensure_started(inets) of
        ok -> ok;
        {error, {already_started, inets}} -> ok;
        {error, Reason} ->
            logger:error("Failed to start inets: ~p", [Reason]),
            {error, inets_start_failed}
    end,

    case application:ensure_started(ssl) of
        ok -> ok;
        {error, {already_started, ssl}} -> ok;
        {error, Reason2} ->
            logger:error("Failed to start ssl: ~p", [Reason2]),
            {error, ssl_start_failed}
    end,

    HttpOptions = [{timeout, 10000}, {connect_timeout, 5000}],
    Options = [],

    case httpc:request(get, {UrlStr, []}, HttpOptions, Options) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, list_to_binary(Body)};
        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            logger:error("HTTP request failed with status: ~p", [StatusCode]),
            {error, download_failed};
        {error, Reason} ->
            logger:error("HTTP request error: ~p", [Reason]),
            {error, download_failed}
    end.

%% Extract JSON from markdown code block
extract_json_from_markdown(Content) ->
    %% Look for ```json ... ``` blocks
    case binary:split(Content, [<<"```json">>, <<"```">>], [global]) of
        [_, JsonContent | _] ->
            %% Remove any leading/trailing whitespace
            JsonBinary = string:trim(JsonContent),
            try
                DecodedJson = jsx:decode(JsonBinary, [return_maps]),
                {ok, DecodedJson}
            catch
                error:Reason ->
                    logger:error("JSON decode error: ~p", [Reason]),
                    {error, parse_error}
            end;
        _ ->
            %% Try to parse the entire content as JSON (fallback)
            try
                DecodedJson = jsx:decode(Content, [return_maps]),
                {ok, DecodedJson}
            catch
                error:_ ->
                    {error, parse_error}
            end
    end.

%% Create recipe record and shopping items
create_recipe_and_items(ListId, UserId, RecipeData, SourceUrl) ->
    %% Extract recipe metadata
    RecipeId = case maps:get(<<"recipe_id">>, RecipeData, undefined) of
        undefined -> happyshopr_db:generate_uuid();
        ExistingId -> ExistingId
    end,

    RecipeName = maps:get(<<"title">>, RecipeData, <<"Untitled Recipe">>),
    Ingredients = maps:get(<<"ingredients">>, RecipeData, []),

    %% Save recipe to recipe table
    case save_recipe(RecipeId, UserId, RecipeName, SourceUrl) of
        ok ->
            %% Convert ingredients to shopping items
            ItemsData = lists:map(fun(Ingredient) ->
                Name = maps:get(<<"name">>, Ingredient, <<"">>),
                Quantity = maps:get(<<"quantity">>, Ingredient, undefined),
                Notes = maps:get(<<"notes">>, Ingredient, undefined),
                Category = maps:get(<<"category">>, Ingredient, undefined),

                #{
                    name => Name,
                    quantity => case Quantity of
                        <<"">> -> undefined;
                        undefined -> undefined;
                        Q -> Q
                    end,
                    notes => case Notes of
                        <<"">> -> undefined;
                        undefined -> undefined;
                        N -> N
                    end,
                    category => case Category of
                        <<"">> -> undefined;
                        undefined -> undefined;
                        C -> C
                    end,
                    recipe_id => RecipeId,
                    recipe_name => RecipeName
                }
            end, Ingredients),

            %% Add items to shopping list
            case happyshopr_db:add_items(ListId, ItemsData) of
                {ok, AddedItems} ->
                    {ok, RecipeId, length(AddedItems)};
                {error, Reason} ->
                    logger:error("Failed to add items: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Save recipe to database
save_recipe(RecipeId, UserId, RecipeName, SourceUrl) ->
    Now = erlang:system_time(second),

    Recipe = #recipe{
        id = RecipeId,
        user_id = UserId,
        name = RecipeName,
        source = SourceUrl,
        created_at = Now,
        last_used = Now
    },

    F = fun() ->
        mnesia:write(recipe, Recipe, write)
    end,

    case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            logger:error("Failed to save recipe: ~p", [Reason]),
            {error, Reason}
    end.
