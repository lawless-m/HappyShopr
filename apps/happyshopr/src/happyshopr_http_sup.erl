-module(happyshopr_http_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(happyshopr, http_port),

    logger:info("Initializing HTTP server on port ~p", [Port]),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            % Static files
            {"/", cowboy_static, {priv_file, happyshopr, "static/index.html"}},
            {"/shopr", cowboy_static, {priv_file, happyshopr, "static/index.html"}},
            {"/shopr/", cowboy_static, {priv_file, happyshopr, "static/index.html"}},
            {"/css/[...]", cowboy_static, {priv_dir, happyshopr, "static/css"}},
            {"/js/[...]", cowboy_static, {priv_dir, happyshopr, "static/js"}},
            {"/shopr/css/[...]", cowboy_static, {priv_dir, happyshopr, "static/css"}},
            {"/shopr/js/[...]", cowboy_static, {priv_dir, happyshopr, "static/js"}},

            % API endpoints
            {"/health", health_handler, []},
            {"/shopr/health", health_handler, []},
            {"/api/v1/lists", lists_handler, [list]},
            {"/shopr/api/v1/lists", lists_handler, [list]},
            {"/api/v1/lists/:list_id", lists_handler, [get_list]},
            {"/shopr/api/v1/lists/:list_id", lists_handler, [get_list]},
            {"/api/v1/lists/:list_id/items", items_handler, [items]},
            {"/shopr/api/v1/lists/:list_id/items", items_handler, [items]},
            {"/api/v1/lists/:list_id/items/completed", items_handler, [clear_completed]},
            {"/shopr/api/v1/lists/:list_id/items/completed", items_handler, [clear_completed]},
            {"/api/v1/lists/:list_id/items/:item_id", items_handler, [item]},
            {"/shopr/api/v1/lists/:list_id/items/:item_id", items_handler, [item]},
            {"/api/v1/lists/:list_id/items/:item_id/complete", items_handler, [complete]},
            {"/shopr/api/v1/lists/:list_id/items/:item_id/complete", items_handler, [complete]},
            {"/api/v1/lists/:list_id/items/:item_id/required", items_handler, [required]},
            {"/shopr/api/v1/lists/:list_id/items/:item_id/required", items_handler, [required]},
            {"/api/v1/lists/:list_id/recipes", items_handler, [recipes_summary]},
            {"/shopr/api/v1/lists/:list_id/recipes", items_handler, [recipes_summary]},
            {"/api/v1/lists/:list_id/recipes/:recipe_id/items", items_handler, [recipe_items]},
            {"/shopr/api/v1/lists/:list_id/recipes/:recipe_id/items", items_handler, [recipe_items]},
            {"/api/v1/lists/:list_id/import-recipe", recipe_import_handler, []},
            {"/shopr/api/v1/lists/:list_id/import-recipe", recipe_import_handler, []}
        ]}
    ]),

    %% Start Cowboy
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    logger:info("HTTP server started on port ~p", [Port]),

    %% This supervisor doesn't manage any child processes directly
    %% Cowboy manages its own supervision tree
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    {ok, {SupFlags, []}}.
