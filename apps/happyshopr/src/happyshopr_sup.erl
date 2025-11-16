-module(happyshopr_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    %% HTTP server supervisor
    HttpSup = #{
        id => happyshopr_http_sup,
        start => {happyshopr_http_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [happyshopr_http_sup]
    },

    {ok, {SupFlags, [HttpSup]}}.
