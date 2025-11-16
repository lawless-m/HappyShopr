-module(happyshopr_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:info("Starting HappyShopr application"),

    %% Initialize database
    ok = happyshopr_db:init(),

    %% Start the supervisor
    case happyshopr_sup:start_link() of
        {ok, Pid} ->
            logger:info("HappyShopr started successfully"),
            {ok, Pid};
        Error ->
            logger:error("Failed to start HappyShopr: ~p", [Error]),
            Error
    end.

stop(_State) ->
    logger:info("Stopping HappyShopr application"),
    ok.
