-module(health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    {ok, Vsn} = application:get_key(happyshopr, vsn),

    %% Get uptime in seconds
    {UpTime, _} = erlang:statistics(wall_clock),
    UptimeSeconds = UpTime div 1000,

    Response = #{
        <<"status">> => <<"ok">>,
        <<"version">> => list_to_binary(Vsn),
        <<"uptime_seconds">> => UptimeSeconds
    },

    Req2 = happyshopr_handler:json_response(Req, 200, Response),
    {ok, Req2, State}.
