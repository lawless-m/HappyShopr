-module(happyshopr_auth).

-export([
    validate_key/1,
    get_user_id/1,
    extract_token/1
]).

%% Validate API key and return user_id
validate_key(ApiKey) ->
    {ok, ApiKeys} = application:get_env(happyshopr, api_keys),

    case lists:keyfind(ApiKey, 1, ApiKeys) of
        {ApiKey, UserId} ->
            {ok, UserId};
        false ->
            {error, invalid_key}
    end.

%% Get user_id from validated API key
get_user_id(ApiKey) ->
    case validate_key(ApiKey) of
        {ok, UserId} -> {ok, UserId};
        {error, _} -> {error, invalid_key}
    end.

%% Extract Bearer token from Authorization header
extract_token(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {error, missing_auth};
        <<"Bearer ", Token/binary>> ->
            {ok, Token};
        _ ->
            {error, invalid_auth_format}
    end.
