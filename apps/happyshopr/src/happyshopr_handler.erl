-module(happyshopr_handler).

-export([
    json_response/3,
    error_response/3,
    require_auth/1,
    parse_json_body/1
]).

%% Send JSON response
json_response(Req, Status, Data) ->
    Body = jsx:encode(Data),
    cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req).

%% Send error response
error_response(Req, Status, ErrorCode) ->
    Message = error_message(ErrorCode),
    Body = jsx:encode(#{
        <<"error">> => #{
            <<"code">> => atom_to_binary(ErrorCode),
            <<"message">> => Message
        }
    }),
    cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req).

%% Require authentication and return user_id
require_auth(Req) ->
    case happyshopr_auth:extract_token(Req) of
        {ok, Token} ->
            case happyshopr_auth:validate_key(Token) of
                {ok, UserId} ->
                    {ok, UserId};
                {error, _} ->
                    {error, unauthorized}
            end;
        {error, _} ->
            {error, unauthorized}
    end.

%% Parse JSON body
parse_json_body(Req) ->
    {ok, Body, _Req2} = cowboy_req:read_body(Req),
    case Body of
        <<>> ->
            {ok, #{}};
        _ ->
            try
                Data = jsx:decode(Body, [return_maps]),
                {ok, Data}
            catch
                _:_ ->
                    {error, invalid_json}
            end
    end.

%% Error messages
error_message(bad_request) -> <<"Invalid request">>;
error_message(unauthorized) -> <<"Authentication required">>;
error_message(forbidden) -> <<"Access denied">>;
error_message(not_found) -> <<"Resource not found">>;
error_message(invalid_json) -> <<"Invalid JSON in request body">>;
error_message(internal_error) -> <<"Internal server error">>;
error_message(_) -> <<"Unknown error">>.
