-module(ids).

-behaviour(nitrogen_rest).

-export([get/1, post/1, delete/1, put/1]).

post("new") ->
    wf:content_type("application/json"),
    ID = erlang:unique_integer([positive, monotonic]),
    Body = #{id => integer_to_binary(ID + 100000000)},
    {201, wf:json_encode(Body)};
post(_Any) ->
    wf:content_type("application/json"),
    Body =
        #{"error" => <<"Not Found">>, "message" => <<"The requested resource does not exist.">>},
    {404, wf:json_encode(Body)}.

get(_Any) ->
    not_allowed().

delete(_Any) ->
    not_allowed().

put(_Any) ->
    not_allowed().

not_allowed() ->
    wf:content_type("application/json"),
    wf:header("Allow", "POST"),
    Body = #{"error" => <<"Method Not Allowed">>},
    {405, wf:json_encode(Body)}.
