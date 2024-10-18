-module(notes).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%% REST API 
main() ->
    case wf:request_method() of 
        post -> 
            JsonBody = wf:request_body(),
            % RequestHeader = wf:request_header(),
            % io:format("Request Body:~p~n",[JsonBody])   
            Body = wf:json_decode(JsonBody),
            Topic = proplists:get_value(<<"topic">>, Body),
            Type = proplists:get_value(<<"type">>, Body),
            Event = proplists:get_value(<<"event">>, Body),
            Source = proplists:get_value(<<"source">>, Body),
            Question = proplists:get_value(<<"question">>, Body),
            Tags = proplists:get_value(<<"tags">>, Body),
            Note = proplists:get_value(<<"note">>, Body),
            UserID = "D4VCCWSEXQH0",
            Date = qdate:to_string("m/d/Y"),
            Map2 = #{ user_id => UserID, date => Date, topic => binary_to_term(Topic), type => binary_to_term(Type),
                     event => binary_to_term(Event), source => binary_to_term(Source), question => binary_to_term(Question), tags => Tags, note => binary_to_term(Note)},
            Record = nnote_api:map_to_record(Map2),
            Record2 = nnote_api:put_record(Record,[{user_id,UserID},{date,nnote_db_riak:date(Record)},{type,Type}]),
            Map3 = nnote_api:record_to_map(Record2),
            List = proplists:from_map(Map3),
            wf:status_code(201),
            wf:content_type("application/json"),
            wf:json_encode(List);
        _Any -> 
            wf:status_code(404),
            wf:content_type("application/json"),
            wf:json_encode([{"responseStatus",<<"404">>}, {"responseStatusMeaning", <<"Method Not Supported">>}])
    end.

