-module(n_utils).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

get_nickname() -> " Marsha".
get_user_id() -> "123".

create_id() ->
    Rand = rand:uniform(1000000000),
    Seconds = qdate:unixtime(),
    GigaSeconds = Seconds * 1000000000,
    ID = GigaSeconds + Rand,
    integer_to_list(ID, 36).

id_created(ID) ->
    IntID = list_to_integer(ID, 36),
    Seconds = IntID div 1000000000,
    qdate:to_date(Seconds).

% field_names(FieldList) ->
%     [atom_to_list(FieldName) || FieldName <- FieldList ].
% cap_field_names(FieldNames) ->
%     [string:titlecase(String) || String <- FieldNames].

% fieldnames_to_params(FieldNames) ->
%     Caps = cap_field_names(FieldNames),
%     string:join(Caps, ", ").

% to_function_head(FieldList) ->
%     FieldNames = field_names(FieldList),
%     Params = fieldnames_to_params(FieldNames),
%     lists:flatten(["populate_record([", Params, "]) ->"]).

% to_function_body(Record, FieldList) ->
%     FieldNames = field_names(FieldList),
%     Caps = cap_field_names(FieldNames),
%     Zip = lists:zip(FieldNames, Caps),
%     Assignments = lists:map(fun({Name, Cap}) ->
%         Name ++ " = " ++ Cap
%     end, Zip),

%     Delimited = string:join(Assignments, ", "),
%     lists:flatten([" #", Record, "{", Delimited, "}."]).

% synthesize_populate_record(Record, FieldList) ->
%     Head = to_function_head(FieldList),
%     Body = to_function_body(Record, FieldList),
%     io:format("~s~n~s~n", [Head, Body]).

%%% THE BELOW IS DONE FOR CHAPTER 10 HOMEWORK #5
%%% This logic will work for converting any record to Map

%% This should be called like follows:
%% map_to_record(#nnote{}, record_info(fields, nnote), Map).
%% There are other ways to make this record that might be easier, but this is
%% a pretty future-proof implementation

%% All of this is unnecessary if you're using dynarec: https://github.com/dieswaytoofast/dynarec
map_to_record(EmptyRecord, FieldList, Map) ->

    %% NumberedFields is effectively a proplist to look up field name and get the field number
    NumberedFields = number_fields(FieldList),

    %% Converts the map to a proplist so we can easily iterate over the items
    MapList = maps:to_list(Map),

    %% Now we're going to iterate overall fields in the map
    lists:foldl(fun({Field, Value}, Record) ->

        %% Is the field is an actual field in the record
        case lists:member(Field, FieldList) of
            false ->
                %% If not, just skip it and move onto the next
                Record;
            true ->
                %% Get the field number
                FieldNum = proplists:get_value(Field, NumberedFields),
                %% and set the value in the record.
                %% We set it to FieldNum+1 because if you recall, the first element in a record tuple
                %% is the name of the tuple.
                setelement(FieldNum+1, Record, Value)
        end
    end, EmptyRecord, MapList).

%% Just like map_to_record, this should be called as follows:
%% record_to_map(NNoteRecord, record_info(fields, nnote)).
record_to_map(Record, FieldList) ->
    %% Proplistify the numbered fields
    NumberedFields = number_fields(FieldList),

    %% This time we iterate over the numbered fields
    lists:foldl(fun({Field, FieldNum}, Map) ->
        %% Get the value from the record
        Value = element(FieldNum+1, Record),

        %% Put the value into the properly named field in the map
        maps:put(Field, Value, Map)
    end, #{}, NumberedFields).

%% Converts a list of fields to numbered fields:
%% For example:
%% > numbered_fields([a,b,c])
%% [{a,1}, {b,2}, {c,3}].
number_fields(FieldList) ->
    lists:zip(FieldList, lists:seq(1, length(FieldList))).

draw_link(Record) ->
    ID = nnote_api:id(Record),
    NoteType = nnote_api:type(Record),
    Date = nnote_api:date(Record),
    Date2 = qdate:to_string("m/d/Y", Date),
    Topic = nnote_api:topic(Record),
    EditUrl = ["/nnote/add_edit?",
    wf:to_qs([{id, ID}, {note_type, NoteType}])],
    Menuid = wf:temp_id(),
    
        Wrapperid = wf:temp_id(),
        #panel{id=Wrapperid, body=[
            #link {
                body=[Date2, " ", "&#8212;", " ", Topic],
                click=#toggle{target=Menuid}
                },
                
                #panel{id=Menuid, style="display:none", body=[
                #link {text="edit", url=EditUrl},
                " | ",
                #link {text="delete", postback={delete, ID, Wrapperid}}
                ]},
            #br{}
        ]}.

