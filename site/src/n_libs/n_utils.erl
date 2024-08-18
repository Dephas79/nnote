-module(n_utils).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

get_nickname() -> " Marsha".

create_id() ->
    Rand = rand:uniform(1000000000),
    Seconds = qdate:unixtime(),
    GigaSeconds = Seconds * 1000000000,
    ID = GigaSeconds + Rand,
    integer_to_list(ID, 16).

id_created(ID) ->
    IntID = list_to_integer(ID, 16),
    Seconds = IntID div 1000000000,
    qdate:to_date(Seconds).

field_names(FieldList) ->
    [atom_to_list(FieldName) || FieldName <- FieldList ].
cap_field_names(FieldNames) ->
    [string:titlecase(String) || String <- FieldNames].

fieldnames_to_params(FieldNames) ->
    Caps = cap_field_names(FieldNames),
    string:join(Caps, ", ").

to_function_head(FieldList) ->
    FieldNames = field_names(FieldList),
    Params = fieldnames_to_params(FieldNames),
    lists:flatten(["populate_record([", Params, "]) ->"]).

to_function_body(Record, FieldList) ->
    FieldNames = field_names(FieldList),
    Caps = cap_field_names(FieldNames),
    Zip = lists:zip(FieldNames, Caps),
    Assignments = lists:map(fun({Name, Cap}) ->
        Name ++ " = " ++ Cap
    end, Zip),
    Delimited = string:join(Assignments, ", "),
    lists:flatten([" #", Record, "{", Delimited, "}."]).

synthesize_populate_record(Record, FieldList) ->
    Head = to_function_head(FieldList),
    Body = to_function_body(Record, FieldList),
    io:format("~s~n~s~n", [Head, Body]).

