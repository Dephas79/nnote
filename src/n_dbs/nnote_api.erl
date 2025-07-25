-module(nnote_api).

-record(nnote,
        {id = n_utils:create_id(),
         user_id,
         type,
         date,
         event,
         source,
         topic,
         question,
         tags,
         note}).

-export([put_record/1, put_record/2, get_all_values/1, get_all/1, get_record/1, delete/1,
         map_to_record/1, record_to_map/1, get_records_by_type/2, get_records_by_date/3, search/3,
         id/1, user_id/1, date/1, type/1, event/1, source/1, topic/1, question/1, tags/1, note/1,
         id/2, user_id/2, date/2, type/2, event/2, source/2, topic/2, question/2, tags/2, note/2]).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE, nnote).
-define(NODES, [node() | nodes()]).

%% Copy and paste the following functions

put_record(Record) ->
    put_record(Record, []).

put_record(Record, ListOfIndexes) ->
    {ok, Pid} = n_common:open_connection(),
    FormattedDate = qdate:to_string("Y-m-d", date(Record)),
    Record2 = date(Record, FormattedDate),
    Obj = riakc_obj:new(<<"nnote">>, list_to_binary(Record2#nnote.id), Record2),
    ObjMetaData = riakc_obj:get_update_metadata(Obj),
    MD = n_common:set_secondary_indexes(ObjMetaData, ListOfIndexes),
    Obj2 = riakc_obj:update_metadata(Obj, MD),
    riakc_pb_socket:put(Pid, Obj2),
    n_common:close_connection(Pid),
    Record2.

get_all_values(Record) ->
    [_ | Tail] = tuple_to_list(Record),
    Tail.

get_all(UserID) ->
    {ok, Pid} = n_common:open_connection(),
    Index_name = user_id,
    Index_name_ =
        case is_atom(Index_name) of
            true ->
                atom_to_list(Index_name);
            false ->
                Index_name
        end,

    {ok, {_, List_of_matching_objs_keys, _, _}} =
        riakc_pb_socket:get_index(Pid,
                                  <<"nnote">>,
                                  {binary_index, Index_name_},
                                  term_to_binary(UserID)),

    F = fun(Key) ->
           {ok, Record} = riakc_pb_socket:get(Pid, <<"nnote">>, Key),
           binary_to_term(riakc_obj:get_value(Record))
        end,
    List = lists:map(F, List_of_matching_objs_keys),
    n_common:close_connection(Pid),
    List.

get_record(Key) ->
    {ok, Pid} = n_common:open_connection(),
    {ok, Record} = riakc_pb_socket:get(Pid, <<"nnote">>, Key),
    n_common:close_connection(Pid),
    binary_to_term(riakc_obj:get_value(Record)).

delete(Key) ->
    {ok, Pid} = n_common:open_connection(),
    riakc_pb_socket:delete(Pid, <<"nnote">>, Key),
    n_common:close_connection(Pid).

map_to_record(Map) ->
    n_utils:map_to_record(#nnote{}, record_info(fields, nnote), Map).

record_to_map(Record) ->
    n_utils:record_to_map(Record, record_info(fields, nnote)).

get_records_by_type(UserID, Type) ->
    {ok, Pid} = n_common:open_connection(),
    Index_name = user_id,
    Index_name_ =
        case is_atom(Index_name) of
            true ->
                atom_to_list(Index_name);
            false ->
                Index_name
        end,

    {ok, {_, List_of_matching_objs_keys, _, _}} =
        riakc_pb_socket:get_index(Pid,
                                  <<"nnote">>,
                                  {binary_index, Index_name_},
                                  term_to_binary(UserID)),

    F = fun(Key) ->
           {ok, Record} = riakc_pb_socket:get(Pid, <<"nnote">>, Key),
           binary_to_term(riakc_obj:get_value(Record))
        end,
    List = lists:map(F, List_of_matching_objs_keys),
    List2 = [N || N <- List, N#nnote.type >= Type],
    n_common:close_connection(Pid),
    List2.

get_records_by_date(UserID, Type, Date) ->
    DateTime = qdate:to_date(Date),
    {FirstDate, LastDate} = n_dates:date_span(DateTime, 7),
    {ok, Pid} = n_common:open_connection(),
    Index_name = user_id,
    Index_name_ =
        case is_atom(Index_name) of
            true ->
                atom_to_list(Index_name);
            false ->
                Index_name
        end,

    {ok, {_, List_of_matching_objs_keys, _, _}} =
        riakc_pb_socket:get_index(Pid,
                                  <<"nnote">>,
                                  {binary_index, Index_name_},
                                  term_to_binary(UserID)),

    F = fun(Key) ->
           {ok, Record} = riakc_pb_socket:get(Pid, <<"nnote">>, Key),
           binary_to_term(riakc_obj:get_value(Record))
        end,
    List = lists:map(F, List_of_matching_objs_keys),
    List2 =
        [N || N <- List, N#nnote.type >= Type, qdate:between(FirstDate, N#nnote.date, LastDate)],
    n_common:close_connection(Pid),
    List2.

search(_, _, undefined) ->
    [];
search(UserID, NoteType, SearchList) ->
    {ok, Pid} = n_common:open_connection(),
    Index_name = user_id,
    Index_name_ =
        case is_atom(Index_name) of
            true ->
                atom_to_list(Index_name);
            false ->
                Index_name
        end,

    {ok, {_, List_of_matching_objs_keys, _, _}} =
        riakc_pb_socket:get_index(Pid,
                                  <<"nnote">>,
                                  {binary_index, Index_name_},
                                  term_to_binary(UserID)),

    F = fun(Key) ->
           {ok, Record} = riakc_pb_socket:get(Pid, <<"nnote">>, Key),
           binary_to_term(riakc_obj:get_value(Record))
        end,
    List = lists:map(F, List_of_matching_objs_keys),
    List2 = [N || N <- List, N#nnote.type >= NoteType, n_search:filter(SearchList, N)],
    n_common:close_connection(Pid),
    List2.

%% GETTERS
id(Record) ->
    Record#nnote.id.

user_id(Record) ->
    Record#nnote.user_id.

date(Record) ->
    Record#nnote.date.

type(Record) ->
    Record#nnote.type.

event(Record) ->
    Record#nnote.event.

source(Record) ->
    Record#nnote.source.

topic(Record) ->
    Record#nnote.topic.

question(Record) ->
    Record#nnote.question.

tags(Record) ->
    Record#nnote.tags.

note(Record) ->
    Record#nnote.note.

%% SETTERS
id(Record, ID) ->
    Record#nnote{id = ID}.

user_id(Record, UserID) ->
    Record#nnote{user_id = UserID}.

date(Record, Date) ->
    Record#nnote{date = Date}.

type(Record, Type) ->
    Record#nnote{type = Type}.

event(Record, Event) ->
    Record#nnote{event = Event}.

source(Record, Source) ->
    Record#nnote{source = Source}.

topic(Record, Topic) ->
    Record#nnote{topic = Topic}.

question(Record, Question) ->
    Record#nnote{question = Question}.

tags(Record, Tags) ->
    Record#nnote{tags = Tags}.

note(Record, Note) ->
    Record#nnote{note = Note}.
