-module(account_db_riak).
-record(n_account, {
    id = n_utils:create_id(),
    username,
    email,
    date = qdate:unixtime(),
    pwhash
}).

-export([ get_records_by_username/1,
          put_record/1,
          get_all_values/1,
          update_read/1,
          update_write/2,
          update_write/3,
          get_all/0,
          get_record/1,
          delete/1,
          map_to_record/1,
          record_to_map/1,
          new_account/3,
          attempt_login/2,
          id/1,
          username/1,
          email/1,
          date/1,
          pwhash/1,
          id/2,
          username/2,
          email/2,
          date/2,
          pwhash/2
        ]).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE, n_account).
-define(NODES, [node()|nodes()]).
%% Copy and paste the following functions
put_record(Record) ->
	put_record(Record, []).

put_record(Record, ListOfIndexes) ->
	{ok, Pid} = n_common:open_connection(),
    FormattedDate = qdate:to_string("Y-m-d", date(Record)),
    Record2 = date(Record, FormattedDate),
    Obj = riakc_obj:new(<<"n_account">>,list_to_binary(Record2#n_account.id), Record2),
    ObjMetaData = riakc_obj:get_update_metadata(Obj),
    MD = n_common:set_secondary_indexes(ObjMetaData, ListOfIndexes),
    Obj2 = riakc_obj:update_metadata(Obj, MD),
    riakc_pb_socket:put(Pid, Obj2),
    n_common:close_connection(Pid).

update_read(Key) ->
    %% Put a write lock immediately as you read to update a record
        %% Note: The record is not stored in mnesia, but lets mnesia
        %% lock out updating processes from reading a record from riak
        %% until the current process has finished updating the record
        %% and removed the write lock on the record.
        %% Note - the order of calls matters
        mnesia:read(n_account, Key, write),
        get_record(Key).

update_write(Key, Record) ->
    update_write(Key, Record, []).

update_write(Key, Record, ListOfIndexes) ->
    mnesia:write(n_account, Record, write),
    put_record(Record, ListOfIndexes),
    mnesia:delete(n_account, Key, write).

get_all_values(Record) ->
    [_|Tail] = tuple_to_list(Record),
    Tail.

get_all() ->
    [].
get_record(Key) ->
	{ok,Pid} = n_common:open_connection(),
	{ok, Record} = riakc_pb_socket:get(Pid, <<"n_account">>, Key),
    n_common:close_connection(Pid),
    binary_to_term(riakc_obj:get_value(Record)).

delete(Key) ->
    {ok, Pid} = n_common:open_connection(),
  riakc_pb_socket:delete(Pid, <<"n_account">>, Key),
  n_common:close_connection(Pid).
map_to_record(Map) ->
    n_utils:map_to_record(#n_account{}, record_info(fields, n_account), Map).

record_to_map(Record) ->
    n_utils:record_to_map(Record, record_info(fields, n_account)).

get_records_by_username(Username) ->

    {ok,Pid} = n_common:open_connection(),
    Index_name = username,
    Index_name_ = case is_atom(Index_name) of 
            true -> atom_to_list(Index_name);
            false -> Index_name 
        end,

    {ok, {_,List_of_matching_objs_keys,_,_}} = riakc_pb_socket:get_index(Pid, <<"n_account">>, {binary_index,Index_name_}, term_to_binary(Username)),
    
    F = fun(Key) -> 
        	{ok, Record} = riakc_pb_socket:get(Pid, <<"n_account">>, Key),
	        binary_to_term(riakc_obj:get_value(Record))
         end,
    List = lists:map(F,List_of_matching_objs_keys),
    n_common:close_connection(Pid),
    List.

%%***************************************************************************************
%% Login Details
new_account(Username, Email, Password) ->
    PWHash = erlpass:hash(Password),
    Record = #n_account{username=Username,
                        email=Email,
                        pwhash=PWHash},
    put_record(Record,[{username,Username}]),
    Record.

attempt_login(UserName, Password) ->
    io:format("Username:~p~n password: ~p~n",[UserName,Password]),
    Records = get_records_by_username(UserName),
    case Records of 
        [] -> undefined;
        [Record] -> 
            PWHash = Record#n_account.pwhash,
            io:format("PWHash: ~p~n",[PWHash]),
            case erlpass:match(Password, PWHash) of 
                false -> undefined;
                true -> Record
            end
        end.

%% GETTERS
id(Record) -> Record#n_account.id.
username(Record) -> Record#n_account.username.
email(Record) -> Record#n_account.email.
date(Record) -> Record#n_account.date.
pwhash(Record) -> Record#n_account.pwhash.

%% SETTERS
id(Record, ID) -> Record#n_account{id=ID}.
username(Record, Username) -> Record#n_account{username = Username}.
email(Record, Email) -> Record#n_account{email = Email}.
date(Record, Date) -> Record#n_account{date = Date}.
pwhash(Record, PWHash) -> Record#n_account{pwhash = PWHash}.
