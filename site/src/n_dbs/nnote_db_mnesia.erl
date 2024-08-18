-module(nnote_db_mnesia).
-record(nnote, {
    id = n_utils:create_id(),
    user_id,
    type,
    date,
    event,
    source,
    topic,
    question,
    tags,
    note
}).
-export([ init_table/0
          
]).
-include_lib("stdlib/include/qlc.hrl").
-define(TABLE, nnote).

init_table() ->
    mnesia:create_table(?TABLE,
            [ {disc_copies, [node()]},
              {attributes, record_info(fields, ?TABLE)}
            ]).

