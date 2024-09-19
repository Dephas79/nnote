-module(n_mnesia).
-export([   init_tables/0,
            one_time/0,
            start/0,
            info/0,
            stop/0
]).
-define(NODES, [node()|nodes()]).
% start() ->
%     one_time(),
%     mnesia:start().
init_tables() ->
    nnote_db_mnesia:init_table(),
    account_db_mnesia:init_table().

one_time() ->
    schema(),
    init_tables().

schema() ->
    [rpc:call(Node, mnesia, stop, [])|| Node <- ?NODES],
    case mnesia:create_schema(?NODES) of
        ok  -> ok;
        {error, {_, {already_exists, _}}} -> ok;
        Other  -> exit(Other)
    end,
    [rpc:call(Node, mnesia, start, [])|| Node <- ?NODES].

start() ->  mnesia:start().
info() ->   mnesia:info().
stop() ->   mnesia:stop().

