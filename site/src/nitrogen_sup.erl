%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(nitrogen_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %%n_mnesia:one_time(),
    erlias:build(nnote_db_mnesia, nnote_api),
    erlias:build(account_db_mnesia, account_api),
    application:load(nitrogen_core),
    application:ensure_all_started(nitro_cache),
    application:ensure_all_started(crypto),
    application:ensure_all_started(nprocreg),
    application:ensure_all_started(qdate),
    application:ensure_all_started(simple_bridge),
    application:ensure_all_started(erlpass),
    application:ensure_all_started(canister),

    {ok, { {one_for_one, 5, 10}, []} }.

