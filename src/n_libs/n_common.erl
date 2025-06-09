-module(n_common).

-include_lib("nitrogen_core/include/wf.hrl").

-compile(export_all).

-define(PAGE, wf:page_module()).
-define(TEMPLATE, "./priv/templates/n_apps.html").

open_connection() ->
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    {ok, Pid}.

close_connection(Pid) ->
    riakc_pb_socket:stop(Pid).

%%-------------------------------------------------------------------------------------------------------------------------------------
%% Setting secondary Indexes
set_secondary_indexes(MD, []) ->
    MD;
set_secondary_indexes(MD, [{Index_name, Index_value} | Others]) ->
    Index_name_str =
        case is_atom(Index_name) of
            true ->
                atom_to_list(Index_name);
            false ->
                Index_name
        end,
    MD1 = riakc_obj:set_secondary_index(MD,
                                        [{{binary_index, Index_name_str},
                                          [term_to_binary(Index_value)]}]),
    set_secondary_indexes(MD1, Others).

%%----------------------------------------------------------------------------------------------------------------------------------------

template() ->
    Access = get_access(),
    case can_access(Access) of
        true ->
            #template{file = ?TEMPLATE};
        false ->
            wf:redirect_to_login("/login")
    end.

get_access() ->
    case erlang:function_exported(?PAGE, access, 0) of
        true ->
            apply(?PAGE, access, []);
        false ->
            public
    end.

can_access(public) ->
    true;
can_access(private) ->
    n_utils:get_user_id() =/= undefined.

main_menu() ->
    #panel{id = main_menu, body = apply(?PAGE, main_menu, [])}.

get_page_vars() ->
    Vars = apply(?PAGE, url_vars, []),
    lists:foldl(fun(Var, Map) ->
                   {VarName, Value} = get_url_var(Var),
                   maps:put(VarName, Value, Map)
                end,
                #{},
                Vars).

get_url_var({Var, atom}) ->
    {Var,
     wf:to_existing_atom(
         wf:q(Var))};
get_url_var({Var, int}) ->
    {Var,
     wf:to_integer(
         wf:q(Var))};
get_url_var(Var) ->
    {Var, wf:q(Var)}.

sidebar() ->
    Vars = get_page_vars(),
    #panel{id = sidebar, body = apply(?PAGE, sidebar, [Vars])}.

content() ->
    Vars = get_page_vars(),
    [#flash{}, #panel{id = content, body = apply(?PAGE, content, [Vars])}].
