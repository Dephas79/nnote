-module(n_common).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).
-define(PAGE, (wf:page_module())).
-define(TEMPLATE, "./site/templates/n_apps.html").

template() ->
    #template{file=?TEMPLATE}.
main_menu() ->
    #panel{id=main_menu, body=?PAGE:main_menu()}.

get_page_vars() ->
    Vars = ?PAGE:url_vars(),
    wf:q_map(Vars).

sidebar() ->
    Vars = get_page_vars(),
    #panel {id=sidebar, body=?PAGE:sidebar(Vars)}.

content() ->
    Vars = get_page_vars(),
    [
    #flash{},
    #panel {id=content, body=?PAGE:content(Vars)}
].

