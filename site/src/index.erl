%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
%%*************************************************************
%% Macros [template and title]
%%*************************************************************
-define(TEMPLATE, "./site/templates/n_apps.html").
-define(MMSELECTED, "home").
-define(TITLE, "Welcome!").
-define(TOP, "Build it with Nitrogen").
-define(UVARS, [id, note_type, task]).

%%***********************************************************
%% Page state functions
%%***********************************************************
get_page_vars() ->
    wf:q_map(?UVARS).

main() -> #template { file=?TEMPLATE }.

title() -> ?TITLE.
top() ->
    #h1 {text=?TOP}.
main_menu() ->
    #panel { id=main_menu, body=
    n_menus:show_main_menu(?MMSELECTED)
}.
%%************************************************************
%% Tips

tips()->
    [ #h2{text="Tips & Info"},
      #p{body="The applications in this framework were developed
               Difus abd Derrick for their book
               <em>Build it with Nitrogen</em>.
               These applications are available for use and modification
               under the MIT LIscence."}
].

%%*************************************************************
%% panel definitions
%%*************************************************************
sidebar() ->
    Vars = get_page_vars(),
    #panel {id=sidebar, body=sidebar(Vars)}.

content() ->
    Vars = get_page_vars(),
    #panel {id=content, body=content(Vars)}.

content(#{}) ->
    greeting().

greeting() ->
    [#h2{text=["Welcome to ", n_utils:get_nickname(), "'s",
                " Nitrogen Application!"]},
     #p{body="Our moto: <em>\"Build it Fast with Nitrogen\"/em}
    ].

%%*************************************************************
%% side menus
%%*************************************************************
side_menu("WEB SITE") ->
    [ {"nitrogen", {goto,
            "http://nitrogenproject.com/"}},
      {"erlang", {goto,
            "http://erlang.org/doc/apps/stdlib/"}},
      {"hacker news", {goto,
            "https://news.ycombinator.com/"}}
].

%%*************************************************************
%% Sidebar events
%%*************************************************************
evet({goto, Link}) ->
    wf:redirect(Link).

sidebar(#{}) ->
    [ #h3 {text="SELECT"},
      show_side_menu("WEB SITE", unselected)
].

show_side_menu(Menu, Selected) ->
    [ #h4 {class=select, text=Menu},
        [ n_menus:show_menu_item(MenuItem, Selected) || MenuItem <- side_menu(Menu)]
].

