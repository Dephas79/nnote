-module(n_menus).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%%***************************************************************
%% Main Menu
%%***************************************************************
main_menu() ->
    [
    {"home",      "/"},
    {"nindex",    "/nindex"},
    {"nnote",     "/nnote"},
    {"Tips & Info", tips}
].

%%**************************************************************
%% Main Menu Executive
%%**************************************************************
show_main_menu(Selected) ->
    MenuList = main_menu(),
    [show_main_menu_item(MenuItem, Selected) || MenuItem <- MenuList].

%%**************************************************************
%% Menu Helpers
%%**************************************************************
show_main_menu_item(MenuItem, Selected) ->
    {Text, Postback} = MenuItem,
    Class = if_selected(Text, Selected),
    #link {class=Class, text=Text, postback=Postback, delegate=?MODULE}.

if_selected(Text, Selected) ->
    case text == Selected of 
        true -> "mmselected";
        false -> "mm"
    end.

%%***************************************************************
%% Main menu events
%%***************************************************************
event(tips) ->
    ok;
event(logout) ->
    wf:logout(),
    wf:redirect("/");
event(URL) -> wf:redirect(URL).

%%**************************************************************
%% Side Bar menus
%%**************************************************************
show_menu_item(MenuItem, Selected) ->
    {Text, Postback} = MenuItem,
    [#radio{name=side_menu_item,
            text=Text,
            checked= (Text==Selected),
            value=Text,
            postback=Postback
},
#br{}].

