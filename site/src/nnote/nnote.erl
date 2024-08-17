%% -*- mode: nitrogen -*-
-module (nnote).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-behaviour(n_apps).
%%*************************************************************
%% Macros [template and title]
%%*************************************************************
-define(MMSELECTED, "nnote").
-define(TITLE, "Welcome to nnnote!").
-define(TOP, "nnote").

url_vars() -> [id, note_type, task].

%%***********************************************************
%% Page state functions
%%***********************************************************

main() -> n_common:template().

title() -> ?TITLE.
top() ->
    #h1 {text=?TOP}.
main_menu() ->
   %% #panel { id=main_menu, body=
    n_menus:show_main_menu(?MMSELECTED).
%%}.
%%************************************************************
%% Tips

tips()->
    [ #h2{text="Tips & Info"},
      #p{body="The applications in this framework were developed
               Difus and Derrick for their book
               <em>Build it with Nitrogen</em>.
               These applications are available for use and modification
               under the MIT LIscence."}
].

%%*************************************************************
%% side menus
%%*************************************************************
side_menu("NOTE TYPE") ->
    [ { "conference",{select,"conference"}},
    {"idea",         {select,"idea"}},
    {"interview",    {select,"interview"}},
    {"lab",          {select,"lab"}},
    {"lecture",      {select,"lecture"}},
    {"research",    {select,"research"}},
    {"web",          {select,"web"}}
].

%%*************************************************************
%% Sidebar events 
event({select, NoteType}) ->
    Redirect = [wf:path(), "?",
            wf:to_qs([{note_type, NoteType}])],
    wf:redirect(Redirect).

sidebar(#{note_type:=NoteType}) ->
    [ #h4 {text="SELECT"},
      show_side_menu("NOTE TYPE", NoteType)
].

show_side_menu(Menu, Selected) ->
    [ #h4 {class=select, text=Menu},
        [ n_menus:show_menu_item(MenuItem, Selected) || MenuItem <- side_menu(Menu)]
].

%%*************************************************************
%% Content executives

content(#{note_type:=undefined, task:=undefined}) ->
    [ content_headline(),
    #p{text="Select note type."}
    %%search_by_tag()
];
content(#{note_type:=NoteType, task:=Task}) ->
     Records = case Task of 
         undefined -> undefined;
         search_by_tag -> tag_search(NoteType);
         search_by_date -> date_search(NoteType)
 end,
 display_forms(NoteType, Records);
content(#{}) ->
    [content_headline(),
   #p{text="Select note type."}
    ].

content_headline() ->
    [#h2{class=content, text="My Notes"}].

tag_search(_NoteType) -> [].
date_search(_NoteType) -> [].

%%********************************************************************
%% Content
%%********************************************************************
display_forms(NoteType, Records) ->
    [content_headline(),
    add_note_button(NoteType),
    search_by_tag(),
    search_by_date(),
    search_results(Records)
].

search_results(undefined) -> [];
    %%io:format("Show nothing");
search_results([]) -> 
    [#hr{},
    #h2{text="Search Results"},
    #p{text="No notes found"}    
];
    %%io:format("No notes found");
search_results(Records) ->
    [#hr{},
    #h2{text="Search results"},
    [n_utils:draw_link(Record) || Record <- Records]
].
   %% io:format("Display results").

add_note_button(NoteType) ->
    ButtonText = ["Enter new",NoteType,"note"],
    #button{text=ButtonText, postback={add_note, NoteType}}.

search_by_tag() ->
   [#label{text="enter search words"},
   #textbox{id=search_words},
   #button{text="Search", postback=search_by_tag},
   #button{text="Info", postback={info, search_by_tag}}
   ].
search_by_date() ->
    io:format("Search by date~n"),
    [].

%%*************************************************************
%% Sidebar events
%%*************************************************************

