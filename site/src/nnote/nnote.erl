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

%% Info
info(search_by_tag) ->
    [ #h2{body=["<i>Search Words</i>"]},
    #p{text=["Search word documentation goes here"]}
];

info(search_by_date) ->
    [ #h2{body="<i>Search Date</i>"},
      #p{text="Search date documentation goes here"}
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

event(search_by_tag) ->
    NoteType = wf:q(note_type),
    Content = content(#{note_type=>NoteType, task=>search_by_tag}),
    wf:update(content, Content);

event(search_by_date) ->
    NoteType = wf:q(note_type),
    Content = content(#{note_type=>NoteType, task=>search_by_date}),
    wf:update(content, Content);

event({add_note, NoteType}) ->
    Redirect=["/nnote/add_edit?",
        wf:to_qs([{id,"new"}, {note_type,NoteType}])],
        wf:redirect(Redirect);

%% Info Events
event({info, Function}) ->
    wf:flash(info(Function));

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

tag_search(NoteType) -> 
    UserID = n_utils:get_user_id(),
    SearchList = wf:q(search_words),
    nnote_api:search(UserID, NoteType, SearchList).

date_search(NoteType) -> 
    UserID = n_utils:get_user_id(),
    Date = wf:q(search_date),
    nnote_api:get_records_by_date(UserID, NoteType, Date).

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
    ButtonText = ["Enter ",NoteType," note"],
    #button{text=ButtonText, postback={add_note, NoteType}}.

search_by_tag() ->
   [ #br{},
   #label{text="enter search words"},
   #br{},
   #textbox{id=search_words},
   #button{text="Search", postback=search_by_tag},
   #button{text="Info", postback={info, search_by_tag}}
   ].
search_by_date() ->
    [ #br{},
      #label{text="Enter date"},
      #br{},
      n_dates:datepicker(search_date, ""),
      #button{text="Search", postback=search_by_date},
      #button{text = "Info", postback = {info, search_by_date}}
].

