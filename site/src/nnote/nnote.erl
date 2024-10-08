%% -*- mode: nitrogen -*-
-module (nnote).
-compile(export_all).
-behavior(n_apps).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros
%% ***************************************************
-define(MMSELECTED, "nnote").
-define(TITLE, "Welcome to nnote!").
-define(TOP, "nnote").
url_vars() -> [id, {note_type, atom}, {task, atom}].

%% ***************************************************
%% Template and Title
%% ***************************************************

main() ->
    n_common:template().

title() ->
    ?TITLE.

%% ***************************************************
%% Panel definitions
%% ***************************************************

top() ->
    #h1 {text=?TOP}.

main_menu() ->
    n_menus:show_main_menu(?MMSELECTED).

content(#{note_type:=undefined, task:=undefined}) ->
    [content_headline(),
     #p{text="Select note type."}
    ];
content(#{note_type:=NoteType, task:=Task}) ->
    Records = records_from_task(NoteType, Task),
    display_forms(NoteType, Records).

records_from_task(_, undefined) -> undefined;
records_from_task(NoteType, search_by_tag) -> tag_search(NoteType);
records_from_task(NoteType, search_by_date) -> date_search(NoteType).

tag_search(NoteType) ->
    UserID = n_utils:get_user_id(),
    SearchList = wf:q(search_words),
    nnote_api:search(UserID, NoteType, SearchList).

date_search(NoteType) ->
    UserID = n_utils:get_user_id(),
    Date = wf:q(search_date),
    nnote_api:get_records_by_date(UserID, NoteType, Date).

search_results(undefined) ->
    [];
search_results([]) ->
    [#hr{},
     #h2{text="Search Results"},
     #p{text="No notes found"}
    ];
search_results(Records) ->
    [#hr{},
     #h2{text="Search Results"},
     [n_utils:draw_link(Record) || Record <- Records]
    ].

%% ***************************************************
%% Content
%% ***************************************************
display_forms(NoteType, Records) ->
    [content_headline(),
     add_note_button(NoteType),
     search_by_tag(NoteType),
     search_by_date(NoteType),
     #panel{id=search_results, body=search_results(Records)}
    ].

content_headline() ->
    [#h2{class=content, text="My Notes"}].

add_note_button(NoteType) ->
    ButtonText = ["Enter new ",NoteType," note"],
    #button{text=ButtonText, postback={add_note, NoteType}}.

search_by_tag(NoteType) ->
    [#br{},
     #label{text="enter search words"},#br{},
     #textbox{id=search_words},#br{},
     #button{text="Search", postback={search_by_tag, NoteType}},
     #button{text="Info", postback={info, search_by_tag}},#br{}
    ].

 search_by_date(NoteType) ->
     io:format("Search by date~n"),
     [ #label{text="enter date"},#br{},
       n_dates:datepicker(search_date, ""),#br{},
       #button{text="Search", postback={search_by_date, NoteType}},
       #button{text="Info", postback={info, search_by_date}}
     ].

%% ***************************************************
%% Tips
%% ***************************************************
tips() ->
    [ #h2{text="Tips & Info"},
      #p{body="The applications in this framework
         were developed by Jesse Gumm and
         Lloyd R. Prentice for their book
         <em>Build it with Nitrogen</em>. These
         applications are available for use and
         modification under the MIT License."}
    ].

%% ***************************************************
%% Info
%% ***************************************************
info(search_by_tag) ->
    [ #h2{body=["<i>Search Words</i>"]},
      #p{text=["Search word documentation goes here"]}
    ];
info(search_by_date) ->
    [ #h2{body="<i>Search Date</i>"},
      #p{text="Search date documentation goes here"}
    ].

event({SearchTask, NoteType}) when SearchTask==search_by_tag;
                                   SearchTask==search_by_date ->
    Records = records_from_task(NoteType, SearchTask),
    wf:update(search_results, search_results(Records));
event({add_note, NoteType}) ->
    Redirect=["/nnote/add_edit?",
              wf:to_qs([{id,"new"}, {note_type,NoteType}])],
    wf:redirect(Redirect);

event({delete, ID, Wrapperid}) ->
        wf:wire(#confirm{text="Really delete this note?",
                         postback={confirm_delete, ID, Wrapperid}});

event({confirm_delete, ID, Wrapperid}) ->
        wf:remove(Wrapperid),
            nnote_api:delete(ID);

%% ***************************************************
%% Info events
%% ***************************************************
event({info, Function}) ->
    wf:flash(info(Function));
%% ***************************************************
%% Sidebar events
%% ***************************************************
event({select, NoteType}) ->
    Redirect = [wf:path(), "?",
                wf:to_qs([ {note_type, NoteType} ]) ],
    wf:redirect(Redirect).

%% ***************************************************
%% Sidebar executives
%% ***************************************************
sidebar(#{note_type:=NoteType}) ->
    [ #h3 {text="SELECT"},
      show_side_menu("NOTE TYPE", NoteType)
    ].

%% ***************************************************
%% Sidebar functions
%% ***************************************************
show_side_menu(Menu, Selected) ->
    [ #h4 {class=select, text=Menu},
      [n_menus:show_menu_item(MenuItem, Selected) ||
       MenuItem <- n_menus:note_type_side_menu()]
    ].

