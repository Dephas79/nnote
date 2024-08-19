%% -*- mode: nitrogen -*-
%% @doc nnote add/edit

-module (nnote_add_edit).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-behaviour(n_apps).
%%*************************************************************
%% Macros [template and title]
%%*************************************************************
-define(MMSELECTED, "nnote").
-define(TITLE, "Add/Edit note").
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
%%*************************************************************
%% Sidebar events 
event({select, NoteType}) ->
    Redirect = [wf:path(), "?",
            wf:to_qs([{id, "new"}, {note_type, NoteType}])],
    wf:redirect(Redirect);

event({save_note, ID, UserID, NoteType}) ->
    wf:wire(#confirm{text = "Save?",
                    postback={confirm_save, ID, UserID, NoteType}});

event({confirm_save, ID, UserID, NoteType}) ->
    save(ID, UserID, NoteType);

event(cancel) ->
    wf:redirect("/nnote").

sidebar(#{note_type:=NoteType}) ->
    [ #h4 {text="SELECT"},
      show_side_menu("NOTE TYPE", NoteType)
].

show_side_menu(Menu, Selected) ->
    [ #h4 {class=select, text=Menu},
        [ n_menus:show_menu_item(MenuItem, Selected) || MenuItem <- side_menu(Menu)]
].

%% Save Function
save(ID,UserID, NoteType) ->
    Params = wf:mq([date, event, source, topic, question, tags, note]),
    Params2 = [UserID, NoteType | Params],
    Record = nnote_api:populate_record(Params2),
    Record2 = case ID of 
        "new" -> Record;
        _ -> nnote_api:id(Record, ID)
    end,
    nnote_api:put_record(Record2),
    Redirect = ["/nnote", "?",
        wf:to_qs([{note_type, NoteType}])],
    wf:redirect(Redirect).
%%*************************************************************
%% Content executives
content(#{id:=undefined, note_type:=undefined}) ->
    #h2{class=content, text="My Notes"};
content(#{id:=ID, note_type:=NoteType}) ->
   [ 
    content_headline(ID, NoteType),
    add_edit_form(ID, NoteType)
    ].

content_headline(ID, NoteType) ->
    Action = case ID of
        "new" -> "Enter";
        _ -> "Edit"
    end,
    #h2{class=content, text=[Action, " ", string:titlecase(NoteType), " Note"]}.

%%*************************************************************
%%  add/edit form
add_edit_form("new", NoteType) ->
    UserID = n_utils:get_user_id(),
    Date = qdate:to_string("Y-m-d"),
    form("new", UserID, NoteType, Date, "", "", "", "", "", "");

add_edit_form(ID, NoteType) ->
    Record = nnote_api:get_record(ID),
    [ID, UserID, NoteType, Date, Event, Source, Topic, Question, Tags, Note] =
        nnote_api:get_all_values(Record),
    form(ID, UserID, NoteType, Date, Event, Source, Topic, Question, Tags, Note)
    .
form(ID, UserID, NoteType, Date, Event, Source, Topic, Question, Tags, Note)  ->
%% Installing the functions
    ShowEvent = show_event(NoteType),
    ShowSource = show_source(NoteType),
    ShowQuestion = show_question(NoteType),

%% Define the validators
    wf:defer(save_note, topic, #validate{validators = [#is_required{text="Topic required"}]}),
    wf:defer(save_note, note, #validate{validators = [#is_required{text="Note required"}]}),
    wf:defer(save_note, event, #validate{validators = [#is_required{text="Event required"}]}),
    wf:defer(save_note, source, #validate{validators = [#is_required{text="SOurce required"}]}),

        [   #label{text="Date"}, #br{},
            n_dates:datepicker(date, Date), #br{},
            #label{text = event_label(NoteType), show_if=ShowEvent}, #br{},
            #textbox{id=event, text = Event, show_if=ShowEvent},#br{},
            #label{text = source_label(NoteType), show_if=ShowSource}, #br{},
            #textbox{id=source, text = Source, show_if=ShowSource},#br{},
            #label{text = "Topic"}, #br{},
            #textbox{id=topic, text = Topic},
            #label{text = question_label(NoteType), show_if=ShowQuestion},
            #textbox{id=question, text = Question, show_if=ShowQuestion}, #br{},
            #label{text = "Search Words"}, #br{},
            #textbox{id=tags, text = Tags}, #br{},
            #label{text = "Note"}, #br{},
            #textarea{id=note, text = Note},
            #br{},
            #button{id=save_note, text = "Submit", postback = {save_note, ID, UserID, NoteType}},
            #button{text = "Cancel", postback = cancel}
    ].
%%************************************************************************
%% Content Helpers
button_text("nwe") -> "Enter new note";
button_text(_ID) -> "Submit changes".

%% Metadata Funtions
event_label("conference") -> "Conference";
event_label("lecture") -> "Event";
event_label(_) -> "".

source_label("conference") -> "Speaker";
source_label("idea") -> "";
source_label("lab") -> "";
source_label("lecture") -> "Speaker";
source_label("web") -> "URL";
source_label(_) -> "Source".

question_label("conference") -> "";
question_label("idea") -> "";
question_label("web") -> "";
question_label(_) -> "Question".

%% Suppress unnecessary fields
show_event("conference") -> true;
show_event("lecture") -> true;
show_event(_) -> false.

show_source("idea") -> false;
show_source("lab") -> false;
show_source(_) -> true.

show_question("interview") -> true;
show_question("lab") -> true;
show_question("lecture") -> true;
show_question("research") -> true;
show_question(_) -> false.

search_results([]) -> 
    [#hr{},
    #h2{text="Search Results"},
    #p{text="No notes found"}    
];

search_results(Records) ->
    [#hr{},
    #h2{text="Search results"},
    [n_utils:draw_link(Record) || Record <- Records]
].

