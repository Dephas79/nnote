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

url_vars() -> [id, {note_type, atom}, {task, atom}].

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
        [ n_menus:show_menu_item(MenuItem, Selected) || MenuItem <- n_menus:note_type_side_menu()]
].

%% Save Function
save(ID,UserID, NoteType) ->
     Map = wf:q_map([date, event, source, topic, tags, note]),
    Map2 = Map#{user_id=>UserID,
                type=>NoteType},
    Record = nnote_api:map_to_record(Map2),
    
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
    NoteType2 = wf:to_list(NoteType),
    #h2{class=content, text=[Action, " ", string:titlecase(NoteType2), " Note"]}.

%% This bit below where we're using a Map to pass arguments to form/1 works,
%% but even it has some fragility that might be worth exploring eliminating.
%% The simplest solution would be to make a new nnote_db_mnesia:new() function
%% which simply returns an initialized #nnote{} record. Then you could just use
%% the setters and getters to interact with the record.  This is the safest
%% approach, and would save a few lines of code, but this is manageable.

%%*************************************************************
%%  add/edit form
add_edit_form("new", NoteType) ->
    UserID = n_utils:get_user_id(),
    Date = qdate:to_string("m/d/Y"),
    Map = #{id=>"new",
            user_id=>UserID,
            type=>NoteType,
            date=>Date},
    form(Map);

add_edit_form(ID, _NoteType) ->   
    Record = nnote_api:get_record(ID),
     Map = nnote_api:record_to_map(Record),
    form(Map). 

%% We can only match on the 4 fields we know for sure will be there.
%% The rest of the fields, we'll get with maps:get/3 (the 3rd argument is
%% the default value if the field isn't present in the Map).
form(Map = #{id:=ID,
             user_id:=UserID,
             type:=NoteType,
             date:=Date}) ->
    Event = maps:get(event, Map, ""),
    Source = maps:get(source, Map, ""),
    Question = maps:get(question, Map, ""),
    Tags = maps:get(tags, Map, ""),
    Topic = maps:get(topic, Map, ""),
    Note = maps:get(note, Map, ""),

%% Installing the functions
    ShowEvent = show_event(NoteType),
    ShowSource = show_source(NoteType),
    ShowQuestion = show_question(NoteType),

%% Define the validators
    wf:defer(save_note, topic, #validate{validators = [#is_required{text="Topic required"}]}),
    wf:defer(save_note, note, #validate{validators = [#is_required{text="Note required"}]}),
    ?WF_IF(ShowEvent, wf:defer(save_note, event, #validate{validators = [#is_required{text="Event required"}]})),
    ?WF_IF(ShowSource, wf:defer(save_note, source, #validate{validators = [#is_required{text="SOurce required"}]})),

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
button_text("new") -> "Save";
button_text(_ID) -> "Submit changes".

%% Metadata Funtions
event_label(conference) -> "Conference";
event_label(lecture) -> "Event";
event_label(_) -> "".

source_label(conference) -> "Speaker";
source_label(idea) -> "";
source_label(lab) -> "";
source_label(lecture) -> "Speaker";
source_label(web) -> "URL";
source_label(_) -> "Source".

question_label(conference) -> "";
question_label(idea) -> "";
question_label(web) -> "";
question_label(_) -> "Question".

%% Suppress unnecessary fields
show_event(conference) -> true;
show_event(lecture) -> true;
show_event(_) -> false.

show_source(idea) -> false;
show_source(lab) -> false;
show_source(_) -> true.

show_question(interview) -> true;
show_question(lab) -> true;
show_question(lecture) -> true;
show_question(research) -> true;
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

