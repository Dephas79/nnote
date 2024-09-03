%% -*- mode: nitrogen -*-
-module(login).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-behaviour(n_apps).
%%*************************************************************
%% Macros [template and title]
%%*************************************************************
-define(MMSELECTED, "home").
-define(TITLE, "Registration Page").
-define(TOP, "Build it with Nitrogen").

url_vars() -> [{task, atom}].
access() -> public.

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
      #p{body="The applications in this framework
               were developed
               Difus and Derrick for their book
               <em>Build it with Nitrogen</em>.
               These applications are available 
               for use and modification
               under the MIT LIscence."}
].

%%*************************************************************
%% panel definitions
%%*************************************************************

content(#{task:=undefined}) ->
    #h3{text="Choose an option from the left"};
content(#{task:=create}) ->
    new_account_form();
content(#{task:=signin}) ->
    signin_form().

new_account_form() ->
    wf:defer(save, username, #validate{validators = [#is_required{text="Username Required"}]}),
    wf:defer(save, email, #validate{validators = [#is_required{text="Email Required"}]}),
    wf:defer(save, password, #validate{validators = [
                #is_required{text="Password Required"}]}),
    wf:defer(save, password2, #validate{validators = [
                #confirm_same{text="Passwords do not match", confirm_id=password}]}),

    [   #h1{text="Create Account"},
        #label{text = "Username"}, #br{},
        #textbox{id=username,placeholder = "Your Username"}, #br{},

        #label{text = "Email"},#br{},
        #textbox{id=email, placeholder = "example@gmail.com"},
        #br{},
        #label{text = "Password"},#br{},
        #password{id=password}, #br{},
        #label{text = "Confirm Password"}, #br{},
        #password{id=password2},

        #br{},
        #button{id=save, text="Save Account", postback=save}
].

signin_form() ->
    wf:defer(signin, username, #validate{validators = [
                #is_required{text="Username Required"}]}),
    wf:defer(signin, password, #validate{validators = [
                #is_required{text="Password Required"}]}),
    [   
        #h1{body="Sign In"},
        #label{text="Username"},#br{},
        #textbox{id=username},#br{},
        #label{text="password"},#br{},
        #password{id=password},
        #br{},
        #button{id=signin, text="Sign In", postback=signin}
    ].

%%*************************************************************
%% Sidebar events
%%*************************************************************
event({open, Task}) ->
    UrlVars = #{task=>Task},
    wf:update(content, content(UrlVars));

event(logout) ->
    UrlVars = #{task=>undefined},
    wf:logout(),
    wf:update(content, content(UrlVars)),
    wf:update(sidebar, sidebar(UrlVars));

event(save) ->
    [Username, Email, Password] = wf:mq([username, email, password]),
    Record = account_api:new_account(Username, Email, Password),
    UserID = account_api:id(Record),
    wf:user(UserID),
    wf:session(username, Username),
    wf:redirect_from_login("/");

event(signin) ->
    [Username, Password] = wf:mq([username, password]),
    case account_api:attempt_login(Username, Password) of
        undefined ->
            wf:flash("Invalid Username or Password. Try Again!");
        Record ->
            UserID = account_api:id(Record),
            wf:user(UserID),
            wf:session(username, Username),
            wf:redirect_from_login("/")
    end.

logged_in_msg(undefined) -> "Not Logged In";
logged_in_msg(Username) -> ["Logged In as ",Username].

sidebar(#{}) ->
    SignedOut = (n_utils:get_user_id()==undefined),
    Username = n_utils:get_nickname(),

    [   #h2{text=logged_in_msg(Username)},
        #button{show_if=SignedOut, text="Create Account",
                postback={open, create}},
        #br{},
        #button{show_if=SignedOut, text="Sign In", postback={open, signin}},
        #br{},
        #button{show_if=not(SignedOut), text="Log Out", postback=logout}
].

