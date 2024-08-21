%% -*- mode: nitrogen -*-
-module (register).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-behaviour(n_apps).
%%*************************************************************
%% Macros [template and title]
%%*************************************************************
-define(MMSELECTED, "home").
-define(TITLE, "REgistration Page").
-define(TOP, "Build it with Nitrogen").

url_vars() -> [].
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

content(#{}) ->
    new_account_form().

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

        #label{text = "Password"},#br{},
        #password{id=password}, #br{},
        #label{text = "Confirm Password"}, #br{},
        #password{id=password2},

        #br{},
        #button{id=svae, text="Save Account", postback=save}
].

%%*************************************************************
%% Sidebar events
%%*************************************************************
evet({goto, Link}) ->
    wf:redirect(Link).
sidebar(#{}) ->
    [ 
].

