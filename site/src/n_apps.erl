-module(n_apps).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).
-optional_callbacks([access/0]).

-callback main() -> body().
-callback access() -> private | public. %% returns either, atoms, tuples,string, binary, iolists
-callback url_vars() -> [atom()]. %% returns a list of atoms
-callback main_menu() -> body().
-callback sidebar(map()) -> body().
-callback content(map()) -> body().

