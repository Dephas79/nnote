-module(n_utils).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

get_nickname() -> " Marsha".

create_id() ->
    Rand = rand:uniform(1000000000),
    Seconds = qdate:unixtime(),
    GigaSeconds = Seconds * 1000000000,
    ID = GigaSeconds + Rand,
    integer_to_list(ID, 16).

id_created(ID) ->
    IntID = list_to_integer(ID, 16),
    Seconds = IntID div 1000000000,
    qdate:to_date(Seconds).

