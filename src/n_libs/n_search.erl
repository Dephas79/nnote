-module(n_search).
-compile(export_all).
%%-----------------------------------------------------------------------
%% @doc Search by keyword
unique_words_from_string(String) ->
    Normalized = normalized(String),
    Tokens =string:lexemes(Normalized, " "),
    sets:from_list(Tokens).

unique_words_from_record(Record) ->
    Values = nnote_api:get_all_values(Record),
    Sets = [unique_words_from_string(Value) || Value <- Values, is_list(Value)],
    sets:union(Sets).
normalized(String) ->
    Clean = re:replace(String, "[,.?!()-]", "", [global,{return, list}]),
    string:to_upper(Clean).

filter(SearchString, Record) ->
    NoteSet = unique_words_from_record(Record),
    SearchSet = unique_words_from_string(SearchString),
    ShareWords = sets:intersection(NoteSet, SearchSet),
    sets:size(ShareWords) > 0.

