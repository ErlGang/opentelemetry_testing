-module(span_matcher).

-include_lib("kernel/include/logger.hrl").

-define(FAILED_MATCH(Map), failure_map(Map, ?FUNCTION_NAME)).
-define(FAILED_MATCH(Value, Pattern),
        ?FAILED_MATCH(#{value => Value, pattern => Pattern})).

-define(MATCH_VALUE(Value, Pattern), match_value(Value, Pattern)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% type definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type value() :: term().
-type pattern() :: term().
-type failure_map() :: #{
                         matcher := atom(),
                         reason := atom(),
                         term() => term()
                        }.
-type failure_stack() :: [failure_map()].
-type match_result() :: true | {false, failure_stack()}.
-type fn_pattern() :: fun((value()) -> boolean()).

-export_type([failure_map/0,
              failure_stack/0,
              match_result/0,
              value/0,
              pattern/0,
              fn_pattern/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([match/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc
%%
%% This function can be used for pattern-matching nested spans.
%% The pattern-matching rules are the following:
%%   * '_' atom matches anything.
%%   * Match function (function with arity 1, fun matcher_fn/1),
%%     should return a boolean value. but crashing or any non 'true'
%%     value is treated as a failed matching. If you want to check
%%     for equality to some function with arity 1, you have to use
%%     a match function:
%%       fun(Fn) -> Fn =:= fun some_module:some_function/1 end.
%%   * Empty list ([]) matches an empty list only.
%%   * Non-empty pattern list ([_ | _]) matches any list containing
%%     elements that match patterns in the list. Patterns in the
%%     list are checked one by one against every item in the data
%%     list until the first match is found. Matched data elements
%%     are not tested against subsequent patterns. The length of
%%     the pattern list doesn't have to be the same as the length of
%%     the data list, e.g. ['_'] pattern matches any non-empty list.
%%     Less restrictive patterns should be placed at the end of the list,
%%     e.g. the pattern ['_', a] will not match the list [a, b], while
%%     the pattern [a, '_'] will match.
%%   * Note that Erlang strings are technically lists, so the pattern
%%     "this is a test" will successfully match the data "is this a test?".
%%     If you intend to test a string for equality, use the match function
%%     pattern instead:
%%       fun(String) -> String =:= "this is test" end.
%%   * For tuple patterns every element in the tuple is tested
%%     against the corresponding data element. The size of the pattern
%%     tuple must be equal to the size of the data tuple.
%%   * For map patterns, the corresponding data map must have identical
%%     keys as the pattern map, and the corresponding data values are
%%     matched against pattern values. An empty map #{} pattern
%%     matches any map.
%%
%% @end
-spec match(value(), pattern()) -> match_result().
match(Term, Pattern) -> ?MATCH_VALUE(Term, Pattern).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec failure_map(map(), atom()) -> failure_map().
failure_map(Map, Matcher) ->
    Map#{matcher => Matcher, reason => match_failed}.


-spec match_value(value(), pattern()) -> match_result().
match_value(_Value, '_') ->
    true;
match_value(Value, FnPattern) when is_function(FnPattern, 1) ->
    match_function(Value, FnPattern);
match_value(Value, MapPattern) when is_map(MapPattern) ->
    match_map(Value, MapPattern);
match_value(Value, TuplePattern) when is_tuple(TuplePattern) ->
    match_tuple(Value, TuplePattern);
match_value(Value, ListPattern) when is_list(ListPattern) ->
    match_list(Value, ListPattern);
match_value(Value, Pattern) ->
    match_equal(Value, Pattern).


-spec match_function(value(), fn_pattern()) -> match_result().
match_function(Value, FnPattern) ->
    try FnPattern(Value) of
        true -> true;
        _ -> {false, [?FAILED_MATCH(Value, FnPattern)]}
    catch
        Type:Reason:Stacktrace ->
            ?LOG_ERROR(
              "match function crashed: ErrorType == ~p," ++
              " Reason == ~p, Stacktrace == ~p",
              [Type, Reason, Stacktrace]),
            {false, [?FAILED_MATCH(Value, FnPattern)]}
    end.


-spec match_map(value(), map()) -> match_result().
match_map(Value, MapPattern) when is_map(Value) =/= true ->
    FailureMap = ?FAILED_MATCH(Value, MapPattern),
    {false, [FailureMap#{reason => not_a_map}]};
match_map(Value, MapPattern) ->
    PatternKeys = maps:keys(MapPattern),
    ValueKeys = maps:keys(Value),
    case lists:sort(PatternKeys -- ValueKeys) of
        [] ->
            MatchKeyFn =
                fun(Key, Acc) ->
                        match_map_key(Value, MapPattern, Key, Acc)
                end,
            case lists:foldl(MatchKeyFn, true, PatternKeys) of
                true ->
                    true;
                {false, FailureStack} ->
                    {false, [?FAILED_MATCH(Value, MapPattern) | FailureStack]}
            end;
        MissingKeys ->
            FailureMap = ?FAILED_MATCH(Value, MapPattern),

            {false, [FailureMap#{reason => missing_keys, missing_keys => MissingKeys}]}
    end.


-spec match_map_key(map(), map(), value(), match_result()) -> match_result().
match_map_key(Value, MapPattern, Key, true) ->
    KeyValue = maps:get(Key, Value),
    KeyPattern = maps:get(Key, MapPattern),
    case ?MATCH_VALUE(KeyValue, KeyPattern) of
        true ->
            true;
        {false, FailureStack} ->
            FailureMap = ?FAILED_MATCH(KeyValue, KeyPattern),
            {false, [FailureMap#{key => Key} | FailureStack]}
    end;
match_map_key(_Value, _MapPattern, _Key, Acc) ->
    Acc.


-spec match_tuple(value(), tuple()) -> match_result().
match_tuple(Value, TuplePattern) when is_tuple(Value) =/= true ->
    FailureMap = ?FAILED_MATCH(Value, TuplePattern),
    {false, [FailureMap#{reason => not_a_tuple}]};
match_tuple(Value, TuplePattern)
  when tuple_size(Value) =/= tuple_size(TuplePattern) ->
    FailureMap = ?FAILED_MATCH(Value, TuplePattern),
    {false, [FailureMap#{reason => tuple_size_mismatch}]};
match_tuple({}, {}) ->
    true;
match_tuple(Value, TuplePattern) ->
    ElementPos = lists:seq(1, tuple_size(TuplePattern)),
    MatchElementFn =
        fun(Pos, Acc) ->
                match_tuple_element(Value, TuplePattern, Pos, Acc)
        end,
    case lists:foldl(MatchElementFn, true, ElementPos) of
        true ->
            true;
        {false, FailureStack} ->
            {false, [?FAILED_MATCH(Value, TuplePattern) | FailureStack]}
    end.


-spec match_tuple_element(tuple(), tuple(), pos_integer(), match_result()) ->
          match_result().
match_tuple_element(Value, TuplePattern, Pos, true) ->
    ElementValue = element(Pos, Value),
    PatternValue = element(Pos, TuplePattern),
    case ?MATCH_VALUE(ElementValue, PatternValue) of
        true ->
            true;
        {false, FailureStack} ->
            FailureMap = ?FAILED_MATCH(ElementValue, PatternValue),
            {false, [FailureMap#{position => Pos} | FailureStack]}
    end;
match_tuple_element(_Value, _TuplePattern, _Pos, Acc) ->
    Acc.


-spec match_list(value(), list()) -> match_result().
match_list(Value, ListPattern) when is_list(Value) =/= true ->
    FailureMap = ?FAILED_MATCH(Value, ListPattern),
    {false, [FailureMap#{reason => not_a_list}]};
match_list(Value, ListPattern) when length(Value) < length(ListPattern) ->
    %% the list contains fewer items than the pattern
    FailureMap = ?FAILED_MATCH(Value, ListPattern),
    {false, [FailureMap#{reason => list_length_mismatch}]};
match_list([_ | _] = Value, [] = ListPattern) ->
    %% special case, the empty list pattern positively
    %% matches only an empty list
    FailureMap = ?FAILED_MATCH(Value, ListPattern),
    {false, [FailureMap#{reason => list_is_not_empty}]};
match_list(Value, ListPattern) ->
    case match_list_item(Value, ListPattern, [], 1, 1) of
        true ->
            true;
        {false, FailureStack} ->
            {false, [?FAILED_MATCH(Value, ListPattern) | FailureStack]}
    end.


-spec match_list_item(list(), list(), list(), pos_integer(), pos_integer()) ->
          match_result().
match_list_item(_ItemList, PatternList, _FailedMatches, _ItemIndex, PatternIndex)
  when PatternIndex > length(PatternList) ->
    true;
match_list_item(ItemList, PatternList, FailedMatches, ItemIndex, PatternIndex)
  when ItemIndex > length(ItemList) ->
    Pattern = lists:nth(PatternIndex, PatternList),
    FailureMap = #{
                   pattern_index => PatternIndex,
                   pattern => Pattern,
                   failed_matches => lists:reverse(FailedMatches),
                   matched_against => ItemList
                  },
    {false, [?FAILED_MATCH(FailureMap)]};
match_list_item(ItemList, PatternList, FailedMatches, ItemIndex, PatternIndex) ->
    Item = lists:nth(ItemIndex, ItemList),
    Pattern = lists:nth(PatternIndex, PatternList),
    case ?MATCH_VALUE(Item, Pattern) of
        true ->
            NewItemList = lists:delete(Item, ItemList),
            match_list_item(NewItemList, PatternList, [], 1, PatternIndex + 1);
        {false, FailureStack} ->
            NewFailedMatches = [{false, FailureStack} | FailedMatches],
            match_list_item(
              ItemList,
              PatternList,
              NewFailedMatches,
              ItemIndex + 1,
              PatternIndex)
    end.


-spec match_equal(value(), pattern()) -> match_result().
match_equal(Value, Pattern) ->
    case Value == Pattern of
        true -> true;
        false -> {false, [?FAILED_MATCH(Value, Pattern)]}
    end.
