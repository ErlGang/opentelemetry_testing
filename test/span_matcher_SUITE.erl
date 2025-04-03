-module(span_matcher_SUITE).
-behavior(ct_suite).

-include_lib("stdlib/include/assert.hrl").
-include_lib("test_logs/include/test_logs.hrl").

-export([all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([match_anything_test/1,
         %% match_function_tests
         positive_function_match_test/1,
         not_a_matching_function_test/1,
         function_equality_check_test/1,
         negative_function_match_test/1,
         %% match_map_tests
         positive_map_match_test/1,
         not_a_map_test/1,
         missing_keys_test/1,
         negative_map_match_test/1,
         %% match_tuple_tests
         positive_tuple_match_test/1,
         not_a_tuple_test/1,
         tuple_size_mismatch_test/1,
         negative_tuple_match_test/1,
         %% match_list_tests
         positive_list_match_test/1,
         not_a_list_test/1,
         list_length_mismatch_test/1,
         list_is_not_empty_test/1,
         negative_list_match_test/1,
         %% match_equal_tests
         positive_equal_match_test/1,
         negative_equal_match_test/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [{group, basic},
     {group, nested}].


groups() ->
    [{basic, [], all_test_cases()},
     {nested, [], all_test_cases()}].


all_test_cases() ->
    lists:flatten([match_anything_test,
                   match_function_tests(),
                   match_map_tests(),
                   match_tuple_tests(),
                   match_list_tests(),
                   match_equal_tests()]).


match_function_tests() ->
    [positive_function_match_test,
     not_a_matching_function_test,
     function_equality_check_test,
     negative_function_match_test].


match_map_tests() ->
    [positive_map_match_test,
     not_a_map_test,
     missing_keys_test,
     negative_map_match_test].


match_tuple_tests() ->
    [positive_tuple_match_test,
     not_a_tuple_test,
     tuple_size_mismatch_test,
     negative_tuple_match_test].


match_list_tests() ->
    [positive_list_match_test,
     not_a_list_test,
     list_length_mismatch_test,
     list_is_not_empty_test,
     negative_list_match_test].


match_equal_tests() ->
    [positive_equal_match_test,
     negative_equal_match_test].


init_per_group(Group, Config) ->
    [{group, Group} | Config].


end_per_group(_Group, Config) ->
    Config.


init_per_testcase(negative_function_match_test, Config) ->
    test_logs:add_handler(),
    Config;
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(negative_function_match_test, Config) ->
    test_logs:remove_handler(),
    Config;
end_per_testcase(_, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


match_anything_test(Config) ->
    assert_positive_match(123, '_', Config),
    assert_positive_match([123], '_', Config),
    assert_positive_match({123}, '_', Config),
    assert_positive_match(#{123 => 321}, '_', Config),
    assert_positive_match(<<"123">>, '_', Config),
    assert_positive_match(make_ref(), '_', Config).


positive_function_match_test(Config) ->
    %% positive match
    assert_positive_match(123, fun positive_match_function/1, Config).


not_a_matching_function_test(Config) ->
    %% The function of arity other than 1 is not treated as a match function.
    %% match_equal matcher must be used if such a function is used as a pattern
    NotMatchFn = fun not_a_match_function/0,
    assert_positive_match(NotMatchFn, NotMatchFn, Config),

    %% if value function is not equal to the pattern pattern, then matching fails
    AnotherFn = fun crash_match_function/1,
    assert_negative_equal_match(AnotherFn, NotMatchFn, Config).


function_equality_check_test(Config) ->
    %% if you want to check for equality to some function with arity 1,
    %% you have to use matcher function.
    PositiveValue = fun negative_match_function/1,
    NegativeValue = fun crash_match_function/1,
    MatchFunction = fun(X) -> X =:= PositiveValue end,
    assert_positive_match(PositiveValue, MatchFunction, Config),
    assert_negative_function_match(NegativeValue, MatchFunction, Config).


negative_function_match_test(Config) ->
    %% failing matches
    Value = 123,
    assert_negative_function_match(Value, fun negative_match_function/1, Config),
    assert_negative_function_match(Value, fun invalid_return_match_function/1, Config),
    test_logs:set_pid(),
    assert_negative_function_match(Value, fun crash_match_function/1, Config),
    ?assertLogEvent({"match function crashed" ++ _, _}, error, _).


positive_map_match_test(Config) ->
    %% positive matches
    Value = #{
              <<"some_key">> => some_value,
              {another_key} => 123,
              yet_another_key => make_ref()
             },

    %% an empty map pattern matches any map
    assert_positive_match(Value, #{}, Config),

    %% only the values ​​for keys in the pattern map are matched against
    %% the values ​​associated with these keys in the data map
    assert_positive_match(Value, #{{another_key} => '_'}, Config),
    assert_positive_match(Value, Value#{yet_another_key => '_'}, Config).


not_a_map_test(Config) ->
    %% not_a_map failing match
    Value = [not_a_map],
    Pattern = #{{another_key} => '_'},
    FailureStack = [match_failed(Value, Pattern, match_map, not_a_map)],
    assert_negative_match(Value, Pattern, FailureStack, Config).


missing_keys_test(Config) ->
    %% key_is_missing failing match
    Value = #{
              <<"some_key">> => some_value,
              {another_key} => 123,
              yet_another_key => make_ref()
             },
    Pattern = Value#{missing_key1 => any_value, missing_key2 => 1},
    FailureStack = [maps:put(missing_keys,
                             lists:sort([missing_key1, missing_key2]),
                             match_failed(Value, Pattern, match_map, missing_keys))],
    assert_negative_match(Value, Pattern, FailureStack, Config).


negative_map_match_test(Config) ->
    %% failing value match
    Value = #{
              <<"some_key">> => some_value,
              {another_key} => 123,
              yet_another_key => make_ref()
             },
    Key = {another_key},
    KeyPattern = invalid_value,
    Pattern = Value#{Key := KeyPattern},
    KeyValue = maps:get(Key, Value),
    FailureStack = [match_failed(Value, Pattern, match_map),
                    (match_failed(KeyValue, KeyPattern, match_map_key))#{key => Key},
                    match_failed(KeyValue, KeyPattern, match_equal)],
    assert_negative_match(Value, Pattern, FailureStack, Config).


positive_tuple_match_test(Config) ->
    assert_positive_match({}, {}, Config),
    assert_positive_match({some_value}, {'_'}, Config),
    assert_positive_match({some_value, another_value}, {'_', another_value}, Config).


not_a_tuple_test(Config) ->
    Value = [some_list],
    Pattern = {some_tuple},
    FailureStack = [match_failed(Value, Pattern, match_tuple, not_a_tuple)],
    assert_negative_match(Value, Pattern, FailureStack, Config).


tuple_size_mismatch_test(Config) ->
    Value = {some_tuple},
    Pattern = {another, tuple},
    FailureStack = [match_failed(Value, Pattern, match_tuple, tuple_size_mismatch)],
    assert_negative_match(Value, Pattern, FailureStack, Config).


negative_tuple_match_test(Config) ->
    assert_negative_tuple_match({a}, {b}, 1, Config),
    assert_negative_tuple_match({a, b}, {a, c}, 2, Config),
    assert_negative_tuple_match({a, b, c}, {a, c, b}, 2, Config).


positive_list_match_test(Config) ->
    assert_positive_match([], [], Config),
    assert_positive_match([some_value], ['_'], Config),
    assert_positive_match([some_value, another_value], [another_value], Config),
    assert_positive_match([some_value, another_value],
                          [another_value, some_value],
                          Config),
    assert_positive_match("test_this", "this_test", Config),
    %% '_' is in the end of the list, so it's matching anything that remains
    assert_positive_match([1, 2, 3], [1, 2, '_'], Config).


not_a_list_test(Config) ->
    Value = {some_tuple},
    Pattern = [some_list],
    FailureStack = [match_failed(Value, Pattern, match_list, not_a_list)],
    assert_negative_match(Value, Pattern, FailureStack, Config).


list_length_mismatch_test(Config) ->
    %% the length of the Pattern list is higher than the length of the Value list
    Value = [some_list],
    Pattern = [another, list],
    FailureStack = [match_failed(Value, Pattern, match_list, list_length_mismatch)],
    assert_negative_match(Value, Pattern, FailureStack, Config).


list_is_not_empty_test(Config) ->
    Value1 = [some_list],
    Pattern1 = [],
    FailureStack1 = [match_failed(Value1, Pattern1, match_list, list_is_not_empty)],
    assert_negative_match(Value1, Pattern1, FailureStack1, Config),

    Value2 = "some_list",
    Pattern2 = "",
    FailureStack2 = [match_failed(Value2, Pattern2, match_list, list_is_not_empty)],
    assert_negative_match(Value2, Pattern2, FailureStack2, Config).


negative_list_match_test(Config) ->
    Value = lists:seq(1, 10),
    %% 5 appears twice in the pattern list, while 10 is not present
    Pattern = lists:seq(9, 5, -1) ++ lists:seq(1, 5),
    assert_negative_list_match(Value, Pattern, 10, [10], Config),

    assert_negative_list_match([a, b, c], [<<"a">>, b, c], 1, [a, b, c], Config),
    %% 1 is matched by '_' pattern
    assert_negative_list_match([1, 2, 3], ['_', 1, 2], 2, [2, 3], Config).


positive_equal_match_test(Config) ->
    assert_positive_match(123, 123, Config),
    assert_positive_match(<<"value">>, <<"value">>, Config),
    assert_positive_match(some_atom, some_atom, Config),
    Ref = make_ref(),
    assert_positive_match(Ref, Ref, Config).


negative_equal_match_test(Config) ->
    assert_negative_equal_match(<<"some_binary">>, some_atom, Config),
    assert_negative_equal_match(123, 12, Config),
    assert_negative_equal_match(<<"value1">>, <<"value2">>, Config),
    assert_negative_equal_match(some_atom, another_atom, Config),
    assert_negative_equal_match(make_ref(), make_ref(), Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


positive_match_function(_) -> true.


negative_match_function(_) -> false.


crash_match_function(_) -> error(some_error).


invalid_return_match_function(_) -> <<"invalid_return">>.


%% the arity of this function is 0, so it's not treated as a match function
not_a_match_function() -> false.


assert_positive_match(Value, Pattern, Config) ->
    Data = maybe_nested_data(Value, Config),
    NestedPattern = maybe_nested_pattern(Pattern, Config),
    ?assertEqual(true, span_matcher:match(Data, NestedPattern)).


assert_negative_match(Value, Pattern, FailureStack, Config)
  when is_list(FailureStack) ->
    Data = maybe_nested_data(Value, Config),
    InvalidPattern = maybe_nested_pattern(Pattern, Config),
    Failure = maybe_nested_failure(FailureStack, Value, Pattern, Config),
    ?assertEqual(Failure, span_matcher:match(Data, InvalidPattern));
assert_negative_match(Value, Pattern, Matcher, Config) when is_atom(Matcher) ->
    FailureStack = [match_failed(Value, Pattern, Matcher)],
    assert_negative_match(Value, Pattern, FailureStack, Config).


match_failed(Value, Pattern, Matcher) ->
    match_failed(Value, Pattern, Matcher, match_failed).


match_failed(Value, Pattern, Matcher, Reason) ->
    #{
      reason => Reason,
      value => Value,
      pattern => Pattern,
      matcher => Matcher
     }.


assert_negative_function_match(Value, Pattern, Config) ->
    assert_negative_match(Value, Pattern, match_function, Config).


assert_negative_tuple_match(Value, Pattern, Pos, Config) ->
    ElValue = element(Pos, Value),
    ElPattern = element(Pos, Pattern),
    FailureStack = [match_failed(Value, Pattern, match_tuple),
                    maps:put(position,
                             Pos,
                             match_failed(ElValue, ElPattern, match_tuple_element)),
                    match_failed(ElValue, ElPattern, match_equal)],
    assert_negative_match(Value, Pattern, FailureStack, Config).


assert_negative_list_match(Value, Pattern, Index, RemList, Config) ->
    ItemPattern = lists:nth(Index, Pattern),
    FailedMatches = [ {false, [match_failed(Item, ItemPattern, match_equal)]}
                      || Item <- RemList ],
    FailureStack = [match_failed(Value, Pattern, match_list),
                    #{
                      reason => match_failed,
                      pattern => ItemPattern,
                      matcher => match_list_item,
                      pattern_index => Index,
                      failed_matches => FailedMatches,
                      matched_against => RemList
                     }],
    assert_negative_match(Value, Pattern, FailureStack, Config).


assert_negative_equal_match(Value, Pattern, Config) ->
    assert_negative_match(Value, Pattern, match_equal, Config).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helpers for nested test group
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maybe_nested_data(Data, Config) ->
    case proplists:get_value(group, Config) of
        basic ->
            Data;
        nested ->
            {#{},
             #{
               some_key => some_value,
               another_key => [another_value, {}, {yet_another_value, Data}]
              },
             []}
    end.


maybe_nested_pattern(Pattern, Config) ->
    case proplists:get_value(group, Config) of
        basic -> Pattern;
        nested -> {'_', #{another_key => [{yet_another_value, Pattern}]}, '_'}
    end.


maybe_nested_failure(FailureStack, Value, Pattern, Config) ->
    case proplists:get_value(group, Config) of
        basic ->
            {false, FailureStack};
        nested ->
            NestedFailureStack =
                [#{
                   reason => match_failed,
                   value => {#{},
                             #{
                               some_key => some_value,
                               another_key => [another_value, {}, {yet_another_value, Value}]
                              },
                             []},
                   pattern => {'_', #{another_key => [{yet_another_value, Pattern}]}, '_'},
                   matcher => match_tuple
                  },
                 #{
                   position => 2,
                   reason => match_failed,
                   value => #{
                              some_key => some_value,
                              another_key => [another_value, {}, {yet_another_value, Value}]
                             },
                   pattern => #{another_key => [{yet_another_value, Pattern}]},
                   matcher => match_tuple_element
                  },
                 #{
                   reason => match_failed,
                   value => #{
                              some_key => some_value,
                              another_key => [another_value, {}, {yet_another_value, Value}]
                             },
                   pattern => #{another_key => [{yet_another_value, Pattern}]},
                   matcher => match_map
                  },
                 #{
                   reason => match_failed,
                   value => [another_value, {}, {yet_another_value, Value}],
                   pattern => [{yet_another_value, Pattern}],
                   key => another_key,
                   matcher => match_map_key
                  },
                 #{
                   reason => match_failed,
                   value => [another_value, {}, {yet_another_value, Value}],
                   pattern => [{yet_another_value, Pattern}],
                   matcher => match_list
                  },
                 #{
                   reason => match_failed,
                   pattern => {yet_another_value, Pattern},
                   matcher => match_list_item,
                   pattern_index => 1,
                   failed_matches => [{false, [#{
                                                 reason => not_a_tuple,
                                                 value => another_value,
                                                 pattern => {yet_another_value, Pattern},
                                                 matcher => match_tuple
                                                }]},
                                      {false, [#{
                                                 reason => tuple_size_mismatch,
                                                 value => {},
                                                 pattern => {yet_another_value, Pattern},
                                                 matcher => match_tuple
                                                }]},
                                      {false, [#{
                                                 reason => match_failed,
                                                 value => {yet_another_value, Value},
                                                 pattern => {yet_another_value, Pattern},
                                                 matcher => match_tuple
                                                },
                                               #{
                                                 position => 2,
                                                 reason => match_failed,
                                                 value => Value,
                                                 pattern => Pattern,
                                                 matcher => match_tuple_element
                                                } | FailureStack]}],
                   matched_against => [another_value, {}, {yet_another_value, Value}]
                  }],
            {false, NestedFailureStack}
    end.
