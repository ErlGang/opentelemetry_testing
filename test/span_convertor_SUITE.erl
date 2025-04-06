-module(span_convertor_SUITE).
-behavior(ct_suite).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([init_test/1,
         add_record_test/1,
         span_conversion_prop_test/1,
         recursive_records_conversion_prop_test/1]).

-define(NUMBER_OF_REPETITIONS, 200).

-define(ADD_RECORD(RecordName, Acc),
        span_convertor:add_record(RecordName, record_info(fields, RecordName), Acc)).

-define(RECORD_NAME, span).

%%% declaring span type so that it can be used for the PropEr generator.
-type span() :: #span{}.

%% some dummy records and types for testing
-record(r1, {r1f1 :: term(), r1f2 :: term(), r1f3 :: term()}).
-record(r2, {r2f1 :: term(), r2f2 :: term()}).
-record(r3, {r3f1 :: term()}).

-define(RECORD_GEN(RecordName, X),
        record_gen(RecordName, record_info(size, RecordName), X)).
%% generate a tuple with a number of elements greater than the record size
-define(FALSE_RECORD_GEN(RecordName, X),
        record_gen(RecordName, record_info(size, RecordName) + 1, X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [init_test,
     add_record_test,
     span_conversion_prop_test,
     recursive_records_conversion_prop_test].


init_per_testcase(recursive_records_conversion_prop_test, Config) ->
    %% adding a code path is required to use safe type generators
    %% from the ct_proper_ext module and avoid the "no more index
    %% entries in atom_tab" crash during the test.
    code:add_path(code:lib_dir(common_test) ++ "/proper_ext"),
    clean_record_definitions(),
    Config;
init_per_testcase(_TestCase, Config) ->
    clean_record_definitions(),
    Config.


end_per_testcase(_TestCase, Config) ->
    clean_record_definitions(),
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init_test(_Config) ->
    %% ensure that record definitions from otel_span.hrl are added correctly.
    ExpectedOtelRecords1 = ?ADD_RECORD(span, #{}),
    ExpectedOtelRecords2 = ?ADD_RECORD(span_limits, ExpectedOtelRecords1),
    ExpectedOtelRecords3 = ?ADD_RECORD(link, ExpectedOtelRecords2),
    ExpectedOtelRecords4 = ?ADD_RECORD(event, ExpectedOtelRecords3),
    ?assertEqual(ok, span_convertor:init()),
    OtelRecords = span_convertor:get_record_definitions(),
    ExpectedKeys = maps:keys(ExpectedOtelRecords4),
    ?assertEqual(ExpectedOtelRecords4, maps:with(ExpectedKeys, OtelRecords)),

    %% check that span_convertor:init/0 doesn't fail if called twice.
    ?assertEqual(ok, span_convertor:init()),
    ?assertEqual(OtelRecords, span_convertor:get_record_definitions()),

    %% check that span_convertor:init/0 crashes but doesn't change stored
    %% record definitions if they differ from the definitions extracted
    %% from OTEL modules.
    span_convertor:store_record_definitions(ExpectedOtelRecords4),
    ?assertError({error, inconsistent_record_definition_map,
                         ExpectedOtelRecords4, OtelRecords},
                 span_convertor:init()).


add_record_test(_config) ->
    %% adding the same record a second time does not change the Acc.
    Acc = ?ADD_RECORD(?RECORD_NAME, #{}),
    ?assertEqual(Acc, ?ADD_RECORD(?RECORD_NAME, Acc)),

    %% adding a record with the same name and size but different fields causes a failure
    Fields = record_info(fields, ?RECORD_NAME),
    InvalidFields = [invalid_field | tl(Fields)],
    ?assertError({inconsistent_record_definitions, Acc, [?RECORD_NAME | InvalidFields]},
                 span_convertor:add_record(?RECORD_NAME, InvalidFields, Acc)).


span_conversion_prop_test(_Config) ->
    ?assertEqual(ok, span_convertor:init()),
    PropTest = span_conversion_property(),
    ?assertEqual(true, proper:quickcheck(PropTest, [?NUMBER_OF_REPETITIONS, noshrink])),
    ok.


recursive_records_conversion_prop_test(_Config) ->
    ExpectedOtelRecords1 = ?ADD_RECORD(r1, #{}),
    ExpectedOtelRecords2 = ?ADD_RECORD(r2, ExpectedOtelRecords1),
    ExpectedOtelRecords3 = ?ADD_RECORD(r3, ExpectedOtelRecords2),
    span_convertor:store_record_definitions(ExpectedOtelRecords3),
    PropTest = recursive_records_conversion_property(),
    ?assertEqual(true, proper:quickcheck(PropTest, [?NUMBER_OF_REPETITIONS, noshrink])),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


span_conversion_property() ->
    ?FORALL(Record,
            span_gen(),
            begin
                ?assertMatch({true, [_ | _]}, contains_known_records(Record)),
                Map = span_convertor:records_to_maps(Record),
                ?assertEqual(false, contains_known_records(Map)),
                ?assertEqual(Record, span_convertor:maps_to_records(Record)),
                true
            end).


recursive_records_conversion_property() ->
    ?FORALL(Term,
            random_type_gen(4),
            recursive_records_conversion_property(Term)).


recursive_records_conversion_property(Term) ->
    ct:log("Term = ~p", [Term]),
    ct:log("term_depth(Term) = ~p", [term_depth(Term)]),
    case contains_known_records(Term) of
        false ->
            ct:log("contains_known_records(Term) == false"),
            ct:log("contains_known_records(Term, true) == ~p",
                   [contains_known_records(Term, true)]),
            ct:log("contains_false_records(Term, true) == ~p",
                   [contains_false_records(Term, true)]),
            ?assertEqual(Term, span_convertor:records_to_maps(Term)),
            ?assertEqual(Term, span_convertor:maps_to_records(Term));
        {true, [_ | _]} = Ret ->
            ct:log("contains_known_records(Term) == ~p", [Ret]),
            ConvertedTerm = span_convertor:records_to_maps(Term),
            ?assertEqual(false, contains_known_records(ConvertedTerm)),
            ?assertEqual(Term, span_convertor:maps_to_records(ConvertedTerm))
    end,
    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


span_gen() ->
    %% without wrapping span() type into a ?LET macro compilation fails
    %% with the following error:
    %%   function span/0 undefined
    ?LET(X, span(), X).


random_type_gen(X0) when X0 > 0 ->
    X = X0 - 1,
    ?LAZY(oneof([safe_any(),
                 valid_record_gen(X),
                 false_record_gen(X),
                 nested_list_gen(X),
                 nested_tuple_gen(X),
                 nested_map_gen(X)]));
random_type_gen(_X) ->
    safe_any().


safe_any() ->
    oneof([ct_proper_ext:safe_atom(),
           integer(),
           float(),
           bitstring(10),
           binary(5)]).


short_non_empty_list(Type, MaxLength) ->
    ?LET(Length, integer(1, MaxLength), vector(Length, Type)).


short_list(Type, MaxLength) ->
    frequency([{10, short_non_empty_list(Type, MaxLength)},
               {1, []}]).


small_tuple(Type, MaxSize) ->
    ?LET(List, short_list(Type, MaxSize), list_to_tuple(List)).


small_map(KeyType, ValueType, MaxSize) ->
    ?LET(PropList,
         short_list({KeyType, ValueType}, MaxSize),
         maps:from_list(PropList)).


nested_list_gen(X) ->
    short_list(random_type_gen(X), 5).


nested_tuple_gen(X) ->
    small_tuple(random_type_gen(X), 5).


nested_map_gen(X) ->
    small_map(random_type_gen(X), random_type_gen(X), 5).


valid_record_gen(X) ->
    oneof([?RECORD_GEN(r1, X), ?RECORD_GEN(r2, X), ?RECORD_GEN(r3, X)]).


false_record_gen(X) ->
    oneof([?FALSE_RECORD_GEN(r1, X),
           ?FALSE_RECORD_GEN(r2, X),
           ?FALSE_RECORD_GEN(r3, X)]).


record_gen(Name, Size, X) ->
    ?LET(List,
         vector(Size - 1, random_type_gen(X)),
         list_to_tuple([Name | List])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


clean_record_definitions() ->
    %% 'undefined' is the special value used to erase previously stored
    %% record definitions.
    span_convertor:store_record_definitions(undefined).


contains_known_records(Term) ->
    contains_known_records(Term, false).


contains_known_records(Term, SearchInMapKeys) ->
    OtelRecords = span_convertor:get_record_definitions(),
    KnownRecords = maps:keys(OtelRecords),
    VerifyFN = fun is_known_record/2,
    contains_records(Term, KnownRecords, SearchInMapKeys, VerifyFN).


contains_false_records(Term, SearchInMapKeys) ->
    OtelRecords = span_convertor:get_record_definitions(),
    KnownRecords = maps:keys(OtelRecords),
    VerifyFN = fun is_false_record/2,
    contains_records(Term, KnownRecords, SearchInMapKeys, VerifyFN).


contains_records(Term, KnownRecords, SearchInMapKeys, VerifyFN) ->
    List = contains_records(0, Term, KnownRecords, SearchInMapKeys, VerifyFN),
    case [ Item || {Level, _} = Item <- lists:flatten(List), is_integer(Level) ] of
        [] -> false;
        FlatList -> {true, lists:uniq(FlatList)}
    end.


term_depth(Term) ->
    List = contains_records(0, Term, [], true, fun(_X, _Y) -> false end),
    lists:max([ Level || {leaf, Level} <- lists:flatten(List) ]).


contains_records(Level, List, KnownRecords, SearchInMapKeys, VerifyFN)
  when is_list(List) ->
    [{leaf, Level}] ++
    [ contains_records(Level + 1, Elem, KnownRecords, SearchInMapKeys, VerifyFN)
      || Elem <- List ];
contains_records(Level, Map, KnownRecords, SearchInMapKeys, VerifyFN)
  when is_map(Map) ->
    [{leaf, Level}] ++
    [ contains_records(Level + 1, Value, KnownRecords, SearchInMapKeys, VerifyFN)
      || _Key := Value <- Map ] ++
    [ contains_records(Level + 1, Key, KnownRecords, SearchInMapKeys, VerifyFN)
      || Key := _Value <- Map, SearchInMapKeys =:= true ];
contains_records(Level, Tuple, KnownRecords, SearchInMapKeys, VerifyFN)
  when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    case [ Record || Record <- KnownRecords, VerifyFN(Tuple, Record) ] of
        [KnownRecord | _] ->
            OtherNestedRecords =
                contains_records(Level, List, KnownRecords, SearchInMapKeys, VerifyFN),
            [{Level, KnownRecord} | OtherNestedRecords];
        [] -> contains_records(Level, List, KnownRecords, SearchInMapKeys, VerifyFN)
    end;
contains_records(Level, _Term, _KnownRecords, _SearchInMapKeys, _VerifyFN) ->
    [{leaf, Level}].


is_false_record(Tuple, {RecordName, RecordSize}) ->
    is_record(Tuple, RecordName) andalso
    not is_record(Tuple, RecordName, RecordSize).


is_known_record(Tuple, {RecordName, RecordSize}) ->
    is_record(Tuple, RecordName, RecordSize).
