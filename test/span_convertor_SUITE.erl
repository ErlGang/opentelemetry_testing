-module(span_convertor_SUITE).
-behavior(ct_suite).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([init_test/1, span_conversion_prop_test/1,
         recursive_records_conversion_prop_test/1]).

-define(NUMBER_OF_REPETITIONS, 100).

-define(ADD_RECORD(RecordName, Acc),
        span_convertor:add_record(RecordName, record_info(fields, RecordName), Acc)).

%%% declaring span type so that it can be used for the PropEr generator.
-type span() :: #span{}.

%% some dummy records and types for testing
-record(r1, {r1f1 :: term(), r1f2 :: term(), r1f3 :: term()}).
-record(r2, {r2f1 :: term(), r2f2 :: term()}).
-record(r3, {r3f1 :: term()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [init_test,
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
    ?assertError({error,inconsistent_record_definition_map,
                  ExpectedOtelRecords4, OtelRecords},
                 span_convertor:init()).

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
            begin
                case contains_known_records(Term) of
                    false ->
                        ct:log("contains_known_records(Term) == false"),
                        ?assertEqual(Term, span_convertor:records_to_maps(Term)),
                        ?assertEqual(Term, span_convertor:maps_to_records(Term));
                    {true, [_ | _]} ->
                        ct:log("contains_known_records(Term) == true"),
                        ConvertedTerm = span_convertor:records_to_maps(Term),
                        ?assertEqual(false, contains_known_records(ConvertedTerm)),
                        ?assertEqual(Term, span_convertor:maps_to_records(ConvertedTerm))
                end,
                true
            end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

span_gen() ->
    %% without wrapping into a ?LET macro compilation fails
    %% with the following error:
    %%   function span/0 undefined
    ?LET(X, span(), X).

random_type_gen(X0) when X0 > 0 ->
    X = X0 - 1,
    ?LAZY(oneof([ct_proper_ext:safe_any(),
                 some_record_gen(X),
                 nested_list_gen(X),
                 nested_tuple_gen(X),
                 nested_map_gen(X)]));
random_type_gen(_X) ->
    ct_proper_ext:safe_any().

nested_list_gen(X) ->
    list(random_type_gen(X)).

nested_tuple_gen(X) ->
    loose_tuple(random_type_gen(X)).

nested_map_gen(X) ->
    map(ct_proper_ext:safe_any(), random_type_gen(X)).

some_record_gen(X) ->
    oneof([t1_gen(X), t2_gen(X), t3_gen(X)]).

t1_gen(X) ->
    ?LET({F1,F2,F3},
            {random_type_gen(X),random_type_gen(X),random_type_gen(X)},
            #r1{r1f1 = F1, r1f2 = F2, r1f3 = F3}).

t2_gen(X) ->
    ?LET({F1,F2},
            {random_type_gen(X),random_type_gen(X)},
            #r2{r2f1 = F1, r2f2 = F2}).

t3_gen(X) ->
    ?LET({F1},
            {random_type_gen(X)},
            #r3{r3f1 = F1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_record_definitions() ->
    %% 'undefined' is the special value used to erase previously stored
    %% record definitions.
    span_convertor:store_record_definitions(undefined).

contains_known_records(Term) ->
    OtelRecords = span_convertor:get_record_definitions(),
    KnownRecords = maps:keys(OtelRecords),
    List = contains_known_records(Term, KnownRecords),
    case lists:flatten(List) of
        [] -> false;
        FlatList -> {true, lists:uniq(FlatList)}
    end.

contains_known_records(List, KnownRecords) when is_list(List) ->
    [contains_known_records(Elem, KnownRecords) || Elem <- List];
contains_known_records(Map, KnownRecords) when is_map(Map) ->
    [contains_known_records(Value, KnownRecords) || _Key := Value <- Map];
contains_known_records(Tuple, KnownRecords) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    case [Record || Record <- KnownRecords, is_known_record(Tuple, Record)] of
        [KnownRecord] -> [KnownRecord | contains_known_records(List, KnownRecords)];
        [] -> contains_known_records(List, KnownRecords)
    end;
contains_known_records(_Term, _KnownRecords) -> [].

is_known_record(Tuple, {RecordName, RecordSize}) ->
    is_record(Tuple, RecordName, RecordSize).
