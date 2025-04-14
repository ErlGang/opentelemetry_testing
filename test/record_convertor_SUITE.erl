-module(record_convertor_SUITE).
-behavior(ct_suite).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([add_record_test/1,
         recursive_records_conversion_prop_test/1]).

-export([contains_nested_records/2]).

-define(NUMBER_OF_REPETITIONS, 200).

-define(ADD_RECORD(RecordName, RecordDefinitionsMap),
        record_convertor:add_record(RecordName,
                                    record_info(fields, RecordName),
                                    RecordDefinitionsMap)).

%% some dummy records and types for testing
-record(r1, {r1f1 :: term(), r1f2 :: term(), r1f3 :: term()}).
-record(r2, {r2f1 :: term(), r2f2 :: term()}).
-record(r3, {r3f1 :: term()}).

-define(RECORD_GEN(RecordName, X),
        record_gen(RecordName, record_info(size, RecordName), X)).
%% generate a tuple with a number of elements greater than the record size
-define(FALSE_RECORD_GEN(RecordName, X),
        record_gen(RecordName, record_info(size, RecordName) + 1, X)).

-define(RECORD_NAME, r1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper functions used in other suites
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


contains_nested_records(Term, RecordDefinitionsMap) ->
    KnownRecords = maps:keys(RecordDefinitionsMap),
    List = contains_known_records(Term, KnownRecords),
    case lists:flatten(List) of
        [] -> false;
        FlatList -> {true, lists:uniq(FlatList)}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [add_record_test,
     recursive_records_conversion_prop_test].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


add_record_test(_config) ->
    %% adding the same record a second time does not change the map.
    RecordDefinitionsMap = ?ADD_RECORD(?RECORD_NAME, #{}),
    MapKey = {?RECORD_NAME, record_info(size, ?RECORD_NAME)},
    ?assert(is_map_key(MapKey, RecordDefinitionsMap), RecordDefinitionsMap),
    ?assertEqual(RecordDefinitionsMap,
                 ?ADD_RECORD(?RECORD_NAME, RecordDefinitionsMap)),

    %% adding a record with the same name and size but
    %% different fields causes a failure
    Fields = record_info(fields, ?RECORD_NAME),
    InvalidFields = [invalid_field | tl(Fields)],
    ?assertError(
      {inconsistent_record_definitions, RecordDefinitionsMap,
                                        MapKey, InvalidFields},
      record_convertor:add_record(r1, InvalidFields, RecordDefinitionsMap)).


recursive_records_conversion_prop_test(_Config) ->
    PropTest = recursive_records_conversion_property(),
    ?assert(proper:quickcheck(PropTest, [?NUMBER_OF_REPETITIONS, noshrink])),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


recursive_records_conversion_property() ->
    ?FORALL(Term,
            random_type_gen(6),
            recursive_records_conversion_property(Term)).


recursive_records_conversion_property(Term) ->
    RecordDefinitionsMap1 = ?ADD_RECORD(r1, #{}),
    RecordDefinitionsMap2 = ?ADD_RECORD(r2, RecordDefinitionsMap1),
    RecordDefinitionsMap = ?ADD_RECORD(r3, RecordDefinitionsMap2),
    case contains_nested_records(Term, RecordDefinitionsMap) of
        false ->
            ?assertEqual(
              Term,
              record_convertor:records_to_maps(Term,
                                               RecordDefinitionsMap)),
            ?assertEqual(
              Term,
              record_convertor:maps_to_records(Term,
                                               RecordDefinitionsMap));
        {true, [_ | _]} ->
            ConvertedTerm =
                record_convertor:records_to_maps(Term, RecordDefinitionsMap),
            ?assertNot(contains_nested_records(ConvertedTerm,
                                               RecordDefinitionsMap)),
            ?assertEqual(
              Term,
              record_convertor:maps_to_records(ConvertedTerm,
                                               RecordDefinitionsMap))
    end,
    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


random_type_gen(X0) when X0 > 0 ->
    X = X0 - 1,
    oneof([safe_any(),
           valid_record_gen(X),
           false_record_gen(X),
           nested_list_gen(X),
           nested_tuple_gen(X),
           nested_map_gen(X)]);
random_type_gen(_X) ->
    safe_any().


safe_any() ->
    oneof([atom(),
           integer(),
           float(),
           ?LET(Length, integer(0, 10), bitstring(Length)),
           ?LET(Length, integer(0, 5), binary(Length))]).


small_list(Type, MaxLength) ->
    ?LET(Length, integer(0, MaxLength), vector(Length, Type)).


small_tuple(Type, MaxSize) ->
    ?LET(List, small_list(Type, MaxSize), list_to_tuple(List)).


small_map(KeyType, ValueType, MaxSize) ->
    ?LET(PropList,
         small_list({KeyType, ValueType}, MaxSize),
         maps:from_list(PropList)).


nested_list_gen(X) ->
    small_list(random_type_gen(X), 5).


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


contains_known_records(List, KnownRecords) when is_list(List) ->
    [ contains_known_records(Elem, KnownRecords) || Elem <- List ];
contains_known_records(Map, KnownRecords) when is_map(Map) ->
    [ contains_known_records(Value, KnownRecords) || _Key := Value <- Map ];
contains_known_records(Tuple, KnownRecords) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    case [ Record || Record <- KnownRecords, is_known_record(Tuple, Record) ] of
        [KnownRecord] -> [KnownRecord | contains_known_records(List, KnownRecords)];
        [] -> contains_known_records(List, KnownRecords)
    end;
contains_known_records(_Term, _KnownRecords) -> [].


is_known_record(Tuple, {RecordName, RecordSize}) ->
    is_record(Tuple, RecordName, RecordSize).
