-module(span_convertor_SUITE).
-behavior(ct_suite).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-export([all/0]).

-export([get_record_definitions_test/1,
         span_conversion_prop_test/1]).

-define(PERSISTENT_TERM_KEY, {span_convertor, record_definition_map}).

-define(NUMBER_OF_REPETITIONS, 200).

-define(ADD_RECORD(RecordName, RecordDefinitionsMap),
        record_convertor:add_record(RecordName,
                                    record_info(fields, RecordName),
                                    RecordDefinitionsMap)).

%%% declaring span type so that it can be used for the PropEr generator.
-type span() :: #span{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [get_record_definitions_test,
     span_conversion_prop_test].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_record_definitions_test(_Config) ->
    %% check that ?PERSISTENT_TERM_KEY is valid and remove persistent_term data
    persistent_term:put(?PERSISTENT_TERM_KEY, test_value),
    ?assertEqual(test_value, span_convertor:get_record_definitions()),
    persistent_term:erase(?PERSISTENT_TERM_KEY),

    %% ensure that record definitions from otel_span.hrl are added correctly
    %% on the first span_convertor:get_record_definitions/0 call.
    ExpectedOtelRecords1 = ?ADD_RECORD(span, #{}),
    ExpectedOtelRecords2 = ?ADD_RECORD(span_limits, ExpectedOtelRecords1),
    ExpectedOtelRecords3 = ?ADD_RECORD(link, ExpectedOtelRecords2),
    ExpectedOtelRecords4 = ?ADD_RECORD(event, ExpectedOtelRecords3),
    OtelRecords = span_convertor:get_record_definitions(),
    ExpectedKeys = maps:keys(ExpectedOtelRecords4),
    ?assertEqual(ExpectedOtelRecords4, maps:with(ExpectedKeys, OtelRecords)),

    %% check that subsequent calls to span_convertor:get_record_definitions/0
    %% return the same result
    ?assertEqual(OtelRecords, span_convertor:get_record_definitions()),
    ?assertEqual(OtelRecords, span_convertor:get_record_definitions()).


span_conversion_prop_test(_Config) ->
    PropTest = span_conversion_property(),
    ?assert(proper:quickcheck(PropTest, [?NUMBER_OF_REPETITIONS, noshrink])),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


span_gen() ->
    %% without wrapping span() type into a ?LET macro compilation fails
    %% with the following error:
    %%   function span/0 undefined
    ?LET(X, span(), X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


contains_known_records(Term) ->
    RecordDefinitionsMap = span_convertor:get_record_definitions(),
    record_convertor_SUITE:contains_nested_records(Term, RecordDefinitionsMap).
