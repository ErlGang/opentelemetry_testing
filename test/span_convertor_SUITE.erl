-module(span_convertor_SUITE).
-behavior(ct_suite).

-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-compile([export_all, nowarn_export_all]).

-define(ADD_RECORD(RecordName, Acc),
        span_convertor:add_record(RecordName, record_info(fields, RecordName), Acc)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [init_test].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_test(_Config) ->
    %% ensure that record definitions from otel_span.hrl are added correctly.
    ExpectedOtelRecords1 = ?ADD_RECORD(span, #{}),
    ExpectedOtelRecords2 = ?ADD_RECORD(span_limits, ExpectedOtelRecords1),
    ExpectedOtelRecords3 = ?ADD_RECORD(link, ExpectedOtelRecords2),
    ExpectedOtelRecords4 = ?ADD_RECORD(event, ExpectedOtelRecords3),
    clean_record_definitions(),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_record_definitions() ->
    %% 'undefined' is the special value used to erase previously stored
    %% record definitions.
    span_convertor:store_record_definitions(undefined).
