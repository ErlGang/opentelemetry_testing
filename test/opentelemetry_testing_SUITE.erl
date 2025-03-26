-module(opentelemetry_testing_SUITE).
-behavior(ct_suite).

-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [ensure_started_test].

init_per_suite(Config) ->
    opentelemetry_testing:ensure_started(),
    Config.

end_per_suite(Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_started_test(_Config) ->
    %% basic check if opentelemetry_testing:ensure_started/0
    %% interface is idempotent
    ?assertEqual(ok, opentelemetry_testing:ensure_started()),
    ?assertEqual(ok, opentelemetry_testing:ensure_started()).
