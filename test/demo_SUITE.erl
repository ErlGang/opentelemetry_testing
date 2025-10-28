-module(demo_SUITE).
-behavior(ct_suite).

-include_lib("stdlib/include/assert.hrl").

%% opentelemetry.hrl contains #span_ctx{} record declaration
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
%% otel_tracer.hrl defines tracing macros (e.g. ?with_span())
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([demo_test/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CT callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    [demo_test].


init_per_suite(Config) ->
    opentelemetry_testing:ensure_started(),
    opentelemetry_testing:reset(),
    Config.


end_per_suite(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% demo test case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
demo_test(_Config) ->
    %% wrap demo_scenario() in a root span and extract its trace_id and span_id.
    {TraceId, SpanId} =
        ?with_span(<<"dummy root span">>,
                   fun(SpanCtx) ->
                           demo_scenario(),
                           #span_ctx{trace_id = TraceId, span_id = SpanId} = SpanCtx,
                           {TraceId, SpanId}
                   end),

    %% wait for the root span to be reported.
    TimeoutMs = 300,
    ?assertMatch(
      {ok, #{trace_id := TraceId, span_id := SpanId, name := <<"dummy root span">>}},
      opentelemetry_testing:wait_for_span(TraceId, SpanId, TimeoutMs)),

    %% build a span tree structure for the root span.
    {ok, SpanTree} = opentelemetry_testing:build_span_tree(TraceId, SpanId),

    %% create a span tree template with all the expected span attributes,
    %% links, events, etc. You can also use '$special_atoms' to extract
    %% some data, such as the trace_id/span_id of the linked spans.
    SpanTreePattern =
        {#{name => <<"dummy root span">>},
         [{#{
             name => <<"main span">>,
             attributes => #{
                             'span attr 1' => <<"1">>,
                             <<"span attr 3">> => 3
                            },
             status => #{code => error},
             kind => client,
             events => [#{
                          name => 'event 2',
                          attributes => #{'event attr 2' => <<"2">>}
                         }],
             links => [#{
                         attributes => #{'link attr 1' => <<"1">>},
                         trace_id => '$linked_trace_id',
                         span_id => '$linked_span_id'
                        }]
            },
           [{#{
               name => nested_span,
               attributes => #{<<"span attr 2">> => 2}
              },
             []}]}]},

    %% verify the span tree by matching it against the pattern.
    {true, MatchedValues} = opentelemetry_testing:match(SpanTree, SpanTreePattern),

    %% use MatchedValues map for further analysis, e.g. check the linked spans.
    #{
      '$linked_trace_id' := LinkedTraceId,
      '$linked_span_id' := LinkedSpanId
     } = MatchedValues,

    %% get the linked span tree and verify it.
    {ok, LinkedSpanTree} =
        opentelemetry_testing:build_span_tree(LinkedTraceId, LinkedSpanId),

    LinkedSpanTreePattern =
        {#{
           name => linked_span,
           events => [#{
                        name => <<"event 1">>,
                        attributes => #{<<"event attr 1">> => 1}
                       }]
          },
         []},

    {true, _} = opentelemetry_testing:match(LinkedSpanTree, LinkedSpanTreePattern).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% demo scenario
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
demo_scenario() ->

    %% linked span
    Self = self(),
    spawn(fun() ->
                  SpanCtx = ?with_span(linked_span,
                                       fun(SpanCtx) ->
                                               ?add_event(<<"event 1">>,
                                                          [{<<"event attr 1">>, 1}]),
                                               SpanCtx
                                       end),
                  Self ! {linked_span_ctx, SpanCtx}
          end),
    LinkedSpanCtx = receive {linked_span_ctx, SpanCtx} -> SpanCtx end,
    Links = [{LinkedSpanCtx, [{'link attr 1', <<"1">>}]}],

    %% nested span
    ?with_span(
      <<"main span">>,
      #{links => Links, kind => client},
      fun(_SpanCtx1) ->
              ?set_attributes([{'span attr 1', <<"1">>}]),

              ?with_span(
                nested_span,
                fun(_SpanCtx2) ->
                        ?set_attributes([{<<"span attr 2">>, 2}])
                end),

              ?set_attributes([{<<"span attr 3">>, 3}]),
              ?add_event('event 2', [{'event attr 2', <<"2">>}]),
              ?set_status(error)
      end).
