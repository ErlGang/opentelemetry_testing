-module(span_collector).
-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-define(TABLE, '$spans_table').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([ensure_started/0,
         reset/0,
         get_span_id_by_name/1,
         get_spans_by_name/1,
         wait_for_span/2,
         build_span_tree/1, build_span_tree/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ensure_started() ->
    case gen_server:start({local, ?MODULE}, ?MODULE, no_args, []) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} ->
            %% ensure the process (and ETS table) is already initialized
            %% and otel_exporter_pid is configured properly.
            gen_server:call(?MODULE, wait_for_init);
        Error ->
            ?LOG_ERROR("failed to start span_collector: ~p", [Error]),
            {error, failed_to_start_span_collector}
    end.


reset() ->
    gen_server:call(?MODULE, reset).


get_span_id_by_name(Name) ->
    MatchPattern = #span{name = Name, span_id = '$1', _ = '_'},
    case ets:match(?TABLE, MatchPattern) of
        [[SpandId]] -> {ok, SpandId};
        [] -> {error, not_found};
        [_, _ | _] -> {error, name_is_not_unique}
    end.


get_spans_by_name(Name) ->
    MatchPattern = #span{name = Name, _ = '_'},
    ets:match_object(?TABLE, MatchPattern).


wait_for_span(_SpanId, Timeout) when Timeout < 0 ->
    {error, timeout};
wait_for_span(SpanId, Timeout) ->
    RetryAfter = 150,
    MatchPattern = #span{span_id = SpanId, _ = '_'},
    case ets:match_object(?TABLE, MatchPattern) of
        [_] -> ok;
        [] ->
            timer:sleep(RetryAfter),
            wait_for_span(SpanId, Timeout - RetryAfter);
        [_, _ | _] ->
            %% this should never happen
            {error, span_id_is_not_unique}
    end.


build_span_tree(SpanId) ->
    build_span_tree(SpanId, fun take_span_as_is/1).


build_span_tree(SpanId, ConvertSpanFn) ->
    MatchPattern = #span{span_id = SpanId, parent_span_id = undefined, _ = '_'},
    case ets:match_object(?TABLE, MatchPattern) of
        [Span] ->
            build_tree_for_span(Span, ConvertSpanFn);
        [] ->
            {error, not_found};
        [_, _ | _] ->
            %% this should never happen
            {error, span_id_is_not_unique}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init(no_args) ->
    ets:new(?TABLE,
            [bag,
             protected,
             named_table,
             {read_concurrency, true},
             {keypos, #span.parent_span_id}]),
    configure_opentelemetry(),
    {ok, no_state}.


handle_call(reset, _From, State) ->
    ets:delete_all_objects(?TABLE),
    {reply, ok, State};
handle_call(wait_for_init, _From, State) ->
    configure_opentelemetry(),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    ?LOG_ERROR("unexpected call request: ~p, state: ~p", [Request, State]),
    {stop, {unexpected_call_request, Request}, not_implemented, State}.


handle_cast(Request, State) ->
    ?LOG_ERROR("unexpected cast request: ~p, state: ~p", [Request, State]),
    {stop, {unexpected_cast_request, Request}, State}.


handle_info({span, #span{} = Span}, State) ->
    ets:insert(?TABLE, Span),
    {noreply, State};
handle_info(Info, State) ->
    ?LOG_ERROR("unexpected info message: ~p, state: ~p", [Info, State]),
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


configure_opentelemetry() ->
    ok = otel_simple_processor:set_exporter(otel_exporter_pid, self()).


take_span_as_is(Span) -> Span.


build_tree_for_span(Span, ConvertSpanFn) ->
    %% ?TABLE is a 'bag' table with the parent_span_id field used as key
    case ets:lookup(?TABLE, Span#span.span_id) of
        [] -> {ConvertSpanFn(Span), []};
        SubSpans ->
            SubSpansWithChildren = [ build_tree_for_span(S, ConvertSpanFn) || S <- SubSpans ],
            {ConvertSpanFn(Span), SubSpansWithChildren}
    end.
