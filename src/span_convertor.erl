-module(span_convertor).

-define(PERSISTENT_TERM_KEY, {?MODULE, record_definition_map}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([records_to_maps/1, maps_to_records/1]).

-ifdef(TEST).

-export([get_record_definitions/0]).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec records_to_maps(term()) -> term().
records_to_maps(Term) ->
    RecordDefinitionMap = get_record_definitions(),
    record_convertor:records_to_maps(Term, RecordDefinitionMap).


-spec maps_to_records(term()) -> term().
maps_to_records(Term) ->
    RecordDefinitionMap = get_record_definitions(),
    record_convertor:maps_to_records(Term, RecordDefinitionMap).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_record_definitions() ->
    case persistent_term:get(?PERSISTENT_TERM_KEY, undefined) of
        undefined ->
            store_record_definitions();
        RecordDefinitionMap -> RecordDefinitionMap
    end.


store_record_definitions() ->
    %% read internal record definitions for attributes, events and links.
    %% both otel_links and otel_events modules include otel_span.hrl, so
    %% there is no need to add records defined in that header manually.
    Modules = [otel_attributes, otel_events, otel_links],
    RecordDefinitionMap =
        lists:foldl(fun(M, Acc) -> read_records(M, Acc) end, #{}, Modules),
    persistent_term:put(?PERSISTENT_TERM_KEY, RecordDefinitionMap),
    RecordDefinitionMap.


read_records(Module, #{} = Acc) ->
    Path = code:which(Module),
    {ok, {_, [{abstract_code, {_, AbstractCode}}]}} =
        beam_lib:chunks(Path, [abstract_code]),
    lists:foldl(fun maybe_add_record/2, Acc, AbstractCode).


maybe_add_record({attribute, _, record, {RecordName, RecordFields}}, Acc) ->
    Fields = [ get_field_name(F) || F <- RecordFields ],
    record_convertor:add_record(RecordName, Fields, Acc);
maybe_add_record(_, Acc) -> Acc.


get_field_name({record_field, _, {atom, _, FieldName}}) -> FieldName;
get_field_name({record_field, _, {atom, _, FieldName}, _Default}) -> FieldName;
get_field_name({typed_record_field, RecordField, _Type}) -> get_field_name(RecordField).
