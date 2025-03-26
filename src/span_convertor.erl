-module(span_convertor).

-define(PERSISTENT_TERM_KEY, {?MODULE, record_definition_map}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/0, records_to_maps/1, maps_to_records/1]).

-ifdef(TEST).

-export([store_record_definitions/1, get_record_definitions/0, add_record/3]).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    %% read internal record definitions for attributes, events and links.
    %% both otel_links and otel_events modules include otel_span.hrl, so
    %% there is no need to add records defined in that header manually.
    OtelRecords1 = get_records(otel_attributes, #{}),
    OtelRecords2 = get_records(otel_events, OtelRecords1),
    OtelRecords3 = get_records(otel_links, OtelRecords2),
    maybe_store_record_definitions(OtelRecords3).

records_to_maps(Term) ->
    RecordDefinitionMap = get_record_definitions(),
    recursive_record_to_map(Term, RecordDefinitionMap).

maps_to_records(Term) ->
    RecordDefinitionMap = get_record_definitions(),
    recursive_map_to_record(Term, RecordDefinitionMap).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recursive_record_to_map(List, RecordDefinitions) when is_list(List) ->
    [recursive_record_to_map(Element, RecordDefinitions) || Element <- List];
recursive_record_to_map(Map, RecordDefinitions) when is_map(Map) ->
    #{Key => recursive_record_to_map(Value, RecordDefinitions) || Key := Value <- Map};
recursive_record_to_map(Tuple, RecordDefinitions) when is_tuple(Tuple) ->
    case maybe_record_to_map(Tuple, RecordDefinitions) of
        {true, Map} when is_map(Map) ->
            recursive_record_to_map(Map, RecordDefinitions);
        false ->
            List1 = tuple_to_list(Tuple),
            List2 = recursive_record_to_map(List1, RecordDefinitions),
            list_to_tuple(List2)
    end;
recursive_record_to_map(Term, _RecordDefinitions) ->
    Term.

recursive_map_to_record(List, RecordDefinitions) when is_list(List) ->
    [recursive_map_to_record(Element, RecordDefinitions) || Element <- List];
recursive_map_to_record(Map, RecordDefinitions) when is_map(Map) ->
    case maybe_map_to_record(Map, RecordDefinitions) of
        {true, Tuple} ->
            recursive_map_to_record(Tuple, RecordDefinitions);
        false ->
            #{Key => recursive_map_to_record(Value, RecordDefinitions) || Key := Value <- Map}
    end;
recursive_map_to_record(Tuple, RecordDefinitions) when is_tuple(Tuple) ->
    List1 = tuple_to_list(Tuple),
    List2 = recursive_map_to_record(List1, RecordDefinitions),
    list_to_tuple(List2);
recursive_map_to_record(Term, _RecordDefinitions) ->
    Term.

maybe_record_to_map({}, _RecordDefinitions) -> false;
maybe_record_to_map(Tuple, RecordDefinitions) ->
    case {element(1,Tuple), tuple_size(Tuple)} of
        MapKey when is_map_key(MapKey, RecordDefinitions) ->
            Fields = maps:get(MapKey, RecordDefinitions),
            {true, record_to_map(Tuple, Fields)};
        _ ->
            false
    end.

maybe_map_to_record(#{'$record_name' := RecordName} = Map, RecordDefinitions)
        when is_map_key({RecordName, map_size(Map)}, RecordDefinitions) ->
    Fields = maps:get({RecordName, map_size(Map)}, RecordDefinitions),
    {true, map_to_record(Map, Fields)};
maybe_map_to_record(_Map, _RecordDefinitions) ->
    false.

record_to_map(Record, Fields) when length(Fields) =:= tuple_size(Record),
                                   element(1, Record) =:= hd(Fields) ->
    Values = tuple_to_list(Record),
    Keys = ['$record_name' | tl(Fields)],
    KeyValueList = lists:zip(Keys, Values),
    maps:from_list(KeyValueList).

map_to_record(#{'$record_name' := RecordName} = Map, Fields)
        when length(Fields) =:= map_size(Map), RecordName =:= hd(Fields) ->
    Keys = ['$record_name' | tl(Fields)],
    %% maps:get/2 crashes if there's no such key in the map.
    Values = [maps:get(K, Map) || K <- Keys],
    list_to_tuple(Values).

get_records(Module, #{} = Acc) ->
    Path = code:which(Module),
    {ok, {_, [{abstract_code, {_, AbstractCode}}]}} =
        beam_lib:chunks(Path, [abstract_code]),
    lists:foldl(fun maybe_add_record/2, Acc, AbstractCode).

maybe_add_record({attribute, _, record, {RecordName, RecordFields}}, Acc) ->
    Fields = [get_field_name(F) || F <- RecordFields],
    add_record(RecordName, Fields, Acc);
maybe_add_record(_, Acc) -> Acc.

get_field_name({record_field, _, {atom, _, FieldName}}) -> FieldName;
get_field_name({record_field, _, {atom, _, FieldName}, _Default}) -> FieldName;
get_field_name({typed_record_field, RecordField, _Type}) -> get_field_name(RecordField).

add_record(RecordName, Fields, #{} = Acc) ->
    FieldsWithName = [RecordName | Fields],
    RecordSize = length(FieldsWithName),
    MapKey = {RecordName, RecordSize},
    case Acc of
        #{MapKey := Value} when Value =:= FieldsWithName ->
            Acc;
        #{MapKey := Value} when Value =/= FieldsWithName ->
            error({inconsistent_record_definitions, Acc, FieldsWithName});
        _ ->
            Acc#{MapKey => FieldsWithName}
    end.

maybe_store_record_definitions(NewRecordDefinitionMap) ->
    case persistent_term:get(?PERSISTENT_TERM_KEY, undefined) of
        undefined ->
            store_record_definitions(NewRecordDefinitionMap);
        RecordDefinitionMap when RecordDefinitionMap =:= NewRecordDefinitionMap ->
            ok;
        RecordDefinitionMap ->
            error({error, inconsistent_record_definition_map,
                   RecordDefinitionMap, NewRecordDefinitionMap})
    end.

store_record_definitions(undefined) ->
    persistent_term:erase(?PERSISTENT_TERM_KEY);
store_record_definitions(RecordDefinitionMap) ->
    persistent_term:put(?PERSISTENT_TERM_KEY, RecordDefinitionMap).

get_record_definitions() ->
    persistent_term:get(?PERSISTENT_TERM_KEY).
