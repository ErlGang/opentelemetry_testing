-module(record_convertor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([add_record/3, records_to_maps/2, maps_to_records/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


add_record(RecordName, Fields, #{} = RecordDefinitionsMap) ->
    RecordSize = length(Fields) + 1,
    MapKey = {RecordName, RecordSize},
    case RecordDefinitionsMap of
        #{MapKey := Value} when Value =:= Fields ->
            RecordDefinitionsMap;
        #{MapKey := Value} when Value =/= Fields ->
            error({inconsistent_record_definitions, RecordDefinitionsMap,
                                                    MapKey, Fields});
        _ ->
            RecordDefinitionsMap#{MapKey => Fields}
    end.


records_to_maps(Term, RecordDefinitionsMap) ->
    recursive_record_to_map(Term, RecordDefinitionsMap).


maps_to_records(Term, RecordDefinitionsMap) ->
    recursive_map_to_record(Term, RecordDefinitionsMap).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


recursive_record_to_map(List, RecordDefinitions) when is_list(List) ->
    [ recursive_record_to_map(Element, RecordDefinitions) || Element <- List ];
recursive_record_to_map(Map, RecordDefinitions) when is_map(Map) ->
    #{ Key => recursive_record_to_map(Value, RecordDefinitions) || Key := Value <- Map };
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
    [ recursive_map_to_record(Element, RecordDefinitions) || Element <- List ];
recursive_map_to_record(Map, RecordDefinitions) when is_map(Map) ->
    case maybe_map_to_record(Map, RecordDefinitions) of
        {true, Tuple} ->
            recursive_map_to_record(Tuple, RecordDefinitions);
        false ->
            #{ Key => recursive_map_to_record(Value, RecordDefinitions) || Key := Value <- Map }
    end;
recursive_map_to_record(Tuple, RecordDefinitions) when is_tuple(Tuple) ->
    List1 = tuple_to_list(Tuple),
    List2 = recursive_map_to_record(List1, RecordDefinitions),
    list_to_tuple(List2);
recursive_map_to_record(Term, _RecordDefinitions) ->
    Term.


maybe_record_to_map({}, _RecordDefinitions) -> false;
maybe_record_to_map(Tuple, RecordDefinitions) ->
    case {element(1, Tuple), tuple_size(Tuple)} of
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


record_to_map(Record, Fields) when length(Fields) + 1 =:= tuple_size(Record) ->
    Values = tuple_to_list(Record),
    Keys = ['$record_name' | Fields],
    KeyValueList = lists:zip(Keys, Values),
    maps:from_list(KeyValueList).


map_to_record(#{'$record_name' := _} = Map, Fields)
  when length(Fields) + 1 =:= map_size(Map) ->
    Keys = ['$record_name' | Fields],
    %% maps:get/2 crashes if there's no such key in the map.
    Values = [ maps:get(K, Map) || K <- Keys ],
    list_to_tuple(Values).
