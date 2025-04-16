-module(span_tree_generator).

%% PropEr generators
-export([span_tree_input_data_gen/2]).

-include_lib("proper/include/proper.hrl").

%% opentelemetry.hrl contains ?OTEL_STATUS_* definitions
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PropEr generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


span_tree_input_data_gen(MaxBranchWidth, MaxBranchDepth) when MaxBranchDepth > 0 ->
    ?LET(Children,
         ?LET(Length,
              integer(0, MaxBranchWidth),
              vector(Length,
                     span_tree_input_data_gen(MaxBranchWidth, MaxBranchDepth - 1))),
         {span_input_data_gen(), Children});
span_tree_input_data_gen(_MaxBranchWidth, _MaxBranchDepth) ->
    {span_input_data_gen(), []}.


span_input_data_gen() ->
    ?LET({Name, Kind, Status, Attributes, Events},
         {name_gen(), kind_gen(), status_gen(), attributes_gen(), events_gen()},
         #{
           name => Name,
           kind => Kind,
           attributes => Attributes,
           events => Events,
           status => Status
          }).


name_gen() ->
    not_empty_gen(oneof([atom(), small_binary()])).


not_empty_gen(Type) ->
    ?SUCHTHAT(Name, Type, Name =/= <<>> andalso Name =/= '' andalso Name =/= '_').


kind_gen() ->
    ?LET(X, opentelemetry:span_kind(), X).


status_gen() ->
    oneof([undefined,
           status_map_gen()]).


small_binary() ->
    ?LET(Length, integer(0, 10), binary(Length)).


status_map_gen() ->
    ?LET({StatusCode, Message},
         {opentelemetry:status_code(), small_binary()},
         case StatusCode of
             ?OTEL_STATUS_ERROR ->
                 #{code => StatusCode, message => Message};
             _ ->
                 %% the message is ignored and reset to <<"">>
                 %% for any status except ?OTEL_STATUS_ERROR
                 #{code => StatusCode, message => <<"">>}
         end).


attributes_gen() ->
    ?LET(PropList,
         ?LET(Length, integer(0, 5), vector(Length, attribute_gen())),
         %% ensure that invalid attributes are not added
         otel_attributes:process_attributes(PropList)).


attribute_gen() ->
    {name_gen(), attribute_value_gen()}.


attribute_value_gen() ->
    %% opentelemetry:attribute_value() also supports tuple values, but they are
    %% immediately converted into lists by otel_attributes:process_attributes/1
    oneof([?LET(Length, integer(0, 5), vector(Length, attribute_simple_value_gen())),
           attribute_simple_value_gen()]).


attribute_simple_value_gen() ->
    oneof([small_binary(), atom(), number(), boolean()]).


events_gen() ->
    ?LET(Length, integer(0, 5), vector(Length, event_gen())).


event_gen() ->
    ?LET({Name, Attributes},
         {name_gen(), attributes_gen()},
         #{name => Name, attributes => Attributes}).
