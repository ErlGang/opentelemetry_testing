-module(test_logger_handler).
-compile([export_all, nowarn_export_all]).


add_handler() ->
    ok = logger:add_handler(?MODULE, ?MODULE, #{}),
    logger:i().


remove_handler() ->
    ok = logger:remove_handler(?MODULE),
    logger:i().


set_pid() ->
    ok = logger:set_handler_config(?MODULE, config, self()),
    logger:i().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% logger_handler callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log(LogEvent, Config) ->
    ct:log("LogEvent = ~p, Config = ~p", [LogEvent, Config]),
    %% The return value from this function is ignored by Logger
    case maps:get(config, Config, undefined) of
        Pid when is_pid(Pid) ->
            erlang:send(Pid, {log_event, LogEvent});
        _ -> ok
    end.
