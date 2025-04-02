-define(assertReceive(Pattern, Timeout),
        begin
            ((fun() ->
                  receive
                      Pattern -> ok
                  after
                      Timeout ->
                          erlang:error({assertReceive,
                                        [{module, ?MODULE},
                                         {line, ?LINE},
                                         {pattern, (??Pattern)},
                                         {timeout, Timeout},
                                         process_info(self(), messages)]})
                  end
              end)())
        end).

-define(assertLogMessage(LogEventPattern),
        ?assertReceive({log_event, LogEventPattern}, 100)).

-define(assertLogMessage(MsgFormatPattern, Level, MFA),
        ?assertLogMessage(#{
                            meta := #{mfa := MFA},
                            msg := {MsgFormatPattern ++ _, _},
                            level := Level
                           })).
