ExUnit.start()

## Use "mix test --force" instead of "mix test" to ensure that the :opentelemetry_testing
## application (_build/test/lib/opentelemetry_testing) is rebuilt after execution of
## the "rebar ct" command, since mix and rebar3 generate different lists of runtime
## application dependencies in the *.app file. For more information, see the :applications
## key in the _build/test/lib/opentelemetry_testing/ebin/opentelemetry_testing.app file.
IO.inspect(Application.loaded_applications())
IO.inspect(Application.started_applications())
IO.inspect(Application.get_all_env(:opentelemetry))
IO.inspect(Application.ensure_all_started(:opentelemetry))
Process.sleep(1000)

OpentelemetryTesting.ensure_started()
