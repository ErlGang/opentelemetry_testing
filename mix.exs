defmodule OpentelemetryTesting.MixProject do
  use Mix.Project

  @app :opentelemetry_testing

  def project do
    [
      app: @app,
      version: version(),
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    {:ok, rebar_config} = :file.consult("rebar.config")

    for {hex_dependency, version} <- rebar_config[:deps] do
      {hex_dependency, "#{version}"}
    end
  end

  defp version do
    {:ok, [{:application, @app, app_config}]} =
      :file.consult("src/#{@app}.app.src")

    "#{app_config[:vsn]}"
  end
end
