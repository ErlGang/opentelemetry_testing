defmodule OpentelemetryTesting.MixProject do
  use Mix.Project

  @app :opentelemetry_testing

  def project do
    [
      app: @app,
      version: version(),
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      erlc_options: erlc_options(Mix.env()),
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

    rebar_deps =
      for {hex_dependency, version} <- rebar_config[:deps] do
        {hex_dependency, "#{version}"}
      end

    [
      {:stream_data, "~> 1.2", only: :test},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false}
      | rebar_deps
    ]
  end

  defp version do
    {:ok, [{:application, @app, app_config}]} =
      :file.consult("src/#{@app}.app.src")

    "#{app_config[:vsn]}"
  end

  defp elixirc_paths(:test), do: ["lib", "test"]
  defp elixirc_paths(_), do: ["lib"]

  defp erlc_options(:test), do: [{:d, :TEST, true}]
  defp erlc_options(_), do: []
end
