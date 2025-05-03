defmodule Mix.Tasks.Codecov do
  use Mix.Task
  @shortdoc "Build json report from exported test coverage"
  def run(args) do
    Mix.ensure_application!(:tools)
    project_config = Mix.Project.config()
    test_coverage_config = project_config[:test_coverage]
    ignore_modules = test_coverage_config[:ignore_modules]

    cover_paths =
      case args do
        [] -> ["cover"]
        _ -> args
      end

    {:ok, pid} = :cover.start()
    {:ok, string_io} = StringIO.open("")
    Process.group_leader(pid, string_io)

    case Enum.flat_map(cover_paths, &Path.wildcard(Path.join(&1, "*.coverdata"))) do
      [] ->
        Mix.shell().error(
          "Could not find .coverdata file in any of the paths: " <>
            Enum.join(cover_paths, ", ")
        )

      entries ->
        for entry <- entries do
          Mix.shell().info("Importing cover results: #{entry}")
          :ok = :cover.import(String.to_charlist(entry))
        end
    end

    modules =
      for m <- :cover.imported_modules(),
          ## analyze only loaded modules
          Code.ensure_loaded?(m),
          not Enum.member?(ignore_modules, m),
          reduce: %{} do
        acc ->
          {:ok, coverage} = :cover.analyse(m, :calls, :line)
          compile_info = m.module_info(:compile)

          source_file =
            compile_info[:source]
            |> List.to_string()
            |> Path.relative_to(File.cwd!())

          cover_info =
            case Map.get(acc, source_file) do
              nil ->
                source_code = File.read!(source_file)

                number_of_lines =
                  (Regex.scan(~r"\n", source_code, return: :index) |> length()) + 1

                ## element with index 0 is dropped later, hence adding 1 to the number_of_lines
                coverage_array = :array.new(number_of_lines + 1, default: nil)

                %{name: source_file, source: source_code, coverage: coverage_array}

              map ->
                map
            end

          coverage =
            for {{^m, line}, calls} <- coverage,
                ## elixir modules have a weird 0 calls coverage reporting for line 0, ignore it
                line !== 0,
                reduce: cover_info.coverage do
              array -> :array.set(line, calls, array)
            end

          Map.put(acc, source_file, %{cover_info | coverage: coverage})
      end
      |> Map.values()
      |> Enum.map(fn cover_info ->
        ## array always starts with index 0, so drop the first element.
        %{cover_info | coverage: tl(:array.to_list(cover_info.coverage))}
      end)
      |> Enum.sort_by(& &1.name)

    json_data = Jason.encode!(%{source_files: modules})
    File.write!("codecov.json", json_data)
  end
end
