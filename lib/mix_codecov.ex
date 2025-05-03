defmodule Mix.Tasks.Codecov do
  use Mix.Task
  @shortdoc "Build json report from exported test coverage"
  def run(args) do
    Mix.ensure_application!(:tools)

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
          ## ignore itself
          m !== __MODULE__ do
        {:ok, coverage} = :cover.analyse(m, :calls, :line)
        compile_info = m.module_info(:compile)

        source_file =
          compile_info[:source]
          |> List.to_string()
          |> Path.relative_to(File.cwd!())
          |> IO.inspect()

        source_code = File.read!(source_file)

        number_of_lines =
          ((Regex.scan(~r"\n", source_code, return: :index) |> length()) + 1)
          |> IO.inspect(label: :number_of_lines)

        coverage =
          for {{^m, line}, calls} <- coverage,
              ## elixir modules have a weird 0 calls coverage reporting for line 0, ignore it
              line !== 0,
              ## element with index 0 is dropped later, hence adding 1 to the number_of_lines
              reduce: :array.new(number_of_lines + 1, default: nil) do
            array -> :array.set(line, calls, array)
          end

        IO.inspect(:array.size(coverage), label: :size)

        ## array always starts with index 0, so drop the first element.
        %{name: source_file, source: source_code, coverage: tl(:array.to_list(coverage))}
      end
      |> Enum.sort_by(& &1.name)
      ## there can be multiple elixir modules in one file. merge coverage data for such modules
      |> merge_all_repeating_files([])

    json_data = Jason.encode!(%{source_files: modules})
    File.write!("codecov.json", json_data)
  end

  ## File array must be sorted before calling this function
  defp merge_all_repeating_files([], acc) do
    Enum.reverse(acc)
  end

  defp merge_all_repeating_files(
         [%{name: file_name} = f1, %{name: file_name} = f2 | tail],
         acc
       ) do
    f = merge_files(f1, f2)
    merge_all_repeating_files([f | tail], acc)
  end

  defp merge_all_repeating_files([f | tail], acc) do
    merge_all_repeating_files(tail, [f | acc])
  end

  defp merge_files(f1, f2) do
    coverage = merge_coverage(f1.coverage, f2.coverage)
    %{f1 | coverage: coverage}
  end

  defp merge_coverage(c1, c2) do
    Enum.zip_with([c1, c2], &merge_line_coverage/1)
  end

  defp merge_line_coverage([nil, nil]) do
    nil
  end

  defp merge_line_coverage([nil, int]) when is_integer(int) do
    int
  end

  defp merge_line_coverage([int, nil]) when is_integer(int) do
    int
  end
end
