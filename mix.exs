defmodule Vice.Mixfile do
  use Mix.Project

  def project do
    [
      app: :vice,
      version: "0.0.1",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
       applications: [:compiler, :poolgirl, :lager],
       env: [],
       mod: {:vice_app, []}
    ]
  end

  defp deps do
    [
      {:lager, "~> 3.2.0"},
      {:bucs, "~> 1.0.9"},
      {:doteki, "~> 1.0.5"},
      {:poolgirl, "~> 1.1.2"},
      {:jsx, "~> 2.8.0"}
    ]
  end

  defp aliases do
    [compile: &compile_with_hooks/1]
  end

  defp compile_with_hooks(args) do
    pre_compile_hooks()
    result = Mix.Task.run("compile", args)
    post_compile_hooks()
    result
  end

  defp pre_compile_hooks() do
    run_hook_cmd [
      "make -C c_src -f Makefile.build"
    ]
  end

  defp post_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp run_hook_cmd(commands) do
    {_, os} = :os.type
    for command <- commands, do: (fn
      ({regex, cmd}) ->
         if Regex.match?(Regex.compile!(regex), Atom.to_string(os)) do
           Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(trim(x)) end
         end
      (cmd) ->
        Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(trim(x)) end
      end).(command)
  end

  defp trim(x) do
    if Version.compare(System.version, "1.5.0") == :lt do
      String.strip(x)
    else
      String.trim(x)
    end
  end
end