defmodule Evic.Mixfile do
  use Mix.Project

  def project do
    [
      app: :evic,
      version: "0.0.1",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      aliases: aliases
    ]
  end

  def application do
    [
       applications: [:compiler, :poolgirl, :lager],
       env: [],
       mod: {:evic_app, []}
    ]
  end

  defp deps do
    [
      {:lager, "~> 3.2.0"},
      {:bucs, "~> 0.1.9"},
      {:doteki, "~> 0.1.13"},
      {:poolgirl, "~> 0.1.4"},
      {:jsx, "~> 2.8.0"}    
    ]
  end

  defp aliases do
    [compile: [&pre_compile_hooks/1, "compile", &post_compile_hooks/1]]
  end

  defp pre_compile_hooks(_) do
    run_hook_cmd [
      "make -C c_src -f Makefile.build"
    ]
  end

  defp post_compile_hooks(_) do
    run_hook_cmd [
    ]
  end

  defp run_hook_cmd(commands) do
    {_, os} = :os.type
    for command <- commands, do: (fn
      ({regex, cmd}) ->
         if Regex.match?(Regex.compile!(regex), Atom.to_string(os)) do
           Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
         end
      (cmd) ->
        Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
      end).(command)
  end    
end