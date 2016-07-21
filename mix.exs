defmodule Evic.Mixfile do
  use Mix.Project

  def project do
    [
      app: :evic,
      version: "0.0.0",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps
    ]
  end

  def application do
    [
       applications: [:compiler, :poolgirl, :lager, :poolgirl],
       env: [],
       mod: {:evic_app, []}
    ]
  end

  defp deps do
    [
      {:lager, "~> 3.2"},
      {:bucs, "~> 0.1.5"},
      {:doteki, "~> 0.1"},
      {:poolgirl, "~> 0.1"},
      {:jsx, "~> 2.8"}    
    ]
  end
end