defmodule ElixirTest.Mixfile do
  use Mix.Project

  def project do
    [app: :elixir_test,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger],
     mod: {ElixirTest, []}]
  end

  defp deps do
    []
  end
end
