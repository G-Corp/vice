defmodule ElixirTest do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(ElixirTest.Server, [])
    ]

    opts = [strategy: :one_for_one, name: ElixirTest.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
