ExUnit.start

Mix.Task.run "ecto.create", ~w(-r DockersPhoenix.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r DockersPhoenix.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(DockersPhoenix.Repo)

