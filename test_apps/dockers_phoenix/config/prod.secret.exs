use Mix.Config

# In this file, we keep production configuration that
# you likely want to automate and keep it away from
# your version control system.
config :dockers_phoenix, DockersPhoenix.Endpoint,
  secret_key_base: "33sTN/whKGC4eftyvAZ9QXlWO0a5y502wjsCZC5SCYIPkctxzTWKe7Z+2dqkyGbU"

# Configure your database
config :dockers_phoenix, DockersPhoenix.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "dockers_phoenix_prod",
  pool_size: 20
