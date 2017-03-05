# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Configures the endpoint
config :alice_web, AliceWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "O/UrcoddR5BNxj7bceR02s/hPI0oWrcAR3lmBdGNu3T6/82kdTFSY9gh1CVT7Hqj",
  render_errors: [view: AliceWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: AliceWeb.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
