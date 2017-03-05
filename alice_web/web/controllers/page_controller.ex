defmodule AliceWeb.PageController do
  use AliceWeb.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
