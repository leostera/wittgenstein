defmodule WittgensteinAdminWeb.PageController do
  use WittgensteinAdminWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
