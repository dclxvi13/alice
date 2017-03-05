defmodule AliceWeb.RoomChannel do
  @moduledoc false
  use Phoenix.Channel

  import AliceWeb.ConnServer

  def join("lobby", _message, socket) do
    new_socket(self)
    {:ok, socket}
  end

  def handle_in("new_message", %{"message" => message, "name" => name} = payload, socket) do
    broadcast! socket, "new_message", payload
    new_message(message)
    {:noreply, socket}
  end

  def handle_out("new_message", payload, socket) do
    push socket, "new_message", payload
    {:noreply, socket}
  end

  def handle_info(payload, socket) do
    push socket, "new_message", payload
    {:noreply, socket}
  end

end
