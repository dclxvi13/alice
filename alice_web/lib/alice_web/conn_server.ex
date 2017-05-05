defmodule AliceWeb.ConnServer do
  @moduledoc false

  use GenServer

  # API

  def start_link(name \\ nil) do
    #IO.puts "module: #{__MODULE__}"
    GenServer.start_link(AliceWeb.ConnServer, :ok, [name: name])
  end

  def new_socket(socket) do
    GenServer.cast(AliceWeb.ConnServer, {:socket, socket})
  end

  def new_message(message) do
    GenServer.cast(AliceWeb.ConnServer, {:message, message})
  end

  def from_alice(message) do
    GenServer.cast(AliceWeb.ConnServer, {:alice, message})
  end

  def store() do
    GenServer.call(AliceWeb.ConnServer, :store)
  end

  # Handlers

  def init(:ok) do
    {:ok, %{messages: []}}
  end

  def handle_call(:store, _from, %{messages: messages} = state) do
    {:reply, {:messages, messages}, %{state | messages: []}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:socket, socket}, state) do
    {:noreply, %{state | socket: socket}}
  end

  def handle_cast({:message, message}, %{messages: messages} = state) do
    {:noreply, %{state | messages: [message | messages]}}
  end

  def handle_cast({:alice, message}, %{socket: socket} = state) do
    send_to_channel socket, message
    {:noreply, state}
  end

  def handle_cast({:alice, _message}, state) do
    {:noreply, state}
  end

  def handle_cast(_msg, state) do
    IO.puts "unknown"
    {:noreply, state}
  end

  # Internal

  defp send_to_channel(socket, message) do
    send socket, %{"name" => "Alice", "message" => message}
  end

  defp timer_fn() do
    :timer.sleep(:timer.seconds(5))
    from_alice "Hello"
    timer_fn()
  end

end