defmodule AliceWeb.ConnWorker do
  @moduledoc false

  def accept(port) do
    # The options below mean:
    #
    # 1. `:binary` - receives data as binaries (instead of lists)
    # 2. `packet: :line` - receives data line by line
    # 3. `active: false` - blocks on `:gen_tcp.recv/2` until data is available
    # 4. `reuseaddr: true` - allows us to reuse the address if the listener crashes
    #
    {:ok, socket} = :gen_tcp.listen(port,
                      [:binary, packet: :line, active: false, reuseaddr: true])
    loop_acceptor(socket)
  end

  defp loop_acceptor(socket) do
    {:ok, client} = :gen_tcp.accept(socket)
    serve(client)
    loop_acceptor(socket)
  end

  defp serve(socket) do
    case :gen_tcp.recv(socket, 0) do
        {:ok, "store\r\n"} ->
            #IO.puts "store"
            msgs = AliceWeb.ConnServer.store()
            rep = :io_lib.format("~w", [msgs])
            :gen_tcp.send(socket, rep)
        {:error, :closed} ->
          IO.puts "Connection closed"
          exit(:normal)
        line -> IO.inspect line
    end

    serve(socket)
  end

end
