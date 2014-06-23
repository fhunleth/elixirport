defmodule Server do
  use GenServer

  defstruct port: nil,
            somearg: 0

  # Public API
  def start_link(somearg) do
    GenServer.start_link(__MODULE__, somearg)
  end

  def stop(pid) do
    GenServer.cast(pid, :stop)
  end

  def ping(pid) do
    GenServer.call(pid, :ping)
  end

  def add(pid, x, y) do
    GenServer.call(pid, {:add, x, y})
  end

  # gen_server callbacks
  def init(arg) do
    executable = :code.priv_dir(:elixirport) ++ '/test_port'
    port = Port.open({:spawn_executable, executable},
    [{:packet, 2}, :use_stdio, :binary])
    { :ok, %Server{port: port, somearg: arg} }
  end

  def handle_call(:ping, _from, state) do
    {:ok, response} = call_port(state, :ping, [])
    {:reply, response, state }
  end

  def handle_call({:add, x, y}, _from, state) do
    {:ok, response} = call_port(state, :add, {x, y})
    {:reply, response, state }
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  # Private helper functions
  defp call_port(state, command, arguments) do
    msg = {command, arguments}
    send state.port, {self, {:command, :erlang.term_to_binary(msg)}}
    receive do
      {_, {:data, response}} ->
        {:ok, :erlang.binary_to_term(response)}
        _ -> :error
    end
  end

end
