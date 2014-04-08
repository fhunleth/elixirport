defmodule TestServer do
  use GenServer.Behaviour

  defrecord State, port: nil

  # Public API
  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def stop do
    :gen_server.cast __MODULE__, :stop
  end

  def ping() do
    :gen_server.call __MODULE__, :ping
  end

  def add(x, y) do
    :gen_server.call __MODULE__, {:add, x, y}
  end

  # gen_server callbacks
  def init(_args) do
    executable = :code.priv_dir(:elixirport) ++ '/test_port'
    port = Port.open({:spawn_executable, executable},
    [{:packet, 2}, :use_stdio, :binary])
    state = State.new(port: port)
    { :ok, state }
  end

  def handle_call(:ping, _from, state) do
    {:ok, response} = call_port(state, :ping, [])
    {:reply, response, state }
  end

  def handle_call({:add, x, y}, _from, state) do
    {:ok, response} = call_port(state, :add, [x, y])
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
