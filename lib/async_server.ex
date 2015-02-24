defmodule AsyncServer do
  use GenServer

  defstruct port: nil,
            somearg: 0,
            requests: []

  # Public API
  def start_link(somearg, opts \\ []) do
    GenServer.start_link(__MODULE__, somearg, opts)
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
    [{:packet, 2}, :use_stdio, :binary, :exit_status])
    { :ok, %AsyncServer{port: port, somearg: arg} }
  end

  def handle_call(:ping, from, state) do
    send_to_port(state, :ping, [])
    state = %{state | :requests => state.requests ++ [from]}
    {:noreply, state}
  end

  def handle_call({:add, x, y}, from, state) do
    send_to_port(state, :add, {x, y})
    state = %{state | :requests => state.requests ++ [from]}
    {:noreply, state}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_info({_, {:data, message}}, state) do
    handle_response(message, state)
  end
  def handle_info({_, {:exit_status, _}}, state) do
    {:stop, :unexpected_exit, state}
  end

  # Private helper functions
  defp send_to_port(state, command, arguments) do
    msg = {command, arguments}
    send state.port, {self, {:command, :erlang.term_to_binary(msg)}}
  end
  defp handle_response(response, state) do
    [client | next_ones] = state.requests
    state = %{state | :requests => next_ones}

    GenServer.reply client, :erlang.binary_to_term(response)
    {:noreply, state}
  end

end
