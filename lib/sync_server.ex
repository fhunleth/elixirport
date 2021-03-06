defmodule SyncServer do
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
    executable = priv_dir <> "/test_port"
    port = Port.open({:spawn_executable, executable},
      [{:args, ["arg1", "arg2"]}, {:packet, 2}, :use_stdio, :binary, :exit_status])
    { :ok, %SyncServer{port: port, somearg: arg} }
  end

  def handle_call(:ping, _from, state) do
    {:ok, response} = call_port(state, :ping, [])
    {:reply, response, state}
  end

  def handle_call({:add, x, y}, _from, state) do
    {:ok, response} = call_port(state, :add, {x, y})
    {:reply, response, state}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_info({_, {:exit_status, _}}, state) do
    {:stop, :unexpected_exit, state}
  end

  # Private helper functions
  defp priv_dir() do
    to_string(:code.priv_dir(:elixirport))
  end

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
