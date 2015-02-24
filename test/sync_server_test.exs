defmodule SyncServerTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = SyncServer.start_link(:anything)
    on_exit(fn() -> SyncServer.stop(pid) end)
    {:ok, server: pid}
  end

  test "ping/pong test", %{server: pid} do
    assert :pong == SyncServer.ping(pid)
  end

  test "add numbers", %{server: pid}  do
    assert 5 == SyncServer.add(pid, 4, 1)
  end
end
