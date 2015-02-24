defmodule AsyncServerTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = AsyncServer.start_link(:anything)
    on_exit(fn() -> AsyncServer.stop(pid) end)
    {:ok, server: pid}
  end

  test "ping/pong test", %{server: pid} do
    assert :pong == AsyncServer.ping(pid)
  end

  test "add numbers", %{server: pid}  do
    assert 5 == AsyncServer.add(pid, 4, 1)
  end
end
