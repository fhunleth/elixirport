defmodule ServerTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = Server.start_link(:anything)
    on_exit(fn() -> Server.stop(pid) end)
    {:ok, server: pid}
  end

  test "ping/pong test", %{server: pid} do
    assert :pong == Server.ping(pid)
  end

  test "add numbers", %{server: pid}  do
    assert 5 == Server.add(pid, 4, 1)
  end
end
