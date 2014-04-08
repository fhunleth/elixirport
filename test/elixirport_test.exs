defmodule ElixirportTest do
  use ExUnit.Case

  test "ping/pong test" do
		{:ok,_} = TestServer.start_link
		assert :pong == TestServer.ping
  end
end
