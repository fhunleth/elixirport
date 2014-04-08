defmodule ElixirportTest do
  use ExUnit.Case

	setup do
		{:ok,_} = TestServer.start_link
		:ok
	end

	teardown do
		TestServer.stop
		:ok
	end

  test "ping/pong test" do
		assert :pong == TestServer.ping
  end

	test "add numbers" do
		assert 5 == TestServer.add(4, 1)
	end
end
