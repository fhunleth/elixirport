defmodule ElixirportTest do
  use ExUnit.Case

  test "simple request" do
		{:ok,_} = TestServer.start_link
		assert :ok == TestServer.hello
  end
end
