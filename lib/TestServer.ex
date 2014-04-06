defmodule TestServer do
	use GenServer.Behaviour

	# Public API
	def start_link do
		:gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
	end

	def hello() do
		:gen_server.call __MODULE__, :hello
	end

	# GenServer callbacks
	def init(_args) do
		{ :ok, [] }
	end

	def handle_call(:hello, _from, state) do
		{ :reply, :ok, state }
	end

end