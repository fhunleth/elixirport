defmodule Mix.Tasks.Compile.Testport do
  @shortdoc "Compiles the test_port"
  def run(_) do
    0 = Mix.Shell.IO.cmd("make priv/test_port")
  end
end

defmodule Elixirport.Mixfile do
  use Mix.Project

  def project do
    [ app: :elixirport,
      version: "0.0.1",
      elixir: "~> 0.14.0",
      compilers: [:testport, :elixir, :app],
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [mod: { Elixirport, [] }]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # To specify particular versions, regardless of the tag, do:
  # { :barbat, "~> 0.1", github: "elixir-lang/barbat" }
  defp deps do
    []
  end
end
