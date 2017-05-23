# File: Vice.Srt.ex
# This file was generated from vice_srt.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Vice.Srt do
  def unquote(:"parse_file")(arg1) do
    :erlang.apply(:"vice_srt", :"parse_file", [arg1])
  end
  def unquote(:"parse")(arg1) do
    :erlang.apply(:"vice_srt", :"parse", [arg1])
  end
  def unquote(:"write")(arg1, arg2) do
    :erlang.apply(:"vice_srt", :"write", [arg1, arg2])
  end
  def unquote(:"to_string")(arg1) do
    :erlang.apply(:"vice_srt", :"to_string", [arg1])
  end
end
