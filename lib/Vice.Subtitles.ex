# File: Vice.Subtitles.ex
# This file was generated from vice_subtitles.beam
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Vice.Subtitles do
  def unquote(:parse)(arg1) do
    :erlang.apply(:vice_subtitles, :parse, [arg1])
  end
  def unquote(:parse_file)(arg1) do
    :erlang.apply(:vice_subtitles, :parse_file, [arg1])
  end
  def unquote(:to_string)(arg1, arg2) do
    :erlang.apply(:vice_subtitles, :to_string, [arg1, arg2])
  end
  def unquote(:to_string)(arg1, arg2, arg3) do
    :erlang.apply(:vice_subtitles, :to_string, [arg1, arg2, arg3])
  end
  def unquote(:to_file)(arg1, arg2) do
    :erlang.apply(:vice_subtitles, :to_file, [arg1, arg2])
  end
  def unquote(:to_file)(arg1, arg2, arg3) do
    :erlang.apply(:vice_subtitles, :to_file, [arg1, arg2, arg3])
  end
  def unquote(:to_file)(arg1, arg2, arg3, arg4) do
    :erlang.apply(:vice_subtitles, :to_file, [arg1, arg2, arg3, arg4])
  end
end
