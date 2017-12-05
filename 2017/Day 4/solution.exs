defmodule Passphrases do
  def valid?(:a, text) do
    words = String.split(text, ~r/\s+/)
    length(words) == length(Enum.uniq words)
  end

  def valid?(:b, text) do
    words_with_sorted_letters =
      text
      |> String.split(~r/\s+/)
      |> Enum.map(&(&1 |> String.graphemes |> Enum.sort))
    length(words_with_sorted_letters) == length(Enum.uniq words_with_sorted_letters)
  end

  def read_lines(io, lines \\ []) do
    case IO.read(io, :line) do
      :eof -> lines
      {:error, reason} -> raise reason
      data ->
        line = String.trim data
        read_lines io, [line | lines]
    end
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
{valid, _invalid} =
  :stdio
  |> Passphrases.read_lines
  |> Enum.split_with(&(Passphrases.valid?(part, &1)))

IO.puts length(valid)
