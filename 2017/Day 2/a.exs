defmodule Spreadsheet do
  def checksum(lines) do
    lines
    |> Enum.map(&(minmax &1))
    |> Enum.reduce(0, fn({min, max}, a) -> a + (max - min) end)
  end

  def parse_lines(lines) do
    Enum.map lines, fn(line) ->
      line
      |> String.split(~r/\s+/)
      |> Enum.map(&(String.to_integer &1))
    end
  end

  def read_lines(io, lines \\ []) do
    case IO.read(:stdio, :line) do
      :eof -> lines
      {:error, reason} -> raise reason
      data ->
        line = String.trim data
        read_lines io, lines ++ [line]
    end
  end

  defp minmax(line) do
    {Enum.min(line), Enum.max(line)}
  end
end

checksum =
  :stdio
  |> Spreadsheet.read_lines
  |> Spreadsheet.parse_lines
  |> Spreadsheet.checksum

IO.inspect checksum
