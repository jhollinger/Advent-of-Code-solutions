defmodule Spreadsheet do
  def moddivs(lines) do
    Enum.reduce lines, 0, fn(line, sum) ->
      {a, b} = even_div line
      sum + div(a, b)
    end
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

  defp even_div(nums) do
    Enum.find_value nums, fn(a) ->
      case even_div(a, nums) do
        {:ok, b} -> {a, b}
        :eol -> false
      end
    end
  end

  defp even_div(_, []), do: :eol
  defp even_div(a, [n | tail]) do
    cond do
      a == n -> even_div(a, tail)
      rem(a, n) == 0 -> {:ok, n}
      true -> even_div(a, tail)
    end
  end
end

checksum =
  :stdio
  |> Spreadsheet.read_lines
  |> Spreadsheet.parse_lines
  |> Spreadsheet.moddivs

IO.inspect checksum
