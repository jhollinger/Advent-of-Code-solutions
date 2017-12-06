defmodule MemoryBanks do
  def run(banks, :a), do: redistribute_until_repeat banks
  def run(banks, :b), do: redistribution_repeat_loop_size banks

  def redistribute_until_repeat(banks, log \\ MapSet.new) do
    updated_banks = redistribute banks
    cond do
      MapSet.member?(log, updated_banks) ->
        MapSet.size(log) + 1
      true ->
        updated_banks
        |> redistribute_until_repeat(MapSet.put(log, updated_banks))
    end
  end

  def redistribution_repeat_loop_size(banks, log \\ %{}) do
    updated_banks = redistribute banks
    cond do
      iteration = log[updated_banks] ->
        map_size(log) - iteration + 1
      true ->
        iteration = map_size(log) + 1
        updated_banks
        |> redistribution_repeat_loop_size(Map.put(log, updated_banks, iteration))
    end
  end

  defp redistribute(banks) do
    {idx, blocks} = banks |> largest
    banks
    |> Map.put(idx, 0)
    |> distribute(blocks, idx+1)
  end

  defp distribute(banks, 0, _idx), do: banks
  defp distribute(banks, blocks, idx) when idx >= map_size(banks), do: distribute banks, blocks, 0
  defp distribute(banks, blocks, idx) do
    banks
    |> Map.put(idx, banks[idx] + 1)
    |> distribute(blocks - 1, idx + 1)
  end

  defp largest(banks) do
    banks
    |> Enum.sort(fn({idx_a, blocks_a}, {idx_b, blocks_b}) ->
      blocks_a > blocks_b or (blocks_a == blocks_b and idx_a < idx_b)
    end)
    |> Enum.at(0)
  end

  def read_input(io) do
    io
    |> IO.read(:all)
    |> String.trim
    |> String.split(~r/\s+/)
    |> Enum.with_index
    |> Enum.reduce(%{}, fn({n, idx}, a) ->
      Map.put(a, idx, String.to_integer(n))
    end)
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio
|> MemoryBanks.read_input
|> MemoryBanks.run(part)
|> IO.inspect
