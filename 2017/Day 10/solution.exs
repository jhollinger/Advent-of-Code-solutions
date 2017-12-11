defmodule Day10 do
  def run(input, :a) do
    raw =
      input
      |> String.split(~r/\s*,\s*/)
      |> Enum.map(&String.to_integer/1)
      |> KnotHash.single_round
    raw[0] * raw[1]
  end

  def run(input, :b) do
    input |> String.to_charlist |> KnotHash.hash
  end
end

defmodule KnotHash do
  use Bitwise, only_operators: true
  @size 256
  @slots 0..@size-1 |> Enum.reduce(%{}, fn(x, a) -> Map.put(a, x, x) end)
  @rounds 64
  @suffix [17, 31, 73, 47, 23]

  def hash(input) do
    raw = @slots |> rounds(input ++ @suffix, @rounds)
    sparse_hash = 0..@size-1 |> Enum.map(&(raw[&1]))
    dense_hash = sparse_hash |> Enum.chunk(16) |> Enum.reduce(<<>>, fn(chunk, a) ->
      bit = chunk |> Enum.reduce(fn(n, a) -> a ^^^ n end)
      a <> <<bit>>
    end)
    dense_hash |> Base.encode16(case: :lower)
  end

  def single_round(input), do: @slots |> hash_round(input, 0, 0) |> elem(0)

  defp rounds(slots, input, round, pos \\ 0, skip \\ 0)
  defp rounds(slots, _input, 0, _pos, _skip), do: slots
  defp rounds(slots, input, round, pos, skip) do
    {new_slots, new_pos, new_skip} = slots |> hash_round(input, pos, skip)
    new_slots |> rounds(input, round - 1, new_pos, new_skip)
  end

  defp hash_round(slots, [], pos, skip), do: {slots, pos, skip}
  defp hash_round(slots, input, pos, skip) when pos > @size - 1 do
    slots |> hash_round(input, pos - @size, skip)
  end
  defp hash_round(slots, [len | input], pos, skip) do
    segment_keys = calc_idxs(pos, len)
    reversed_vals = segment_keys |> Enum.reduce([], fn(key, a) -> [slots |> Map.fetch!(key) | a] end)
    reversed_segment = segment_keys |> Enum.zip(reversed_vals) |> Map.new
    slots
    |> Map.merge(reversed_segment)
    |> hash_round(input, pos + len + skip, skip + 1)
  end

  defp calc_idxs(pos, len) when pos + len <= @size - 1, do: pos..(pos + len-1)
  defp calc_idxs(pos, len) when pos + len > @size - 1 do
    part1 = pos..@size - 1
    part2 = 0..len - 1 - Enum.count(part1)
    # TODO fix this
    if part2 == 0..-1 do
      part1
    else
      Enum.concat(part1, part2)
    end
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
input = :stdio |> IO.read(:all) |> String.trim
input |> Day10.run(part) |> IO.puts
