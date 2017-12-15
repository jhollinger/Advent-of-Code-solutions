defmodule Foo do
  @grid_size 128

  def run(input, :a) do
    input |> parse |> Enum.reduce(0, fn(row, a) ->
      a + (row |> String.replace("0", "") |> String.length)
    end)
  end

  def run(input, :b) do
    input |> parse |> Enum.to_list |> regions |> Enum.count
  end

  defp regions(rows) do
    grid = rows |> build_grid
    regions(%{}, grid, rows |> Enum.with_index)
  end
  defp regions(map, _grid, []), do: map |> Map.values |> Enum.uniq
  defp regions(map, grid, [{row, y} | tail]) do
    row
    |> String.codepoints
    |> Enum.with_index
    |> Enum.reduce(map, fn
      {"0", _x}, a -> a
      {"1", x}, a ->
        region = a |> find_region(grid, x, y)
        a |> append_region([{x, y} | region])
    end)
    |> regions(grid, tail)
  end

  defp find_region(%{} = map, %{} = grid, x, y) do
    adjacent = grid |> find_adjacent(x, y)
    case adjacent |> Enum.any? do
      false -> []
      true ->
        {region, _} = adjacent |> nearest_mapped_region(map, grid, [{x, y}])
        region || []
    end
  end

  def nearest_mapped_region([], _map, _grid, breadcrumbs), do: {nil, breadcrumbs}
  def nearest_mapped_region([{x, y} = coords | tail], map, grid, breadcrumbs) do
    cond do
      map |> Map.has_key?(coords) -> {map[coords], breadcrumbs}
      true ->
        adjacent = find_adjacent(grid, x, y) -- breadcrumbs
        case adjacent |> nearest_mapped_region(map, grid, [coords | breadcrumbs]) do
          {nil, breadcrumbs} -> tail |> nearest_mapped_region(map, grid, breadcrumbs)
          {region, breadcrumbs} -> {region, breadcrumbs}
        end
    end
  end

  defp find_adjacent(grid, x, y) do
    [{x-1, y}, {x, y-1}, {x+1, y}, {x, y+1}]
    |> Enum.filter(fn {adj_x, adj_y} ->
      adj_x >= 0 && adj_y >= 0 && adj_x < @grid_size && adj_y < @grid_size && grid[adj_y][adj_x] == "1"
    end)
  end

  defp append_region(%{} = map, [new_coords | old_region] = updated_region) do
    map
    # replace all instance of `old_region` with `updated_region` so we can uniq them later
    |> Enum.reduce(%{}, fn
      {key, ^old_region}, a -> a |> Map.put(key, updated_region)
      {key, region}, a -> a |> Map.put(key, region)
    end)
    |> Map.put(new_coords, updated_region)
  end

  defp build_grid(rows) do
    rows
    |> Enum.with_index
    |> Map.new(fn({row_str, idx}) ->
      row = row_str |> String.codepoints |> Enum.with_index |> Map.new(fn({v,i}) -> {i,v} end)
      {idx, row}
    end)
  end

  #defp append_to_region(map, x, y) do
  #  old_region = map[{x-1, y}] || map[{x, y-1}] || []
  #  updated_region = [{x, y} | old_region]
  #  map
  #  # replace all instance of `old_region` with `updated_region` so we can uniq them later
  #  |> Enum.reduce(%{}, fn
  #    {key, ^old_region}, a -> a |> Map.put(key, updated_region)
  #    {key, region}, a -> a |> Map.put(key, region)
  #  end)
  #  |> Map.put({x, y}, updated_region)
  #end

  defp parse(keystring) do
    (0..@grid_size - 1)
    |> Stream.map(fn i -> "#{keystring |> String.trim}-#{i}" end)
    |> Stream.map(fn row -> row |> KnotHash.hash end)
    |> Stream.map(fn hash ->
      hash
      |> String.codepoints
      |> Enum.map(fn n -> n |> Integer.parse(16) |> elem(0) |> Integer.to_string(2) |> String.pad_leading(4, "0") end)
      |> Enum.join("")
    end)
  end
end

defmodule KnotHash do
  use Bitwise, only_operators: true
  @size 256
  @slots 0..@size-1 |> Enum.reduce(%{}, fn(x, a) -> Map.put(a, x, x) end)
  @rounds 64
  @suffix [17, 31, 73, 47, 23]

  def hash(input) do
    chars = input |> String.to_charlist
    raw = @slots |> rounds(chars ++ @suffix, @rounds)
    sparse_hash = 0..@size-1 |> Enum.map(&(raw[&1]))
    dense_hash = sparse_hash |> Enum.chunk(16) |> Enum.reduce(<<>>, fn(chunk, a) ->
      bit = chunk |> Enum.reduce(fn(n, a) -> a ^^^ n end)
      a <> <<bit>>
    end)
    dense_hash |> Base.encode16(case: :lower)
  end

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
:stdio
|> IO.read(:all)
|> String.trim
|> Foo.run(part)
|> IO.inspect
