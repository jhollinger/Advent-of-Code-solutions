defmodule Art do
  def run(%{} = rulebook, :a) do
    rulebook |> Map.keys
    [".#.", "..#", "###"]
    |> enhance(rulebook, 4)
    |> Enum.join("")
    |> String.codepoints
    |> Enum.filter(fn x -> x == "#" end)
    |> Enum.count
  end

  def enhance(input, _rulebook, 0), do: input
  def enhance(input, %{} = rulebook, iter) when rem(length(input), 3) == 0 do
    #IO.puts "!!!!!"
    #IO.inspect input
    input
    |> slices(3)
    |> Enum.map(fn pattern -> rulebook |> Map.fetch!(pattern) end)
    |> join_grid(3)
    |> enhance(rulebook, iter - 1)
  end
  def enhance(input, %{} = rulebook, iter) when rem(length(input), 2) == 0 do
    IO.puts "!!!!!"
    IO.inspect input
    input
    |> slices(2)
    |> Enum.map(fn pattern ->
      IO.inspect pattern
      rulebook |> Map.fetch!(pattern)
    end)
    |> join_grid(2)
    |> enhance(rulebook, iter - 1)
  end

  defp join_grid(groups, asdf, debug \\ false) do
    IO.puts "??????????????????????????????"
    IO.puts asdf
    IO.puts length(groups)
    IO.puts length(groups |> Enum.at(0))
    if debug, do: IO.puts "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    if debug, do: IO.inspect groups
    #IO.puts "!!!!!!!!!!!!!!!!!!!!!!!!!"
    #groups |> IO.inspect
    chunk_size = groups |> Enum.at(0) |> length
    if debug, do: IO.puts chunk_size
    #IO.inspect groups |> Enum.chunk(chunk_size, chunk_size, [])
    groups |> Enum.chunk(asdf, asdf, []) |> Enum.reduce([], fn chunk, acc ->
      rows = 0..chunk_size-1 |> Enum.map(fn idx ->
        chunk |> Enum.reduce("", fn row, acc2 ->
          #IO.puts "#{inspect acc2} <> #{inspect Enum.at(row, idx)}"
          #IO.inspect row
          acc2 <> (row |> Enum.at(idx))
        end)
      end)
      acc ++ rows
    end)
  end

  defp slices(input, size) do
    input_length = length(input)
    input |> Enum.chunk(size, size, []) |> Enum.reduce([], fn y_group, acc ->
      group = 0..input_length-1 |> Enum.chunk(size, size, []) |> Enum.map(fn slice ->
        a = List.first(slice)
        b = List.last(slice)
        y_group |> Enum.map(fn row -> row |> String.slice(a..b) end)
      end)
      acc ++ group
    end)
  end

  @rules ~r'^([./#]+)\s*=>\s*([./#]+)$'
  def rulebook(io, book \\ %{}) do
    case io |> IO.read(:line) do
      :eof -> book
      line ->
        [_, pattern, output] = @rules |> Regex.run(line)
        rows = pattern |> String.split("/")
        output = output |> String.split("/")
        book2 =
          ([rows, flip_v(rows), flip_h(rows)] ++ rotate([rows]) ++ rotate([flip_v(rows)]) ++ rotate([flip_h(rows)]))
          |> Enum.uniq
          #|> Enum.map(fn rows -> rows |> Enum.join("/") end)
          |> Enum.reduce(book, fn p, a -> a |> Map.put(p, output) end)
        io |> rulebook(book2)
    end
  end

  defp flip_v(rows), do: rows |> Enum.reverse
  defp flip_h(rows), do: rows |> Enum.map(&String.reverse/1)
  defp rotate([prev | _] = rotations) when length(rotations) == length(prev) * length(prev), do: rotations
  defp rotate([prev | _] = rotations) do
    new_rows = 1..length(prev) |> Enum.map(fn _ -> "" end)
    rotation = prev |> Enum.with_index |> Enum.reduce(new_rows, fn {old_row, _col_idx}, a ->
      old_row |> String.codepoints |> Enum.reverse |> Enum.with_index |> Enum.reduce(a, fn {val, row_idx}, aa ->
        new_row = aa |> Enum.at(row_idx)
        aa |> List.replace_at(row_idx, new_row <> val)
      end)
    end)
    [rotation | rotations] |> rotate
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio |> Art.rulebook |> Art.run(part) |> IO.inspect
