numbers =
  IO.read(:stdio, :all)
  |> String.trim
  |> String.graphemes
  |> Enum.map(&(String.to_integer &1))

last_idx = length(numbers) - 1

total =
  numbers
  |> Enum.with_index
  |> Enum.reduce(0, fn({n, idx}, a) ->
    check_idx = if idx == last_idx, do: 0, else: idx + 1
    next_n = Enum.at(numbers, check_idx)
    cond do
      n == next_n -> a + n
      true -> a
    end
  end)

IO.puts total
