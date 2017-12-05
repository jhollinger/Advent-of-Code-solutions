numbers =
  IO.read(:stdio, :all)
  |> String.trim
  |> String.graphemes
  |> Enum.map(&(String.to_integer &1))

num_nums = length(numbers)
last_idx = num_nums - 1
idx_incr = div(length(numbers), 2)

total =
  numbers
  |> Enum.with_index
  |> Enum.reduce(0, fn({n, idx}, a) ->
    check_idx = if idx + idx_incr > last_idx do
      -1 + (idx + idx_incr - last_idx)
    else
      idx + idx_incr
    end

    next_n = Enum.at(numbers, check_idx)
    cond do
      n == next_n -> a + n
      true -> a
    end
  end)

IO.puts total
