defmodule Foo do
  defmodule State do
    defstruct x: 0, y: 0, dx: 0, dy: -1
  end

  def sum_after(target_sum, grid \\ %{}, state \\ %State{}) do
    {coords, new_state} = iterate state
    sum = case coords do
      {0, 0} -> 1
      _ -> grid |> Map.take(adjacent(coords)) |> Map.values |> Enum.sum
    end
    cond do
      sum > target_sum ->
        sum
      true ->
        sum_after(target_sum, Map.put(grid, coords, sum), new_state)
    end
  end

  defp iterate(%State{x: x, y: y} = state) do
    {dx, dy} = cond do
      abs(x) == abs(y) and {state.dx, state.dy} != {1,0} ->
        {-state.dy, state.dx}
      x > 0 and y == 1-x ->
        {-state.dy, state.dx}
      true ->
        {state.dx, state.dy}
    end
    {{x, y}, %State{x: x+dx, y: y+dy, dx: dx, dy: dy}}
  end

  defp adjacent({x, y}) do
    [
      {x, y+1},   # up one
      {x-1, y+1},  # up one, left one
      {x-1, y},   # left one
      {x-1, y-1}, # left one, down one
      {x, y-1},   # down one
      {x+1, y-1}, # right one, down one
      {x+1, y},   # right one
      {x+1, y+1}  # right one, up one
    ]
  end

  @doc """
  For testing purposes only
  """
  def build_grid(size, grid \\ [], state \\ %State{}) do
    cond do
      length(grid) == size * size ->
        grid
      true ->
        {coords, new_state} = iterate state
        build_grid(size, grid ++ [coords], new_state)
    end
  end
end

n = :stdio |> IO.read(:line) |> String.trim |> String.to_integer
IO.puts Foo.sum_after(n)
