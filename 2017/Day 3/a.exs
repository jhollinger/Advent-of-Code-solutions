defmodule Grid do
  def steps_to_center(n) do
    [outer_box | _] = boxes n
    outer_box.size - 1 - Grid.Box.distance_to_nearest_corner(outer_box, n)
  end

  def boxes(n) do
    get_levels_to(n, [Grid.Box.new(1)])
  end

  defp get_levels_to(n, [box | _] = boxes) do
    cond do
      box.max < n ->
        get_levels_to(n, [Grid.Box.new(box.num + 1) | boxes])
      true ->
        boxes
    end
  end
end

defmodule Grid.Box do
  @enforce_keys [:num, :size, :min, :max]
  defstruct [:num, :size, :min, :max]

  def new(1), do: %Grid.Box{num: 1, size: 1, min: 1, max: 1}
  def new(num) do
    size = num + (num - 1)
    max = size * size
    min = max - (size + size + size - 2 + size - 2) + 1
    %Grid.Box{num: num, size: size, min: min, max: max}
  end

  def distance_to_nearest_corner(%Grid.Box{} = box, n) do
    wrap_around_threshold = box.min + div(box.size, 2) - 1
    cond do
      n <= wrap_around_threshold ->
        n - box.min + 1
      true ->
        box
        |> corners
        |> Enum.map(fn(c) -> abs c - n end)
        |> Enum.sort
        |> Enum.at(0)
    end
  end

  defp corners(%Grid.Box{size: size, min: min, max: max} = box) do
    [
      max,
      max - (size - 1),
      max - (size - 1) - (size - 1),
      min + (size - 2)
    ]
  end
end

n = :stdio |> IO.read(:line) |> String.trim |> String.to_integer
IO.puts Grid.steps_to_center n
