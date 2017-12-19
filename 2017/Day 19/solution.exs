defmodule Diagram do
  defmodule M, do: defstruct rows: nil, height: nil, width: nil

  def run(%M{} = map, :a) do
    start_idx = find_start(map.rows[0])
    {crumbs, _steps} = map |> follow({start_idx, 0}, :down)
    crumbs |> Enum.join("")
  end

  def run(%M{} = map, :b) do
    start_idx = find_start(map.rows[0])
    {_crumbs, steps} = map |> follow({start_idx, 0}, :down)
    steps
  end

  defp follow(%M{height: h, width: w} = map, {x, y} = coords, dir, breadcrumbs \\ [], steps \\ 0) when x >= 0 and y >= 0 and x < w and y < h do
    case map.rows[y] |> String.at(x) do
      "|" ->
        new_coords = coords |> next_step(dir)
        map |> follow(new_coords, dir, breadcrumbs, steps + 1)
      "-" ->
        new_coords = coords |> next_step(dir)
        map |> follow(new_coords, dir, breadcrumbs, steps + 1)
      "+" ->
        {new_coords, new_dir} = map |> next_leg(coords, dir)
        map |> follow(new_coords, new_dir, breadcrumbs, steps + 1)
      " " ->
        {breadcrumbs, steps}
      cap ->
        new_coords = coords |> next_step(dir)
        map |> follow(new_coords, dir, breadcrumbs ++ [cap], steps + 1)
    end
  end

  defp next_step({x, y}, :down), do: {x, y+1}
  defp next_step({x, y}, :up), do: {x, y-1}
  defp next_step({x, y}, :right), do: {x+1, y}
  defp next_step({x, y}, :left), do: {x-1, y}

  defp next_leg(%M{} = map, {x, y}, dir) when dir == :down or dir == :up do
    map |> find_leg([{{x+1, y}, :right}, {{x-1, y}, :left}])
  end
  defp next_leg(%M{} = map, {x, y}, dir) when dir == :left or dir == :right do
    map |> find_leg([{{x, y-1}, :up}, {{x, y+1}, :down}])
  end

  defp find_leg(map, options) do
    options |> Enum.find(fn {{x, y}, _dir} ->
      y < map.height && x < map.width && map.rows[y] |> String.at(x) != " "
    end)
  end

  defp find_start(row), do: row |> String.codepoints |> Enum.find_index(fn x -> x == "|" end)

  def parse(input) do
    rows = input |> String.split("\n") |> Enum.with_index |> Enum.reduce(%{}, fn {row, idx}, a ->
      a |> Map.put(idx, row)
    end)
    %M{rows: rows, height: map_size(rows), width: String.length(rows[0])}
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio |> IO.read(:all) |> Diagram.parse |> Diagram.run(part) |> IO.puts
