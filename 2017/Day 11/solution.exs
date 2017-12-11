defmodule HexGrid do
  def run(steps, :a) do
    origin = {0,0,0}
    dest = steps |> move(origin)
    hex_distance origin, dest
  end

  def run(steps, :b) do
    steps |> max_dist({0, 0, 0})
  end

  defp move([], {x, y, z}), do: {x, y, z}
  defp move([step | steps], coords) do
    next_coords = step |> calc(coords)
    steps |> move(next_coords)
  end

  defp max_dist(steps, origin, coords \\ nil, d1 \\ 0)
  defp max_dist([], _origin, _coords, d1), do: d1
  defp max_dist([step | steps], origin, coords, d1) do
    next_coords = step |> calc(coords || origin)
    d2 = hex_distance origin, next_coords
    steps |> max_dist(origin, next_coords, max(d1, d2))
  end

  # http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/
  defp calc("n",  {x, y, z}), do: {x, y+1, z-1}
  defp calc("ne", {x, y, z}), do: {x-1, y+1, z}
  defp calc("se", {x, y, z}), do: {x-1, y, z+1}
  defp calc("s",  {x, y, z}), do: {x, y-1, z+1}
  defp calc("sw", {x, y, z}), do: {x+1, y-1, z}
  defp calc("nw", {x, y, z}), do: {x+1, y, z-1}

  defp hex_distance({x1, y1, z1}, {x2, y2, z2}) do
    [abs(x2 - x1), abs(y2 - y1), abs(z2 - z1)] |> Enum.max
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio |> IO.read(:all) |> String.trim |> String.split(",") |> HexGrid.run(part) |> IO.puts
