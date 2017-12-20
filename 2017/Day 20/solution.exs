defmodule Particles do
  defmodule P do
    defstruct id: nil, p: nil, v: nil, a: nil
    def distance(%P{p: {x, y, z}}), do: abs(x) + abs(y) + abs(z)
    def increase(%P{} = p), do: p |> increase_v |> increase_p
    defp increase_v(%P{v: {x, y, z}, a: {ax, ay, az}} = p), do: %{p | v: {x + ax, y + ay, z + az}}
    defp increase_p(%P{p: {x, y, z}, v: {vx, vy, vz}} = p), do: %{p | p: {x + vx, y + vy, z + vz}}
  end

  def run(input, :a) do
    closest = input |> parse |> tick(10_000) |> Enum.sort_by(fn p -> p |> P.distance end) |> Enum.at(0)
    closest.id
  end

  def run(input, :b), do: input |> parse |> tick_and_collide(10_000) |> Enum.count

  defp tick(particles, 0), do: particles
  defp tick(particles, n) do
    particles
    |> Enum.map(fn p -> p |> P.increase end)
    |> tick(n - 1)
  end

  defp tick_and_collide(particles, 0), do: particles |> annihilate
  defp tick_and_collide(particles, n) do
    particles
    |> annihilate
    |> Enum.map(fn p -> p |> P.increase end)
    |> tick_and_collide(n - 1)
  end

  defp annihilate(particles) do
    by_coords = particles |> Enum.group_by(fn %P{p: xzy} -> xzy end)
    particles |> Enum.reject(fn %P{p: xzy} ->
      length(by_coords[xzy]) > 1
    end)
  end

  @pattern ~r/p=<([^>]+)>,\s*v=<([^>]+)>,\s*a=<([^>]+)>/
  defp parse(input) do
    split = fn x -> x |> String.trim |> String.split(~r/\s*,\s*/) |> Enum.map(&String.to_integer/1) end
    input |> String.trim |> String.split("\n") |> Enum.with_index |> Enum.map(fn {line, i} ->
      [_, p, v, a] = @pattern |> Regex.run(line)
      [px, py, pz] = split.(p)
      [vx, vy, vz] = split.(v)
      [ax, ay, az] = split.(a)
      %P{id: i, p: {px, py, pz}, v: {vx, vy, vz}, a: {ax, ay, az}}
    end)
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio |> IO.read(:all) |> Particles.run(part) |> IO.puts
