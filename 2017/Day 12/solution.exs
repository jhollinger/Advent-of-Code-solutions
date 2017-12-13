defmodule Pipes do
  @pipe ~r/^([0-9]+)\s+<->\s+([0-9,\s]+)$/

  def run(map, :a) do
    {_, links} = map |> connected(0)
    links |> Enum.count
  end

  def run(map, :b) do
    map
    |> Map.keys
    |> Enum.map(fn key ->
      map |> connected(key) |> elem(1)
    end)
    |> Enum.uniq
    |> Enum.count
  end

  def connected(map, nodes, collector \\ MapSet.new)
  def connected(map, [], collector), do: {map, collector}
  def connected(map, [n | nodes], collector) do
{trimmed_map, links} = map |> connected(n, collector)
    trimmed_map
    |> Map.delete(n)
    |> connected(nodes, links |> Enum.into(collector))
  end
  def connected(map, root_node, collector) do
    map
    |> Map.delete(root_node)
    |> connected(map[root_node] || [], collector |> MapSet.put(root_node))
  end

  def direct_links(pipes, map \\ %{})
  def direct_links([], map), do: map
  def direct_links([{_left, []} | pipes], map), do: pipes |> direct_links(map)
  def direct_links([{left, [right | rights]} | pipes], map) do
    updated_map =
      map
      |> Map.put(left, [right | (map[left] || [])] |> Enum.uniq)
      |> Map.put(right, [left | (map[right] || [])] |> Enum.uniq)
    direct_links([{left, rights} | pipes], updated_map)
  end

  def parse(io, pipes \\ []) do
    case IO.read(io, :line) do
      :eof -> pipes
      line ->
        [_, left_str, rights_str] = @pipe |> Regex.run(line |> String.trim)
        left = left_str |> String.to_integer
        rights = rights_str |> String.split(~r/\s*,\s*/) |> Enum.map(&String.to_integer/1)
        io |> parse([{left, rights} | pipes])
    end
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio
|> Pipes.parse
|> Pipes.direct_links
|> Pipes.run(part)
|> IO.inspect
