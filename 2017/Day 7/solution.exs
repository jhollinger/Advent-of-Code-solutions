defmodule Tree do
  defmodule Node do
    defstruct name: nil, weight: nil, children: []
  end

  def run(nodes, :a) do
    (nodes |> find_root).name
  end

  def run(nodes, :b) do
    root = nodes |> find_root
    map = Enum.reduce nodes, %{}, fn(n, a) ->
      a |> Map.put(n.name, n)
    end
    {node, weight_diff} = unbalanced(map, root)
    node.weight + weight_diff
  end

  def unbalanced(%{} = map, %Tree.Node{} = n, weight_diff \\ 0) do
    children_by_weight =
      map
      |> Map.take(n.children)
      |> Map.values
      |> Enum.reduce([], fn(c, a) ->
        [{c, branch_weight(c, map)} | a]
      end)
      |> Enum.group_by(fn({_, w}) -> w end)

    case children_by_weight |> map_size do
      # no children - "n" is the unbalanced one
      0 -> {n, weight_diff}
      # children are balanced - "n" is the unbalanced one
      1 -> {n, weight_diff}
      # children are unbalanced.
      _ ->
        # NOTE it's a given that only 1 child is unbalanced and that there at least 3 of them
        {bad_weight, bad_group} =
          children_by_weight
          |> Enum.find(fn({_, group}) -> length(group) == 1 end)

        {good_weight, _} =
          children_by_weight
          |> Enum.find(fn({_, group}) -> length(group) != 1 end)

        {child, _} = bad_group |> Enum.at(0)
        map |> unbalanced(child, good_weight - bad_weight)
    end
  end

  def branch_weight(%Tree.Node{} = n, %{} = map) do
    map
    |> Map.take(n.children)
    |> Map.values
    |> Enum.reduce(n.weight, fn(c, a) ->
      a + branch_weight(c, map)
    end)
  end

  defp find_root(nodes) do
    Enum.find nodes, fn(%{name: name}) ->
      Enum.all? nodes, fn(%{children: children}) ->
        !Enum.member?(children, name)
      end
    end
  end

  def parse(lines) do
    Enum.map lines, fn(l) ->
      captures = ~r/^([a-z]+) \(([0-9]+)\)( -> (.+))?$/ |> Regex.run(l)
      children = case captures |> Enum.at(4) do
        nil -> []
        str -> str |> String.split(~r/\s*,\s*/)
      end
      %Tree.Node{
        name: captures |> Enum.at(1),
        weight: captures |> Enum.at(2) |> String.to_integer,
        children: children
      }
    end
  end

  def read_lines(io, lines \\ []) do
    case IO.read(io, :line) do
      :eof -> lines
      {:error, reason} -> raise reason
      data ->
        line = String.trim data
        read_lines io, [line | lines]
    end
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio
|> Tree.read_lines
|> Tree.parse
|> Tree.run(part)
|> IO.inspect
