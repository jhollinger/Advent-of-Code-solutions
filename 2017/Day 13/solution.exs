defmodule Foo do
  def run(layers, :a) do
    layers |> traverse |> elem(2)
  end

  def run(layers, :b) do
    layers |> calculate_delay
  end

  def traverse(layers) do
    num = layers |> Map.keys |> Enum.max
    0..num
    |> Enum.reduce({layers, :missed, 0}, fn(depth, {state, caught, score}) ->
      {new_caught, new_score} = case state |> calc_score(depth) do
        {:caught, n} -> {:caught, score + n}
        {:missed} -> {caught, score}
      end
      {state |> tick, new_caught, new_score}
    end)
  end

  def calculate_delay(state, delay \\ 0) do
    case state |> traverse do
      {_, :missed, 0 = _score} -> delay
      {_, :caught, _score} -> state |> tick |> calculate_delay(delay + 1)
    end
  end

  defp calc_score(state, depth) do
    case state[depth] do
      {range, _dir, 1 = _pos} -> {:caught, depth * range}
      {_range, _dir, _pos} -> {:missed}
      nil -> {:missed}
    end
  end

  defp tick(state) do
    state |> Enum.reduce(%{}, fn({depth, {range, dir, pos}}, a) ->
      {new_dir, new_pos} = pos |> next_pos(dir, range)
      a |> Map.put(depth, {range, new_dir, new_pos})
    end)
  end

  defp next_pos(1, _, _range), do: {:down, 2}
  defp next_pos(pos, _, range) when pos == range, do: {:up, pos - 1}
  defp next_pos(pos, :down, _), do: {:down, pos + 1}
  defp next_pos(pos, :up, _), do: {:up, pos - 1}

  def parse(input) do
    input |> String.trim |> String.split("\n") |> Enum.reduce(%{}, fn(line, a) ->
      [_, depth, range] = ~r/^([0-9]+):\s*([0-9]+)$/ |> Regex.run(line)
      a |> Map.put(depth |> String.to_integer, {range |> String.to_integer, :down, 1})
    end)
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio
|> IO.read(:all)
|> Foo.parse
|> Foo.run(part)
|> IO.puts
