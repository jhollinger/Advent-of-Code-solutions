defmodule Dance do
  @dancers ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"]
  @num_dancers Enum.count @dancers

  def run(input, :a) do
    moves = input |> parse
    @dancers |> step(moves) |> Enum.join("")
  end

  def run(input, :b) do
    moves = input |> parse
    loop_size = @dancers |> detect_loop(moves)
    n = rem(1_000_000_000, loop_size)
    @dancers |> rounds(moves, n) |> Enum.join("")
  end

  defp detect_loop(dancers, moves, i \\ 0)
  defp detect_loop(dancers, _moves, i) when dancers == @dancers and i > 0, do: i
  defp detect_loop(dancers, moves, i), do: dancers |> step(moves) |> detect_loop(moves, i+1)

  defp rounds(dancers, _moves, 0), do: dancers
  defp rounds(dancers, moves, n), do: dancers |> step(moves) |> rounds(moves, n-1)

  defp step(dancers, []), do: dancers

  defp step(dancers, [{:spin, n} | moves]) do
    {head, tail} = dancers |> Enum.split(@num_dancers - n)
    (tail ++ head) |> step(moves)
  end

  defp step(dancers, [{:exchange, a, b} | moves]) do
    da = dancers |> Enum.at(a)
    db = dancers |> Enum.at(b)
    dancers
    |> List.replace_at(a, db)
    |> List.replace_at(b, da)
    |> step(moves)
  end

  defp step(dancers, [{:partner, a, b} | moves]) do
    a_pos = dancers |> Enum.find_index(fn d -> d == a end)
    b_pos = dancers |> Enum.find_index(fn d -> d == b end)
    dancers
    |> List.replace_at(a_pos, b)
    |> List.replace_at(b_pos, a)
    |> step(moves)
  end

  def parse(input) do
    input |> String.trim |> String.split(",") |> Enum.map(fn move ->
      case {move |> String.at(0), move |> String.slice(1..-1)} do
        {"s", n} ->
          {:spin, n |> String.to_integer}
        {"x", args} ->
          [a, b] = args |> String.split("/")
          {:exchange, String.to_integer(a), String.to_integer(b)}
        {"p", args} ->
          [a, b] = args |> String.split("/")
          {:partner, a, b}
      end
    end)
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio |> IO.read(:all) |> Dance.run(part) |> IO.puts
