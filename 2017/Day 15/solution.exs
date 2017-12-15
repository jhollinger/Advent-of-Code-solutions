defmodule Generators do
  use Bitwise, only: :operators
  @factor_a 16807
  @factor_b 48271
  @div 2147483647

  def run(input, :a) do
    input
    |> parse
    |> scanner(40_000_000, fn x -> gen(x, @factor_a) end, fn x -> gen(x, @factor_b) end)
    |> Enum.reduce(0, fn {a, b}, acc ->
      if (a &&& 0xffff) == (b &&& 0xffff), do: acc + 1, else: acc
    end)
  end

  def run(input, :b) do
    input
    |> parse
    |> scanner(5_000_000, fn x -> multiples_gen(x, @factor_a, 4) end, fn x -> multiples_gen(x, @factor_b, 8) end)
    |> Enum.reduce(0, fn {a, b}, acc ->
      if (a &&& 0xffff) == (b &&& 0xffff), do: acc + 1, else: acc
    end)
  end

  def scanner({_, _} = seed, rounds, gen_a, gen_b) do
    seed
    |> Stream.iterate(fn {a, b} -> {gen_a.(a), gen_b.(b)} end)
    |> Stream.drop(1) # drop the seed from the results
    |> Stream.take(rounds)
  end

  def gen(x, factor), do: rem(x * factor, @div)

  def multiples_gen(x, factor, m) do
    next = gen x, factor
    case rem(next, m) do
      0 -> next
      _ -> multiples_gen(next, factor, m)
    end
  end

  @regex ~r/([0-9]+)$/
  def parse(input) do
    [line1, line2] = input |> String.trim |> String.split("\n")
    a = @regex |> Regex.run(line1) |> Enum.at(1) |> String.to_integer
    b = @regex |> Regex.run(line2) |> Enum.at(1) |> String.to_integer
    {a, b}
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio |> IO.read(:all) |> Generators.run(part) |> IO.puts
