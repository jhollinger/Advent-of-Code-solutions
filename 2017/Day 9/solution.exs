defmodule Tokenizer do
  def run(input, :a) do
    input |> String.codepoints |> scan |> Enum.reduce(0, fn
      {:group, n}, a -> a + n
      {_, _}, a -> a
    end)
  end

  def run(input, :b) do
    input |> String.codepoints |> scan |> Enum.reduce(0, fn
      {:garbage, n}, a -> a + n
      {_, _}, a -> a
    end)
  end

  defp scan(input, level \\ 0, res \\ [])
  defp scan([], _level, res), do: res
  defp scan(["{" | tail], level, res), do: tail |> scan(level + 1, res)
  defp scan(["}" | tail], level, res), do: tail |> scan(level - 1, [{:group, level} | res])
  defp scan(["," | tail], level, res), do: tail |> scan(level, res)
  defp scan(["<" | garbage], level, res) do
    {tail, num_chars} = garbage |> scan_garbage
    tail |> scan(level, [{:garbage, num_chars} | res])
  end
  defp scan([x | _], _level, _res), do: raise "Unexpected token: #{x}"

  defp scan_garbage(input, num_chars \\ 0)
  defp scan_garbage([], _), do: raise "Unexpected end of garbage!"
  defp scan_garbage([">" | tail], num_chars), do: {tail, num_chars}
  defp scan_garbage(["!" | [_ | garbage]], num_chars), do: garbage |> scan_garbage(num_chars)
  defp scan_garbage([_ | garbage], num_chars), do: garbage |> scan_garbage(num_chars + 1)
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio |> IO.read(:all) |> String.trim |> Tokenizer.run(part) |> IO.inspect
