defmodule Audio do
  def play(instructions) do
    instructions
    |> run(fn
      {:rcv, _}, _reg -> true
      _, _ -> false
    end)
    |> Map.get(:buf)
  end

  def run(instructions, _break \\ fn _ins, _reg -> false end, n \\ 0, registers \\ %{})
  def run(_instructions, _break, n, registers) when n < 0, do: registers
  def run(instructions, _break, n, registers) when n >= map_size(instructions), do: registers
  def run(instructions, break, n, registers) do
    ins = instructions[n]
    case ins |> exec(registers) do
      {:jump, m} ->
        instructions |> run(break, n + m, registers)
      %{} = registers2 ->
        if break.(ins, registers2) do
          registers2
        else
          instructions |> run(break, n + 1, registers2)
        end
    end
  end

  defp exec({:snd, reg}, registers) when is_atom(reg) do
    {:snd, registers[reg] || 0} |> exec(registers)
  end
  defp exec({:snd, freq}, registers) when is_integer(freq) do
    registers |> Map.put(:buf, freq)
  end

  defp exec({:set, reg, pointer}, registers) when is_atom(pointer) do
    {:set, reg, registers[pointer] || 0} |> exec(registers)
  end
  defp exec({:set, reg, n}, registers) when is_integer(n) do
    registers |> Map.put(reg, n)
  end

  defp exec({:add, reg, pointer}, registers) when is_atom(pointer) do
    {:add, reg, registers[pointer] || 0} |> exec(registers)
  end
  defp exec({:add, reg, n}, registers) when is_integer(n) do
    old_val = registers[reg] || 0
    registers |> Map.put(reg, old_val + n)
  end

  defp exec({:mul, reg_a, pointer}, registers) when is_atom(pointer) do
    {:mul, reg_a, registers[pointer] || 0} |> exec(registers)
  end
  defp exec({:mul, reg_a, n}, registers) when is_integer(n) do
    val_a = registers[reg_a] || 0
    registers |> Map.put(reg_a, val_a * n)
  end

  defp exec({:mod, reg, pointer}, registers) when is_atom(pointer) do
    {:mod, reg, registers[pointer] || 0} |> exec(registers)
  end
  defp exec({:mod, reg, n}, registers) when is_integer(n) do
    old_val = registers[reg] || 0
    registers |> Map.put(reg, rem(old_val, n))
  end

  defp exec({:rcv, reg}, registers) when is_atom(reg) do
    {:rcv, registers[reg] || 0} |> exec(registers)
  end
  defp exec({:rcv, n}, registers) when n > 0, do: registers
  defp exec({:rcv, n}, _) when n <= 0, do: {:jump, 1}

  defp exec({:jgz, reg, n}, registers) when is_atom(reg) do
    {:jgz, registers[reg] || 0, n} |> exec(registers)
  end
  defp exec({:jgz, val, n}, _registers) when val > 0, do: {:jump, n}
  defp exec({:jgz, val, _n}, registers) when val <= 0, do: registers

  @pattern ~r/^([a-z]+)\s+([^\s]+)\s*(.*)?$/
  def parse(io) do
    io |> IO.read(:all) |> String.trim |> String.split("\n") |> Enum.with_index |> Enum.reduce(%{}, fn {line, idx}, a ->
      [_ | matches] = @pattern |> Regex.run(line)
      cmd = matches |> Enum.at(0) |> String.to_atom
      arg1_str = matches |> Enum.at(1)
      arg1 = if ~r/[0-9]/ |> Regex.match?(arg1_str), do: String.to_integer(arg1_str), else: String.to_atom(arg1_str)
      ins = case matches |> Enum.at(2) do
        "" -> {cmd, arg1}
        arg2_str ->
          arg2 = if ~r/[0-9]/ |> Regex.match?(arg2_str), do: String.to_integer(arg2_str), else: String.to_atom(arg2_str)
          {cmd, arg1, arg2}
      end
      a |> Map.put(idx, ins)
    end)
  end
end

:stdio
|> Audio.parse
|> Audio.play
|> IO.inspect
