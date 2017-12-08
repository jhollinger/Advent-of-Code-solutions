defmodule Reg do
  defmodule Instruction, do: defstruct reg: nil, op: nil, amount: nil, cond: nil
  @pattern ~r/^([a-z]+) ([a-z]+) (-?[0-9]+) if ([a-z]+) ([^0-9\s]+) (-?[0-9]+)$/

  def run(instructions, :a) do
    instructions
    |> Enum.reduce(%{}, fn(ins, reg) ->
      reg |> execute(ins) |> elem(0)
    end)
    |> Map.values
    |> Enum.sort
    |> Enum.at(-1)
  end

  def run(instructions, :b) do
    instructions
    |> Enum.reduce({%{}, 0}, fn(ins, {reg, highest}) ->
      {new_reg, val} = reg |> execute(ins)
      {new_reg, (if val > highest, do: val, else: highest)}
    end)
    |> elem(1)
  end

  defp execute(registers, %Reg.Instruction{cond: {cond_reg, comp, cond_val}} = i) do
    if comp |> compare(registers[cond_reg] || 0, cond_val) do
      new_val = i.op |> op(registers[i.reg] || 0, i.amount)
      {registers |> Map.put(i.reg, new_val), new_val}
    else
      {registers, registers[i.reg] || 0}
    end
  end

  defp op(:inc, val, n), do: val + n
  defp op(:dec, val, n), do: val - n

  defp compare(:>, a, b), do: a > b
  defp compare(:<, a, b), do: a < b
  defp compare(:>=, a, b), do: a >= b
  defp compare(:<=, a, b), do: a <= b
  defp compare(:==, a, b), do: a == b
  defp compare(:"!=", a, b), do: a != b

  def parse(lines) do
    Stream.map lines, fn(line) ->
      [_, reg, op, amount, cond_reg, cond_comp, cond_val] = @pattern |> Regex.run(line)
      %Reg.Instruction{reg: reg, op: op |> String.to_atom, amount: amount |> String.to_integer, cond: {
        cond_reg,
        cond_comp |> String.to_atom,
        cond_val |> String.to_integer
      }}
    end
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio |> IO.stream(:line) |> Reg.parse |> Reg.run(part) |> IO.inspect
