defmodule Reg do
  defmodule Instruction do
    defstruct reg: nil, op: nil, amount: nil, cond: nil
  end

  def run(instructions, :a) do
    instructions
    |> Enum.reduce(%{}, fn(ins, reg) ->
      reg |> execute(ins)
    end)
    |> Map.values
    |> Enum.sort
    |> Enum.at(-1)
  end

  def run(instructions, :b) do
    instructions
    |> Enum.reduce({%{}, 0}, fn(ins, {reg, highest}) ->
      new_reg = reg |> execute(ins)
      new_val = new_reg[ins.reg] || 0
      {new_reg, (if new_val > highest, do: new_val, else: highest)}
    end)
    |> elem(1)
  end

  defp execute(registers, %Reg.Instruction{} = i) do
    if registers |> eval_cond(i.cond) do
      new_val = registers |> eval_op(i)
      registers |> Map.put(i.reg, new_val)
    else
      registers
    end
  end

  defp eval_op(registers, %Reg.Instruction{reg: reg, op: op, amount: n}) do
    current_val = registers[reg] || 0
    case op do
      "inc" -> current_val + n
      "dec" -> current_val - n
    end
  end

  defp eval_cond(registers, {reg, comp, asserted_val}) do
    reg_val = registers[reg] || 0
    case comp do
      ">" -> reg_val > asserted_val
      "<" -> reg_val < asserted_val
      ">=" -> reg_val >= asserted_val
      "<=" -> reg_val <= asserted_val
      "==" -> reg_val == asserted_val
      "!=" -> reg_val != asserted_val
    end
  end

  def parse(lines) do
    pattern = ~r/^([a-z]+) ([a-z]+) (-?[0-9]+) if ([a-z]+) ([^0-9\s]+) (-?[0-9]+)$/
    Stream.map lines, fn(line) ->
      [_, reg, op, amount, cond_reg, cond_comp, cond_val] = pattern |> Regex.run(line)
      %Reg.Instruction{reg: reg, op: op, amount: amount |> String.to_integer, cond: {
        cond_reg,
        cond_comp,
        cond_val |> String.to_integer
      }}
    end
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio
|> IO.stream(:line)
|> Reg.parse
|> Reg.run(part)
|> IO.inspect
