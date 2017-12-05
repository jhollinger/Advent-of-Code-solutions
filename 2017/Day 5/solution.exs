defmodule Instructions do
  @part System.argv |> Enum.at(0) |> String.to_atom

  def step(instructions, jump_to \\ 0, total_steps \\ 0)
  def step(instructions, jump_to, total_steps) when jump_to >= length(instructions), do: total_steps
  def step(instructions, jump_to, total_steps) do
    instruction = Enum.at(instructions, jump_to)
    next_jump = jump_to + instruction
    instructions
    |> List.replace_at(jump_to, instruction + incr(@part, instruction))
    |> step(next_jump, total_steps + 1)
  end

  defp incr(:a, _), do: 1
  defp incr(:b, instruction) when instruction >= 3, do: -1
  defp incr(:b, _), do: 1

  def read_input(io) do
    io
    |> IO.read(:all)
    |> String.trim
    |> String.split(~r/\s+/)
    |> Enum.map(&(String.to_integer &1))
  end
end

:stdio
|> Instructions.read_input
|> Instructions.step
|> IO.puts
