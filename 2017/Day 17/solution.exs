defmodule CircleBuffer do
  @iterations 2017
  def run(jump, :a) do
    {buffer, pos} = [0] |> step(0, 0, jump, @iterations)
    buffer |> Enum.join("\n")
    next_pos = length(buffer) |> calc_jump(pos, 1)
    buffer |> Enum.at(next_pos)
  end

  @iterations 50_000_000
  def run(jump, :b) do
    1 |> track(0, 0, 0, jump, @iterations)
  end

  defp track(_tracked_idx, tracked_val, _pos, curr_val, _jump, iterations) when curr_val == iterations, do: tracked_val
  defp track(tracked_idx, tracked_val, pos, curr_val, jump, iterations) do
    next_val = curr_val + 1
    jump_to = next_val |> calc_jump(pos, jump)
    next_pos = jump_to + 1
    tracked_val = case next_pos do
      ^tracked_idx -> next_val
      _ -> tracked_val
    end
    tracked_idx |> track(tracked_val, next_pos, next_val, jump, iterations)
  end

  defp step(buffer, pos, _, _, 0), do: {buffer, pos}
  defp step(buffer, pos, curr_val, jump, iterations) do
    jump_to = length(buffer) |> calc_jump(pos, jump)
    next_val = curr_val + 1
    buffer
    |> List.insert_at(jump_to + 1, next_val)
    |> step(jump_to + 1, next_val, jump, iterations - 1)
  end

  defp calc_jump(buffer_size, pos, jump) do
    rem(rem(pos + jump, buffer_size) + buffer_size, buffer_size)
  end
end

part = System.argv |> Enum.at(0) |> String.to_atom
:stdio
|> IO.read(:line)
|> String.trim
|> String.to_integer
|> CircleBuffer.run(part)
|> IO.puts
