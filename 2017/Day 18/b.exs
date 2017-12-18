defmodule Threads do
  defmodule T do
    defstruct id: nil, pos: 0, registers: nil, out_buf: [], in_buf: [], write_cnt: 0, state: :running

    def new(id), do: %T{id: id, registers: %{p: id}}
  end

  def run(instructions, concurrency) do
    threads = 1..concurrency |> Enum.map(fn id -> T.new(id-1) end)
    t1 = threads |> execute(instructions) |> Enum.find(fn %T{id: id} -> id == 1 end)
    t1.write_cnt
  end

  def execute(threads, instructions) do
    cond do
      threads |> Enum.all?(fn t -> t.state != :running end) ->
        threads
      true ->
        threads2 =
          threads
          |> Enum.map(fn
            %T{state: :dead} = t -> t
            %T{} = t -> t |> step(instructions)
          end)

        threads2
        |> deliver_messages
        |> execute(instructions)
    end
  end

  defp deliver_messages(all_threads) do
    all_threads
    |> Enum.map(fn t ->
      messages = t |> get_messages(all_threads)
      %{t | in_buf: t.in_buf ++ messages, out_buf: []}
    end)
  end

  defp get_messages(%T{} = t, all_threads) do
    all_threads
    |> Enum.reject(fn ot -> ot.id == t.id end)
    |> Enum.flat_map(fn ot -> ot.out_buf end)
  end

  def step(%T{pos: pos} = thread, _instructions) when pos < 0, do: %{thread | state: :dead}
  def step(%T{pos: pos} = thread, instructions) when pos >= map_size(instructions), do: %{thread | state: :dead}
  def step(%T{} = thread, instructions) do
    ins = instructions[thread.pos]
    case ins |> exec(thread) do
      {:jump, m} ->
        %{thread | pos: thread.pos + m}
      %T{state: :blocked} = thread2 ->
        thread2
      %T{} = thread2 ->
        %{thread2 | pos: thread2.pos + 1}
    end
  end

  defp exec({:snd, reg}, %T{} = thread) when is_atom(reg) do
    {:snd, thread.registers[reg] || 0} |> exec(thread)
  end
  defp exec({:snd, freq}, %T{} = thread) when is_integer(freq) do
    buf = thread.out_buf ++ [freq]
    %{thread | out_buf: buf, write_cnt: thread.write_cnt + 1}
  end

  defp exec({:set, reg, pointer}, %T{} = thread) when is_atom(pointer) do
    {:set, reg, thread.registers[pointer] || 0} |> exec(thread)
  end
  defp exec({:set, reg, n}, %T{} = thread) when is_integer(n) do
    registers = thread.registers |> Map.put(reg, n)
    %{thread | registers: registers}
  end

  defp exec({:add, reg, pointer}, %T{} = thread) when is_atom(pointer) do
    {:add, reg, thread.registers[pointer] || 0} |> exec(thread)
  end
  defp exec({:add, reg, n}, %T{} = thread) when is_integer(n) do
    old_val = thread.registers[reg] || 0
    registers = thread.registers |> Map.put(reg, old_val + n)
    %{thread | registers: registers}
  end

  defp exec({:mul, reg_a, pointer}, %T{} = thread) when is_atom(pointer) do
    {:mul, reg_a, thread.registers[pointer] || 0} |> exec(thread)
  end
  defp exec({:mul, reg_a, n}, %T{} = thread) when is_integer(n) do
    val_a = thread.registers[reg_a] || 0
    registers = thread.registers |> Map.put(reg_a, val_a * n)
    %{thread | registers: registers}
  end

  defp exec({:mod, reg, pointer}, %T{} = thread) when is_atom(pointer) do
    {:mod, reg, thread.registers[pointer] || 0} |> exec(thread)
  end
  defp exec({:mod, reg, n}, %T{} = thread) when is_integer(n) do
    old_val = thread.registers[reg] || 0
    registers = thread.registers |> Map.put(reg, rem(old_val, n))
    %{thread | registers: registers}
  end

  defp exec({:rcv, _reg}, %T{in_buf: []} = thread), do: %{thread | state: :blocked}
  defp exec({:rcv, reg}, %T{in_buf: [head | tail]} = thread) do
    registers = thread.registers |> Map.put(reg, head)
    %{thread | registers: registers, in_buf: tail, state: :running}
  end

  defp exec({:jgz, reg, n}, %T{} = thread) when is_atom(reg) do
    {:jgz, thread.registers[reg] || 0, n} |> exec(thread)
  end
  defp exec({:jgz, val, reg}, %T{} = thread) when is_atom(reg) do
    {:jgz, val, thread.registers[reg] || 0} |> exec(thread)
  end
  defp exec({:jgz, val, n}, _registers) when val > 0, do: {:jump, n}
  defp exec({:jgz, val, _n}, %T{} = thread) when val <= 0, do: thread

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
|> Threads.parse
|> Threads.run(2)
|> IO.inspect
