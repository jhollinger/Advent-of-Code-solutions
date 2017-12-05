numbers = $stdin.
  read.
  strip.
  each_char.
  map { |x| x.to_i }

last_idx = numbers.size - 1

total = numbers.
  each_with_index.
  reduce(0) { |a, (n, idx)|
    next_n = idx == last_idx ? numbers[0] : numbers[idx + 1]
    n == next_n ? a + n : a
  }

puts total
