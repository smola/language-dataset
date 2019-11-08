class Problem88 {
  Void main() {
    max := 12_000
    Int[] min := (0..max).map { it * 2 }
    factors := [2]
    prod := 2.pow(factors.size)
    while (true) {
      if (prod > max * 2) break
      len := prod - sum(factors) + factors.size
      if (len <= max && min[len] > prod) min[len] = prod
      for (k := 0; k < factors.size; k++) {
        next := factors[k] + 1
        prod /= factors[k]
        factors[k] = 1
        if (prod * next.pow(k + 1) <= max * 2) {
          (0..k).each { factors[it] = next }
          prod *= next.pow(k + 1)
          break
        } else if (k == factors.size - 1) {
          factors = (1..factors.size + 1).map { 2 }
          prod = 2.pow(factors.size)
          break
        }
      }
    }
    res := sum(min.unique) - 2
    echo(res)
  }

  Int sum(Int[] list) {
    return list.reduce(0, |Int a, Int b -> Int| { a + b })
  }
}
