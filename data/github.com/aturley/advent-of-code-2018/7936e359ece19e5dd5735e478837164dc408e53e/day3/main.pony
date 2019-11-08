use "aoc-tools"
use "collections"
use "debug"

class val Claim
  let id: U64
  let x: U64
  let y: U64
  let width: U64
  let height: U64

  new val create(id': U64, x': U64, y': U64, width': U64, height': U64) =>
    id = id'
    x = x'
    y = y'
    width = width'
    height = height'

  fun string(): String =>
    "id=" + id.string() + " x=" + x.string() + " y=" + y.string() + " w=" + width.string() + " h=" + height.string()

primitive ParseClaim
  fun apply(line: String): Claim ? =>
    // line = "#1 @ 1,3: 4x4"
    let parts = line.split(" ")

    let id = parts(0)?.substring(1).u64()?
    let xy = parts(2)?.clone().>remove(":").split(",")
    let x = xy(0)?.u64()?
    let y = xy(1)?.u64()?
    let wh = parts(3)?.split("x")
    let w = wh(0)?.u64()?
    let h = wh(1)?.u64()?

    Claim(id, x, y, w, h)

class Day3 is AOCApp
  fun part1(file_lines: Array[String] val): (String | AOCAppError) =>
    let claims = Array[Claim]

    for l in file_lines.values() do
      try
        claims.push(ParseClaim(l)?)
      else
        return AOCAppError("error parsing claim '" + l + "'")
      end
    end

    let grid = SparseGrid[U64]

    var count_overlap: U64 = 0

    for c in claims.values() do
      let x = c.x
      let y = c.y
      let w = c.width
      let h = c.height

      for i in Range[ISize](x.isize(), (x + w).isize()) do
        for j in Range[ISize](y.isize(), (y + h).isize()) do
          let v = try grid(i, j)? else 0 end

          if v == 1 then
            count_overlap = count_overlap + 1
          end

          grid(i, j) = v + 1
        end
      end
    end

    count_overlap.string()

  fun part2(file_lines: Array[String] val): (String | AOCAppError) =>
    let claims = Array[Claim]

    for l in file_lines.values() do
      try
        claims.push(ParseClaim(l)?)
      else
        return AOCAppError("error parsing claim '" + l + "'")
      end
    end

    let grid = SparseGrid[SetIs[U64]]

    var count_overlap: U64 = 0

    var max_x: U64 = 0
    var max_y: U64 = 0

    let id_set = SetIs[U64]

    for c in claims.values() do
      id_set.set(c.id)
      let x = c.x
      let y = c.y
      let w = c.width
      let h = c.height

      for i in Range[ISize](x.isize(), (x + w).isize()) do
        for j in Range[ISize](y.isize(), (y + h).isize()) do
          try
            grid(i, j) = grid(i, j)?.>set(c.id)
          else
            grid(i, j) = SetIs[U64].>set(c.id)
          end

          max_x = max_x.max(i.u64())
          max_y = max_y.max(j.u64())
        end
      end
    end

    Debug([max_x; max_y])

    for i in Range[ISize](0, max_x.isize()) do
      for j in Range[ISize](0, max_y.isize()) do
        try
          if grid(i, j)?.size() > 1 then
            for id in grid(i, j)?.values() do
              id_set.unset(id)
            end
          end
        end
      end
    end

    var output = ""
    for x in id_set.values() do
      output = x.string()
    end

    output

actor Main
  new create(env: Env) =>
    AOCAppRunner(Day3, env)
