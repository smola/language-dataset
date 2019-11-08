Node = Origin mimic do(
  initialize = method(x, y, alive, world,
    self x = x
    self y = y
    self alive = alive
    self world = world
  )

  step = method(world,
    new_alive = alive
    active_neighbours = neighbours map(c, c alive) select length
    if(active_neighbours < 2 || active_neighbours > 3, new_alive = false)
    if(!(alive) && active_neighbours == 3, new_alive = true)
    Node mimic(x, y, new_alive, world)
  )

  show = method(if(alive, "[]", "  ") print)

  out_of_bounds = method(n, n < 0 || n >= world nodes first length)
  neighbour_vectors = [{x: -1, y: -1}, {x: 0, y: -1}, {x: 1, y: -1},
    {x: -1, y: 0}, {x: 1, y: 0},
    {x: -1, y: 1}, {x: 0, y: 1}, {x: 1, y: 1}]

  neighbours = method(vectors: neighbour_vectors,
    v = vectors[0]

    if(v == nil, return [])
    node = if(out_of_bounds(x + v[:x]) || out_of_bounds(y + v[:y]),
      [],
      [world nodes[y + v[:y]][x + v[:x]]])

    node + neighbours(vectors: vectors rest)
  )
)

World = Origin mimic do(
  setup_nodes = method(nodes, self nodes = nodes. self)

  setup_randomise = method(dimension,
    self nodes = []
    dimension times(y,
      nodes[y] = []
      dimension times(x,
        nodes[y][x] = Node mimic(x, y, System randomNumber() < 0.1, self)
      )
    )
    self
  )

  step = method(
    new_world = World mimic
    new_world setup_nodes(nodes map(r, r map(n, n step(new_world))))
  )

  show = method(
    (nodes first length * 2) times("-" print). "" println
    nodes map(r, r map(c, c show). "|" println)
  )
)

world = World mimic setup_randomise(30)
loop(
  world show
  world = world step
)
