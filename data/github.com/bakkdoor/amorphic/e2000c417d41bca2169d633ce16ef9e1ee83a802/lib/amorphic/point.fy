class Amorphic {
  Point = Tuple

  class Point {
    def x {
      at: 0
    }

    def y {
      at: 1
    }

    def x: x {
      at: 0 put: x
    }

    def y: y {
      at: 1 put: y
    }

    def in_rect?: rect {
      r_x, r_y, r_height, r_width = rect get_slots: ['x, 'y, 'height, 'width]
      @x, @y = x, y
      ((@x >= r_x) && (@x <= (r_x + r_width))) && {
        ((@y >= r_y) && (@y <= (r_y + r_height)))
      }
    }

    def + other {
      (at: 0 + (other at: 0), at: 1 + (other at: 1))
    }

    def - other {
      (at: 0 - (other at: 0), at: 1 - (other at: 1))
    }
  }
}