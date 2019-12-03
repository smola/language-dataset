// Serlo coporate identity colours
pen serlo_blue = rgb("007ec1");

// TODO make pen for Serlo fons

// Arrow heads
arrowbar coordinate_arrow = Arrow(TeXHead);
arrowbar vect_arrow = Arrow(size=4, angle=20);

// If we have a pointy arrow the arrow tip is slighly too long.
// To compensate for this issue, you can slightliy shorten the arrow
// by a fixed length, e.g. 0.025. (0.025 works for unitsize = 1cm)
real arrow_length_correction = 0.025;

void draw_vector(path vect, pen p = currentpen) {
  pair b = beginpoint(vect);
  pair e = endpoint(vect);

  // Prevent error from empty vector path
  if(b == e)
    return;
  
  if(linejoin(defaultpen) == 0) {
    pair direction = unit(e - b);
    e -= direction*arrow_length_correction;
  }
  draw(b -- e, arrow=vect_arrow, p);
}
