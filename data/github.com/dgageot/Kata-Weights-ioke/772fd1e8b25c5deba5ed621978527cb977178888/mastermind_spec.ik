;Response to challenge at http://beust.com/weblog/2011/10/30/a-new-coding-challenge/

N = 40

combinations = for(
	a <- 1..N-3,
	b <- a..N-3,
	c <- b..N-3,
	d <- c..N-3,
	a+b+c+d == N,
	[a,b,c,d]
)

can_weight_all = method(weights,
	(1..N) all?(w, can_weight(w, *weights))
)

can_weight = method(weight, a, b, c, d,
	weightable = for:set(
		w <- -1..1,
		x <- -1..1,
		y <- -1..1,
		z <- -1..1,
		w*a+x*b+y*c+z*d
	)
	weightable include?(weight) || (weightable include?(weight - 1) && weightable include?(weight + 1))
)

combinations each(x, if(can_weight_all(x), x println))
