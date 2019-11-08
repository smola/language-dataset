Pseq([
	EventPattern((dur: 0.1, degree: (1..8).pseq(2))),
	EventPattern((dur: [0.1, 0.2].prand, degree: (8..1).pseq(2))),
	EventPattern((dur: [0.1, 0.2].prand,
		degree: [10, 2, [0, 3, 5] + [-10, 10].pwhite(4)].prand(15)))
]).play;
//:
EventPattern((
	dur: [0.1, 0.2].prand,
	degree: [10, [0, 2] + [0, 10].pwhite(4), [0, 3, 5] + [-10, 10].pwhite(4)].prand(25)
)).play
//:
a = EventPattern((
	dur: [0.1, 0.2].prand,
	degree: [10, [0, 2] + [0, 10].pwhite(4), [0, 3, 5] + [-10, 10].pwhite(4)].prand(25)
)).asStream;
a.next.play;
//:

EventPattern((dur: 0.01, freq: [400, 500, 10].pbrown)).play