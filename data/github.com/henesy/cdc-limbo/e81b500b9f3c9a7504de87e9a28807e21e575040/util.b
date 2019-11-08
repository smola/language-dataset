implement Util;

include "util.m";

include "sys.m";
include "lock.m";

sys: Sys;
lock: Lock;
Semaphore: import lock;

# Lock for spin()
slock: ref Semaphore;

# initialize the module
init() {
	lock = load Lock Lock->PATH;
	lock->init();
	slock = Semaphore.new();
	sys = load Sys Sys->PATH;
}

# Replace within a string
tr(s: string, a: int, b: int): string {
	for(i := 0; i < len s; i++)
		if(s[i] == a)
			s[i] = b;

	return s;
}

# Spin for an arbitrary period of time ­ seconds
spin(n: int, schan: chan of string) {
	slock.obtain();

	sys->sleep(n / 1000);

	schan <-= "idle";

	slock.release();
}
