
public aspect LockerMessage {
	final private int Locker.id = 1300;
	
	public int Locker.lock() { 
		return id; 
		}	
	public void Locker.unlock(int try_id) { 
		if (id == try_id) System.out.println("Finished unlocking"); 
		}	
	void Foo.log(String msg) { 
		System.err.println(msg); 
		}
	
	pointcut main(String parameter, Locker locker) : 
		call(void Foo.main(String, Locker ))
		&& args(parameter, locker);
	
	pointcut logging(Foo foo, int i) : 
		call(void firstOperation(int))
		&& target(foo)
		&& args(i);
	
	
	void around(String parameter, Locker locker) : main(parameter, locker) {
		int lockId = locker.lock();
		try {
			proceed(parameter, locker);
		} finally {
			locker.unlock(lockId);
		}
	}
	
	
	after(Foo foo, int i) : logging(foo, i) {
		foo.log("first Operation executed with parameter " + i);
	}
	
}