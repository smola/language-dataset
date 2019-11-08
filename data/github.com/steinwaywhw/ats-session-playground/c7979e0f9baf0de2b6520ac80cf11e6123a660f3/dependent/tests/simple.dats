
staload "../session.sats"


stadef proto = pmsg(0,int)::pmsg(0,int)::pmsg(1,bool)::pend(0)

extern fun server (chan (1, proto)): void
extern fun client (chan (0, proto)): void


implement server (ch) = let 
	val a = recv (ch)
	val b = recv (ch)
//    val b = 2
//	val _ = send (ch, a = b)
in 
	wait ch 
end

implement client (ch) = let 
	val _ = send (ch, 1) 
	val _ = send (ch, 1) 
	val c = recv (ch)
	val _ = println! c
in 
	close ch 
end


extern fun test (): void 
implement test () = let 
	val c = create {0,1} (llam s => server s)
in 
	client c
end