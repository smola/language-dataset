module Milestone2/RDT20

open util/ordering[State]

one sig ACK, NAK extends Data{}

sig Checksum{}

one sig CorruptChecksum extends Checksum{}

sig Data{}

sig Packet{ 
	data: Data,
	checksum : Checksum
}

sig Receiver{
	sender: one Sender
}

sig Sender{
	receiver: one Receiver
}

sig State{
	sent: set Packet,
	senders: (Sender -> Data),
	receivers: (Receiver -> Data),
	buffer: (Sender -> Packet),
	lastSent: (Sender -> Data),
	replyBuffer: (Receiver -> Packet),
	replies: (Sender -> Data)
}



pred SendPacket[s,s':State]{
	(one send:Sender |
		(s.buffer[send] = none) and
		(one p:(Packet - s.sent) |
			p in s'.sent and
			(one d:(Data - (ACK + NAK)) |
				d = p.data and
				(let sendPair = (send->p) |
					(let sendData = (send -> d) |
						((((send->NAK) in s.replies) => (sendData in s.lastSent)) and
						(((send->ACK) in s.replies) => (sendData in s.senders)) and
						#s.replyBuffer = 0 and
						sendData !in s'.senders and
						sendPair in s'.buffer and
						sendData in s'.lastSent and
						#s.buffer = 0 and
						s'.senders = s.senders - sendData and
						s.receivers = s'.receivers and
						s'.lastSent = (s.lastSent) - (send->s.lastSent[send]) + sendData and
						s.replies = s'.replies and
						s.replyBuffer = s'.replyBuffer and
						s.sent = s'.sent - p and
						s.buffer = s'.buffer - sendPair))))))
}

pred ReceivePacket[s,s':State]{
	(one send:Sender | 
		(one p:Packet |
			(one d:(Data - (ACK + NAK)) |
				(one r: Receiver|
				d = p.data and
					(let sendPair = (send->p) |
						(let receiveData = (r -> d) |
							((#s.replyBuffer = 0) and (#s'.replyBuffer = 1) and
							(p.ErrorCheck[] => (r.(s'.replyBuffer)).data = ACK) and
							(!p.ErrorCheck[] => (r.(s'.replyBuffer)).data = NAK) and
							(#s'.buffer = 0) and
							((s.buffer[send] = p) and
							(send.receiver = r) and
							s'.buffer = s.buffer - sendPair and
							(p.ErrorCheck[] => receiveData in s'.receivers) and
							(!p.ErrorCheck[] => receiveData !in s'.receivers) and
							receiveData !in s.receivers and
							s.lastSent = s'.lastSent and
							s'.receivers - receiveData = s.receivers and
							s.replies = s'.replies and
							s.sent = s'.sent and
							s.senders = s'.senders))))))))
}

pred ReceiveReply[s,s':State]{
	(one r:Receiver|
		(one d:(ACK + NAK)|
			(one p:(Packet - s.sent) |
				p.data = d and
				p in s'.sent and
				(one send:Sender|
					send = r.sender and
					((p.ErrorCheck[] => s'.replies = (s.replies - (send->s.replies[send]))+ (send->d))) and
					(!p.ErrorCheck[] => (s'.replies = (s.replies - (send->s.replies[send])) + (send->((ACK + NAK) - d)))) and
					s.lastSent = s'.lastSent and
					s.receivers = s'.receivers and
					s.buffer = s'.buffer and
					s.senders = s'.senders and
					s.sent = s'.sent - p and
					(r->p) in s.replyBuffer and
					s'.replyBuffer = s.replyBuffer - (r->p)))))
}

pred Packet.ErrorCheck{
	this.checksum != CorruptChecksum
}

pred State.Done[]{
//	#this.senders = 0 and #this.buffer = 0 and #this.replyBuffer = 0
	all d:(Data - (ACK + NAK)) | d in Receiver.(this.receivers)
}

pred State.Init[]{
	#this.receivers = 0 and #this.buffer = 0  and #this.replyBuffer = 0 and #this.lastSent = 0 and this.replies = {Sender->ACK}
	and all s:Sender | (s->ACK) !in this.senders and (s->NAK) !in this.senders and this.sent = none
}

pred Transition[s, s':State]{
	SendPacket[s, s'] or 
	ReceivePacket[s, s'] or ReceiveReply[s, s']
}

fact{
	receiver = ~sender
}

fact{
//	all d:Data | some p:Packet | p.data = d and p.checksum != CorruptChecksum
}

fact{
	some p:Packet | p.checksum = CorruptChecksum
}

fact{
	(no d:Data | (some r:Receiver | (some r2:Receiver - r | (r->d) in State.receivers and (r2->d) in State.receivers))
		or (some s:Sender| (some s2:Sender -s | ((s->d) in State.senders and (s2->d) in State.senders)
			or ((s->d) in State.lastSent and (s2->d) in State.lastSent)
			or (some p:Packet | p.data = d and ((s->p) in State.buffer and (s2->p) in State.buffer)))))
}

fact{
	(all st:State | no d:Data | (d in Sender.(st.senders) and d in Receiver.(st.receivers))
		or (d in Sender.(st.senders) and d in (Sender.(st.buffer)).data)
		or (d in (Sender.(st.buffer)).data and d in Receiver.(st.receivers))) 
}

assert AlwaysWorks{
	(first.Init[]  and
		all s:State - last |
			let s' = s.next |
				Transition[s, s'] )=> last.Done[]
}

pred Trace[]{
	first.Init[] and
		all s:State - last |
			let s' = s.next |
				Transition[s, s'] and last.Done[]
}

pred show{}
check AlwaysWorks for 1 but exactly 10 State, exactly 6 Packet, exactly 3 Data, exactly 2 Checksum
run Trace for 2 but exactly 9 State, exactly 5 Packet, exactly 4 Data
run show for 2 but exactly 5 State
