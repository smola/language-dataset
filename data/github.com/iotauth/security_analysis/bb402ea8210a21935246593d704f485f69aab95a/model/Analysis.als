open KeyDistribution

/**
	Security properties for Auth & 
	commands to run analysis
**/

/** Security properties **/

pred confidentiality {
	-- Non-attacker's secret should never be known to an attacker
	no d : Device - Attacker, s : d.secrets, t : Time |  
		knows[Attacker, s, t]
}
pred integrity {
	-- Attacker must not know keys for integrity and message authenticity
	no d : Device - Attacker, a : Auth,  t : Time, s : Name.(d.sessionKey.t)
			+ AuthID.(d.authDistrKey.t) + (d.pubKey.pair) + (a.pubKey.pair)|  
		knows[Attacker, s, t]
}

/*
pred integrity {
	-- Attacker's secret should never flow into a non-attacker device
	no d : Device - Attacker, s : Attacker.secrets, t : Time |
		knows[d, s, t]	
}
*/

/** Assumptions needed to satisfy security properties **/
pred assumptions {
	-- No two devices can share names
	all disj d1, d2 : Device | d1.name != d2.name
	-- No two entities can share public keys
	all disj e1, e2 : Entity | e1.pubKey != e2.pubKey or no (e1.pubKey + e2.pubKey)
	-- It should never be the case that a public key of one entity is a private key of another
	no disj e1, e2 : Entity |
 		some e1.pubKey and some e2.pubKey and e1.pubKey.pair = e2.pubKey
	-- Session key request can't be from one device to itself
	all m : SessionKeyReqNoDistrKey | m.requester != m.target
	-- No two devices share secrets
	all disj d1, d2 : Device | no d1.secrets & d2.secrets	
	-- No session keys in init state (not for security, but for simplification only)
	all d : Device | no d.sessionKey.first
	all a : Auth | no a.sessionKey.first
	-- Policy assumption: Eve cannot appear in the policy table
	all n1, n2 : Name | 
		n1 -> n2 in (AuthX + AuthY).policy implies Eve not in (n1 + n2)
	-- Initial distribution keys correctly configured among devices and Auths
	all d : Device, a : Auth | 
		(d.authDistrKey.first)[a.id] = (a.entityDistrKey.first)[d.name] 
	-- Each entity gets assigned a unique distribution key
	all a : Auth, n1, n2 : Name |
		n1 != n2 implies
			(a.entityDistrKey[n1] != a.entityDistrKey[n2] or no a.entityDistrKey[n1])
	-- distribution keys aren't initially shared among different devices
	all disj d1, d2 : Device, a : Auth |
		(d1.authDistrKey.first)[a.id] != (d2.authDistrKey.first)[a.id] or
		no ((d1 + d2).authDistrKey.first)[a.id]
	-- public keys are configured correctly
	all n : Name, k : AsymKey |
		n -> k in AuthX.entityPublicKey implies	
			let device = name.n | k = device.pubKey
	all d : Device, i : AuthID, k : AsymKey |
		i -> k in d.authPublicKey implies
			let auth = (Auth <: id).i | auth.pubKey = k

	/** Assumptions related to multiple Auths **/
	-- Auths don't share distribution keys
	all disj a1, a2 : Auth, t : Time |
		no (a1.entityDistrKey.t)[Name] & (a2.entityDistrKey.t)[Name]
	
	-- Keys created by distinct Auths must be different
	all disj a1, a2 : Auth, t : Time | no a1.usedKeys.t & a2.usedKeys.t
}

fact AttackerModel {
	Attacker.name = Eve
	Attacker.pubKey = PubkAttacker
	Attacker.secrets = MaliciousPayload
}

// these "one sig"s are like constants
one sig Eve, MyEV, MyChargingStation extends Name {}
one sig PubkEV, PrvkEV, 	-- public and private keys of EV, etc.,
	PubkStation, PrvkStation, 
	PubkAttacker, PrvkAttacker, 
	PubkAuthX, PrvkAuthX,
	PubkAuthY, PrvkAuthY extends AsymKey {}
one sig MyAuthX, MyAuthY extends AuthID {}
one sig EVSecret, MaliciousPayload extends Payload {}

/** Simple configuration with one Auth (AuthX) and three devices **/
one sig EV extends Device {} {
	name = MyEV
	pubKey = PubkEV
	secrets = EVSecret
	authPublicKey = MyAuthX -> PubkAuthX
	(authDistrKey.first).SymKey in MyAuthX
//	no sessionKey.first
}
one sig ChargingStation extends Device {}{
	name = MyChargingStation
	pubKey = PubkStation
	authPublicKey = MyAuthY -> PubkAuthY
	(authDistrKey.first).SymKey in MyAuthY
//	no sessionKey.first
}

one sig AuthX extends Auth {} {
	pubKey = PubkAuthX
	id = MyAuthX
	entityPublicKey = MyEV -> PubkEV + Eve -> PubkAttacker
	otherAuthPublicKey = MyAuthY -> PubkAuthY
}
one sig AuthY extends Auth {} {
	pubKey = PubkAuthY
	id = MyAuthY
	entityPublicKey = MyChargingStation -> PubkStation + Eve -> PubkAttacker
	otherAuthPublicKey = MyAuthX -> PubkAuthX
}

fact {
	-- fix the key pairs
	pair = 
		PubkEV -> PrvkEV + PrvkEV -> PubkEV +
		PubkStation -> PrvkStation + PrvkStation -> PubkStation +
		PubkAttacker -> PrvkAttacker +  PrvkAttacker -> PubkAttacker +
		PubkAuthX -> PrvkAuthX + PrvkAuthX -> PubkAuthX +
		PubkAuthY -> PrvkAuthY + PrvkAuthY -> PubkAuthY
}

/** Commands **/

-- Generate some random scenario with at most 4 messages and 20 data elements
run GenerateRandomScenario {
	ChargingStation.(authDistrKey.first).SymKey in MyAuthY
} for 1 but 5 Time, 5 Message,  2 Payload, 3 Name, 13 Key///, 20 Data

run CommunicationExample {
	assumptions
	no (sender + receiver).Attacker
	-- Generate a scenario that involves transfer of some payload (s) from
	-- one device (d1) to another device (d2) such that d2 eventually 
	-- has access to s.
	some s : Payload, d1, d2 : Device, t : Time |
		s in d1.initKnowledge and s not in d2.initKnowledge and
		knows[d2, s, t]	and
		d1 + d2 in Device - Attacker	
} for 
//1 but 4 Time, 4 Message, 2 Payload, 3 Name, 11 Key
//1 but 4 Time, 4 Message, 2 Payload, 3 Name, 13 Key
//1 but 6 Time, 6 Message, 2 SKID, 2 Payload, 3 Name, 13 Key
//1 but 6 Time, 6 Message, 2 Payload, 3 Name, 13 Key
1 but 6 Time, 7 Message, 2 SKID, 2 Payload, 3 Name, 13 Key

-- Check whether there's a scenario that leads to a violation of confidentiality
check CheckConfidentiality {
	assumptions implies confidentiality
} for 
//1 but 4 Time, 4 Message, 2 Payload, 3 Name, 11 Key
//1 but 4 Time, 4 Message, 2 Payload, 3 Name, 13 Key
//1 but 5 Time, 5 Message, 2 Payload, 3 Name, 11 Key
//1 but 5 Time, 5 Message, 2 Payload, 3 Name, 13 Key
//1 but 6 Time, 6 Message, 2 Payload, 3 Name, 13 Key
//1 but 7 Time, 7 Message, 2 Payload, 3 Name, 13 Key
//1 but 5 Time, 7 Message, 2 SKID, 2 Payload, 3 Name, 13 Key
//1 but 6 Time, 6 Message, 2 SKID, 2 Payload, 3 Name, 13 Key
//1 but 7 Time, 7 Message, 2 SKID, 2 Payload, 3 Name, 13 Key
//1 but 10 Time, 10 Message, 2 SKID, 2 Payload, 3 Name, 13 Key
1 but 2 Time, 2 Message, 2 SKID, 2 Payload, 3 Name, 13 Key

-- Check the integrity property
check CheckIntegrity {
	assumptions implies integrity
} for 
//1 but 6 Time, 6 Message//, 20 Data
1 but 5 Time, 5 Message, 2 Payload, 3 Name, 13 Key

/** Some other random stuff **/

fun sendsTo : Entity -> Entity -> Time {
	{from, to : Entity, t : Time | 
		some m : at.t | from = m.sender and to = m.receiver }
}

fun relevantNodes : Entity -> Time {
	{e : Entity, t : Time |
		some m : at.t | e in m.(receiver + sender) }
}

run {
	assumptions
	some m : SessionKeyRespNoDistrKey {
		m.receiver = EV
		learns[EV, m.distrKey, m]
		m.distrKey in m.content
		learns[EV, m.sessionKey, m]
		some m2 : SessionKeyRespNoDistrKey {
			m2.receiver = ChargingStation
			m2.sessionKey = m.sessionKey
			learns[ChargingStation, m2.sessionKey, m2]
			some m3 : SendSecretMessage {
				m3.sender = EV
				m3.receiver = ChargingStation				
				m3.at in m2.at.nexts
				some m3.content & EV.secrets
				EV.secrets -> m.sessionKey in m3.encryptedWith						
				no m3.content & ChargingStation.initKnowledge
				learns[ChargingStation, EV.secrets, m3]	
				knows[ChargingStation, EV.secrets, m3.at.next]
//				some m3.content & EV.secrets
//				no SKID & m3.content
/*
				m3.content in EV.initKnowledge
				m3.content not in ChargingStation.initKnowledge
				knows[EV, m2.sessionKey, t]
				knows[ChargingStation, m2.sessionKey, t]
*/
			}
		}
	}
} for
1 but 9 Time, 9 Message, 2 SKID, 2 Payload, 3 Name, 13 Key

run {
	assumptions
	some m : SessionKeyRespNoDistrKey {
		m.receiver = EV
		learns[EV, m.distrKey, m]
		m.distrKey in m.content
		learns[EV, m.sessionKey, m]
		m.at = first.next
		some m2 : SendSecretMessage {
			m2.receiver = ChargingStation
			m2.sender = EV
			m2.at = m.at.next
			some m3 : AuthSessionKeyResp {
				m3.sender = AuthX
				m3.receiver = AuthY
				m3.sessionKey = m.sessionKey
				m3.at = m2.at.next			
				some m4 : SessionKeyRespNoDistrKey {
					m4.sender = AuthY
					m4.receiver = ChargingStation
					m4.sessionKey = m3.sessionKey
					m4.at = m3.at.next.next
					some m5 : SendSecretMessage {
						m5.sender = EV
						m5.receiver = ChargingStation
						m5.content = EV.secrets
						m5.at = m4.at.next
						EV.secrets -> m.sessionKey in m5.encryptedWith
						learns[ChargingStation, EV.secrets, m5]
					}
				}
			}
		}
	}
} for
1 but 7 Time, 7 Message, 2 SKID, 2 Payload, 3 Name, 13 Key

run {
	assumptions
	some m : SessionKeyRespNoDistrKey {
		m.receiver = EV
		learns[EV, m.distrKey, m]
		m.distrKey in m.content
		learns[EV, m.sessionKey, m]
//		m.at = first.next
		some m2 : SendSecretMessage {
			m2.receiver = ChargingStation
			m2.sender = EV
//			m2.at = m.at.next
			some m3 : AuthSessionKeyResp {
				m3.sender = AuthX
				m3.receiver = AuthY
				m3.sessionKey = m.sessionKey
//				m3.at = m2.at.next			
				some m4 : SessionKeyRespNoDistrKey {
					m4.sender = AuthY
					m4.receiver = ChargingStation
					m4.sessionKey = m3.sessionKey
//					m4.at = m3.at.next.next
					some m5 : SendSecretMessage {
						m5.sender = EV
						m5.receiver = ChargingStation
						m5.content = EV.secrets
//						m5.at = m4.at.next
						EV.secrets -> m.sessionKey in m5.encryptedWith
						learns[ChargingStation, EV.secrets, m5]
					}
				}
			}
		}
	}
} for
1 but 5 Time, 7 Message, 2 SKID, 2 Payload, 3 Name, 13 Key

run {
	some disj m1, m2 : Message | m1.at = m2.at
} for 1 but 4 Time, 4 Message, 2 SKID, 2 Payload, 3 Name, 13 Key

