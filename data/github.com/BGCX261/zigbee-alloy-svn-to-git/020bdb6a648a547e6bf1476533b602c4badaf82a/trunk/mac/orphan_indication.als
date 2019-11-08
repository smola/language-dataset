/** 
 * @file	zigbee_join/mac/orphan_indication.als
 * @brief	MAC layer handling of OrphanNotification packet
 * @author	Andrey Kupriyanov (akupriyanov@acm.org)
 * @date	2008, April 29
 * @since	2008, April 28
 *
 */
module zigbee_join/mac/orphan_indication

open zigbee_join/base as z
open zigbee_join/nwk/orphan_indication as orphan_indication


// MLME-ORPHAN.indication/response
sig MacOrphanIndication extends MacIndication 
{
	// NWK layer indications
	indication: NwkOrphanIndication,

	// Packets
	notification: OrphanNotification,
	realignment: lone CoordinatorRealignment	
} {
	// Each OrphanIndication is a result of one OrphanNotification packet
	notification in receive[OrphanNotification,t,t]
	indication = orphanIndication[notification.sourceAddress]
	
	// If we have found orphan child, send to it CoordinatorRealignment packet
	isTrue[indication.associatedMember] => {
		realignment.coordinatorShortAddress = node.nwkNetworkAddress.t'
		realignment.shortAddress = indication.shortAddress
		sendTo[realignment,t',notification.sourceAddress]
	}
	else {
		no realignment
	}
	noEventsExcept[indication+realignment]
}

fact OrphanNotification
{
	all p: OrphanNotification, n: Node, t: Time {
		lone e: MacOrphanIndication {
			e.source=p
			e.node = n
			e.@t = t
		}
	}
}

fact CoordinatorRealignment
{
	all p: CoordinatorRealignment {
		one e: MacOrphanIndication {
			p.source=e
			e.realignment = p
		}
	}
}
