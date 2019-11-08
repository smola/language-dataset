%dw 1.0


%function addToItems()
	payload.QueueListReply.queueView.item map ((item, idx) -> {
		 (
			recLoc: {
				reservation: {
					controlNumber: item.recLoc.reservation.controlNumber[0]
				}
			}
		)
	})
	
%function getYear(d) 
	(
		//d as :datetime as :string {format: "yyyy/MM/dd"}
		(d as :datetime).year
	)
	
%function getMonth(d)
	( (d as :datetime).month
		
	)

%function getDay(d)
	(
		(d as :datetime).day
	)
	
%function getHour(d)
	(
		(d as :datetime).hour
	)
	
%function getMinutes(d)
	(
		(d as :datetime).minutes
	)



%output application/json
---
payload map ((object, idx) -> {
	
	QueueListReply: {
		queueView: {
			queueNumber: {
				queueDetails: {
					number: object.QueueListReply.queueView.queueNumber.queueDetails.number
				}
			},
			categoryDetails: {
				subQueueInfoDetails: {
					itemNumber: object.QueueListReply.queueView.categoryDetails.subQueueInfoDetails.itemNumber
				}
			},
			item: (object.QueueListReply.queueView.item map (item, iIdx) -> {
				recLoc: {
					reservation: {
						controlNumber: item.recLoc.reservation.controlNumber
					}
				},
				segment: {
					flightDate: {
						departureDate: item.segment.flightDate.departureDate
					},
					boardPointDetails: {
						trueLocation: item.segment.boardPointDetails.trueLocation
					},
					offpointDetails: {
						trueLocation: item.segment.offpointDetails.trueLocation
					},
					companyDetails: {
						marketingCompany: item.segment.companyDetails.marketingCompany
					},
					flightIdentification: {
						flightNumber: item.segment.flightIdentification.flightNumber,
						operationalSuffix: item.segment.flightIdentification.operationalSuffix
					}
				},
				pnrdates: (item.pnrdates map (pnr, pnrIdx) -> {
					timeMode: pnr.timeMode,					
					dateTime: {
						year: getYear(pnr.dateTime),
						month: getMonth(pnr.dateTime),
						day: getDay(pnr.dateTime),
						hour: getHour(pnr.dateTime),
						minutes: getMinutes(pnr.dateTime)
					}
					
					
				})
			})
		}
	}
})