:- object(osm_tests, extends(lgtunit)).

	:- info([
		author is 'Sergio Castro',
		comment is 'OSM tests'
			]).

	setup :- 
		osm::assertz(node(brussels_central, coordinates(50.8454800,4.3570678),['name:en'-'Brussels Central Station',railway-station])),
		osm::assertz(node(restaurant1, coordinates(50.8323923,4.3495339),['addr:housenumber'-140,amenity-restaurant])),
		osm::assertz(node(restaurant2, coordinates(50.8325324,4.3523506),['addr:street'-'Rue d\'Écosse - Schotlandstraat',amenity-restaurant])),
		osm::assertz(way(way1, [restaurant1, restaurant2], [])).
		
	cleanup :- 
		osm::retract(node(brussels_central,_,_)),
		osm::retract(node(restaurant1,_,_)),
		osm::retract(node(restaurant2,_,_)),
		osm::retract(way(way1,_,_)).
	
	succeeds(tags_central) :- osm::node(brussels_central, NodeCentral), NodeCentral::tag(railway,station).
	
	succeeds(tags_restaurants) :- osm::node(restaurant1, R1), R1::has_tags([amenity-restaurant]), osm::node(restaurant2, R2), R2::has_tags([amenity-restaurant]).
	
	succeeds(distance_central_restaurant1) :- osm::node(brussels_central, NodeCentral), osm::node(restaurant1, Restaurant1), Restaurant1::coordinates(CoordinatesR1), NodeCentral::distancekm(CoordinatesR1, DistanceR1), 
	NodeCentral::near(CoordinatesR1, DistanceR1), SmallerDistance is DistanceR1-0.1, \+NodeCentral::near(CoordinatesR1, SmallerDistance).

	succeeds(distance_way) :- osm::way(way1, Way), osm::node(brussels_central, NodeCentral), NodeCentral::coordinates(NodeCoordinates), Way::near(NodeCoordinates, 1.53), \+Way::near(NodeCoordinates, 1.52).
		
:- end_object.
