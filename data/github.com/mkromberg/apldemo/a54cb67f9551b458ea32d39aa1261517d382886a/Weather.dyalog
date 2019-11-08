 Weather←{
     ⍝ Get Weather and Location data sequentially

     host←'http://geographic-services.herokuapp.com:8000'
     nearbyPath←'/places/nearby' ⋄ weatherPath←'/weather'
     (lat lon radius units)←'lat=19.01' 'lon=72.8' 'radius=25' 'unit=km'

     placesNearbyUrl←host,nearbyPath,'?',lat,∊'&',¨lon radius units
     weatherUrl←host,weatherPath,'?',lat,'&',lon

     placesNearbyData←getRequestData placesNearbyUrl
     weatherData←getRequestData weatherUrl
     '{ "weather" : ',weatherData,', "placesNearby": ',placesNearbyData,' }"'
 }
