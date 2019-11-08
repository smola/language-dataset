module org::maracas::Maracas

import IO;
import lang::java::m3::Core;
import org::maracas::delta::Delta;
import org::maracas::delta::DeltaBuilder;
import org::maracas::delta::Detector;
import org::maracas::delta::Migration;
import ValueIO;


Delta delta(loc oldAPI, loc newAPI) 
	= createDelta(createM3FromJar(oldAPI), createM3FromJar(newAPI));

Delta delta(M3 oldAPI, M3 newAPI) 
	= createDelta(oldAPI, newAPI);
	
Delta classDelta(Delta delta)
	= getClassDelta(delta);

Delta methodDelta(Delta delta)
	= getMethodDelta(delta);
	
Delta fieldDelta(Delta delta)
	= getFieldDelta(delta);

set[Detection] detections(loc oldClient, Delta delta) 
	= detections(createM3FromJar(oldClient), delta);

set[Detection] detections(M3 oldClient, Delta delta) 
	= detections(oldClient, delta);
		
set[Migration] migrations(loc newClient, set[Detection] detects)
	= migrations(newClient, detects);