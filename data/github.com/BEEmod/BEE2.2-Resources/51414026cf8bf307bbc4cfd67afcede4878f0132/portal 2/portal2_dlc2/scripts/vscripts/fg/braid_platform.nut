//Braid Platforms!

numCaptures <- 60; //How many captures to save
timeFlow <- 0.5; //How long between captures
pastPlaces <- [];
disabled <- 0;

for(local i = 0; i < numCaptures; i++){
	pastPlaces.append(self.GetOrigin());
}

function AddToList(){
	if(disabled <= 0){
		pastPlaces[numCaptures-1] = self.GetOrigin();
		
		for(local i = 0; i < numCaptures - 1; i++){
			pastPlaces[i] = pastPlaces[i+1];
		}
		
//		printl("BRAID: "+self.GetName()+ " (Platform) Captured!");
	}
	
	EntFireByHandle(self,"RunScriptCode","AddToList();",timeFlow,null,null); //Schedule next step
}

function SetBack(captures){
	printl("BRAID: Platform teleporting back "+captures+" timecaptures");
	if(captures > numCaptures) return;
//	pastPlaces[numCaptures-1] = pastPlaces[numCaptures-captures];
//	pastAngles[numCaptures-1] = pastAngles[numCaptures-captures];
	self.SetOrigin(pastPlaces[numCaptures-captures]);
}

::braid_cubes.append(self);

printl("BRAID: Platform "+self.GetName()+" Ready!");

AddToList();