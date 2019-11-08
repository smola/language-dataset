
initialize() {

	//Source File With Helper Functions
	source("./MyRecipes1/dev_Utils.slim");
	//source("./dev_Utils.slim");

	//The Path Where You want sampling files written to
	pathToOutputFile = "./Output1/MyRecipe9_0_1/";
	//pathToOutputFile = "~/Desktop/";

	//KNOBS//

	endOfSimulation = 80000; 		//80000 for real runs

	introduce = 40000;			//40000 for real runs

	samplingInterval = 50;			//50 for real runs

	numberOfLakes = 50;	
	
	optimumPhenoMarine = 10;
	
	optimumPhenoFresh = -10;

	individualsInOcean = 10000;
	
	individualsInFresh = 10000;

	numberOfEffectRegions = 10;

	sizeOfEffectRegions = 100000;

	sizeOfNonEffectBuffer = 4950000;

	mutationRateInEffectRegions = 1e-10;
	
	mutationRateInNeutralRegions = 0; //Because TreeSeq :-) 
	
	recombinationRateInEffectRegions = 1e-8;

	recombinationRateInNeutralRegions = 1e-8;

	migrationRate = 1e-4;

	//For the Header of Files

	individualsPerFresh = individualsInFresh/numberOfLakes;	
	lengthOfPops = asFloat(numberOfLakes);
	numberOfSamplingPoints = endOfSimulation/samplingInterval;	

	//Define constants 	

	defineConstant("END",endOfSimulation);
	defineConstant("INT",samplingInterval);
	defineConstant("NLAKES",numberOfLakes);
	defineConstant("IIO", individualsInOcean);
	defineConstant("IIF", individualsInFresh);	
	defineConstant("LOP", lengthOfPops);
	defineConstant("IPF",individualsPerFresh);
	defineConstant("PATH",pathToOutputFile);
	defineConstant("C", numberOfEffectRegions);    	// number of Effect Regions;
	defineConstant("W", sizeOfNonEffectBuffer);  	// size of neutral buffer on each side
	defineConstant("P", sizeOfEffectRegions);   	// Size of contribution (Effect) Loci 
	defineConstant("INP", introduce);
	defineConstant("MGR", migrationRate);

	//Marine individuals need a one-dimentional spatial position

	initializeSLiMOptions(dimensionality="x");
	initializeTreeSeq();

	//initialize all mutation types
	//Move individual trait value in the:

	initializeMutationType("m1", 0.5, "g", 0.5, 1.0);  //Positive Direction, Dominant 
	initializeMutationType("m2", 0.5, "g", -0.5, 1.0); //Negative Direction, Dominant
	//initializeMutationType("m3", 0.5, "f", 0.0);       //Neither Direction, Neutral
	initializeMutationType("m4", 0.5, "g", 0.5, 1.0);  //Positive Direction, Recessive
	initializeMutationType("m5", 0.5, "g", -0.5, 1.0); //Negative Direction, Recessive
	initializeMutationType("m6", 0.5, "g", 0.6, 1.0);  //Positive Direction, Additive
	initializeMutationType("m7", 0.5, "g", -0.6, 1.0); //Negative Direction, Additive

	//Genomic elements: 	

	initializeGenomicElementType("g1", m1, 0.0); //Neutral, although m1 is defined to arise, notice the mutRate in these regions is 0
	initializeGenomicElementType("g2", c(m1,m2,m4,m5,m6,m7), c(1.0,1.0,1.0,1.0,1.0,1.0)); 	//Potential to impact individual trait value

	//Make all mutations stack "Late" meaning the last mutation to arise at a locus takes precedent

	m1.mutationStackPolicy = "l";
	m2.mutationStackPolicy = "l";
	//m3.mutationStackPolicy = "l";
	m4.mutationStackPolicy = "l";
	m5.mutationStackPolicy = "l";
	m6.mutationStackPolicy = "l";
	m7.mutationStackPolicy = "l";
	
	//initialize genomic arch.
	// |----------NonEffect-----------|-Effect(g3)-|----------NonEffect----------|

	mutRateVector = NULL;
	recombVector = NULL;
	lociVector = NULL;

	pos = 0;
	q = NULL;
	for (i in 1:C)
	{
		initializeGenomicElement(g1, pos, pos + W-1);    
		pos = pos + W;
		recombVector = c(recombVector,recombinationRateInNeutralRegions);
		mutRateVector = c(mutRateVector,mutationRateInNeutralRegions);
		lociVector = c(lociVector,pos-1);

		initializeGenomicElement(g2, pos, pos + P-1);
		q = c(q,c(pos,pos+P-1));
		pos = pos + P;	
		recombVector = c(recombVector,recombinationRateInEffectRegions);
		mutRateVector = c(mutRateVector,mutationRateInEffectRegions);
		lociVector = c(lociVector,pos-1);

		initializeGenomicElement(g1, pos, pos + W-1);
		pos = pos + W;
		recombVector = c(recombVector,recombinationRateInNeutralRegions);
		mutRateVector = c(mutRateVector,mutationRateInNeutralRegions);
		lociVector = c(lociVector,pos-1);
	}

	catn("end of chromosome position: "+pos);
	totalNumOfLoci = pos;

	defineConstant("TL", totalNumOfLoci);
	defineConstant("Q", q);     // remember our QTL positions

	initializeRecombinationRate(recombVector,lociVector);	
	initializeMutationRate(mutRateVector,lociVector); 	

	//Interaction Function for Oceanic mateChoice() callback...

	initializeInteractionType(1, "x", reciprocal=T, maxDistance=1.0);
	i1.setInteractionFunction("n", 0.5, 0.5);

	//Interation Function for Freshwater mateChoice() callback...

	initializeInteractionType(2, "x", reciprocal=T, maxDistance=0.02);
	i2.setInteractionFunction("n", 0.5, 0.02);

	//file header for ALL sampling files = numberOfSamplingPoints + sampingInterval + introduction + endofsim

	header = numberOfSamplingPoints+" "+INT+" "+INP+" "+END;

	//Initialize all sampling files with a header
	
	writeFile(PATH+"AveragePhenotypeThroughout.txt",header,append = F);
//	writeFile(PATH+"EffectMutationFrequenciesThroughout.txt",header,append=F);	
	writeFile(PATH+"AvgNumLakesFreshAlleleAppearsIn.txt",header,append=F);
	writeFile(PATH+"numfreshAlleles.txt",header,append=F);
	writeFile(PATH+"AvgFreshAllelesPerMarineInd.txt",header,append=F);
//	writeFile(PATH+"OceanFreshwaterFstRoc.txt",header,append = F);
	writeFile(PATH+"OceanFreshwaterFst.txt",header,append = F);
	writeFile(PATH+"OceanFreshwater2Fst.txt",header,append = F);
	writeFile(PATH+"FreshwaterFreshwater2Fst.txt",header,append = F);
	writeFile(PATH+"StandingGeneticVariationThroughout.txt",header,append = F);
	writeFile(PATH+"avgFWAA.txt",header,append = F);	
	writeFile(PATH+"avgFWAA_divTotal.txt",header,append = F);	
	writeFile(PATH+"Adaptation.txt",header,append = F);	
//	writeFile(PATH+"MeanFstThroughoutNeutral.txt",header,append = F);  //Fst Sampling is going to be done in python with the trees. 
//	writeFile(PATH+"MeanFstThroughoutEffect.txt",header,append = F);
	writeFile(PATH+"FWAAPerMarineIndCounts",header,append = F);
	
}


1 early(){

	//right off the bat, set adapted to false because the new populations have not adapted
	//also schedule the introduction blocks to the defined value in init
	sim.setValue("Adapted",F);

	sim.rescheduleScriptBlock(s1,INP,INP);

	for (block in c(s2,s3,s4,s5)){
		sim.rescheduleScriptBlock(block,INP);
	}
	
	sim.rescheduleScriptBlock(s6,END,END);
	
}

//--INTRODUCE MARINE AND FRESHWATER POPULATION--

1 late(){	

	
	sim.addSubpop("p1", IIO);  //Marine 
	sim.addSubpop("p2", IIF);  //Freshwater
	
	//set length of pops equal to 1 unit per lake

	p1.setSpatialBounds(c(0.0, LOP));
	p2.setSpatialBounds(c(0.0, LOP));

	//set migration to 1 individual per generation from each pop to another 

	//migrationRateIntoOcean = asFloat(NLAKES/IIF);
	//migrationRateIntoFresh = asFloat(1/IIO);
	
	p1.setMigrationRates(p2,MGR);
	p2.setMigrationRates(p1,MGR);
	
	//unifomily distribute oceanic individuals.

	for (ind in p1.individuals){
		ind.setSpatialPosition(p1.pointUniform());
	}
	
	lakeIndividuals = p2.individuals;

	//Put an even amount of individuals in each "lake" (location)

	for (i in 0:(NLAKES-1)){
		for (j in 0:(IPF-1)){
			lakeIndividuals[IPF*i + j].setSpatialPosition((i+1)-(0.5));						
		}
	}

}

//--INTRODUCE NEW FRESH POPULATION AS A COPY OF MARINE INDIVIDUALS--

s1 40000 late(){

	//before introduction, lets sample the ditribution of FWAA in Marine individuals.
	recordNumberOfFWAAPerMarineInd();

	//Lets also measure true-negatives and statistical power 
	//TrueNegative_StatPower();

	sim.addSubpopSplit("p3",IIF,p1);
	p3.setSpatialBounds(c(0.0, LOP));
	
	lakeIndividuals2 = p3.individuals;
	
	p1.setMigrationRates(p3,MGR);
	p3.setMigrationRates(p1,MGR);

	for (i in 0:(NLAKES-1)){
		for (j in 0:(IPF-1)){
			lakeIndividuals2[IPF*i + j].setSpatialPosition((i+1)-(0.5));						
		}
	}
	
	//Remeber the original, introduced individuals in the tree seqeunce
	sim.treeSeqRememberIndividuals(p3.individuals);

	//Output full so that we can dig into the simulation directly after introducing the new population
	sim.treeSeqOutput(PATH+"TreesAtIntroduction.trees");
}
	
1: late() {  


	
	//--CALCULATE PHENOTYPE--
		
	substitutionOffset = 2 * sum(sim.substitutions.selectionCoeff);

	for (individual in sim.subpopulations.individuals) //Calculate fitness (.tagF) 
	{
		dominantMarine = 2 * sum(individual.uniqueMutationsOfType(m1).selectionCoeff);
		dominantFreshwater = 2 * sum(individual.uniqueMutationsOfType(m2).selectionCoeff);

		aM = sum(individual.genomes.mutationsOfType(m4).selectionCoeff);
		aF = sum(individual.genomes.mutationsOfType(m5).selectionCoeff);
		dM = 2 * sum(individual.uniqueMutationsOfType(m4).selectionCoeff);
		dF = 2 * sum(individual.uniqueMutationsOfType(m5).selectionCoeff);

		recessiveMarine = (2 * aM) - dM; 
		recessiveFreshwater = (2 * aF) - dF;

		additiveMarine = sum(individual.genomes.mutationsOfType(m6).selectionCoeff);
		additiveFreshwater = sum(individual.genomes.mutationsOfType(m7).selectionCoeff);
	
		individual.tagF = dominantMarine+recessiveFreshwater+dominantFreshwater+recessiveMarine+additiveMarine+additiveFreshwater+substitutionOffset;		
	}

	//every generation, get reference to average phenotype.

	marine_mean_phenotype = mean(p1.individuals.tagF);
	freshwater_mean_phenotype = mean(p2.individuals.tagF);

	sim.setValue("FAP",freshwater_mean_phenotype);
	sim.setValue("MAP",marine_mean_phenotype);
	i1.evaluate(p1);

	if (sim.generation >= INP){
		introduced_mean_phenotype = mean(p3.individuals.tagF);
		sim.setValue("IAP",introduced_mean_phenotype);
		i2.evaluate(c(p2,p3));		
	}else{
		i2.evaluate(p2);
	}
	
	//--SAMPLE PHENOTYPE--
	
	if((sim.generation % INT == 0) | (sim.generation == END))
	{
		//TODO Number of mutations per individual. 
	
		//cat("-------------------------------\n");
		//cat("Output1 for generation " + sim.generation + ":\n\n");

		samplePhenotype();
	}

	//--CALCULATE FITNESS BASED UPON PHENOTYPE--
	
	for (individual in p2.individuals){
		individual.tagF = dnorm(-10.0 - individual.tagF,0.0,15.0);
	}
	for (individual in p1.individuals){
		individual.tagF = dnorm(10 - individual.tagF,0.0,15.0);
	}
	if (sim.generation >= INP){
		for (individual in p3.individuals){
			individual.tagF = dnorm(-10 - individual.tagF,0.0,15.0);
		}
	}

	//--NORMALIZE FITNESS IN LAKES SO PROPOTIONAL AMOUNT GET CHOSEN IN THE NEXT GEN--
 
	for (l in 1:NLAKES){
		locationOfLake = (l - 0.5);
		lake_individuals = p2.individuals[abs(p2.individuals.spatialPosition - locationOfLake) < 1e-8];
		sumOfPhenos = sum(lake_individuals.tagF);
		lake_individuals.tagF =(lake_individuals.tagF*IPF)/(sumOfPhenos);
	}
	
	if (sim.generation >= INP){
		for (l in 1:NLAKES){
			locationOfLake = (l - 0.5);
			lake_individuals2 = p3.individuals[abs(p3.individuals.spatialPosition - locationOfLake) < 1e-8];
			sumOfPhenos2 = sum(lake_individuals2.tagF);
			lake_individuals2.tagF =(lake_individuals2.tagF*IPF)/(sumOfPhenos2);
		}
	}

}


1: late(){		

	if((sim.generation % INT == 0) | (sim.generation == END))
	{
		//GET MUTATIONS THAT ONLY EXIST IN WANTED POPULATIONS FOR SAMPLING.

		m1s = sim.mutationsOfType(m1);			
		m2s = sim.mutationsOfType(m2);			
		m4s = sim.mutationsOfType(m4);			
		m5s = sim.mutationsOfType(m5);			
		m6s = sim.mutationsOfType(m6);			
		m7s = sim.mutationsOfType(m7);			
		
		effectMutations = c(m1s,m2s,m4s,m5s,m6s,m7s);

		//recordFrequencies(effectMutations);
		
		recordFreshAdaptedAlleles(effectMutations);
		
	}			
}

//--ANALYZE THE RECONSTRUCTION OF THE FRESHWATER GENOTYPE 

s2 40000: late(){

	//TODO RECORD P2 HAPLO AT TIME OF INTRODUCTION ... 


	//time of adaptation
	if (abs(sim.getValue("FAP") - sim.getValue("IAP")) <= 0.5 & !(sim.getValue("Adapted"))){

		sim.setValue("Adapted",T);
		AdaptationSampling();		
		sim.treeSeqOutput(PATH+"TreesAtAdaptation.trees");
		

		// TODO RECORD FREQUENCIES AND CORRELATIONS ....

	}	

	if((sim.generation % INT == 0) | (sim.generation == END)){
		
		m1s = sim.mutationsOfType(m1);			
		m2s = sim.mutationsOfType(m2);			
		m4s = sim.mutationsOfType(m4);			
		m5s = sim.mutationsOfType(m5);			
		m6s = sim.mutationsOfType(m6);			
		m7s = sim.mutationsOfType(m7);			

		effectMutations = c(m1s,m2s,m4s,m5s,m6s,m7s);

		MeanPreExistingFAA_PerIndividual(effectMutations);

		//recordFsts();

		recordStandingGeneticVariation(c(p1,p2,p3),effectMutations);	
	}
	
}

//--TO OVERRIDE SLIM'S DEFAULT DISTRIBUTION OF FITNESS EFFECTS--

fitness(m1) {return 1.0;}
fitness(m2) {return 1.0;}
fitness(m4) {return 1.0;}
fitness(m5) {return 1.0;}
fitness(m6) {return 1.0;}
fitness(m7) {return 1.0;}

//--FITNESS == 
//--WE DEFINE FITNESS TO BE DIRECTLY CORRELATED TO THIER PHENOTYPE--

fitness(NULL, p1) {
	return individual.tagF;
}
fitness(NULL, p2) {
	return individual.tagF;
}
s3 40000: fitness(NULL, p3){
	return individual.tagF;
}

//--CHOOSE MATES BASED UPON FITNESS & SPATIAL LOCATION--

1: mateChoice(p2) {
	return  i2.strength(individual) * weights; 
}
1: mateChoice(p1){
	return i1.strength(individual) * weights;
}
s4 40000: mateChoice(p3){
	return i2.strength(individual) * weights;
}

//--IF (THE OFFSPRING IS A MIGRANT FROM THE OCEAN) 
       	//FIND THE CLOSEST LAKE TO MIGRATE TO
//--ELSE 
	//MIGRATION FROM PARENT PULLED FROM NORMAL DISTRIBUTION
 
modifyChild(p1){

	if(sourceSubpop != subpop){  //mirant from the Ocean -> Lake 
		num = parent1.spatialPosition + rnorm(1,0,0.5);
		num = p2.pointReflected(num);
		nearestMult = round(num);
		if(num < nearestMult){
			child.setSpatialPosition(nearestMult - 0.5);
		}else{
			child.setSpatialPosition(nearestMult + 0.5);		
		}		
	}else{
		pos = parent1.spatialPosition + rnorm(1,0,0.5);
		child.setSpatialPosition(p1.pointReflected(pos));	
	}
	return T;
}

//--IF(THE OFFSPRING IS MIGRANT FROM THE LAKE)
	//MIGRATION FROM PARENT'S LAKE PULLED FROM NORMAL DIASTRIBUTION
//--ELSE
	//OFFSPRING IS PLACED IN EXACTLY THE SAME LAKE (POSITION) AS PARENT

modifyChild(p2){

	if(sourceSubpop != subpop){  //migrant from Lake -> Oceanic 
		pos = parent1.spatialPosition + rnorm(1,0,0.5);
		child.setSpatialPosition(p1.pointReflected(pos));	
	}else{
		child.setSpatialPosition(parent1.spatialPosition);
	}
	return T;
}

s5 40000: modifyChild(p3){

	if(sourceSubpop != subpop){  //migrant from Lake -> Oceanic 
		pos = parent1.spatialPosition + rnorm(1,0,0.5);
		child.setSpatialPosition(p1.pointReflected(pos));	
	}else{
		child.setSpatialPosition(parent1.spatialPosition);
	}
	return T;
}

s6 160001 late() {

	sim.treeSeqOutput(PATH+"EndTrees.trees");
	sim.simulationFinished();

	}



