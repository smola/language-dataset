#pragma rtGlobals=3		// Use modern global access method and strict wave access.
#pragma IgorVersion = 6.3 // Minimum Igor version required
#pragma version = 0.2-alpha

// Copyright (c) 2018 Michael C. Heiber
// This source file is part of the Excimontec_Analysis project, which is subject to the MIT License.
// For more information, see the LICENSE file that accompanies this software.
// The Excimontec_Analysis project can be found on Github at https://github.com/MikeHeiber/Excimontec_Analysis

Function/S EMT_ChooseJob(test_type)
	String test_type
	String original_folder = GetDataFolder(1)
	// Build the sample list
	String job_id
	SetDataFolder root:Excimontec:$test_type
	String job_list = ""
	DFREF dfr1 = GetDataFolderDFR()
	Variable N_jobs = CountObjectsDFR(dfr1,4)
	String folder_name
	Variable i
	for(i=0;i<N_jobs;i+=1)
		folder_name = GetIndexedObjNameDFR(dfr1,4,i)
		job_list = AddListItem(folder_name,job_list)
	endfor
	// Prompt user to choose the sample
	Prompt job_id, "Choose the job id:", popup, job_list
	DoPrompt "Make Selections",job_id
	// User cancelled operation
	if(V_flag!=0)
		SetDataFolder original_folder
		return ""
	endif
	SetDataFolder original_folder
	return job_id
End

Function/S EMT_ChooseVariant(test_type,job_id)
	String test_type
	String job_id
	String original_folder = GetDataFolder(1)
	// Build the variant list
	SetDataFolder root:Excimontec:$(test_type)
	Wave/T jobs = $("job_id")
	Wave N_variants
	Variable index
	FindValue /TEXT=(job_id) /TXOP=2 jobs
	if(V_value<0)
		SetDataFolder original_folder
		Print "Error: Job data not found."
		return ""
	else
		index = V_value
	endif
	String variant_list = ""
	Variable i
	for(i=0;i<N_variants[index];i++)
		variant_list = AddListItem(num2str(i),variant_list)
	endfor
	// Prompt user to choose the sample
	String variant_id
	Prompt variant_id, "Choose the variant:", popup, variant_list
	DoPrompt "Make Selections",variant_id
	// User cancelled operation
	if(V_flag!=0)
		SetDataFolder original_folder
		return ""
	endif
	SetDataFolder original_folder
	return variant_id
End

Function EMT_ImportData()
	String original_folder = GetDataFolder(1)
	NewDataFolder/O/S root:Excimontec
	// Open new job folder
	NewPath/O/Q folder_path
	if(V_flag!=0)
		return NaN
	endif
	PathInfo folder_path
	String job_name = StringFromList(ItemsInList(S_path,":")-1,S_path,":")
	// Load analysis summary file to extract version number
	LoadWave/J/A=analysisWave/P=folder_path/K=0/Q "analysis_summary.txt"
	Wave/T analysisWave0
	String version_used = StringFromList(1,StringFromList(0,analysisWave0[0]," Results"),"tec ")
	Variable major_version_num = str2num(StringFromList(0,StringFromList(1,version_used,"v"),"."))
	Variable minor_version_num = str2num(StringFromList(1,StringFromList(1,version_used,"v"),"."))
	Variable version_num = major_version_num+0.1*minor_version_num
	// Load parameter file
	String file_list = IndexedFile(folder_path,-1,".txt")
	String parameter_filename
	if(ItemsInList(file_list))
		parameter_filename = StringFromList(0,ListMatch(file_list,"parameters*"))
	else
		Print "Error! Parameter file not found!"
		return NaN
	endif
	LoadWave/A=parameterWave/J/Q/K=2/V={""," $",0,0}/P=folder_path parameter_filename
	Wave/T parameterWave0
	// Check for ToF test
	Variable TOF_Test = 0
	if(StringMatch(StringFromList(0,parameterWave0[36]," //"),"true"))
		NewDataFolder/O/S $("Time of Flight Tests")
		TOF_Test = 1
	// Check for IQE test
	Variable IQE_Test = 0
	elseif(StringMatch(StringFromList(0,parameterWave0[42]," //"),"true"))
		NewDataFolder/O/S $("IQE Tests")
		IQE_Test = 1
	// Check for Dynamics test
	Variable Dynamics_Test = 0
	elseif(StringMatch(StringFromList(0,parameterWave0[45]," //"),"true"))
		NewDataFolder/O/S $("Dynamics Tests")
		Dynamics_Test = 1
	endif
	// Open Data Waves
	Wave/T/Z version
	if(!WaveExists(version))
		Make/N=1/T $"version"
		Wave/T version
	endif
	Wave/Z set_num
	if(!WaveExists(set_num))
		Make/N=1 $"set_num"
		Wave set_num
	endif
	Wave/T/Z job_id
	if(!WaveExists(job_id))
		Make/N=1/T $"job_id"
		Wave/T job_id
	endif
	Wave/T/Z morphology
	if(!WaveExists(morphology))
		Make/N=1/T $"morphology"
		Wave/T morphology
	endif
	Wave/T/Z kmc_algorithm
	if(!WaveExists(kmc_algorithm))
		Make/N=1/T $"kmc_algorithm"
		Wave/T kmc_algorithm
	endif
	Wave/Z lattice_length
	if(!WaveExists(lattice_length))
		Make/N=1/D $"lattice_length"
		Wave lattice_length
	endif
	Wave/Z lattice_width
	if(!WaveExists(lattice_width))
		Make/N=1/D $"lattice_width"
		Wave lattice_width
	endif
	Wave/Z lattice_height
	if(!WaveExists(lattice_height))
		Make/N=1/D $"lattice_height"
		Wave lattice_height
	endif
	Wave/Z unit_size_nm
	if(!WaveExists(unit_size_nm))
		Make/N=1/D $"unit_size_nm"
		Wave unit_size_nm
	endif
	Wave/Z temperature_K
	if(!WaveExists(temperature_K))
		Make/N=1/D $"temperature_K"
		Wave temperature_K
	endif
	Wave/Z internal_potential_V
	if(!WaveExists(internal_potential_V))
		Make/N=1/D $"internal_potential_V"
		Wave internal_potential_V
	endif
	Wave/Z N_tests
	if(!WaveExists(N_tests))
		Make/N=1/D $"N_tests"
		Wave N_tests
	endif
	Wave/Z N_variants
	if(!WaveExists(N_variants))
		Make/N=1/D $"N_variants"
		Wave N_variants
	endif
	Wave/T/Z disorder_model
	if(!WaveExists(disorder_model))
		Make/N=1/T $"disorder_model"
		Wave/T disorder_model
	endif
	Wave/T/Z correlation_model
	if(!WaveExists(correlation_model))
		Make/N=1/T $"correlation_model"
		Wave/T correlation_model
	endif
	Wave/Z correlation_length_nm
	if(!WaveExists(correlation_length_nm))
		Make/N=1 $"correlation_length_nm"
		Wave correlation_length_nm
	endif
	Wave/Z calc_time_min
	if(!WaveExists(calc_time_min))
		Make/N=1/D $"calc_time_min"
		Wave calc_time_min
	endif
	NewDataFolder/O/S $job_name
	Duplicate/O/T parameterWave0 Parameters
	Duplicate/O/T analysisWave0 AnalysisSummary
	KillWaves parameterWave0 analysisWave0
	Variable index
	FindValue /TEXT=(job_name) /TXOP=2 job_id
	index = V_value
	if(index==-1)
		if(StringMatch(job_id[0],""))
			index = 0
		else
			index = numpnts(job_id)
		endif	
	endif
	set_num[index] = {0}
	version[index] = {version_used}
	job_id[index] = {job_name}
	// Record morphology used
	// Neat
	if(StringMatch(StringFromList(0,Parameters[20]," //"),"true"))
		morphology[index] = {"Neat"}
	// Bilayer	
	elseif(StringMatch(StringFromList(0,Parameters[21]," //"),"true"))
		morphology[index] = {"Bilayer - "+StringFromList(0,Parameters[22]," //")+"/"+StringFromList(0,Parameters[23]," //")}
	// Random blend
	elseif(StringMatch(StringFromList(0,Parameters[24]," //"),"true"))
		morphology[index] = {"Random blend - "+StringFromList(0,Parameters[25]," //")}
	// Imported morphology
	elseif(StringMatch(StringFromList(0,Parameters[26]," //"),"true") || StringMatch(StringFromList(0,Parameters[28]," //"),"true"))
		file_list = IndexedFile(folder_path,-1,".txt")
		if(ItemsInList(file_list))
			morphology[index] = {StringFromList(1,ListMatch(file_list,"morphology_*"),"_")}
		else
			morphology[index] = {""}
		endif
	endif
	// Record KMC event recalculation method used
	if(StringMatch(StringFromList(0,Parameters[3]," //"),"true"))
		kmc_algorithm[index] = {"FRM"}
	elseif(StringMatch(StringFromList(0,Parameters[4]," //"),"true"))
		kmc_algorithm[index] = {"selective"}
	elseif(StringMatch(StringFromList(0,Parameters[6]," //"),"true"))
		kmc_algorithm[index] = {"full"}
	endif
	// Record disorder model used
	// Gaussian
	if(StringMatch(StringFromList(0,Parameters[105]," //"),"true"))
		// Correlated
		if(StringMatch(StringFromList(0,Parameters[111]," //"),"true"))
			disorder_model[index] = {"Gaussian-correlated"}
			correlation_length_nm[index] = {str2num(StringFromList(0,Parameters[112]," //"))}
			// Gaussian kernel
			if(StringMatch(StringFromList(0,Parameters[113]," //"),"true"))
				correlation_model[index] = {"Gaussian kernel"}
			// Power kernel
			else
				String power_kernel_exponent = StringFromList(0,Parameters[114]," //")
				correlation_model[index] = {"Power kernel, "+power_kernel_exponent}
			endif
		// Uncorrelated
		else
			disorder_model[index] = {"Gaussian-uncorrelated"}
			correlation_model[index] = {"none"}
			correlation_length_nm[index] = {0}
		endif
	// Exponential
	elseif(StringMatch(StringFromList(0,Parameters[108]," //"),"true"))
		Disorder_model[index] = {"Exponential-uncorrelated"}
		Correlation_model[index] = {"none"}
	// None
	else
		Disorder_model[index] = {"None"}
		Correlation_model[index] = {"none"}
	endif
	lattice_length[index] = {str2num(StringFromList(0,Parameters[12]," //"))}
	lattice_width[index] = {str2num(StringFromList(0,Parameters[13]," //"))}
	lattice_height[index] = {str2num(StringFromList(0,Parameters[14]," //"))}
	unit_size_nm[index] = {str2num(StringFromList(0,Parameters[15]," //"))}
	temperature_K[index] = {str2num(StringFromList(0,Parameters[16]," //"))}
	internal_potential_V[index] = {str2num(StringFromList(0,Parameters[17]," //"))}
	N_tests[index] = {str2num(StringFromList(0,Parameters[34]," //"))}
	N_variants[index] = {str2num(StringFromList(4,analysisSummary[1]," "))}
	calc_time_min[index] = {str2num(StringFromList(4,analysisSummary[2]," "))}
	// Perform TOF Test Specific Operations
	if(TOF_Test)
		SetDataFolder root:Excimontec:$("Time of Flight Tests")
		// Open TOF Data Waves
		Wave/Z disorder_eV
		if(!WaveExists(disorder_eV))
			Make/N=1/D $"disorder_eV"
			Wave disorder_eV
		endif
		Wave/Z N_carriers
		if(!WaveExists(N_carriers))
			Make/N=1/D $"N_carriers"
			Wave N_carriers
		endif
		Wave/Z mobility_avg
		if(!WaveExists(mobility_avg))
			Make/N=1/D $"mobility_avg"
			Wave mobility_avg
		endif
		Wave/Z mobility_stdev
		if(!WaveExists(mobility_stdev))
			Make/N=1/D $"mobility_stdev"
			Wave mobility_stdev
		endif
		Wave/Z localization_nm
		if(!WaveExists(localization_nm))
			Make/N=1/D $"localization_nm"
			Wave localization_nm
		endif
		// Load TOF Files
		SetDataFolder $job_name
		LoadWave/J/D/W/N/O/K=0/P=folder_path/Q "ToF_average_transients.txt"
		LoadWave/J/D/W/N/O/K=0/P=folder_path/Q "ToF_transit_time_dist.txt"
		LoadWave/J/Q/A=resultsWave/P=folder_path/K=2/V={""," $",0,0} "ToF_results.txt"
		// Load charge extraction map data
		if(StringMatch(StringFromList(0,Parameters[44]," //"),"true"))	
			NewDataFolder/O/S :$"Extraction Map Data"
			Variable i
			Variable j
			for(i=0;i<N_variants[index];i++)
				LoadWave/J/D/W/N/O/K=0/P=folder_path/Q "Charge_extraction_map"+num2str(i)+".txt"
				Wave x_vals = $"X_Position"
				Wave y_vals = $"Y_Position"
				Wave prob = $"Extraction_Probability"
				WaveStats/Q x_vals
				Variable X_max = V_max+1
				WaveStats/Q y_vals
				Variable Y_max = V_max+1
				Make/O/D/N=(X_max,Y_max) $("charge_extraction_prob"+num2str(i))
				Wave extraction_prob = $("charge_extraction_prob"+num2str(i))
				for(j=0;j<numpnts(x_vals);j++)
					extraction_prob[x_vals[j]][y_vals[j]] = prob[j]
				endfor
				// Cleanup
				KillWaves x_vals y_vals prob
			endfor
			SetDataFolder ::
		endif
		Wave/T resultsWave0
		// Determine relevant disorder model used
		// Gaussian
		if(StringMatch(StringFromList(0,Parameters[105]," //"),"true"))
			// Electron transport
			if(StringMatch(StringFromList(0,Parameters[37]," //"),"electron"))
				disorder_eV[index] = {str2num(StringFromList(0,parameters[107]," //"))}
			// Hole transport
			else
				disorder_eV[index] = {str2num(StringFromList(0,parameters[106]," //"))}
			endif
		// Exponential
		elseif(StringMatch(StringFromList(0,Parameters[108]," //"),"true"))
			// Electron transport
			if(StringMatch(StringFromList(0,parameters[37]," //"),"electron"))
				disorder_eV[index] = {str2num(StringFromList(0,parameters[110]," //"))}
			// Hole transport
			else
				disorder_eV[index] = {str2num(StringFromList(0,parameters[109]," //"))}
			endif
		endif
		// Determine relevant localization
		// Electron transport
		if(StringMatch(StringFromList(0,parameters[37]," //"),"electron"))
			localization_nm[index] = {1/str2num(StringFromList(0,parameters[90]," //"))}
		// Hole transport
		else
			localization_nm[index] = {1/str2num(StringFromList(0,parameters[89]," //"))}
		endif
		N_carriers[index] = {str2num(StringFromList(0,Parameters[38]," //"))}
		mobility_avg[index] = {str2num(StringFromList(3,resultsWave0[1],","))}
		mobility_stdev[index] = {str2num(StringFromList(4,resultsWave0[1],","))}
		
		// Clean Up
		KillWaves resultsWave0
		// Update Analysis
		SetDataFolder ::
		Duplicate/O mobility_avg field field_sqrt disorder_norm
		field = abs(internal_potential_V)/(1e-7*lattice_height*unit_size_nm)
		field_sqrt = sqrt(abs(internal_potential_V)/(1e-7*lattice_height*unit_size_nm))
		// Calculate effective disorder for Gaussian DOS
		if(StringMatch(StringFromList(0,Parameters[105]," //"),"true"))
			disorder_norm = disorder_eV/(8.617e-5*temperature_K)
		endif
	elseif(IQE_Test)
		SetDataFolder root:Excimontec:$("IQE Tests")
		// Open IQE Data Waves
		Wave/Z disorder_D_eV
		if(!WaveExists(disorder_D_eV))
			Make/N=1/D $"disorder_D_eV"
			Wave disorder_D_eV
		endif
		Wave/Z disorder_A_eV
		if(!WaveExists(disorder_A_eV))
			Make/N=1/D $"disorder_A_eV"
			Wave disorder_A_eV
		endif
		Wave/Z polaron_delocalization_nm
		if(!WaveExists(polaron_delocalization_nm))
			Make/N=1/D $"polaron_delocalization_nm"
			Wave polaron_delocalization_nm
		endif
		Wave/Z IQE
		if(!WaveExists(IQE))
			Make/N=1/D $"IQE"
			Wave IQE
		endif
		Wave/Z dissociation_yield
		if(!WaveExists(dissociation_yield))
			Make/N=1/D $"dissociation_yield"
			Wave dissociation_yield
		endif
		Wave/Z separation_yield
		if(!WaveExists(separation_yield))
			Make/N=1/D $"separation_yield"
			Wave separation_yield
		endif
		Wave/Z extraction_yield
		if(!WaveExists(extraction_yield))
			Make/N=1/D $"extraction_yield"
			Wave extraction_yield
		endif
		// Load charge extraction map data
		if(StringMatch(StringFromList(0,parameters[44]," //"),"true"))
			NewDataFolder/O/S :$(job_name):$"Extraction Map Data"
			for(i=0;i<N_variants[index];i++)
				// Electrons
				LoadWave/J/D/W/N/O/K=0/P=folder_path/Q "Electron_extraction_map"+num2str(i)+".txt"
				Wave x_vals = $"X_Position"
				Wave y_vals = $"Y_Position"
				Wave prob = $"Extraction_Probability"
				WaveStats/Q x_vals
				X_max = V_max+1
				WaveStats/Q y_vals
				Y_max = V_max+1
				Make/O/D/N=(X_max,Y_max) $("electron_extraction_prob"+num2str(i))
				Wave extraction_prob = $("electron_extraction_prob"+num2str(i))
				for(j=0;j<numpnts(x_vals);j++)
					extraction_prob[x_vals[j]][y_vals[j]] = prob[j]
				endfor
				// Holes
				LoadWave/J/D/W/N/O/K=0/P=folder_path/Q "Hole_extraction_map"+num2str(i)+".txt"
				Make/O/D/N=(X_max,Y_max) $("hole_extraction_prob"+num2str(i))
				Wave extraction_prob = $("hole_extraction_prob"+num2str(i))
				for(j=0;j<numpnts(x_vals);j++)
					extraction_prob[x_vals[j]][y_vals[j]] = prob[j]
				endfor
				// Cleanup
				KillWaves x_vals y_vals prob
			endfor
			SetDataFolder ::
		endif
		// Record disorder info
		// Gaussian
		if(StringMatch(StringFromList(0,Parameters[105]," //"),"true"))
			disorder_D_eV[index] = {str2num(StringFromList(0,parameters[106]," //"))}
			disorder_A_eV[index] = {str2num(StringFromList(0,parameters[107]," //"))}
		// Exponential
		elseif(StringMatch(StringFromList(0,Parameters[108]," //"),"true"))
			disorder_D_eV[index] = {str2num(StringFromList(0,parameters[109]," //"))}
			disorder_A_eV[index] = {str2num(StringFromList(0,parameters[110]," //"))}
		endif
		if(StringMatch(StringFromList(0,Parameters[97]," //"),"true"))
			polaron_delocalization_nm[index] = {str2num(StringFromList(0,Parameters[98]," //"))}
		else
			polaron_delocalization_nm[index] = {0}
		endif
		dissociation_yield[index] = {str2num(StringFromList(0,analysisSummary[7],"%"))}
		separation_yield[index] = {100-str2num(StringFromList(0,analysisSummary[15],"%"))}
		extraction_yield[index] = {100-str2num(StringFromList(0,analysisSummary[16],"%"))}
		IQE[index] = {str2num(StringFromList(1,StringFromList(0,analysisSummary[18],"%"),"= "))}
	elseif (Dynamics_test)
		SetDataFolder root:Excimontec:$("Dynamics Tests")
		// Open Dynamics Data Waves
		Wave/T/Z z_periodic
		if(!WaveExists(z_periodic))
			Make/N=1/T $"z_periodic"
			Wave/T z_periodic
		endif
		Wave/Z disorder_D_eV
		if(!WaveExists(disorder_D_eV))
			Make/N=1/D $"disorder_D_eV"
			Wave disorder_D_eV
		endif
		Wave/Z disorder_A_eV
		if(!WaveExists(disorder_A_eV))
			Make/N=1/D $"disorder_A_eV"
			Wave disorder_A_eV
		endif
		Wave/Z polaron_delocalization_nm
		if(!WaveExists(polaron_delocalization_nm))
			Make/N=1/D $"polaron_delocalization_nm"
			Wave polaron_delocalization_nm
		endif
		Wave/Z R_recombination
		if(!WaveExists(R_recombination))
			Make/N=1/D $"R_recombination"
			Wave R_recombination
		endif
		Wave/Z R_electron_hop
		if(!WaveExists(R_electron_hop))
			Make/N=1/D $"R_electron_hop"
			Wave R_electron_hop
		endif
		Wave/Z R_hole_hop
		if(!WaveExists(R_hole_hop))
			Make/N=1/D $"R_hole_hop"
			Wave R_hole_hop
		endif
		Wave/Z R_singlet_hop_D
		if(!WaveExists(R_singlet_hop_D))
			Make/N=1/D $"R_singlet_hop_D"
			Wave R_singlet_hop_D
		endif
		Wave/Z R_singlet_hop_A
		if(!WaveExists(R_singlet_hop_A))
			Make/N=1/D $"R_singlet_hop_A"
			Wave R_singlet_hop_A
		endif
		// Record boundary condition
		if(StringMatch(StringFromList(0,Parameters[11]," //"),"true"))
			z_periodic[index] = {"Yes"}
		else
			z_periodic[index] = {"No"}
		endif
		// Record disorder info
		// Gaussian
		if(StringMatch(StringFromList(0,Parameters[105]," //"),"true"))
			disorder_D_eV[index] = {str2num(StringFromList(0,Parameters[106]," //"))}
			disorder_A_eV[index] = {str2num(StringFromList(0,Parameters[107]," //"))}
		// Exponential
		elseif(StringMatch(StringFromList(0,Parameters[108]," //"),"true"))
			disorder_D_eV[index] = {str2num(StringFromList(0,Parameters[109]," //"))}
			disorder_A_eV[index] = {str2num(StringFromList(0,Parameters[110]," //"))}
		endif
		if(StringMatch(StringFromList(0,Parameters[97]," //"),"true"))
			polaron_delocalization_nm[index] = {str2num(StringFromList(0,Parameters[98]," //"))}
		else
			polaron_delocalization_nm[index] = {0}
		endif
		R_recombination[index] = {str2num(StringFromList(0,Parameters[95]," //"))}
		R_singlet_hop_D[index] = {str2num(StringFromList(0,Parameters[59]," //"))}
		R_singlet_hop_A[index] = {str2num(StringFromList(0,Parameters[60]," //"))}
		R_electron_hop[index] = {str2num(StringFromList(0,Parameters[88]," //"))}
		R_hole_hop[index] = {str2num(StringFromList(0,Parameters[87]," //"))}
		// Import transient data
		SetDataFolder $job_name
		LoadWave/J/D/W/N/O/K=0/P=folder_path/Q "dynamics_average_transients.txt"
		Wave exciton_msdv = $"Exciton_MSDV__cm_2_s__1_"
		Wave electron_msdv = $"Electron_MSDV__cm_2_s__1_"
		Wave hole_msdv = $"Hole_MSDV__cm_2_s__1_"
		Duplicate/O exciton_msdv Exciton_Diffusion_Coef Electron_Mobility Hole_Mobility
		Exciton_Diffusion_Coef = exciton_msdv/6
		Electron_Mobility = electron_msdv/(6*8.61733e-5*temperature_K[index])
		Hole_Mobility = hole_msdv/(6*8.61733e-5*temperature_K[index])
	endif
	SetDataFolder original_folder
End

Function EMT_DifferentiateLog(wave_y,wave_x,)
	// Numerically calculates dy/dx given x values are distributed on a log scale
	Wave wave_y
	Wave wave_x
	String outputName = NameOfWave(wave_y)+"_DIF"
	Duplicate/O wave_y $outputName
	Wave wave_y_DIF = $outputName
	WaveStats/Q wave_y
	Variable length = V_npnts
	Variable x
	for(x=0;x<length;x+=1)
		if(x<7)
			wave_y_DIF[x] = (wave_y[x+3]-wave_y[x])/(wave_x[x+3]-wave_x[x])
		elseif(x<(length-3))
			CurveFit/N/Q/W=2/NTHR=0/TBOX=0 line wave_y[x-7,x+3] /X=wave_x[x-7,x+3]
			Wave coefs = $"W_coef"
			wave_y_DIF[x] = coefs[1]
		else
			CurveFit/N/Q/W=2/NTHR=0/TBOX=0 line wave_y[x-7,length-1] /X=wave_x[x-7,length-1]
			Wave coefs = $"W_coef"
			wave_y_DIF[x] = coefs[1]
		endif
	endfor
	KillWaves $"W_coef" $"W_sigma"
End