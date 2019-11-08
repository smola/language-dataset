MODULE Test;

FROM HPFFile IMPORT
	HPF_ARR_CHANNEL_SENSORS, HPF_ARR_VALUES, HPF_FILE,
	HPF_CHANNEL, HPF_SENSOR, HPF_SET_SENSORS,
	HPFOpenAndInit, HPFCloseFile, HPFReadAtTime;

FROM SYSTEM IMPORT
	LOCSPERBYTE;

FROM Printf IMPORT printf;

FROM SWholeIO IMPORT WriteCard;
CONST
	times : ARRAY OF LONGREAL =
		{9.916201,
		7.2225,
		0.000000,
		0.0009,
		0.0018,
		0.0027,
		0.0036,
		0.0045,
		0.0054,
		0.0063,
		0.0072,
		0.0081,
		0.009000001};

VAR
	monHpf : HPF_FILE;
	sensors : HPF_ARR_CHANNEL_SENSORS;
	channels : HPF_CHANNEL;
	res : HPF_ARR_VALUES;
	i : CARDINAL;
BEGIN
	IF NOT HPFOpenAndInit("Run_number_1_Plot_and_Store_Rep_2.1.hpf", monHpf) THEN
		HALT
	END;

	FOR channels := MIN(HPF_CHANNEL) TO MAX(HPF_CHANNEL) DO
		sensors[channels] := HPF_SET_SENSORS{emg, acc_x, acc_y, acc_z, gyro_x, gyro_y, gyro_z, mag_x, mag_y, mag_z};
	END;

	FOR i := 0 TO HIGH(times) DO
		res := HPFReadAtTime(monHpf, times[i], sensors);
		IF res[0, emg] < -10. THEN
		END;
	END;

	HPFCloseFile(monHpf)
END Test.
