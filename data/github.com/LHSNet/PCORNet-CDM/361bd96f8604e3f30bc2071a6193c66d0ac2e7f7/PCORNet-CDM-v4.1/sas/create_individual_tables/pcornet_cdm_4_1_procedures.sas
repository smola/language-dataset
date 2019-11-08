/*--------------------------------------------------------------------------------------\

CDM 4.1 PROCEDURES TABLE

\--------------------------------------------------------------------------------------*/

proc sql noprint;

create table cdm_out.PROCEDURES (compress=yes encrypt=aes encryptkey=&aeskey) as

SELECT PROCEDURESID ,
       PATID ,
       ENCOUNTERID ,
       ENC_TYPE ,
       INPUT( PUT( ADMIT_DATE , e8601da.), e8601da.) AS ADMIT_DATE format date9.,
       PROVIDERID ,
       INPUT( PUT (PX_DATE , e8601da.), e8601da.) AS PX_DATE format date9.,
       PX ,
       PX_TYPE ,
       PX_SOURCE ,
	   PPX ,
       RAW_PX ,
       RAW_PX_TYPE ,
	   RAW_PPX
FROM cdm_in.PROCEDURES
;

