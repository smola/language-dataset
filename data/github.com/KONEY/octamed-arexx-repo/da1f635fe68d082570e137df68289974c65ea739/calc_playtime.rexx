/* Calculate total playing time of a song.
Will detect tempo changing commands as long as they are on first line of block,
otherwise it would have been too slow. So calculated time is average.
Premature ends of blocks ad stop commands like FFE are not detected. 
As a matter of fact OctaMED allows you to set blocks length so premature end makes not sense.
Will work only on BPM tempos.
While processing PLAY is active so weird audio is supposed to be heard. This is normal :)
->	op_update off: Thanks to AlphA202303 for this command! */


SAY "Calculate total playing time of a song"
SAY "v1.4 - AreXX script by www.KONEY.org 2016"
SAY "--------------------------------------------------"

address OCTAMED_REXX
options results

/* Add libs. */
/*library = 'rexxmathlib.library'
IF ~SHOW( 'L', library ) THEN DO
	IF ~ADDLIB( library, 0, -30, 0 ) THEN DO
		SAY 'Failed to add library ' || library || '.'
		EXIT 10
	END
END*/
library = 'rexxsupport.library'
IF ~SHOW( 'L', library ) THEN DO
	IF ~ADDLIB( library, 0, -30, 0 ) THEN DO
		SAY 'Failed to add library ' || library || '.'
		EXIT 10
	END
END

SG_GETTEMPO var tempo
lines = 0
tpl = 0
lpb = 0
base_tpl=6
total_ms=0
delaytime_ticks = 7
beat_length_ms=(60/tempo)*1000

PL_PLAYBLOCK

	/* CYCLE SEQUENCE */
	ED_GETNUMPLAYSEQ var total_sequence_blocks
	SAY'Please wait while processing 'total_sequence_blocks' blocks in sequence @ 'tempo' BPM...'
	op_update off /* Thanks to AlphA202303 for this command! */
	DO seq_pos = 1 to total_sequence_blocks
		/* WE NEED SOME DELAY TO ALLOW TEMPO CHANGES COMMAND TO TAKE EFFECT*/
		call  delay delaytime_ticks
		/* CHECK FOR TEMPO SETTINGS CHANGES*/
		SG_GETTEMPOTPL  
		tpl = RESULT
		SG_GETTEMPOLPB 
		lpb = RESULT
		/*SAY '## TPL=' tpl*/
		'ED_GETPLAYSEQBLOCK'  seq_pos 'var cur_block'
		'ED_GOTO b' cur_block
		'ED_GOTO l 0'
		ED_GETNUMLINES
		lines = RESULT
		/*DO i = 0 to lines-1
		END  LINES*/
			total_ms = total_ms +  (beat_length_ms/(lpb/(tpl/base_tpl))) * lines
	END /* BLOCK SEQ*/
	op_update on

PL_STOP

/* Thanks to Thomas Rapp from EAB for time conversion code! */
seconds = (total_ms + 500) % 1000
mm = seconds % 60
ss = seconds // 60
time_output = mm":"right(ss,2,'0')

SAY 'TOTAL PLAY TIME' time_output

'wi_showstring TOTAL PLAY TIME' time_output