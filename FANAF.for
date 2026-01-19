C
	PROGRAM		FANAF
C
C	PROGRAM CALCULATES THE AIR FLOWS FOR LINE 5 PROCESS FANS
C	PROGRAM RUNS EVERY 10 SECONDS
C
	IMPLICIT NONE
C
C	LINK FANAF,WESAPI_LIB:WESAPI/OPT
C
	INCLUDE 'WESAPI:SPD.DCL'
	INCLUDE 'WESAPI:SHC_defines.DCL'
	INCLUDE 'WESAPI:SHC_err.DCL'
C
	INTEGER*4	LIB$SYS_TRNLOG
	INTEGER*4 	status
	CHARACTER*20	highway

	CHARACTER*40    spd_filename
	INTEGER*2	spd_filename_len
	INTEGER*2	spd_fd
	INTEGER*2	access_type
	INTEGER*2	c_short_val
	CHARACTER*2	field_name
	CHARACTER*8	pt_name
	BYTE		extended_flag
	BYTE		gp_bit_num
	BYTE		inactive_flag
	BYTE		c_byte_val
	INTEGER*4	gp_sid
	INTEGER*2       get_pkd_group_val
	INTEGER*2       get_pkd_group_stat
	INTEGER*2       gp_force_stat
	INTEGER*2       gp_op_mask
	INTEGER*2       gp_val_mask
	INTEGER*2	AS
	BYTE		gp_bitnum 
	INTEGER*2	digital_val
	INTEGER*2	digital_stat
	INTEGER*2	TIMEOUT
C
	REAL            put_analog_val
	INTEGER*2	PUT_DIGITAL_VAL
C
	INTEGER*4	LEN,POINT_QUALITY
	INTEGER*4	I,J,K,L,M,N
C
	CHARACTER*8	AIT(5)/'AI605011','AI606003','AI606009',
	1			  'AI608053','AI608053'/
	CHARACTER*8	AIPI(5)/'AI605054','AI605057','AI605060',
	1			   'AI612038','AI612040'/
	CHARACTER*8	AIPO(5)/'AI605055','AI605058','AI605061',
	1			   'AI612039','AI612041'/
	CHARACTER*8	AIA(5)/'AI605015','AI602017','AI602014',
	1			  'AI606019','AI606039'/
	CHARACTER*8	AOAF(5)/'AD600101','AD600102','AD600103',
	1			  'AD600104','AD600105'/
	CHARACTER*8	AOSF(5)/'AD600106','AD600107','AD600108',
	1			  'AD600109','AD600110'/
	CHARACTER*8	AOMF(5)/'AD600111','AD600112','AD600113',
	1			  'AD600114','AD600115'/
C
	INTEGER*4	SIDT(5),SIDPI(5),SIDPO(5),SIDA(5)
	INTEGER*4	SIDAF(5),SIDSF(5),SIDMF(5)
C
	REAL*4		TEMP,IPRES,OPRES,AMPS
	REAL*4		PRES_DROP,ACT_FLOW,STD_FLOW,
	1		MASS_FLOW,DENSITY,FAN_EFF,INTERM
C
C	OPEN CONNECTION TO AGGL 1-2 POINT DIRECTORY
C
        status = LIB$SYS_TRNLOG('WESAPI_PDIR_0',
     1			spd_filename_len,spd_filename,,,)
C
	spd_filename(spd_filename_len+1:) = CHAR(0)
C
        access_type = RUNTIME
C
        spd_fd = SPD_open_file(%ref(spd_filename), access_type)
C
	DO 30	J=1,5	!GET ALL FAN PROCESS TEMPERATURE SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(AIT(J)//char(0)),SIDT(J),
     x			gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
30	CONTINUE
C
	DO 32	J=1,5	!GET ALL FAN PROCESS IN PRESSURE SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(AIPI(J)//char(0)),SIDPI(J),
     x			gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
32	CONTINUE
C
	DO 34	J=1,5	!GET ALL FAN PROCESS OUT PRESSURE SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(AIPO(J)//char(0)),SIDPO(J),
     x			gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
34	CONTINUE
C
	DO 36	J=1,5	!GET ALL FAN PROCESS AMPS SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(AIA(J)//char(0)),SIDA(J),
     x			gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
36	CONTINUE
C
	DO 38	J=1,5	!GET ALL FAN PROCESS ACT FLOW OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(AOAF(J)//char(0)),SIDAF(J),
     x			gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
38	CONTINUE
C
	DO 40	J=1,5	!GET ALL FAN PROCESS STD FLOW OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(AOSF(J)//char(0)),SIDSF(J),
     x			gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
40	CONTINUE
C
	DO 42	J=1,5	!GET ALL FAN PROCESS MASS FLOW OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(AOMF(J)//char(0)),SIDMF(J),
     x			gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
42	CONTINUE
C
C  Close the point directory connection
C
	status = SPD_close_file(spd_fd)
C
C  Open the connection to SHC memory for WESAPI access
	status = SHC_open_memory()
C
	IF (status .NE. SHC_OK) THEN
		IF (status .EQ. SHC_E_MEMOPEN)GOTO 88
C
		OPEN	(UNIT=22,FILE='GP:FANAF.ERR',STATUS='UNKNOWN',
	1		 ACCESS='APPEND')
C
		WRITE	(22,77)status
77		FORMAT	(' SHC open memory failure - error =',I4)
C
		CLOSE	(UNIT=22)
C
	   GO TO 99999
C
	ENDIF
C
88	CONTINUE
C
100	CONTINUE
C
	DO 300	I=1,5	!PROCESS EACH PROCESS FAN
C
		status = SHC_get_analog_val_stat( SIDT(I), TEMP, AS)
		status = SHC_get_analog_val_stat( SIDPI(I), IPRES, AS)
		status = SHC_get_analog_val_stat( SIDPO(I), OPRES, AS)
		status = SHC_get_analog_val_stat( SIDA(I), AMPS, AS)
C
		PRES_DROP=OPRES-IPRES
		INTERM=((TEMP-32)*0.5556)+273.15
		INTERM=ALOG10(INTERM)
		INTERM=2.5572-1.00336*INTERM
		INTERM=(10**INTERM)
		DENSITY=INTERM*0.06243
		FAN_EFF=1
		IF (PRES_DROP .EQ. 0)PRES_DROP=0.00001
		ACT_FLOW=(((4160*FAN_EFF*AMPS)/0.118)/PRES_DROP)/1000
		STD_FLOW=(ACT_FLOW*520)/(460+TEMP)
		MASS_FLOW=ACT_FLOW*DENSITY
C
		put_analog_val=ACT_FLOW	!ACTUAL AIR FLOW THIS FAN
		status = SHC_put_analog_val(SIDAF(I), put_analog_val)
		status = SHC_put_point_quality(SIDAF(I), point_quality)
C
		put_analog_val=STD_FLOW	!STANDARD AIR FLOW THIS FAN
		status = SHC_put_analog_val(SIDSF(I), put_analog_val)
		status = SHC_put_point_quality(SIDSF(I), point_quality)
C
		put_analog_val=MASS_FLOW !MASS AIR FLOW THIS FAN
		status = SHC_put_analog_val(SIDMF(I), put_analog_val)
		status = SHC_put_point_quality(SIDMF(I), point_quality)
C
300	CONTINUE
C
	CALL	LIB$WAIT(10.0)
	GOTO 100
C
99999	CONTINUE
C
	CALL	EXIT
C
	END
