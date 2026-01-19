CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	CONFIDENTIAL
C	Property of United States Steel Corporation
C	Copyright 2007 United States Steel Corporation
C	All rights reserved.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	PROGRAM MBTUS
C
C	PROGRAM RUNS EVERY 30 SECS AND CALCULATES MBTUS 
C	PER TON BY LINE FOR OUTPUT TO WESTINGHOUSE FRONT END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       REVISION HISTORY
C
C       CHANGED THE BTU FACTOR FOR WOOD FROM 8.35 TO 7.608 PER JIM JARVI
C       SERVICE CENTER TICKET IM509796  BILL BABICH
C
C	WOOD MBTUS FACTOR CHANGED PER CORY BIRD 05-11-2009
C	FROM 7.608 TO 8.225 IM535433
C
C	WOOD FLOWS AND SETPOINTS PER JIM JARVI 10-08-2009
C	FROM 7.608 TO 8.225 IM598651
C
C	ADDED CROSS SHIPPING OF WOOD SYSTEM INFORMATION BETWEEN STEP12
C	AGGLOMERATOR AND STEP 3 AGGLOMERATOR (FLOWS, PRESSURES, SETPOINTS)
C	BY LINES (3,4,5,6,7) JIM JARVI 10-22-2009 IM603802 WJ BABICH
C
C	ADDED SHIPPING GAS CONSUMPTION PREVIOUS DAY AND PREVIOUS HOUR TO
C	AGGL12 FOR DIAGRAM 2722 AND TO AGGL3 FOR DIAGRAM 2040 FROM 
C	MAINGAS.DAT FILE PER CORY BIRD 01-05-2010 IM628602 WJ BABICH
C
C	ADDED SHIPPING 315-01-4 AND 315-03-4 FLUX PUMP 6 INCH LINE
C	DENSITY TO CONCENTRATOR DCS TO BE DISPLAYED ON CONCENTRATOR  
C	DIAGRAM 2106 PER SCOTT VAGLE 01-06-2010 IM629304 WJ BABICH
C
C	ADDED SHIPPING S009X105, O009X102, S009X101, AND AL809010
C	TO AGGL12 DCS TO BE DISPLAYED ON AGGL12  
C	DIAGRAM 2656 PER APRIL WRIGHT 06-08-2010 IM688705 WJ BABICH
C
C	FORT/NOOP/NOALIGN/WARN=NOALIGN/FLOAT=D_FLOAT MBTUS
C
C	LINK MBTUS,MBTUS/OPT,WESAPI_LIB:WESAPI/OPT
C
	STRUCTURE	/VALUES/
		CHARACTER*8	KEY
		INTEGER*4	HOUR
		REAL*4		SIL(12),AL(12),CA(12),MG(12)
		INTEGER*4	TIME(2,12)
	END STRUCTURE
C
	RECORD/VALUES/DAT
C
	STRUCTURE	/TS/
C
		CHARACTER*23	DATX	!DATE AND TIME
		REAL*4		R10
		REAL*4		LP	
		REAL*4		FC
		REAL*4		FCT
		REAL*4		ACOMP
		REAL*4		T
		REAL*4		TF
		REAL*4		LINES
		REAL*4		APTON
		REAL*4		BENT
C
	END STRUCTURE
C
	RECORD/TS/TSX
C
	INCLUDE 'WESAPI:SPD.DCL'
	INCLUDE 'WESAPI:SHC_defines.DCL'
	INCLUDE 'WESAPI:SHC_err.DCL'
C
	INTEGER*4	UFO_OPEN
	INTEGER*4	LIB$SYS_TRNLOG
	INTEGER*4 	status
C
	CHARACTER*20	highway

	CHARACTER*40    spd_filename
	CHARACTER*80	c_ascii_val
	INTEGER*2	spd_filename_len
	INTEGER*2	spd_fd
	INTEGER*2	access_type
	INTEGER*2	c_short_val
	INTEGER*2	length
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
C
	REAL            put_analog_val
C
	INTEGER*4	LEN,POINT_QUALITY
C
	REAL*4		VALU(3)
	REAL*4		ANVAL(1200),CNC_ANVAL(1900)
	REAL*4		LINE4(6),LINE5(6),LINE3(6),ACCUM,OBS
	REAL*4		ANVALX(1000),TONS_S(5),TONS_D(5)
	REAL*4		TA,TS,TD,FE(5),FCAST(5)
	REAL*4		ACTH(12),LASTH(12),ACTD(12),LASTD(12)
C
	INTEGER*4	I,J,K,L,M,N
	INTEGER*4	ADRS(2)
	INTEGER*4	GAS(3)/1051,1052,1070/
	INTEGER*4	WOOD(3)/1047,1048,1071/
	INTEGER*4	OIL(3)/537,538,853/
	INTEGER*4	TONS(5)/339,340,867,564,565/
	INTEGER*4	CNCAO(14),AO(37),AG3AO(34)
	INTEGER*4	GPT(50),HFLAG,SFLAG,DFLAG
	INTEGER*4	HRS,MINS,TIMEX
C
	CHARACTER*23	DATETIME
	CHARACTER*15	FILEA/'LOGS:ACTIVE.DAT'/
	CHARACTER*14	FILES/'LOGS:SHIFT.DAT'/
	CHARACTER*12	FILED/'LOGS:DAY.DAT'/
	CHARACTER*19	FILEA3/'LOGS:AG3_ACTIVE.DAT'/
	CHARACTER*18	FILES3/'LOGS:AG3_SHIFT.DAT'/
	CHARACTER*16	FILED3/'LOGS:AG3_DAY.DAT'/
	CHARACTER*14	FILE/'GP:MAINGAS.DAT'/
C
	CHARACTER*13	FILET(2)/'ANA:TSIL2.DAT',
	1		     'ANA:TSIL3.DAT'/
	CHARACTER*8	TIM/'0 0:0:30'/
C
	CHARACTER*8	AG3AOP(34)/'AV800007','AV800008','AV800009',
	1		           'AV800010','AV800017','AV800018',
	1			   'AV800019','AV800024','AV800025',
	1			   'AV800026','AV800027','AV800028',
	1			   'AV800029','AV800065','AV800066',
	1			   'AV800067','AV800068','AV800069',
	1			   'AV800070','AV800071','AV800072',
	1			   'AV800073','AV800085','AV800086',
	1			   'AV800087','AV800088','AV800089',
	1			   'AV800090','AV800091','AV800092',
	1			   'AV800093','AV800094','AV800097',
	1			   'AV800098'/
C
	CHARACTER*8	CNCAOP(14)/'AD400055','AD400056','AD400057',
	1		          'AD400058','AD400059','AD400060',
	1			  'AD400061','AD400062','AD400063',
	1			  'AD400100','AD400129','AD400130',
	1			  'AD400131','AD400132'/
C
	CHARACTER*8	AOP(37)/'AD600050','AD600035','AD600036',
	1		       'AD600052','AD600053','AD600054',
	1		       'AD600055','AD600020','AD600021',
	1		       'AD600018','AD600058','AD600059',
	1		       'AD600065','AD600066','AD600067',
	1		       'AD600068','AD600069','AD600070',
	1		       'AD600089','AD600074','AD600080',
	1		       'AD600086','AD600093','AD600094',
	1		       'AD600095','AD600116','AD600117',
	1		       'AD600118','AD600119','AD600120',
	1		       'AD600121','AD600125','AD600126',
	1		       'AD600127','AD600128','AD600129',
	1		       'AD600130'/
C
	COMMON/ANVALTBL/ANVAL
	COMMON/CNC_ANVALTBL/CNC_ANVAL
	COMMON/AG3_ANVALTBL/ANVALX
	COMMON/GOODPTTBL/GPT
C
	EXTERNAL	UFO_OPEN
C
C	OPEN CONNECTION TO CONCENTRATOR POINT DIRECTORY
C
        status = LIB$SYS_TRNLOG('WESAPI_PDIR_4',
     1			spd_filename_len,spd_filename,,,)
C
	spd_filename(spd_filename_len+1:) = CHAR(0)
C
        access_type = RUNTIME
C
        spd_fd = SPD_open_file(%ref(spd_filename), access_type)
C
	DO 10	J=1,14	!GET THE 14 CONCENTRATOR ANALOG OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(CNCAOP(J)//char(0)),CNCAO(J),
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)

C
10	CONTINUE
C
C  Close the point directory connection
C
	status = SPD_close_file(spd_fd)
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
	DO 20	J=1,37	!GET THE 37 AGGL 1-2 ANALOG OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(AOP(J)//char(0)),AO(J),
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)

C
20	CONTINUE
C
C  Close the point directory connection
C
	status = SPD_close_file(spd_fd)
C
C	OPEN CONNECTION TO AGGL 3 POINT DIRECTORY
C
        status = LIB$SYS_TRNLOG('WESAPI_PDIR_1',
     1			spd_filename_len,spd_filename,,,)
C
	spd_filename(spd_filename_len+1:) = CHAR(0)
C
        access_type = RUNTIME
C
        spd_fd = SPD_open_file(%ref(spd_filename), access_type)
C
	DO 30	J=1,34	!GET THE 34 AGGL 3 ANALOG OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(AG3AOP(J)//char(0)),AG3AO(J),
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)

C
30	CONTINUE
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
		OPEN	(UNIT=22,FILE='GP:MBTUS.ERR',STATUS='UNKNOWN',
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
	CALL	SYS$BINTIM(TIM,ADRS)
	CALL	SYS$SCHDWK(,,ADRS,ADRS)
C
	LOGX=0
C
	field_name='ED'
	length=30
	c_ascii_val='STEP 2 FILTER CAKE PCT SIO2   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(12),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(12), point_quality)
C
	c_ascii_val='AGGL 3  FILTER CAKE PCT SIO2  '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(20),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(20), point_quality)
C
	c_ascii_val='STEP 2 FILTER CAKE PCT CAO    '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(9),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(9), point_quality)
C
	c_ascii_val='STEP 2 FILTER CAKE PCT MGO    '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(11),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(11), point_quality)
C
	c_ascii_val='LINE 3 LT SHIFT PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(13),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(13), point_quality)
C
	c_ascii_val='LINE 3 LT DAILY PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(14),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(14), point_quality)
C
	c_ascii_val='LINE 4 LT SHIFT PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(15),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(15), point_quality)
C
	c_ascii_val='LINE 4 LT DAILY PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(16),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(16), point_quality)
C
	c_ascii_val='LINE 5 LT SHIFT PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(17),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(17), point_quality)
C
	c_ascii_val='LINE 5 LT DAILY PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(18),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(18), point_quality)
C
	c_ascii_val='STEP 3 AGGL % MGO             '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(19),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(19), point_quality)
C
	c_ascii_val='STEP 3 AGGL % CAO             '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(10),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(10), point_quality)
C
	c_ascii_val='LINE 3 % FERROUS IRON         '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(20),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(20), point_quality)
C
	c_ascii_val='LINE 4 % FERROUS IRON         '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(21),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(21), point_quality)
C
	c_ascii_val='LINE 5 % FERROUS IRON         '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(22),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(22), point_quality)
C
	c_ascii_val='AGGL FILTERCAKE PCT CAO       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(12),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(12), point_quality)
C
	c_ascii_val='AGGL FILTERCAKE PCT MGO       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(13),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(13), point_quality)
C
	c_ascii_val='LINE 6 LT SHIFT PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(14),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(14), point_quality)
C
	c_ascii_val='LINE 6 LT DAILY PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(15),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(15), point_quality)
C
	c_ascii_val='LINE 7 LT SHIFT PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(16),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(16), point_quality)
C
	c_ascii_val='LINE 7 LT DAILY PELLETS       '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(17),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(17), point_quality)
C
	c_ascii_val='LINE 6 % FERROUS IRON         '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(18),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(18), point_quality)
C
	c_ascii_val='LINE 7 % FERROUS IRON         '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(19),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(19), point_quality)
C
	status = SHC_put_point_quality(AG3AO(8), point_quality)
C
	status = SHC_put_point_quality(AO(1), point_quality)
	status = SHC_put_point_quality(AO(2), point_quality)
	status = SHC_put_point_quality(AO(3), point_quality)
	status = SHC_put_point_quality(AO(4), point_quality)
	status = SHC_put_point_quality(AO(5), point_quality)
C
	status = SHC_put_point_quality(AG3AO(3), point_quality)
	status = SHC_put_point_quality(AG3AO(4), point_quality)
C
	c_ascii_val='218-02-2 THKNR OFLO PUMP AMPS '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(CNCAO(11),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(CNCAO(11), point_quality)
C
	c_ascii_val='218-02-3 THKNR OFLO PUMP AMPS '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(CNCAO(12),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(CNCAO(12), point_quality)
C
	c_ascii_val='LINE 3 PELLET FORECAST LTPH   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(23),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(23), point_quality)
C
	c_ascii_val='LINE 4 PELLET FORECAST LTPH   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(24),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(24), point_quality)
C
	c_ascii_val='LINE 5 PELLET FORECAST LTPH   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(25),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(25), point_quality)
C
	c_ascii_val='LINE 6 PELLET FORECAST LTPH   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(21),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(21), point_quality)
C
	c_ascii_val='LINE 7 PELLET FORECAST LTPH   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(22),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(22), point_quality)
C
	c_ascii_val='000-03-0 WOOD FLOW SETPOINT   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(23),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(23), point_quality)
C
	c_ascii_val='000-04-0 WOOD FLOW SETPOINT   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(24),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(24), point_quality)
C
	c_ascii_val='000-05-0 WOOD FLOW SETPOINT   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(25),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(25), point_quality)
C
	c_ascii_val='000-06-0 WOOD FLOW SETPOINT   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(26),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(26), point_quality)
C
	c_ascii_val='000-07-0 WOOD FLOW SETPOINT   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(27),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(27), point_quality)
C
	c_ascii_val='000-03-0 WOOD FLOW LBS/MIN    '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(26),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(26), point_quality)
C
	c_ascii_val='000-04-0 WOOD FLOW LBS/MIN    '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(27),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(27), point_quality)
C
	c_ascii_val='000-05-0 WOOD FLOW LBS/MIN    '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(28),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(28), point_quality)
C
	c_ascii_val='000-06-0 WOOD FLOW LBS/MIN    '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(28),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(28), point_quality)
C
	c_ascii_val='000-07-0 WOOD FLOW LBS/MIN    '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(29),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(29), point_quality)
C
	c_ascii_val='TOTAL WOOD AVAILABLE LBS/MN   '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(29),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(29), point_quality)
C
	c_ascii_val='000-03-0 WOOD PIPE PRESS PSI  '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(30),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(30), point_quality)
C
	c_ascii_val='000-04-0 WOOD PIPE PRESS PSI  '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(31),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(31), point_quality)
C
	c_ascii_val='000-05-0 WOOD PIPE PRESS PSI  '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(32),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(32), point_quality)
C
	c_ascii_val='000-06-0 WOOD PIPE PRESS PSI  '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(30),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(30), point_quality)
C
	c_ascii_val='000-07-0 WOOD PIPE PRESS PSI  '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(31),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(31), point_quality)
C
	c_ascii_val='000-00-0 TOT GAS PREVIOUS 24HR'
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(32),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(32), point_quality)
C
	c_ascii_val='000-00-0 TOT GAS PREVIOUS HOUR'
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AO(33),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AO(33), point_quality)
C
	c_ascii_val='000-00-0 TOT GAS PREVIOUS 24HR'
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(33),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(33), point_quality)
C
	c_ascii_val='000-00-0 TOT GAS PREVIOUS HOUR'
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(AG3AO(34),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(AG3AO(34), point_quality)
C
	c_ascii_val='315-01-4 6 INCH LINE DENSITY  '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(CNCAO(13),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(CNCAO(13), point_quality)
C
	c_ascii_val='315-03-4 6 INCH LINE DENSITY  '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(CNCAO(14),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
	status = SHC_put_point_quality(CNCAO(14), point_quality)
C
	status = SHC_put_point_quality(AO(34), point_quality)
	status = SHC_put_point_quality(AO(35), point_quality)
	status = SHC_put_point_quality(AO(36), point_quality)
	status = SHC_put_point_quality(AO(37), point_quality)
C
	c_ascii_val='STEP 3 FLUX RATIO SP          '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(CNCAO(34),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
C
	c_ascii_val='STEP 3 FLUX RATIO             '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(CNCAO(35),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
C
	c_ascii_val='STEP 3 FILTER CAKE B/A SP     '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(CNCAO(36),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
C
	c_ascii_val='STEP 3 FILTER CAKE B/A        '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(CNCAO(37),%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
C
100	CONTINUE
C
	CALL	LIB$DATE_TIME(DATETIME)	!GET ASCII DATE AND TIME
C
	DO 1000 I=1,3
C
		VALU(I)=ANVAL(GAS(I))*1000
C		VALU(I)=VALU(I)+ANVAL(WOOD(I))*8.35	!CHAD STEWART 11-4-03
		VALU(I)=VALU(I)+ANVAL(WOOD(I))*8.225	!CORY BIRD 05-11-09
		VALU(I)=VALU(I)+ANVAL(OIL(I))*139
C
		IF (ANVAL(TONS(I)) .EQ. 0)THEN
			VALU(I)=0
		ELSE
			VALU(I)=VALU(I)/ANVAL(TONS(I))
		END IF
C
		ACCUM=0
		OBS=0
C
		IF (I .EQ. 1)THEN
C
			LINE4(6)=LINE4(5)
			LINE4(5)=LINE4(4)
			LINE4(4)=LINE4(3)
			LINE4(3)=LINE4(2)
			LINE4(2)=LINE4(1)
			LINE4(1)=VALU(I)
C
			DO 300	J=1,6
				IF (LINE4(J) .NE. 0)THEN
					ACCUM=ACCUM+LINE4(J)
					OBS=OBS+1
				END IF
300			CONTINUE
C
		END IF
C
		IF (I .EQ. 2)THEN
C
			LINE5(6)=LINE5(5)
			LINE5(5)=LINE5(4)
			LINE5(4)=LINE5(3)
			LINE5(3)=LINE5(2)
			LINE5(2)=LINE5(1)
			LINE5(1)=VALU(I)
C
			DO 400	J=1,6
				IF (LINE5(J) .NE. 0)THEN
					ACCUM=ACCUM+LINE5(J)
					OBS=OBS+1
				END IF
400			CONTINUE
C
		END IF
C
		IF (I .EQ. 3)THEN
C
			LINE3(6)=LINE3(5)
			LINE3(5)=LINE3(4)
			LINE3(4)=LINE3(3)
			LINE3(3)=LINE3(2)
			LINE3(2)=LINE3(1)
			LINE3(1)=VALU(I)
C
			DO 500	J=1,6
				IF (LINE3(J) .NE. 0)THEN
					ACCUM=ACCUM+LINE3(J)
					OBS=OBS+1
				END IF
500			CONTINUE
C
		END IF
C
		IF (ACCUM .NE. 0)THEN
			VALU(I)=ACCUM/OBS
		ELSE
			VALU(I)=0
		END IF
C
1000	CONTINUE
C
	DO 1001 I=1,5
C
	IF (I .LE. 3)THEN !IF AGGL12
C
		OPEN(UNIT=2,NAME=FILEA,FORM='UNFORMATTED',
	1	ACCESS='DIRECT',STATUS='OLD',RECL=2,SHARED,
	1	CARRIAGECONTROL='FORTRAN',RECORDTYPE='FIXED',
	1	USEROPEN=UFO_OPEN)
C
	ELSE	!IF AGGL3
C
		OPEN(UNIT=2,NAME=FILEA3,FORM='UNFORMATTED',
	1	ACCESS='DIRECT',STATUS='OLD',RECL=2,SHARED,
	1	CARRIAGECONTROL='FORTRAN',RECORDTYPE='FIXED',
	1	USEROPEN=UFO_OPEN)
C
	END IF
C
		READ	(2,REC=TONS(I))TA,OBS	!ACTIVE TONS
C
		CLOSE	(UNIT=2)
C
	IF (I .LE. 3)THEN !IF AGGL12
C
		OPEN(UNIT=2,NAME=FILES,FORM='UNFORMATTED',
	1	ACCESS='DIRECT',STATUS='OLD',RECL=2,SHARED,
	1	CARRIAGECONTROL='FORTRAN',RECORDTYPE='FIXED',
	1	USEROPEN=UFO_OPEN)
C
	ELSE	!IF AGGL3
C
		OPEN(UNIT=2,NAME=FILES3,FORM='UNFORMATTED',
	1	ACCESS='DIRECT',STATUS='OLD',RECL=2,SHARED,
	1	CARRIAGECONTROL='FORTRAN',RECORDTYPE='FIXED',
	1	USEROPEN=UFO_OPEN)
C
	END IF
C
		READ	(2,REC=TONS(I))TS,OBS	!SHIFT TONS
C
		CLOSE	(UNIT=2)
C
	IF (I .LE. 3)THEN !IF AGGL12
C
		OPEN(UNIT=2,NAME=FILED,FORM='UNFORMATTED',
	1	ACCESS='DIRECT',STATUS='OLD',RECL=2,SHARED,
	1	CARRIAGECONTROL='FORTRAN',RECORDTYPE='FIXED',
	1	USEROPEN=UFO_OPEN)
C
	ELSE	!IF AGGL3
C
		OPEN(UNIT=2,NAME=FILED3,FORM='UNFORMATTED',
	1	ACCESS='DIRECT',STATUS='OLD',RECL=2,SHARED,
	1	CARRIAGECONTROL='FORTRAN',RECORDTYPE='FIXED',
	1	USEROPEN=UFO_OPEN)
C
	END IF
C
		READ	(2,REC=TONS(I))TD,OBS	!DAY TONS
C
		CLOSE	(UNIT=2)
C
		READ	(DATETIME(13:14),'(I2)')HRS
		READ	(DATETIME(16:17),'(I2)')MINS
		TIMEX=(HRS*100)+MINS
C
		HFLAG=0
		IF (TIMEX .GE.   30)HFLAG=3
		IF (TIMEX .GE.  130)HFLAG=4
		IF (TIMEX .GE.  230)HFLAG=5
		IF (TIMEX .GE.  330)HFLAG=6
		IF (TIMEX .GE.  430)HFLAG=7
		IF (TIMEX .GE.  530)HFLAG=8
		IF (TIMEX .GE.  630)HFLAG=9
		IF (TIMEX .GE.  730)HFLAG=10
		IF (TIMEX .GE.  830)HFLAG=11
		IF (TIMEX .GE.  930)HFLAG=12
		IF (TIMEX .GE. 1030)HFLAG=13
		IF (TIMEX .GE. 1130)HFLAG=14
		IF (TIMEX .GE. 1230)HFLAG=15
		IF (TIMEX .GE. 1330)HFLAG=16
		IF (TIMEX .GE. 1430)HFLAG=17
		IF (TIMEX .GE. 1530)HFLAG=18
		IF (TIMEX .GE. 1630)HFLAG=19
		IF (TIMEX .GE. 1730)HFLAG=20
		IF (TIMEX .GE. 1830)HFLAG=21
		IF (TIMEX .GE. 1930)HFLAG=22
		IF (TIMEX .GE. 2030)HFLAG=23
		IF (TIMEX .GE. 2130)HFLAG=24
		IF (TIMEX .GE. 2230)HFLAG=1
       		IF (TIMEX .GE. 2330)HFLAG=2
C
		SFLAG=0
		IF (HFLAG .EQ. 1)SFLAG=1	!FIRST HR OF 1ST SHIFT
		IF (HFLAG .EQ. 9)SFLAG=1	!FIRST HR OF 2ND SHIFT
		IF (HFLAG .EQ. 17)SFLAG=1	!FIRST HR OF 3RD SHIFT
C
		IF (SFLAG .EQ. 0)THEN
			TONS_S(I)=TA+TS	!SHIFT TO DATE = SHIFT +ACTIVE
		ELSE
			TONS_S(I)=TA	!FIRST HR SHIFT TO DATE = ACTIVE
		END IF
C
		DFLAG=0
C
		IF (HFLAG .EQ. 1)DFLAG=1	!1ST HOUR OF DAY
		IF (HFLAG .EQ. 9)DFLAG=2	!1ST HOUR OF 2ND SHIFT
		IF (HFLAG .EQ. 17)DFLAG=2	!1ST HOUR OF 3RD SHIFT
		IF ((HFLAG .GE. 2).AND.(HFLAG .LE. 8))DFLAG=3
		IF ((HFLAG .GE. 10).AND.(HFLAG .LE. 16))DFLAG=4
		IF ((HFLAG .GE. 18).AND.(HFLAG .LE. 24))DFLAG=4
C
		IF (DFLAG .EQ. 1)TONS_D(I)=TA
		IF (DFLAG .EQ. 2)TONS_D(I)=TA+TD
		IF (DFLAG .EQ. 3)TONS_D(I)=TA+TS
		IF (DFLAG .EQ. 4)TONS_D(I)=TA+TS+TD
C
1001	CONTINUE
C
	put_analog_val=VALU(3)	!MBTUS/TON LINE 3
	status = SHC_put_analog_val(AO(1), put_analog_val)
C
	put_analog_val=VALU(1)	!MBTUS/TON LINE 4
	status = SHC_put_analog_val(AO(2), put_analog_val)
C
	ANVAL(1025)=VALU(1)	!UPDATE ANVAL
	M=(1025/32)+1
	N=MOD(1025,32)
	GPT(M)=JIBSET(GPT(M),N)
C
	put_analog_val=VALU(2)	!MBTUS/TON LINE 5
	status = SHC_put_analog_val(AO(3), put_analog_val)
C
	ANVAL(1026)=VALU(2)	!UPDATE ANVAL
	M=(1026/32)+1
	N=MOD(1026,32)
	GPT(M)=JIBSET(GPT(M),N)
C
	put_analog_val=CNC_ANVAL(1171)	!STEP I 170 CONC TONS LTPH
	status = SHC_put_analog_val(AO(4), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1172)	!STEP III 170 CONC TONS LTPH
	status = SHC_put_analog_val(AO(5), put_analog_val)
C
	put_analog_val=ANVALX(817)	!LINE 6 BENT LBS/TON
	status = SHC_put_point_quality(AG3AO(1), point_quality)
	status = SHC_put_analog_val(AG3AO(1), put_analog_val)
C
	put_analog_val=ANVALX(818)	!LINE 7 BENT LBS/TON
	status = SHC_put_point_quality(AG3AO(2), point_quality)
	status = SHC_put_analog_val(AG3AO(2), put_analog_val)
C
	put_analog_val=ANVALX(880)	!LINE 6 MBTUS/TON
	status = SHC_put_analog_val(AG3AO(3), put_analog_val)
C
	put_analog_val=ANVALX(881)	!LINE 7 MBTUS/TON
	status = SHC_put_analog_val(AG3AO(4), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1171)	!STEP I 170 CONC TONS LTPH
	status = SHC_put_analog_val(AG3AO(5), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1172)	!STEP III 170 CONC TONS LTPH
	status = SHC_put_analog_val(AG3AO(6), put_analog_val)
C
	put_analog_val=ANVAL(334)		!AGGL2 OUTSIDE TEMP
	status = SHC_put_analog_val(AG3AO(7), put_analog_val)
C
	put_analog_val=ANVAL(951)		!TURBINE MAIN GAS
	status = SHC_put_analog_val(AG3AO(8), put_analog_val)
C
	put_analog_val=ANVAL(37)		!205-02-3 SLURRY TANK LEVEL
	status = SHC_put_analog_val(AG3AO(9), put_analog_val)
C
	put_analog_val=ANVAL(708)		!205-02-4 SLURRY TANK LEVEL
	status = SHC_put_analog_val(AG3AO(10), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1386)	!AMINE LBS/TON
	status = SHC_put_point_quality(AG3AO(11), point_quality)
	status = SHC_put_analog_val(AG3AO(11), put_analog_val)
	status = SHC_put_point_quality(AO(8), point_quality)
	status = SHC_put_analog_val(AO(8), put_analog_val)
C
	OPEN	(UNIT=10,FILE='USER7:[FLUX]SLURRY22.DAT',STATUS='OLD',
	1	SHARED,ACCESS='KEYED',ERR=555)
C
	READ	(10,ERR=333)DAT
C
	GOTO 444
333	CONTINUE
C
	CLOSE	(UNIT=10)
	GOTO 555
C
444	CONTINUE
C
	CLOSE	(UNIT=10)
C
	IF (DAT.CA(DAT.HOUR) .EQ. 0)GOTO 22	!SKIP OUTPUT IF ZERO
C
	put_analog_val=DAT.CA(DAT.HOUR)
	status = SHC_put_analog_val(AO(9), put_analog_val)
C
22	CONTINUE
C
	IF (DAT.MG(DAT.HOUR) .EQ. 0)GOTO 555	!SKIP OUTPUT IF ZERO
C
	put_analog_val=DAT.MG(DAT.HOUR)
	status = SHC_put_analog_val(AO(11), put_analog_val)
C
555	CONTINUE
C
	OPEN	(UNIT=10,FILE='USER7:[FLUX]SLURRY33.DAT',STATUS='OLD',
	1	SHARED,ACCESS='KEYED',ERR=5555)
C
	READ	(10,ERR=3333)DAT
C
	GOTO 4444
3333	CONTINUE
C
	CLOSE	(UNIT=10)
	GOTO 5555
C
4444	CONTINUE
C
	CLOSE	(UNIT=10)
C
	IF (DAT.CA(DAT.HOUR) .EQ. 0)GOTO 222	!SKIP OUTPUT IF ZERO
C
	put_analog_val=DAT.CA(DAT.HOUR)
	status = SHC_put_analog_val(AG3AO(12), put_analog_val)
	status = SHC_put_analog_val(AO(10), put_analog_val)
C
222	CONTINUE
C
	IF (DAT.MG(DAT.HOUR) .EQ. 0)GOTO 5555	!SKIP OUTPUT IF ZERO
C
	put_analog_val=DAT.MG(DAT.HOUR)
	status = SHC_put_analog_val(AG3AO(13), put_analog_val)
	status = SHC_put_analog_val(AO(19), put_analog_val)
C
5555	CONTINUE
C
	put_analog_val=ANVAL(694)		!AGGL2 203 LTPH
	status = SHC_put_analog_val(CNCAO(1), put_analog_val)
C
	put_analog_val=ANVAL(37)		!AGGL2 #3 TANK LEVEL
	status = SHC_put_analog_val(CNCAO(2), put_analog_val)
C
	put_analog_val=ANVAL(708)		!AGGL2 #4 TANK LEVEL
	status = SHC_put_analog_val(CNCAO(3), put_analog_val)
C
	put_analog_val=ANVAL(14)		!AGGL2 #1 TANK LEVEL
	status = SHC_put_analog_val(CNCAO(4), put_analog_val)
C
	put_analog_val=ANVAL(28)		!AGGL2 #2 TANK LEVEL
	status = SHC_put_analog_val(CNCAO(5), put_analog_val)
C
	put_analog_val=ANVALX(572)		!AGGL3 203 LTPH
	status = SHC_put_analog_val(CNCAO(6), put_analog_val)
C
	put_analog_val=ANVALX(353)		!AGGL3 #1 TANK
	status = SHC_put_analog_val(CNCAO(7), put_analog_val)
C
	put_analog_val=ANVALX(389)		!AGGL3 #2 TANK
	status = SHC_put_analog_val(CNCAO(8), put_analog_val)
C
	put_analog_val=ANVAL(334)	!AGGL2 OUTSIDE TEMP
	status = SHC_put_analog_val(CNCAO(9), put_analog_val)
C
	put_analog_val=CNC_ANVAL(753)	!CONC AO AO425016
	status = SHC_put_analog_val(CNCAO(10), put_analog_val)
C
	put_analog_val=ANVAL(353)	!218-02-2 THKNR OFLO PUMP AMPS
	status = SHC_put_analog_val(CNCAO(11), put_analog_val)
C
	put_analog_val=ANVAL(354)	!218-02-3 THKNR OFLO PUMP AMPS
	status = SHC_put_analog_val(CNCAO(12), put_analog_val)
C
	put_analog_val=ANVALX(353)		!AGGL3 #1 TANK TO AGGL12
	status = SHC_put_analog_val(AO(6), put_analog_val)
C
	put_analog_val=ANVALX(389)		!AGGL3 #2 TANK TO AGGL12
	status = SHC_put_analog_val(AO(7), put_analog_val)
C
	OPEN	(UNIT=2,FILE=FILET(1),STATUS='OLD',SHARED,
	1	RECL=16,RECORDTYPE='FIXED',FORM='UNFORMATTED',
	1	ACCESS='DIRECT',USEROPEN=UFO_OPEN)
C
	READ	(2,REC=1)TSX	!READ A TSIL RECORD
C
	CLOSE	(UNIT=2)
C
	put_analog_val=TSX.FC	!SEND NEW AGGL12 F CAKE TO WDPF
	status = SHC_put_analog_val( AO(12), put_analog_val)
C
	OPEN	(UNIT=2,FILE=FILET(2),STATUS='OLD',SHARED,
	1	RECL=16,RECORDTYPE='FIXED',FORM='UNFORMATTED',
	1	ACCESS='DIRECT',USEROPEN=UFO_OPEN)
C
	READ	(2,REC=1)TSX	!READ A TSIL RECORD
C
	CLOSE	(UNIT=2)
C
	put_analog_val=TSX.FC	!SEND NEW AGG3 F CAKE TO WDPF
	status = SHC_put_analog_val( AG3AO(20), put_analog_val)
C
	put_analog_val=TONS_S(3)	!LINE 3 SHIFT TO DATE TONS
	status = SHC_put_analog_val(AO(13), put_analog_val)
C
	put_analog_val=TONS_D(3)	!LINE 3 DAY TO DATE TONS
	status = SHC_put_analog_val(AO(14), put_analog_val)
C
	put_analog_val=TONS_S(1)	!LINE 4 SHIFT TO DATE TONS
	status = SHC_put_analog_val(AO(15), put_analog_val)
C
	put_analog_val=TONS_D(1)	!LINE 4 DAY TO DATE TONS
	status = SHC_put_analog_val(AO(16), put_analog_val)
C
	put_analog_val=TONS_S(2)	!LINE 5 SHIFT TO DATE TONS
	status = SHC_put_analog_val(AO(17), put_analog_val)
C
	put_analog_val=TONS_D(2)	!LINE 5 DAY TO DATE TONS
	status = SHC_put_analog_val(AO(18), put_analog_val)
C
	put_analog_val=TONS_S(4)	!LINE 6 SHIFT TO DATE TONS
	status = SHC_put_analog_val(AG3AO(14), put_analog_val)
C
	put_analog_val=TONS_D(4)	!LINE 6 DAY TO DATE TONS
	status = SHC_put_analog_val(AG3AO(15), put_analog_val)
C
	put_analog_val=TONS_S(5)	!LINE 7 SHIFT TO DATE TONS
	status = SHC_put_analog_val(AG3AO(16), put_analog_val)
C
	put_analog_val=TONS_D(5)	!LINE 7 DAY TO DATE TONS
	status = SHC_put_analog_val(AG3AO(17), put_analog_val)
C
	OPEN	(UNIT=1,FILE='LOGS:FEPLUS.DAT',RECL=5,ERR=2222,
	1	STATUS='OLD',ACCESS='DIRECT',RECORDTYPE='FIXED',
	1	SHARED,FORM='UNFORMATTED',USEROPEN=UFO_OPEN)
C
	READ	(1,REC=1,ERR=7777)FE
C
	CLOSE	(UNIT=1)
C
	IF (FE(1) .NE. 0)THEN
		put_analog_val=FE(1)	!LINE 3 FERROUS IRON
		status = SHC_put_analog_val(AO(20), put_analog_val)
	END IF
C
	IF (FE(2) .NE. 0)THEN
		put_analog_val=FE(2)	!LINE 4 FERROUS IRON
		status = SHC_put_analog_val(AO(21), put_analog_val)
	END IF
C
	IF (FE(3) .NE. 0)THEN
		put_analog_val=FE(3)	!LINE 5 FERROUS IRON
		status = SHC_put_analog_val(AO(22), put_analog_val)
	END IF
C
	IF (FE(4) .NE. 0)THEN
		put_analog_val=FE(4)	!LINE 6 FERROUS IRON
		status = SHC_put_analog_val(AG3AO(18), put_analog_val)
	END IF
C
	IF (FE(5) .NE. 0)THEN
		put_analog_val=FE(5)	!LINE 7 FERROUS IRON
		status = SHC_put_analog_val(AG3AO(19), put_analog_val)
	END IF
C
	GOTO 2222
C
7777	CONTINUE
C
	CLOSE	(UNIT=1)
C
2222	CONTINUE
C
	OPEN	(UNIT=1,FILE='LOGS:FORECAST.DAT',RECL=5,ERR=22229,
	1	STATUS='OLD',ACCESS='DIRECT',RECORDTYPE='FIXED',
	1	SHARED,FORM='UNFORMATTED',USEROPEN=UFO_OPEN)
C
	READ	(1,REC=1,ERR=77779)FCAST
C
	CLOSE	(UNIT=1)
C
	put_analog_val=FCAST(1)	!LINE 3 PEL FORECAST LTPH
	status = SHC_put_analog_val(AO(23), put_analog_val)
C
	put_analog_val=FCAST(2)	!LINE 4 PEL FORECAST LTPH
	status = SHC_put_analog_val(AO(24), put_analog_val)
C
	put_analog_val=FCAST(3)	!LINE 5 PEL FORECAST LTPH
	status = SHC_put_analog_val(AO(25), put_analog_val)
C
	put_analog_val=FCAST(4)	!LINE 6 PEL FORECAST LTPH
	status = SHC_put_analog_val(AG3AO(21), put_analog_val)
C
	put_analog_val=FCAST(5)	!LINE 7 PEL FORECAST LTPH
	status = SHC_put_analog_val(AG3AO(22), put_analog_val)
C
	put_analog_val=ANVALX(695)	!LINE 6 WOOD FLOW SETPOINT
	status = SHC_put_analog_val(AO(26), put_analog_val)
C
	put_analog_val=ANVALX(696)	!LINE 7 WOOD FLOW SETPOINT
	status = SHC_put_analog_val(AO(27), put_analog_val)
C
	put_analog_val=ANVAL(542)	!LINE 3 WOOD FLOW SETPOINT
	status = SHC_put_analog_val(AG3AO(23), put_analog_val)
C
	put_analog_val=ANVAL(543)	!LINE 4 WOOD FLOW SETPOINT
	status = SHC_put_analog_val(AG3AO(24), put_analog_val)
C
	put_analog_val=ANVAL(544)	!LINE 5 WOOD FLOW SETPOINT
	status = SHC_put_analog_val(AG3AO(25), put_analog_val)
C
	put_analog_val=ANVALX(647)	!LINE 6 WOOD FLOW LBS/MIN
	status = SHC_put_analog_val(AO(28), put_analog_val)
C
	put_analog_val=ANVALX(648)	!LINE 7 WOOD FLOW LBS/MIN
	status = SHC_put_analog_val(AO(29), put_analog_val)
C
	put_analog_val=ANVALX(697)	!LINE 6 WOOD PRESS PSI
	status = SHC_put_analog_val(AO(30), put_analog_val)
C
	put_analog_val=ANVALX(698)	!LINE 7 WOOD PRESS PSI
	status = SHC_put_analog_val(AO(31), put_analog_val)
C
	put_analog_val=ANVAL(889)	!LINE 3 WOOD FLOW LBS/MIN
	status = SHC_put_analog_val(AG3AO(26), put_analog_val)
C
	put_analog_val=ANVAL(658)	!LINE 4 WOOD FLOW LBS/MIN
	status = SHC_put_analog_val(AG3AO(27), put_analog_val)
C
	put_analog_val=ANVAL(659)	!LINE 5 WOOD FLOW LBS/MIN
	status = SHC_put_analog_val(AG3AO(28), put_analog_val)
C
	put_analog_val=ANVAL(545)	!TOTAL WOOD FLOW
	status = SHC_put_analog_val(AG3AO(29), put_analog_val)
C
	put_analog_val=ANVAL(546)	!LINE 3 WOOD PRESS PSI
	status = SHC_put_analog_val(AG3AO(30), put_analog_val)
C
	put_analog_val=ANVAL(547)	!LINE 4 WOOD PRESS PSI
	status = SHC_put_analog_val(AG3AO(31), put_analog_val)
C
	put_analog_val=ANVAL(548)	!LINE 5 WOOD PRESS PSI
	status = SHC_put_analog_val(AG3AO(32), put_analog_val)
C
	GOTO 22229
C
77779	CONTINUE
C
	CLOSE	(UNIT=1)
C
22229	CONTINUE
C
		OPEN(UNIT=1,NAME=FILE,FORM='UNFORMATTED',
	1	ACCESS='DIRECT',STATUS='OLD',RECL=48,SHARED,
	1	CARRIAGECONTROL='FORTRAN',RECORDTYPE='FIXED',
	1	USEROPEN=UFO_OPEN)
C
	READ	(1,REC=1)ACTH,LASTH,ACTD,LASTD	!READ IN VAX FILE
	CLOSE	(UNIT=1)
C
	put_analog_val=LASTD(4)	!PREVOUS DAY GAS CONSUMPTION
	status = SHC_put_analog_val(AO(32), put_analog_val)
C
	status = SHC_put_analog_val(AG3AO(33), put_analog_val)
C
	put_analog_val=LASTH(4)	!PREVOUS HOUR GAS CONSUMPTION
	status = SHC_put_analog_val(AO(33), put_analog_val)
C
	status = SHC_put_analog_val(AG3AO(34), put_analog_val)
C
	put_analog_val=ANVAL(389)	!315-01-4 6 INCH LINE DENSITY
	status = SHC_put_analog_val(CNCAO(13), put_analog_val)
C
	put_analog_val=ANVALX(586)	!315-03-4 6 INCH LINE DENSITY
	status = SHC_put_analog_val(CNCAO(14), put_analog_val)
C
	put_analog_val=ANVALX(700)	!STEP 3 FLUX RATIO SP
	status = SHC_put_analog_val(AO(34), put_analog_val)
C
	put_analog_val=ANVALX(701)	!STEP 3 FLUX RATIO 
	status = SHC_put_analog_val(AO(35), put_analog_val)
C
	put_analog_val=ANVALX(702)	!STEP 3 FILTER CAKE B/A SP 
	status = SHC_put_analog_val(AO(36), put_analog_val)
C
	put_analog_val=ANVALX(703)	!STEP 3 FILTER CAKE B/A  
	status = SHC_put_analog_val(AO(37), put_analog_val)
C
	CALL	SYS$HIBER()
C
	GOTO 100
C
99999	CONTINUE
C
	CALL	EXIT
C
	END
