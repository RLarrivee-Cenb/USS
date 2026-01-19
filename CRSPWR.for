C
	PROGRAM 	CRSPWR
C
C	THIS PROGRAM SENDS THE MINNESOTA POWER PROJECTED RATE
C	AL POINT RECORD FROM THE CONCENTRATOR WDPF SYSTEM TO THE
C	CRUSHER WDPF SYSTEM VIA THE WEStation AND THE ALPHA A1 
C	COMPUTER.
C
C	FORT/NOOP/NOALIGN/WARN=NOALIGN/FLOAT=D_FLOAT CRSPWR
C
C	LINK CRSPWR,CRSPWR/OPT,WESAPI_LIB:WESAPI/OPT
C
	IMPLICIT NONE
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
	INTEGER*2	length
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
	CHARACTER*80    c_ascii_val
C
	REAL            put_analog_val
	REAL            c_FP_val
C
	INTEGER*4	LEN,POINT_QUALITY
C
	STRUCTURE	/AL/
C
		INTEGER*2	ID,AS
		REAL*4		AV
		BYTE		RT,FM,SR,LU
		INTEGER*2	AW,DG
		CHARACTER*8	PN
		INTEGER*2	TB,BB
		CHARACTER*30	ED
		CHARACTER*6	EU
		REAL*4		EV
		INTEGER*2	CD,HW
		BYTE		AP,AY,MM,NN,AH,MN,CM,LC
		INTEGER*2	IP
		REAL*4		HL,LL,IL,DB,PV,HS,LS
C
	END STRUCTURE
C
	RECORD/AL/ALX
C
	REAL*4		CNC_ANVAL(1900)
	REAL*4		CRS_ANVAL(1000),SUMX
C
	INTEGER*4	ADRS(2),J,K,BIT,I,CRSGP,CRSAO(3),CNCAL,CRSAL
	INTEGER*4	CXSTS(100),SUBS/1718/
	INTEGER*4	AGGL2,AGGL3,CNCAO,CUT
	INTEGER*4	MPL(6)/1884,1885,1886,1887,1888,1889/
	INTEGER*4	TAB(43)/35,51,36,52,37,53,38,54,
	1			39,55,40,56,41,57,42,58,
	1			78,71,83,75,79,72,84,76,
	1			80,73,85,77,15,17,19,21,
	1			23,25,27,29,31,33,65,61,
	1			67,63,69/
C
	INTEGER*2	TEMP
C
	CHARACTER*8	TIM/'0 0:0:05'/	!5 SEC RUN TIME
C
	CHARACTER*8	CRSGPP/'GP200008'/
	CHARACTER*8	CRSAOP(3)/'AD200021','AD200022','AD200023'/
	CHARACTER*8	CNCALP/'AL42800P'/
	CHARACTER*8	CRSALP/'AL200000'/
	CHARACTER*8	AGGL2P/'DD600015'/
	CHARACTER*8	AGGL3P/'DV800008'/
	CHARACTER*8	CNCAOP/'AD400088'/
C
	COMMON/CNC_ANVALTBL/CNC_ANVAL
	COMMON/CRS_ANVALTBL/CRS_ANVAL
	COMMON/CONTACTSCNC/CXSTS	!CONC CONTACT STATUS TABLE
C
C	OPEN CONNECTION TO CRUSHER POINT DIRECTORY
C
        status = LIB$SYS_TRNLOG('WESAPI_PDIR_3',
     1			spd_filename_len,spd_filename,,,)
C
	spd_filename(spd_filename_len+1:) = CHAR(0)
C
        access_type = RUNTIME
C
        spd_fd = SPD_open_file(%ref(spd_filename), access_type)
C
	DO 40	J=1,3	!GET THE 3 CRUSHER ANALOG OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(CRSAOP(J)//char(0)),CRSAO(J),
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)

C
40	CONTINUE
C
C	GET THE 1 CRUSHER PACKED GROUP OUTPUT SID
C
	status = SPD_get_sid(spd_fd,%ref(CRSGPP//char(0)),CRSGP,
	1	gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
C	GET THE 1 CRUSHER AL OUTPUT SID
C
	status = SPD_get_sid(spd_fd,%ref(CRSALP//char(0)),CRSAL,
	1	gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
C  Close the point directory connection
C
	status = SPD_close_file(spd_fd)
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
C	GET THE 1 CONCENTRATOR AL INPUT SID
C
	status = SPD_get_sid(spd_fd,%ref(CNCALP//char(0)),CNCAL,
	1	gp_sid, gp_bit_num, extended_flag, inactive_flag)
C
	status = SPD_get_sid(spd_fd,%ref(CNCAOP//char(0)),CNCAO,
	1	gp_sid, gp_bit_num, extended_flag, inactive_flag)
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
		OPEN	(UNIT=22,FILE='ANA:CRSPWR.ERR',STATUS='UNKNOWN',
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
	CALL	SYS$SCHDWK(,,ADRS,ADRS)	!SCHEDULE 2 SEC WAKEUP
C
100	CONTINUE
C
C	READ IN AL RECORD AL42800P FROM CONC HIGHWAY
C
	status = SHC_get_point_record( CNCAL, %REF(ALX))
C
	ALX.PN='AL200000'
C
C	TURN AROUND AND OUTPUT THIS POINT TO CRUSHER HIGHWAY AS POINT AL200000
C
	put_analog_val=ALX.AV	!OUTPUT ANALOG VALUE
C
	status = SHC_put_point_quality(CRSAL, point_quality)
	status = SHC_put_analog_val(CRSAL, put_analog_val)
C
	put_analog_val=ALX.AV-ALX.HL	!DIFF BETWEEN PROJECTED AND LIMIT
	status = SHC_put_analog_val(CNCAO, put_analog_val)
C

	c_FP_val=ALX.HL
	field_name='HL'
	status = SHC_change_FP_attribute( CRSAL, %REF(field_name),c_FP_val)
C
	c_byte_val=1
	field_name='FM'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	GOTO 555
C
	c_byte_val=ALX.RT
	field_name='RT'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.SR
	field_name='SR'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.LU
	field_name='LU'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.AP
	field_name='AP'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.AY
	field_name='AY'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.MM
	field_name='MM'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.NN
	field_name='NN'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.AH
	field_name='AH'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.MN
	field_name='MN'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.CM
	field_name='CM'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_byte_val=ALX.LC
	field_name='LC'
	status = SHC_change_byte_attribute( CRSAL, %REF(field_name),c_byte_val)
C
	c_short_val=ALX.AS
	field_name='AS'
	status = SHC_change_short_attribute(CRSAL,%REF(field_name),c_short_val)
C
	c_short_val=ALX.AW
	field_name='AW'
	status = SHC_change_short_attribute(CRSAL,%REF(field_name),c_short_val)
C
	c_short_val=ALX.DG
	field_name='DG'
	status = SHC_change_short_attribute(CRSAL,%REF(field_name),c_short_val)
C
	c_short_val=ALX.TB
	field_name='TB'
	status = SHC_change_short_attribute(CRSAL,%REF(field_name),c_short_val)
C
	c_short_val=ALX.BB
	field_name='BB'
	status = SHC_change_short_attribute(CRSAL,%REF(field_name),c_short_val)
C
	c_short_val=ALX.CD
	field_name='CD'
	status = SHC_change_short_attribute(CRSAL,%REF(field_name),c_short_val)
C
	c_short_val=ALX.HW
	field_name='HW'
	status = SHC_change_short_attribute(CRSAL,%REF(field_name),c_short_val)
C
	c_short_val=ALX.IP
	field_name='IP'
	status = SHC_change_short_attribute(CRSAL,%REF(field_name),c_short_val)
C
	c_FP_val=ALX.EV
	field_name='EV'
	status = SHC_change_FP_attribute( CRSAL, %REF(field_name),c_FP_val)
C
	c_FP_val=ALX.LL
	field_name='LL'
	status = SHC_change_FP_attribute( CRSAL, %REF(field_name),c_FP_val)
C
	c_FP_val=ALX.IL
	field_name='IL'
	status = SHC_change_FP_attribute( CRSAL, %REF(field_name),c_FP_val)
C
	c_FP_val=ALX.DB
	field_name='DB'
	status = SHC_change_FP_attribute( CRSAL, %REF(field_name),c_FP_val)
C
	c_FP_val=ALX.PV
	field_name='PV'
	status = SHC_change_FP_attribute( CRSAL, %REF(field_name),c_FP_val)
C
	c_FP_val=ALX.HS
	field_name='HS'
	status = SHC_change_FP_attribute( CRSAL, %REF(field_name),c_FP_val)
C
	c_FP_val=ALX.LS
	field_name='LS'
	status = SHC_change_FP_attribute( CRSAL, %REF(field_name),c_FP_val)
C
555	CONTINUE
C
	put_analog_val=CNC_ANVAL(1322)	!MINS LEFT MPL PWR PER TO CRUSHER
	status = SHC_put_point_quality(CRSAO(1), point_quality)
	status = SHC_put_analog_val(CRSAO(1), put_analog_val)
C
	put_analog_val=ALX.HL		!HI LIM FOR PROJECTED POWER
	status = SHC_put_point_quality(CRSAO(2), point_quality)
	status = SHC_put_analog_val(CRSAO(2), put_analog_val)
C
	c_byte_val=1
	field_name='FM'
C
	status = SHC_change_byte_attribute( CRSAO(2), 
	1		 %REF(field_name),c_byte_val)
C
	TEMP=ALX.AS	!SEND MPL PWR AL REC AS FIELD
	IF (CNC_ANVAL(1322) .LE. 12.0)TEMP=IBCLR(TEMP,6) !START AFTER 3 MIN
C
	J=(SUBS/32)+1	!EXTRACT CXSTS LONGWORD
	K=MOD(SUBS,32)	!EXTRACT CORRECT BIT
	BIT=JIBITS(CXSTS(J),K,1)	!EXTRACT SUBSTATION ALARM CONDITION
C
	IF (BIT .EQ. 1)TEMP=IBSET(TEMP,0)	!SUBSTATION IN ALARM
	IF (BIT .EQ. 0)TEMP=IBCLR(TEMP,0)	!SUBSTATION OK
C
	TEMP=IBCLR(TEMP,4)	!CLEAR 3 MIN REMAIN INDICATION
	IF (CNC_ANVAL(1322) .LE. 3.0)TEMP=IBSET(TEMP,4) !< 3 MIN LEFT IN PER
C
	CUT=0
C
	DO 444	I=1,6
		J=(MPL(I)/32)+1			!EXTRACT CXSTS WORD
		K=MOD(MPL(I),32)                !EXTRACT BIT	
		BIT=JIBITS(CXSTS(J),K,1)        !CHECK CONDITION
		CUT=CUT+BIT
444	CONTINUE
C
	TEMP=IBCLR(TEMP,1)
	IF (CUT .NE. 0)TEMP=IBSET(TEMP,1)
C
	IF (CNC_ANVAL(1322) .LT. 10.0)GOTO 200 !LAST 10 MIN WDPF ALARMS
C
	IF (ALX.AV .GE. ALX.HL)THEN
		TEMP=IBSET(TEMP,3)	!WE ARE IN ALARM
	ELSE
		TEMP=IBCLR(TEMP,3)  	!NO WE ARE NOT
	END IF
C
200	CONTINUE
C
	gp_val_mask=TEMP	!SEND INDICATIONS THRU PACKED GROUP
	gp_op_mask=127		!AFFECT BITS 0 THRU 6 (1 111 111)
C
	status = SHC_put_pkd_group_val(CRSGP,gp_op_mask, gp_val_mask)
C
	SUMX=0
C
	DO 150	I=1,43
C
		SUMX=SUMX+CRS_ANVAL(TAB(I))
C
150	CONTINUE
C
	put_analog_val=SUMX		!TOTAL PWR DRAW OF CRUSHER POOL
	status = SHC_put_analog_val(CRSAO(3), put_analog_val)
C
	CALL	SYS$HIBER()	!DELAY TILL WAKEUP
	GOTO 100
C
99999	CONTINUE
C
	CALL	EXIT
C
	END
