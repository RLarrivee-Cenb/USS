C
	PROGRAM 	CRS004B
C
C	THIS PROGRAM CALCULATES THE TONS ON THE 004-01 AND 004-2 BELTS
C	AND SENDS THEM TO THE CRUSHER WDPF
C
C	MODIFIED ON 10-01-03 TO SEND THE SURGE PILE BALANCE TO THE
C	CRUSHER WDPF BY WJB
C
C	FORT/NOOP/NOALIGN/WARN=NOALIGN/FLOAT=D_FLOAT CRS004B
C
C	LINK CRS004B,CRS004B/OPT,WESAPI_LIB:WESAPI/OPT
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
	REAL*4		CRS_ANVAL(1000),SUMX
	REAL*4		TEMPA,TEMPB,TEMPC,TEMPD,TEMPE
	REAL*4		B31,B32,B33,B34,B41,B42,PILE
C
	INTEGER*4	ADRS(2),J,K,BIT,I,CRSAO(3)
	INTEGER*2	TEMP
C
	CHARACTER*8	TIM/'0 0:0:30'/	!30 SEC RUN TIME
C
	CHARACTER*8	CRSAOP(3)/'AD200024','AD200025','AD200028'/
C
	COMMON/CRS_ANVALTBL/CRS_ANVAL
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
		OPEN	(UNIT=22,FILE='ANA:CRS004B.ERR',STATUS='UNKNOWN',
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
	CALL	SYS$SCHDWK(,,ADRS,ADRS)	!SCHEDULE 10 SEC WAKEUP
C
100	CONTINUE
C
	TEMPA=CRS_ANVAL(87)+CRS_ANVAL(90)+CRS_ANVAL(93)+CRS_ANVAL(96)
	1		+CRS_ANVAL(98)
C
	TEMPB=CRS_ANVAL(103)+CRS_ANVAL(106)+CRS_ANVAL(109)+CRS_ANVAL(113)
	1		+CRS_ANVAL(115)+CRS_ANVAL(118)+CRS_ANVAL(121)
	1		+CRS_ANVAL(126)-CRS_ANVAL(102)
C
	TEMPC=CRS_ANVAL(181)-CRS_ANVAL(112)-CRS_ANVAL(178)
C
	TEMPD=CRS_ANVAL(108)-TEMPB
C
	B31=TEMPA-TEMPD-TEMPC
C
	B32=CRS_ANVAL(157)+CRS_ANVAL(160)+CRS_ANVAL(164)+CRS_ANVAL(168)
	1	+CRS_ANVAL(172)-CRS_ANVAL(112)
C
	B33=CRS_ANVAL(102)
C
	B34=CRS_ANVAL(174)
C
	B41=B31+B33
C
C	B42=B32+B34
C
	B42=CRS_ANVAL(174)+CRS_ANVAL(184)	!PER PETE M. 8-15-2002
C
	put_analog_val=B41	!004-01-1 BELT TONS TO CRUSHER
	status = SHC_put_point_quality(CRSAO(1), point_quality)
	status = SHC_put_analog_val(CRSAO(1), put_analog_val)
C
	put_analog_val=B42		!004-02-1 BELT TONS TO CRUSHER
	status = SHC_put_point_quality(CRSAO(2), point_quality)
	status = SHC_put_analog_val(CRSAO(2), put_analog_val)
C
	OPEN	(UNIT=1,FILE='ANA:PIL080.DAT',STATUS='OLD',
	1	RECL=1,RECORDTYPE='FIXED',FORM='UNFORMATTED',
	1	ACCESS='DIRECT')
C
	READ	(1,REC=1)PILE	!READ IN 080 PILE BALANCE
C
	PILE=PILE+CRS_ANVAL(451)+CRS_ANVAL(395)-CRS_ANVAL(452)-CRS_ANVAL(396)
C
	WRITE	(1,REC=1)PILE   !SAVE NEW 080 PILE BALANCE
C
	CLOSE	(UNIT=1)
C
	put_analog_val=PILE		!CURRENT 080 PILE BALANCE
	status = SHC_put_analog_val(CRSAO(3), put_analog_val)
	status = SHC_put_point_quality(CRSAO(3), point_quality)
C
	CALL	SYS$HIBER()	!DELAY TILL WAKEUP
	GOTO 100
C
99999	CONTINUE
C
	CALL	EXIT
C
	END
