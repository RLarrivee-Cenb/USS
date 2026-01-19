C
	PROGRAM		AGIT
C
C	(AGGL12 FLUX TANK AGITATOR MCX TO AGGL3)
C
C
C	LINK AGIT,AGIT/OPT,WESAPI_LIB:WESAPI/OPT
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
	INTEGER*2	length
	CHARACTER*2	field_name
	CHARACTER*8	pt_name
	CHARACTER*80	c_ascii_val
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
	BYTE		gp_bitnum 
	INTEGER*2	digital_val
	INTEGER*2	digital_stat
	INTEGER*2	TIMEOUT
	INTEGER*2	AS
C
	REAL            put_analog_val
	INTEGER*2	PUT_DIGITAL_VAL
C
	INTEGER*4	UFO_OPEN
	INTEGER*4	LEN,POINT_QUALITY
	INTEGER*4	DO,MCX/1536/
	INTEGER*4	CXSTS(100)
	INTEGER*4	I,J,K,L,M,N,ADDRESS(2),BIT
C
	CHARACTER*23	DATETIME
	CHARACTER*8	TIME/'0 0:0:10'/
C
	CHARACTER*8	DOP/'DV800016'/
C
C
	COMMON/CONTACTS/CXSTS		!CONTACT STATUS TABLE
C
5	CONTINUE
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
C	GET SID FOR DIGITAL OUTPUT (AGITTATOR MCX)
C
	status = SPD_get_sid(spd_fd,%ref(DOP//char(0)),DO,
     x		gp_sid, gp_bit_num, extended_flag, inactive_flag)
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
		OPEN	(UNIT=22,FILE='ANA:AGIT.ERR',STATUS='UNKNOWN',
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
	CALL	SYS$BINTIM(TIME,ADDRESS)
	CALL	SYS$SCHDWK(,,ADDRESS,ADDRESS)
C
	field_name='ED'
	length=30
	c_ascii_val='205-01-2 AGITATOR MCX         '
C
	STATUS=	SHC_CHANGE_ASCII_ATTRIBUTE(DO,%REF(FIELD_NAME),
	1		LENGTH,%REF(C_ASCII_VAL))
100	CONTINUE
C
		J=(MCX/32)+1	!EXTRACT CXSTS LONGWORD
		K=MOD(MCX,32)	!EXTRACT CORRECT BIT
		BIT=JIBITS(CXSTS(J),K,1)	!EXTRACT BIT CONDITION
C
5555	CONTINUE
C
	put_digital_val=BIT	!INDICATION FO AGITATOR MCX
C
	status = SHC_put_point_quality(DO, point_quality)
	status = SHC_put_digital_val( DO, put_digital_val)
C
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
C
