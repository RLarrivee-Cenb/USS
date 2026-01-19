C
	PROGRAM CNC4T5
C
C	PROGRAM RUNS EVERY 10 SECS AND MOVES POINTS FROM
C	CONC NET 4 TO CONC NET 5
C
C	FORT/NOOP/NOALIGN/WARN=NOALIGN/FLOAT=D_FLOAT CNC4T5
C
C	LINK CNC4T5,CNC4T5/OPT,WESAPI_LIB:WESAPI/OPT
C
	INCLUDE 'WESAPI:SPD.DCL'
	INCLUDE 'WESAPI:SHC_defines.DCL'
	INCLUDE 'WESAPI:SHC_err.DCL'
C
	INTEGER*4	LIB$SYS_TRNLOG
	INTEGER*4 	status
C
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
C
	REAL            put_analog_val
C
	INTEGER*4	LEN,POINT_QUALITY
C
	REAL*4		CNC_ANVAL(1900)
	INTEGER*4	I,J,K,L,M,N
	INTEGER*4	ADRS(2)
	INTEGER*4	CNCAO(10)
C
	CHARACTER*8	TIM/'0 0:0:10'/
C
	CHARACTER*8	CNCAOP(10)/'AD400150','AD400091','AD400092',
	1			   'AD400146','AD400147','AD400148',
	1			   'AD400141','AD400142','AD400143',
	1			   'AD400144'/
C
	COMMON/CNC_ANVALTBL/CNC_ANVAL
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
	DO	111	I=1,10
C
	     status = SPD_get_sid(spd_fd,%ref(CNCAOP(I)//char(0)),CNCAO(I),
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)

111	CONTINUE
C
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
		OPEN	(UNIT=22,FILE='GP:CNC4T5.ERR',STATUS='UNKNOWN',
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
	c_byte_val=3
	field_name='FM'
	status =SHC_change_byte_attribute(CNCAO(1),%REF(field_name),c_byte_val))
	status = SHC_put_point_quality(CNCAO(1), point_quality)
	status = SHC_put_point_quality(CNCAO(2), point_quality)
	status = SHC_put_point_quality(CNCAO(3), point_quality)
	status = SHC_put_point_quality(CNCAO(4), point_quality)
	status = SHC_put_point_quality(CNCAO(5), point_quality)
	status = SHC_put_point_quality(CNCAO(6), point_quality)
	status = SHC_put_point_quality(CNCAO(7), point_quality)
	status = SHC_put_point_quality(CNCAO(8), point_quality)
	status = SHC_put_point_quality(CNCAO(9), point_quality)
	status = SHC_put_point_quality(CNCAO(10), point_quality)
C
100	CONTINUE
C
	put_analog_val=CNC_ANVAL(1386)	!AMINE LBS/TON
	status = SHC_put_analog_val(CNCAO(1), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1434)	!PRE-CLASS TOTAL COARSE LTPH
	status = SHC_put_analog_val(CNCAO(2), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1435)	!PRE-CLASS TOTAL FINES LTPH
	status = SHC_put_analog_val(CNCAO(3), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1437)	!PRE-CLASS TOTAL FINES LTPH
	status = SHC_put_analog_val(CNCAO(4), put_analog_val)
C
	put_analog_val=CNC_ANVAL(40)	!FLOAT FEED DENSITY PCTSOL
	status = SHC_put_analog_val(CNCAO(5), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1469)	!FLOAT FEED DENSITY SP PCTSOL
	status = SHC_put_analog_val(CNCAO(6), put_analog_val)
C
	put_analog_val=CNC_ANVAL(50)	!022-03-1 RMF LTPH
	status = SHC_put_analog_val(CNCAO(7), put_analog_val)
C
	put_analog_val=CNC_ANVAL(162)	!022-04-1 RMF LTPH
	status = SHC_put_analog_val(CNCAO(8), put_analog_val)
C
	put_analog_val=CNC_ANVAL(190)	!022-05-1 RMF LTPH
	status = SHC_put_analog_val(CNCAO(9), put_analog_val)
C
	put_analog_val=CNC_ANVAL(208)	!022-06-1 RMF LTPH
	status = SHC_put_analog_val(CNCAO(10), put_analog_val)
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
