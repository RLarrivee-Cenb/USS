C
	PROGRAM	TRIPPER
C
C	FORT/NOOP/NOALIGN/WARN=NOALIGN/FLOAT=D_FLOAT TRIPPER
C
C	LINK TRIPPER,TRIPPER/OPT,WESAPI_LIB:WESAPI/OPT
C
C	THIS PROGRAM SENDS TO THE CRUSHER WDPF THE RUN INDICATIONS
C	FOR CONC LINES 3 THRU 18 FOR 21 BELT 22 BELT AND ROD MILL
C	FOR USE IN CONTROLLING THE 20 BELT TRIPPERS
C
C	THE PROGRAM ALSO SENDS THE STEP I-II TOTAL RMF LTPH AND
C	THE STEP III TOTAL RMF LTPH FOR DISPLAY BY TRIPPER GRAPHICS
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
	INTEGER*2       put_op_mask
	INTEGER*2       put_val_mask
	INTEGER*2       TOGGLE
	BYTE		gp_bitnum 
	INTEGER*2	digital_val
	INTEGER*2	digital_stat
C
	REAL            put_analog_val
C
	INTEGER*4	LEN,POINT_QUALITY
	INTEGER*4	STONE(6)/1773,1778,1774,1775,1776,1777/
C
	CHARACTER*8	TIME/'0 0:0:20'/
C
	INTEGER*4	B211(16)/34,42,48,54,60,74,88,102,116,
	1			130,144,154,164,174,184,194/
	INTEGER*4	B212(16)/36,44,50,56,64,78,92,106,120,
	1			134,147,157,167,177,187,197/
	INTEGER*4	B213(16)/38,46,52,58,68,82,96,110,124,
	1			138,150,160,170,180,190,200/
	INTEGER*4	B214(16)/40,47,53,59,71,85,99,113,127,
	1			141,152,162,172,182,192,202/
	INTEGER*4	B22X(16)/205,209,213,217,221,225,229,233,
	1			237,241,245,250,255,260,265,270/
	INTEGER*4	RMCX(16)/311,315,319,323,327,331,335,339,
	1			343,347,351,355,359,363,367,371/
C
	INTEGER*4	CXSTS(100),PWR(2)/2368,2369/
	INTEGER*4	CXSTS_CRS(100),P170
	INTEGER*2	TEMP,X
	INTEGER*4	CNCAO(64),AO(6),AG3AO(8),CRSAO(25)
	INTEGER*4	CRSGPR(19),CNCGP(20),CRSGP(7)
C
	INTEGER*4	I,J,K,ADDRESS(2),BIT
	INTEGER*4	S12,S3,AL1,AL2,CARS(4),CARST
	INTEGER*4	BIN(45)/325,326,327,328,329,330,331,332,333,
	1			334,335,336,337,338,339,341,342,343,
	1			344,345,346,347,348,349,350,351,352,
	1			353,354,355,411,412,413,414,415,416,
	1			417,418,419,420,421,422,423,424,425/
	INTEGER*4		UFO_OPEN,B91,B92,B94,B95
C
	REAL*4		CNC_ANVAL(1900)
	REAL*4		CRS_ANVAL(1000)
	REAL*4		AG3_ANVAL(1000)
	REAL*4		ANVAL(1200),MIN(4),MINT
	REAL*4		S12WT(2),S1CLTPH
C
	CHARACTER*8	P17034T/'DL423000'/
C
	CHARACTER*8	CRSGPP(7)/'GP200001','GP200002','GP200003',
	1			  'GP200004','GP200005','GP200006',
	1			  'GP200007'/
C
	CHARACTER*8	CNCGPP(20)/'GP400001','GP400002','GP400003',
	1			   'GP400004','GP400005','GP400006',
	1			   'GP400007','GP400008','GP400009',
	1			   'GP400010','GP400011','GP400012',
	1			   'GP400013','GP400014','GP400015',
	1			   'GP400016','GP400017','GP400018',
	1			   'GP400019','GP400022'/
C
	CHARACTER*8	CRSGPRP(19)/'GP221024','GP221025','GP221030',
	1			    'GP221031','GP221033','GP221035',
	1			    'GP221012','GP221013','GP221018',
	1			    'GP221019','GP221021','GP221023',
	1			    'GP222015','GP222012','GP222013',
	1			    'GP222023','GP222020','GP222021',
	1			    'GP221034'/
C
	CHARACTER*8	CRSAOP(25)/'AD200001','AD200002','AD200003','AD200004',
	1		           'AD200005','AD200006','AD200007','AD200008',
	1			   'AD200009','AD200010','AD200011','AD200012',
	1			   'AD200013','AD200014','AD200015','AD200016',
	1			   'AD200018','AD200019','AD200020','AD200060',
	1			   'AD200061','AD200062','AD200063','AD200026',
	1			   'AD200027'/
C
	CHARACTER*8	AOP(6)/'AD600018','AD600019','AD600060','AD600061',
	1		       'AD600062','AD600063'/
C
	CHARACTER*8	AG3AOP(8)/'AV800060','AV800061','AV800062','AV800063',
	1			  'AV800077','AV800078','AV800079','AV800080'/
C
	CHARACTER*8	CNCAOP(64)/'AD400121','AD400122','AD400123','AD400124',
	1			   'AD400125','AD400126','AD400127','AD400128',
	1			   'AD400009','AD400010','AD400011','AD400012',
	1			   'AD400013','AD400014','AD400015','AD400016',
	1			   'AD400017','AD400018','AD400019','AD400020',
	1			   'AD400021','AD400022','AD400023','AD400024',
	1			   'AD400025','AD400026','AD400027','AD400028',
	1			   'AD400029','AD400030','AD400031','AD400032',
	1			   'AD400033','AD400034','AD400035','AD400036',
	1			   'AD400037','AD400038','AD400039','AD400040',
	1			   'AD400041','AD400042','AD400043','AD400044',
	1			   'AD400045','AD400046','AD400047','AD400048',
	1			   'AD400049','AD400050','AD400051','AD400052',
	1			   'AD400053','AD400054','AD400080','AD400081',
	1			   'AD400082','AD400083','AD400089','AD400090',
	1			   'AD400091','AD400092','AD400098','AD400099'/
C
	COMMON/CONTACTSCNC/CXSTS	!CONTACT STATUS TABLE
	COMMON/CNC_ANVALTBL/CNC_ANVAL
	COMMON/CRS_ANVALTBL/CRS_ANVAL
	COMMON/AG3_ANVALTBL/AG3_ANVAL
	COMMON/ANVALTBL/ANVAL	
	COMMON/CONTACTSCRS/CXSTS_CRS	
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
	DO 10	J=1,64	!GET THE 64 CONCENTRATOR ANALOG OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(CNCAOP(J)//char(0)),CNCAO(J),
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)

C
10	CONTINUE
C
	DO 15	J=1,20	!GET THE 20 CONCENTRATOR PACKED GROUP OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(CNCGPP(J)//char(0)),CNCGP(J),
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)

C
15	CONTINUE
C
C	GET SID FOR INDICATION OF 170-06-3T OR 4T RUNNING
C
	        status = SPD_get_sid(spd_fd,%ref(P17034T//char(0)),P170,
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)
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
	DO 20	J=1,6	!GET THE 6 AGGL 1-2 ANALOG OUTPUT SIDS
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
	DO 30	J=1,8	!GET THE 8 AGGL 3 ANALOG OUTPUT SIDS
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
	DO 40	J=1,25	!GET THE 25 CRUSHER ANALOG OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(CRSAOP(J)//char(0)),CRSAO(J),
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)

C
40	CONTINUE
C
	DO 50	J=1,19	!GET THE 19 CRUSHER PACKED GROUP RECEIVE SIDS
C
	        status = SPD_get_sid(spd_fd,
	1		%ref(CRSGPRP(J)//char(0)),CRSGPR(J),
	1		gp_sid, gp_bit_num, extended_flag, inactive_flag)

C
50	CONTINUE
C
	DO 60	J=1,7	!GET THE79 CRUSHER PACKED GROUP OUTPUT SIDS
C
	        status = SPD_get_sid(spd_fd,%ref(CRSGPP(J)//char(0)),CRSGP(J),
     x		             gp_sid, gp_bit_num, extended_flag, inactive_flag)

C
60	CONTINUE
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
		OPEN	(UNIT=22,FILE='GP:TRIPPER.ERR',STATUS='UNKNOWN',
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
	status = SHC_put_point_quality(CRSAO(18), point_quality)
	status = SHC_put_point_quality(CRSAO(19), point_quality)
	status = SHC_put_point_quality(CRSAO(20), point_quality)
	status = SHC_put_point_quality(CRSAO(21), point_quality)
	status = SHC_put_point_quality(CRSAO(22), point_quality)
	status = SHC_put_point_quality(CRSAO(23), point_quality)
	status = SHC_put_point_quality(AG3AO(8), point_quality)
C
	field_name='TB'
	c_short_val=100
C
	DO 66	I=1,8
           status = SHC_change_short_attribute( CNCAO(I), 
     x				%REF(field_name),c_short_val)
66	CONTINUE
C
	CALL	SYS$BINTIM(TIME,ADDRESS)
	CALL	SYS$SCHDWK(,,ADDRESS,ADDRESS)
C
100	CONTINUE
C
	TEMP=0
C
	DO 150 I=1,16
		X=I
		J=(B211(I)/32)+1	!EXTRACT CXSTS LONGWORD
		K=MOD(B211(I),32)	!EXTRACT CORRECT BIT
		BIT=JIBITS(CXSTS(J),K,1)	!EXTRACT BIT CONDITION
		IF (BIT .EQ. 1)THEN
			TEMP=IBSET(TEMP,X-1)
		END IF
150	CONTINUE
C
	gp_op_mask=65535	 !AFFECT ALL BITS
	gp_val_mask=TEMP	!ALL 16 21-1 BELT INDICATIONS
C
	status = SHC_put_pkd_group_val(CRSGP(1),gp_op_mask, gp_val_mask)
C
	TEMP=0
C
	DO 200 I=1,16
		X=I
		J=(B212(I)/32)+1	!EXTRACT CXSTS LONGWORD
		K=MOD(B212(I),32)	!EXTRACT CORRECT BIT
		BIT=JIBITS(CXSTS(J),K,1)	!EXTRACT BIT CONDITION
		IF (BIT .EQ. 1)THEN
			TEMP=IBSET(TEMP,X-1)
		END IF
200	CONTINUE
C
C
	gp_op_mask=65535	 !AFFECT ALL BITS
	gp_val_mask=TEMP	!ALL 16 21-2 BELT INDICATIONS
C
	status = SHC_put_pkd_group_val(CRSGP(2),gp_op_mask, gp_val_mask)
C
	TEMP=0
C
	DO 250 I=1,16
		X=I
		J=(B213(I)/32)+1	!EXTRACT CXSTS LONGWORD
		K=MOD(B213(I),32)	!EXTRACT CORRECT BIT
		BIT=JIBITS(CXSTS(J),K,1)	!EXTRACT BIT CONDITION
		IF (BIT .EQ. 1)THEN
			TEMP=IBSET(TEMP,X-1)
		END IF
250	CONTINUE
C
	gp_op_mask=65535	 !AFFECT ALL BITS
	gp_val_mask=TEMP	!ALL 16 21-3 BELT INDICATIONS
C
	status = SHC_put_pkd_group_val(CRSGP(3),gp_op_mask, gp_val_mask)
C
	TEMP=0
C
	DO 300 I=1,16
		X=I
		J=(B214(I)/32)+1	!EXTRACT CXSTS LONGWORD
		K=MOD(B214(I),32)	!EXTRACT CORRECT BIT
		BIT=JIBITS(CXSTS(J),K,1)	!EXTRACT BIT CONDITION
		IF (BIT .EQ. 1)THEN
			TEMP=IBSET(TEMP,X-1)
		END IF
300	CONTINUE
C
	gp_op_mask=65535	 !AFFECT ALL BITS
	gp_val_mask=TEMP	!ALL 16 21-4 BELT INDICATIONS
C
	status = SHC_put_pkd_group_val(CRSGP(4),gp_op_mask, gp_val_mask)
C
	TEMP=0
C
	DO 350 I=1,16
		X=I
		J=(B22X(I)/32)+1	!EXTRACT CXSTS LONGWORD
		K=MOD(B22X(I),32)	!EXTRACT CORRECT BIT
		BIT=JIBITS(CXSTS(J),K,1)	!EXTRACT BIT CONDITION
		IF (BIT .EQ. 1)THEN
			TEMP=IBSET(TEMP,X-1)
		END IF
350	CONTINUE
C
	gp_op_mask=65535	 !AFFECT ALL BITS
	gp_val_mask=TEMP	!ALL 16 22 BELT INDICATIONS
C
	status = SHC_put_pkd_group_val(CRSGP(5),gp_op_mask, gp_val_mask)
C
	TEMP=0
C
	DO 400 I=1,16
		X=I
		J=(RMCX(I)/32)+1	!EXTRACT CXSTS LONGWORD
		K=MOD(RMCX(I),32)	!EXTRACT CORRECT BIT
		BIT=JIBITS(CXSTS(J),K,1)	!EXTRACT BIT CONDITION
		IF (BIT .EQ. 1)THEN
			TEMP=IBSET(TEMP,X-1)
		END IF
400	CONTINUE
C
	gp_op_mask=65535	 !AFFECT ALL BITS
	gp_val_mask=TEMP	!ALL 16 ROD MILL INDICATIONS
C
	status = SHC_put_pkd_group_val(CRSGP(6),gp_op_mask, gp_val_mask)
C
	status = SHC_get_pkd_group_val_stat(CRSGP(7), 
	1		get_pkd_group_val,
	1		get_pkd_group_stat, gp_force_stat)
C
	TOGGLE=IBITS(get_pkd_group_val,0,1)	!EXTRACT TOGGLE BIT 
C
	IF (TOGGLE .EQ. 0)THEN   !toggle computer acknowledge
		gp_val_mask=1
	ELSE
		gp_val_mask=0
	END IF
C
C	MERGE IN ALL LIMESTONE INDICATIONS (ROD MILL,22 BELT, AND 21 BELTS)
C
	DO 425 I=1,6
		J=(STONE(I)/32)+1	!EXTRACT CXSTS LONGWORD
		K=MOD(STONE(I),32)	!EXTRACT CORRECT BIT
		BIT=JIBITS(CXSTS(J),K,1)	!EXTRACT BIT CONDITION
		IF (BIT .EQ. 1)THEN
			gp_val_mask=IBSET(gp_val_mask,I)
		END IF
425	CONTINUE
C
	status = SHC_put_pkd_group_val(CRSGP(7),gp_op_mask, gp_val_mask)
C
	put_analog_val=CNC_ANVAL(1680)	!STEP I-II RMF LTPH TO CRUSHER
	status = SHC_put_analog_val(CRSAO(1), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1659)	!STEP III  RMF LTPH TO CRUSHER
	status = SHC_put_analog_val(CRSAO(2), put_analog_val)
C
C	put_analog_val=AG3_ANVAL(572)	!STEP III  203 LTPH TO AGGL1-2
C	status = SHC_put_analog_val(AO(2), put_analog_val)
C
	put_analog_val=AG3_ANVAL(573)	!STEP III  PCT CALCIUM TO AGGL1-2
C	status = SHC_put_analog_val(AO(1), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1680)	!STEP I-II RMF LTPH TO CONC
	status = SHC_put_analog_val(CNCAO(54), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1659)	!STEP III  RMF LTPH TO CONC
	status = SHC_put_analog_val(CNCAO(53), put_analog_val)
C
	put_analog_val=CRS_ANVAL(456)	!STEP I-II AVG 100 BIN LVL
	status = SHC_put_analog_val(CNCAO(46), put_analog_val)
C
	put_analog_val=CRS_ANVAL(460)	!STEP I-II AVG 100 BIN RUN LVL
	status = SHC_put_analog_val(CNCAO(47), put_analog_val)
C
	put_analog_val=CRS_ANVAL(461)	!STEP I-II AVG 100 BIN CTL LVL
	status = SHC_put_analog_val(CNCAO(48), put_analog_val)
C
	put_analog_val=CRS_ANVAL(457)	!STEP III AVG 100 BIN LVL
	status = SHC_put_analog_val(CNCAO(49), put_analog_val)
C
	put_analog_val=CRS_ANVAL(462)	!STEP III AVG 100 BIN RUN LVL
	status = SHC_put_analog_val(CNCAO(51), put_analog_val)
C
	put_analog_val=CRS_ANVAL(463)	!STEP I-II AVG 100 BIN CTL LVL
	status = SHC_put_analog_val(CNCAO(52), put_analog_val)
C
	put_analog_val=CRS_ANVAL(455)	!LIMESTONE AVG 100 BIN LVL
	status = SHC_put_analog_val(CNCAO(50), put_analog_val)
C
	put_analog_val=CRS_ANVAL(108)	!009-01-1 LTPH
	status = SHC_put_analog_val(CNCAO(55), put_analog_val)
C
	put_analog_val=CRS_ANVAL(181)	!009-02-1 LTPH
	status = SHC_put_analog_val(CNCAO(56), put_analog_val)
C
	put_analog_val=CRS_ANVAL(233)	!009-04-1 LTPH
	status = SHC_put_analog_val(CNCAO(57), put_analog_val)
C
	put_analog_val=CRS_ANVAL(271)	!009-05-1 LTPH
	status = SHC_put_analog_val(CNCAO(58), put_analog_val)
C
	put_analog_val=ANVAL(14)		!205-01-1 FLUX TANK LEVEL
	status = SHC_put_analog_val(CRSAO(7), put_analog_val)
C
	put_analog_val=ANVAL(28)		!205-01-2 FLUX TANK LEVEL
	status = SHC_put_analog_val(CRSAO(8), put_analog_val)
C
	put_analog_val=ANVAL(37)		!AGL12 #3 TANK LEVEL
	status = SHC_put_analog_val(CRSAO(3), put_analog_val)
C
	put_analog_val=ANVAL(708)		!AGL12 #4 TANK LEVEL
	status = SHC_put_analog_val(CRSAO(4), put_analog_val)
C
	put_analog_val=AG3_ANVAL(353)		!AGL3  #1 TANK LEVEL
	status = SHC_put_analog_val(CRSAO(5), put_analog_val)
C
	put_analog_val=AG3_ANVAL(389)		!AGL3  #2 TANK LEVEL
	status = SHC_put_analog_val(CRSAO(6), put_analog_val)
C
C
	DO 500	I=1,45
		put_analog_val=CRS_ANVAL(BIN(I))	!100 BIN LEVELS
		status = SHC_put_analog_val(CNCAO(I), put_analog_val)
500	CONTINUE
C
	DO 600	I=1,19	!OUTPUT 19 CRS PACKED GROUP POINTS TO CNC
C
		status = SHC_get_pkd_group_val_stat(CRSGPR(I), 
	1		get_pkd_group_val,
	1		get_pkd_group_stat, gp_force_stat)
C
		gp_op_mask=65535	!affect all bits
		gp_val_mask=get_pkd_group_val !crusher value to conc 
C
		status = SHC_put_pkd_group_val(CNCGP(I), 
	1			gp_op_mask, gp_val_mask)
C
600	CONTINUE
C
	TEMP=0
C
	J=(PWR(1)/32)+1	!EXTRACT CXSTS LONGWORD
	K=MOD(PWR(1),32)!EXTRACT CORRECT BIT
	BIT=JIBITS(CXSTS_CRS(J),K,1)	!EXTRACT BIT CONDITION
	IF (BIT .EQ. 1)THEN
		TEMP=IBSET(TEMP,0)	!PWR MNGMNT BYPASS/NOBYPASS
	END IF
C
	J=(PWR(2)/32)+1	!EXTRACT CXSTS LONGWORD
	K=MOD(PWR(2),32)!EXTRACT CORRECT BIT
	BIT=JIBITS(CXSTS_CRS(J),K,1)	!EXTRACT BIT CONDITION
	IF (BIT .EQ. 1)THEN
		TEMP=IBSET(TEMP,1)	!PWR MNGMNT ON/OFF
	END IF
C
	status = SHC_get_digital_val_stat(P170,gp_sid,gp_bitnum,
	1	digital_val, digital_stat)	!SEE IF 170-06-3T OR 4T RUNNING
C
	BIT=digital_val
C
	IF (BIT .EQ. 1)THEN
		TEMP=IBSET(TEMP,2)	!170-06-3T OR 4T IS RUNNING
	END IF
C
	gp_op_mask=65535	 !AFFECT ALL BITS
	gp_val_mask=TEMP	 !PWR MNGMNT INDICATIONS
C
	status = SHC_put_pkd_group_val(CNCGP(20), 
	1		gp_op_mask, gp_val_mask)
C
C	OPEN	(UNIT=1,FILE='RRDH:MINCARX.DAT',ACCESS='DIRECT',
C	1		RECL=8,RECORDTYPE='FIXED',SHARED,
C	1		FORM='UNFORMATTED',STATUS='OLD',USEROPEN=UFO_OPEN)
C
C	READ	(1,REC=1)MIN,CARS
C
C	CLOSE	(UNIT=1)
C
C	MINT=0
C	CARST=0
C
C	DO 700	I=1,3
C
C		MINT=MINT+MIN(I)	!ACCUM MINUTES
C		CARST=CARST+CARS(I)	!ACCUM CARS
C
C		IF (CARS(I) .EQ. 0)THEN
C			put_analog_val=0		!NO CARS
C		ELSE
C			put_analog_val=MIN(I)/CARS(I)	!MINUTES/CAR
C		END IF
C
C		status = SHC_put_analog_val(CRSAO(I+8), put_analog_val)
C
C		put_analog_val=CARS(I)	!CARS DUMPED TO CRUSHER WDPF
C		status = SHC_put_analog_val(CRSAO(I+13), put_analog_val)
C
C		put_analog_val=CARS(I)	!CARS DUMPED TO CONC WDPF
C		status = SHC_put_analog_val(CNCAO(I+58), put_analog_val)
C
C700	CONTINUE
C
C	IF (CARST .EQ. 0)THEN
C		put_analog_val=0
C	ELSE
C		put_analog_val=MINT/CARST	!TOTAL MIN/CAR
C	END IF
C
C	status = SHC_put_analog_val(CRSAO(13), put_analog_val)
C
C	put_analog_val=CARST	!TOTAL CARS DUMPED
C
C	status = SHC_put_analog_val(CRSAO(17), put_analog_val)
C	status = SHC_put_analog_val(CNCAO(62), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1433)	!000-06-0 PSI SIZE
	status = SHC_put_analog_val(CNCAO(59), put_analog_val)
	put_op_mask=65535
	put_val_mask=0
	status = SHC_put_point_stat(CNCAO(59), put_op_mask, put_val_mask)
C
	S1CLTPH=CNC_ANVAL(1583)+CNC_ANVAL(1717)	!L3 AND L5 CONC LTPH
C	S12WT(1)=(CNC_ANVAL(1663)*CNC_ANVAL(1660)+      !CALC S12 CONC
C	1	  CNC_ANVAL(1664)*CNC_ANVAL(1661))	!WT REC
	S12WT(1)=(CNC_ANVAL(1663)*S1CLTPH+      !CALC S12 CONC
	1	  CNC_ANVAL(1664)*CNC_ANVAL(1661))	!WT REC
C
C	S12WT(2)=CNC_ANVAL(1660)+CNC_ANVAL(1661)	!S12 TOTAL TONS
	S12WT(2)=S1CLTPH+CNC_ANVAL(1661)	!S12 TOTAL TONS
C
	IF (S12WT(2) .EQ. 0)THEN
C
		put_analog_val=0
C
	ELSE
C
		put_analog_val=S12WT(1)/S12WT(2)	!S12 CONC WT REC
C
	END IF
C
	status = SHC_put_analog_val(CRSAO(18), put_analog_val)
	status = SHC_put_analog_val(CNCAO(63), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1665)	!S3 CONC WT REC
C
	status = SHC_put_analog_val(CRSAO(19), put_analog_val)
	status = SHC_put_analog_val(CNCAO(64), put_analog_val)
C
	put_analog_val=CNC_ANVAL(1211)	!MINNESOTA POWER DRAW
C
	status = SHC_put_analog_val(CRSAO(12), put_analog_val)
C
	put_analog_val=CNC_ANVAL(845)	!POWER PROJECTED DEMAND TO CRUSHER
	status = SHC_put_analog_val(CRSAO(20), put_analog_val)
C
	put_analog_val=CNC_ANVAL(846)	!POWER 1 MIN AVG        TO CRUSHER
	status = SHC_put_analog_val(CRSAO(21), put_analog_val)
C
	put_analog_val=CNC_ANVAL(847)	!POWER 15 MIN RESET     TO CRUSHER
	status = SHC_put_analog_val(CRSAO(22), put_analog_val)
C
	put_analog_val=CNC_ANVAL(848)	!POWER ACTUAL LIMIT     TO CRUSHER
	status = SHC_put_analog_val(CRSAO(23), put_analog_val)
C
	put_analog_val=CNC_ANVAL(845)	!POWER PROJECTED DEMAND TO AGGL1-2
	status = SHC_put_analog_val(AO(3), put_analog_val)
C
	put_analog_val=CNC_ANVAL(846)	!POWER 1 MIN AVG        TO AGGL1-2
	status = SHC_put_analog_val(AO(4), put_analog_val)
C
	put_analog_val=CNC_ANVAL(847)	!POWER 15 MIN RESET     TO AGGL1-2
	status = SHC_put_analog_val(AO(5), put_analog_val)
C
	put_analog_val=CNC_ANVAL(848)	!POWER ACTUAL LIMIT     TO AGGL1-2
	status = SHC_put_analog_val(AO(6), put_analog_val)
C
	put_analog_val=CNC_ANVAL(845)	!POWER PROJECTED DEMAND TO AGGL3
	status = SHC_put_analog_val(AG3AO(1), put_analog_val)
C
	put_analog_val=CNC_ANVAL(846)	!POWER 1 MIN AVG        TO AGGL3
	status = SHC_put_analog_val(AG3AO(2), put_analog_val)
C
	put_analog_val=CNC_ANVAL(847)	!POWER 15 MIN RESET     TO AGGL3
	status = SHC_put_analog_val(AG3AO(3), put_analog_val)
C
	put_analog_val=CNC_ANVAL(848)	!POWER ACTUAL LIMIT     TO AGGL3
	status = SHC_put_analog_val(AG3AO(4), put_analog_val)
C
	put_analog_val=ANVAL(28)	!205-01-2 FLUX TANK LVL TO AGGL3
	status = SHC_put_analog_val(AG3AO(5), put_analog_val)
C
	put_analog_val=ANVAL(14)	!205-01-1 FLUX TANK LVL TO AGGL3
	status = SHC_put_analog_val(AG3AO(6), put_analog_val)
C
	put_analog_val=ANVAL(694)	!203-01/02 TOTAL LTPH   TO AGGL3
	status = SHC_put_analog_val(AG3AO(7), put_analog_val)
C
	put_analog_val=ANVAL(39)	!000-00-0 FUEL OIL TANK TO AGGL3
	status = SHC_put_analog_val(AG3AO(8), put_analog_val)
C
	put_analog_val=CRS_ANVAL(108)+CRS_ANVAL(181)+CRS_ANVAL(497)-
	1		CRS_ANVAL(495)-CNC_ANVAL(1680)
	status = SHC_put_analog_val(CRSAO(24), put_analog_val) !I=II 100'S
C
	put_analog_val=CRS_ANVAL(271)+CRS_ANVAL(233)+CRS_ANVAL(495)-
	1		CRS_ANVAL(497)-CRS_ANVAL(502)-CNC_ANVAL(1659)
	status = SHC_put_analog_val(CRSAO(25), put_analog_val) !III 100'S
C

	CALL	SYS$HIBER()
	GOTO 100
C
99999	CONTINUE
C
	CALL	EXIT
C
	END
