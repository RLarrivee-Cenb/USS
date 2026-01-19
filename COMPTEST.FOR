REMEMBER,
FORT AND LINK COMPTEST_ORACLE RATHER THAN THIS PROGRAM
AND THEN RENAME IT TO "COMPTEST".
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C			COMPTEST.FOR
C
C
C		LINK COMPTEST,AGDCNC/OPT,WESAPI_LIB:WESAPI/OPT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	implicit none	
C
	INCLUDE '($SSDEF)'
	INCLUDE '($IODEF)'
	INCLUDE 'WESAPI:SPD.DCL'
	INCLUDE 'WESAPI:SHC_DEFINES.DCL'
	INCLUDE 'WESAPI:SHC_ERR.DCL'
C
	integer jj,kk,j,loopcount,it,m,ll
	REAL 		AV,ANVAL(9)
	INTEGER 	CXSTS(100)
C
	integer*4 iiii
C
	CHARACTER MON(12)*3 /'JAN','FEB','MAR','APR','MAY','JUN','JUL',
	1		     'AUG','SEP','OCT','NOV','DEC'/,KY*11
C
C
	INTEGER 	ONSCAN(3)	/1688,1690,1694/,
	1		RDI(3)		/1689,1691,1695/,
C
	1		SAMPLEAN(3)	/1,4,7/,
	1		VALAN(3)	/2,5,8/,
	1		PELAN(3)	/3,6,9/
C
	REAL	 DATA(3,100)
C
	INTEGER  TIMER(2),DTIM
	INTEGER*2 STAT
C
	CHARACTER INTV*6/'0 ::02'/,DAYTIM(3)*23
C
	INTEGER TESTON(3),SAMPLE(3),PELNUM(3),BINTIM(2,3),BINTIME(2,3)
C
C
	STRUCTURE /CT/
C
		CHARACTER*11	KEY		! DATE,SAMPLE NO. YYMMDDSSSRR
		INTEGER		BDAT(2)		! BINARY DATE-TIME (START)
		INTEGER		BDATE(2)	! BINARY DATE-TIME (STOP)
		REAL 		VALUES(100)     ! INDIVIDUAL SAMPLE VALUES
		INTEGER		NUM		! PELLET COUNT FOR THIS SAMPLE
		INTEGER*2	CODE		! 1=STORED IN METREPORT,0=NOT
C
	END STRUCTURE
C
C
	RECORD /CT/ X,Z
c
c
c
c
CCCCCCCCCCCCCCCCCCC
C
C
	INTEGER*4	LIB$SYS_TRNLOG
	INTEGER*4	i
	CHARACTER*10 	choice
	INTEGER*4 	status
	CHARACTER*20	highway

	CHARACTER*40    spd_filename
	INTEGER*2	spd_filename_len
	INTEGER*2	spd_fd
	INTEGER*2	access_type
C
	integer		sid
C
C  Case 2 variables
	REAL		get_analog_val
	INTEGER*2	get_analog_stat
C
C
C  Case 3 variables
	INTEGER*4       gp_sid
	BYTE		gp_bitnum 
	INTEGER*2	digital_val
	INTEGER*2	digital_stat
c
c
	BYTE		extended_flag
	BYTE		gp_bit_num
        BYTE		inactive_flag
c
c  Case 5 variables
	INTEGER*2	get_pkd_group_val
	INTEGER*2	get_pkd_group_stat
	INTEGER*2	gp_force_stat
c
c  Case 10 variables
	INTEGER*2	gp_op_mask
	INTEGER*2	gp_val_mask
c
c
c
	INTEGER SYSID(9),GOOD(9)
	CHARACTER APT*8
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
	COMMON /CONTACTSCNC/CXSTS
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Use the wesapi highway number to form the name of the wesapi logical that
C  names the point directory file and get the file name from the logical
C
           status = LIB$SYS_TRNLOG('WESAPI_PDIR_4',
	1		spd_filename_len,spd_filename,,,)
C
        IF (status .NE. SS$_NORMAL) THEN
	   PRINT *, '  Logical WESAPI_PDIR not defined'
	   stop 'no point dir'
        ENDIF
C
C
C
C  Put a NULL character (0) at the end of the file name string.  This is
C  necessary because the spd library functions expect string arguments
C  to be NULL terminated strings.
	spd_filename(spd_filename_len+1:) = CHAR(0)

C  Call the spd open file function to get access to the point directory file
        access_type = RUNTIME
        spd_fd = SPD_open_file(%ref(spd_filename), access_type)
        IF (spd_fd .LT. 0) THEN
           PRINT *,'  SPD_open_file() Error : ',spd_fd
           STOP
  	ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	  READ IN CX POINT NOS. FROM CXICNC.SRC
C
	OPEN (UNIT=50,FILE='USER_D:[RJM]CTANA.SRC',STATUS='OLD',
	1	SHARED,READONLY)
C
	JJ=0
	KK=1
C
	DO 774 j=1,9
54		READ (50,755,err=442) APT
755		FORMAT (A)
		IF (APT(1:1) .LT. '0') GO TO 54
c
c
C  Call the get sid function.  The point name argument must have a NULL (0)
C  at the end.
           status = SPD_get_sid(spd_fd, %ref(apt//char(0)), sid,
	1		gp_sid, gp_bit_num, extended_flag, inactive_flag)
c
	SYSID(KK)=SID
	KK=KK+1
c
774	continue
C
c
c
c
c
c
c
C  Close the point directory file 
442	status = SPD_close_file(spd_fd)
           IF (status .NE. 0) THEN
    	      PRINT *,'  SPD_close_file() Error : ', status
	      STOP
	   ENDIF
c
c
	status = SHC_open_memory()
c	IF (status .NE. SHC_OK) THEN
c	   PRINT *,' SHC open memory failure - error = ',status
c	   STOP
c	ENDIF
c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
C
C
	DO 44 J=1,3
		TESTON(J)=0
		SAMPLE(J)=0
		PELNUM(J)=0
44	CONTINUE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
1	CALL SYS$HIBER ()  	! WAIT FOR TURN ON FROM CXSCANNERCNC
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
	DO 3004 J=1,9
	STATUS=SHC_GET_ANALOG_VAL_STAT (SYSID(J),GET_ANALOG_VAL,
	1	GET_ANALOG_STAT)
C
c
c
	GOOD(J)=1
	IF (bitest(get_analog_stat,0) .or. bitest(get_analog_stat,8)
	1 .or. bitest(get_analog_stat,9) .or. bitest(get_analog_stat,15))
	1	GOOD(J)=0
C
C
	ANVAL(J)=GET_ANALOG_VAL
C
3004	CONTINUE
C
C
CCCCCCCCCCCCCCC
C
C
C
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c

	LOOPCOUNT=0
C
C
C
C
C
C
C
300	DO 1000 IT=1,3
C
C
C
C
C		
C 
	IF (STAT(ONSCAN(IT)) .EQ. 1) THEN 		! IS SDI UP?
C
C	--------------------   SDI IS UP   -----------------
C
C
		IF (TESTON(IT) .EQ. 1) GO TO 100	!it was ALREADY up, 
C						  	  go look for rdi.
C
c-------------------------  new test is in  ----------------------------------
C
		TESTON(IT)=1	! set indicator that gives last status of sdi
C
C
		PELNUM(IT)=0 			! ZERO OUT PELLET COUNT
C
		DO 66 M=1,100			! zero out data file
66			DATA(IT,M)=-1
C
C
	CALL SYS$ASCTIM (,DAYTIM(IT),,)  ! get ascii date for key when done
C
		IF (DAYTIM(IT)(1:1) .EQ. ' ') DAYTIM(IT)(1:1)='0'
C
C
		CALL SYS$GETTIM (BINTIM(1,IT))     ! save starting time in binary
C
C
C
C		---------------   IS RDI UP   -------------------
C
C
C
100		IF (STAT(RDI(IT)) .EQ. 0) GO TO 1000	!RDI is not up
C
C
C
C
C  	++++++++++++++++    YES ROBERT, THE RDI IS UP     +++++++++++++++++
C
C
		LL=NINT(ANVAL(PELAN(IT)))
C
		IF (LL .LE. 0) GO TO 1000
		IF (LL .GT. 100) GO TO 1000
C
		IF (PELNUM(IT) .EQ. LL) GO TO 1000  !ALREADY READ THIS ONE
C
		if (ll .eq. 1) then
		SAMPLE(IT)=NINT(ANVAL(SAMPLEAN(IT)))	! SAVE SEQ. NUMBER
		end if
c
		PELNUM(IT)=LL
C
		DATA(IT,LL)=ANVAL(VALAN(IT))  ! read the compression value
c
c
		GO TO 1000
C
C
C
	ELSE		! sdi is down
C
C
		IF (TESTON(IT) .EQ. 0) GO TO 1000 	! IT WAS DOWN ALREADY
C
C
C	--------------------     SDI HAS JUST DROPPED    ----------------------
C       --------------------     SAMPLE IS COMPLETED     ----------------------
C
C
		TESTON(IT)=0
C
C
C
		CALL SYS$GETTIM (BINTIME(1,IT))  ! save ending time of sample
C
C
		DO 228 I=1,12
			IF (DAYTIM(IT)(4:6) .EQ. MON(I)) GO TO 229
228		CONTINUE
C
		GO TO 1000	! MONTH BO
C
229		WRITE (KY(3:4), '(I2)') I
C
		IF (KY(3:3) .EQ. ' ')  KY(3:3)='0'
C
		KY(1:6)=DAYTIM(IT)(10:11)//KY(3:4)//DAYTIM(IT)(1:2)
C
		WRITE (KY(7:7),'(I1)') IT
		WRITE (KY(8:9),'(I2)') SAMPLE(IT)
		IF (KY(8:8) .EQ. ' ') KY(8:8)='0'
C
C
C			search for sample. if it exists, increase repeat
c			count by 1 and search again.
C
C
C
		OPEN (UNIT=10,FILE='D_I:COMPTESTS.DAT',ORGANIZATION='INDEXED',
	1		FORM='UNFORMATTED',ACCESS='KEYED',RECL=128,
	1		RECORDTYPE='FIXED',SHARED,KEY=(1:11:CHARACTER),
	1		ERR=1000,STATUS='UNKNOWN')
C
C
C
C
		DO 77 J=1,99
C
		WRITE (KY(10:11),'(I2)') J
		IF (KY(10:10) .EQ. ' ') KY(10:10)='0'
C
		READ (10,KEY=KY,IOSTAT=M) X
C
		IF (M .NE. 36) GO TO 77	! FILE ALREADY EXISTS,UP REPEAT BY 1
C
C
C
C
C  ++++++++++++++++++++++   STORE DATA IN INDEXED FILE  ++++++++++++++++++++++
C
		X=Z
C
		X.KEY=KY
		X.BDAT(1)=BINTIM(1,IT)
		X.BDAT(2)=BINTIM(2,IT)
		X.BDATE(1)=BINTIME(1,IT)
		X.BDATE(2)=BINTIME(2,IT)
		X.NUM=PELNUM(IT)
C
		DO 80 I=1,100
C
80			X.VALUES(I)=DATA(IT,I)
C
C
		WRITE (10) X
		CLOSE (10)
		GO TO 1000
C
C
77		CONTINUE
C
CCCCCCCCCCCCCCCC
C
	END IF						! IS SDI UP?
c
CCCCCCCCCCCCCCCC
C
C
C
1000	CONTINUE
C
C
C
ccccccccccccccccccccccc
C
C	   pass thru both testers one more time before hibernate in case
C	another sample came up.
C
	LOOPCOUNT=LOOPCOUNT+1
C
	IF (LOOPCOUNT .LT. 2) GO TO 300
C
	GO TO 1
C
C
ccccccccccccccccccccccc
C
C
C
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C	function to check open or close on a contact
C	returns .TRUE. or .FALSE.
C
	FUNCTION STAT(J)
	INTEGER CXSTS(100)
 	INTEGER*2 STAT
	COMMON /CONTACTSCNC/CXSTS
	K=J/32+1
	L=MOD(J,32)
	IF (BJTEST(CXSTS(K),L)) THEN
		STAT=1
	ELSE
		STAT=0
	END IF
	END
