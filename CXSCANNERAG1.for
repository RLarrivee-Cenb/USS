C*****************************************************************
C                                                                *
C		CXSCANNER.FOR                                    *
C                                                                *
C*****************************************************************
C
C	THIS PROGRAM SCANS THE DIGITAL INPUT POINTS EVERY SECOND
C	AND LOADS THE STATUS INTO COMMON /CXSTS/.  IT GETS ITS
C	DIGITAL POINT NUMBERS FROM THE COMMON SDXR_00
C
C
C	LINK:  CXSCANNERAG1,DIGCOM/OPT,WESAPI_LIB:WESAPI/OPT
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
	PROGRAM CXSCANNERAG1
C
	INCLUDE '($IODEF)'
	INCLUDE '($SSDEF)'
	INCLUDE 'WESAPI:SPD.DCL'
	INCLUDE 'WESAPI:SHC_DEFINES.DCL'
	INCLUDE 'WESAPI:SHC_ERR.DCL'
C
C
C
C
	INTEGER TIMER(2),SYS$BINTIM,SYS$SCHDWK,SYS$HIBER,MBX,
	1	SYS$WAKE,SYS$ASCEFC,SYS$READEF
C
	CHARACTER*6 PT*1,PT2,ALARM /'0 ::10'/  ! DELAY TIME
	CHARACTER*5 RELPR/'RELCX'/,CLUSTER*3/'CXI'/

C
	INTEGER CXSTS(100),CN,CV,CR,JJ,
	2	CXALRM(100),CREP,DALARMS(100)
C
C
	INTEGER*2 BO,D(1680),J,Q
C	
C
	LOGICAL PSTAT,LSTAT
C
C
	INTEGER LEN,
	1	SYS$QIOW,SYS$ASSIGN,LIB$WAIT
C
C
C
C
C
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
	CHARACTER APT*8
	INTEGER SYSID(1680)
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	COMMON  /CONTACTS/CXSTS,/CXALARM/CXALRM,
	1	/DIG_ALARMS/ DALARMS
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Use the wesapi highway number to form the name of the wesapi logical that
C  names the point directory file and get the file name from the logical
C
           status = LIB$SYS_TRNLOG('WESAPI_PDIR_0',
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
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	  READ IN CX POINT NOS. FROM CXI.SRC
C
	OPEN (UNIT=50,FILE='USER_D:[RJM]CXI.SRC',STATUS='OLD',
	1	SHARED,READONLY)
C
	JJ=0
	KK=1
C
	DO WHILE (JJ .EQ. 0)
54		READ (50,55,IOSTAT=JJ,ERR=1313) APT
55		FORMAT (A)
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
	END DO
C
1313	close (50)
c
C  Close the point directory file 
           status = SPD_close_file(spd_fd)
           IF (status .NE. 0) THEN
    	      PRINT *,'  SPD_close_file() Error : ', status
	      STOP
	   ENDIF
c
c
c	status = SHC_open_memory()
c	IF (status .NE. SHC_OK) THEN
c	   PRINT *,' SHC open memory failure - error = ',status
c	   STOP
c	ENDIF
c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
	PASS=0
C
C
C	SCHEDULING A 1 SECOND WAKEUP REQUEST
C
C       CONVERT ASCII TIME TO BINARY
C
	STATUS=SYS$BINTIM (ALARM,TIMER)   
C
C	SCHEDULE WAKEUP
C
	STATUS=SYS$SCHDWK (,,TIMER,TIMER)
	IF (.NOT. STATUS) CALL LIB$SIGNAL (%VAL(STATUS))
C
C
C
221	MBX=0
	PASS=0
C
	CREP=0   ! INCREASES IF POINTS ARE IN ALARM
C
C
C
C
	DO 1010 JJ=1,1680
C
	CV=JJ/32+1
	CR=MOD(JJ,32)
C
	LSTAT=.FALSE.
	PSTAT=.FALSE.
C
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Call the get digital value and status function.
	gp_sid=0
	gp_bitnum=0  
	status = SHC_get_digital_val_stat( SYSID(JJ), gp_sid, gp_bitnum,
	1		digital_val, digital_stat)
C  Print the information returned from SHC_get_digital_val_stat
C           IF (status .ne. SHC_OK) THEN
C   	      write (302,*)digital_val,digital_val,digital_stat,digital_stat
C302	      FORMAT ('*** digital value = ',0xZ4.4,I8,
C	1	' -- digital status = 0x',Z4.4,I8)
C	   ENDIF
C
C
1314	CONTINUE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C
	IF (BJTEST(CXSTS(CV),CR)) LSTAT=.TRUE.  !SEE IF BIT SET IN STATUS
C
C
C		check if contact is closed in common
C
		IF (BITEST(digital_val,0)) THEN
		CXSTS(CV)=JIBSET(CXSTS(CV),CR)		! SET BIT
		PSTAT=.TRUE.
C
		ELSE
C
C		contact is open, reset bit in status word
C
		CXSTS(CV)=JIBCLR(CXSTS(CV),CR)		! CLEAR BIT
		END IF
C
C
C
C
	IF  (PSTAT .NEQV. LSTAT) THEN	!CHECK IF ANY CXS CHANGED
C
C		IF SO, SET BIT IN CXALRM TABLE
C
	CXALRM(CV)= JIBSET(CXALRM(CV),CR)
C
	CREP=CREP+1
	END IF
C
1010	CONTINUE
C
C
C
C
200	STATUS=SYS$HIBER ()
	IF (.NOT. STATUS) CALL LIB$SIGNAL (%VAL(STATUS))
C
	GO TO 221   	! RETURN TO START OF PROGRAM
C
	END                                                                  
                   
