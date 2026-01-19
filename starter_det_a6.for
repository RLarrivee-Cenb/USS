	PROGRAM 	STARTER_DET_A6
C-------------------------------------------------------------------------------
C	This program runs on a 1 minute fixed frequency on A6, and is used 
C	to start PIT TO CRUSH detached process
C	RUN/DETACHED/NOACCOUNT/ERROR=BP:STARTER_DET_A6.ERR/PROCESS= -
C	STARTER_DET_A6 BP:STARTER_DET_A6
C
C	BP:STARTER_TIMES.DAT saves the time that PIT.EXE gets started
C-------------------------------------------------------------------------------
C
C	10/13/97 - changed Pit start times from 06:45, 14:45, 22:45 to 
C	06:47, 14:47, 22:47 

	INTEGER*4	ADDRESS(2)
C
	CHARACTER*8	LOOPTIME/'0 0:1:00'/
	CHARACTER*8	FILE_NAME/'PITT:PIT'/
	CHARACTER*12	PROCESS_NAME/'PIT_TO_CRUSH'/
	CHARACTER*23	CURTIME
C
	CALL SYS$BINTIM (LOOPTIME,ADDRESS)
	CALL SYS$SCHDWK (,, ADDRESS, ADDRESS)
C
100	CALL SYS$ASCTIM ( , CURTIME, , )
C
C-------------------------------------------------------------------------------
C	Start pit to crusher report.
C-------------------------------------------------------------------------------
C
	IF ((CURTIME(13:17) .EQ. '22:47').OR.
	1	(CURTIME(13:17) .EQ. '06:47').OR.
	1	(CURTIME(13:17) .EQ. '14:47')) THEN
C
		OPEN (UNIT=1,STATUS='UNKNOWN',
	1	FILE='BP:STARTER_TIMES.DAT',ERR=250,
	1	FORM='FORMATTED',RECL=24,ACCESS='APPEND')
C
 		WRITE (1,200) CURTIME
200		FORMAT (' ',A23)
		CLOSE (UNIT=1)
C
250		CALL SYS$CREPRC(, FILE_NAME,,,,,, PROCESS_NAME,,,,)
	END IF


C
	CALL SYS$HIBER ()
C
	GOTO 100
C
	END
