CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CONFIDENTIAL
C       Property of United States Steel Corporation
C       Copyright 2010 United States Steel Corporation
C       All rights reserved.
C******************************************************************************
C               COMPRESS_POTTS.FOR
C **compile using the /old_f77 switch in the for command**
C******************************************************************************
C Revisions:
C       IM322852 20100706 SPA6635 add %<300 compression
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	INCLUDE 'FMS$EXAMPLES:FDVDEF'          
c	include '($ssdef)'
C
	integer status
C
C
	CHARACTER TWELV*1/12/,PUT*3,AINTV/'1'/,
	1	PO*3/'[5i'/,POF*3/'[4i'/,ES/27/,STR*4,STRL*4
C
C
	INTEGER  WORKSPACE( 3 ),	!General workspace
	1	 CHECKWKSP( 3 ),	!Check workspace
	2	 TCA( 3 )
C
	CHARACTER MON(12)*3/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
	1	'AUG','SEP','OCT','NOV','DEC'/,RMD*11,REPD*11,LSD*11,
	1	KFILE(17)*11,KN*9,ALN*40
C
	REAL VAL6(600)
	DOUBLE PRECISION DSUM,DSUM2,DMIN,DMAX,D200,C200,C300,MEAN,STDEV
C
	DOUBLE PRECISION ZJ,FJ,B6,TJ,B1,B2,B3,B4,B5,MEAN6,SD6,C2006
C
	STRUCTURE /CT/
C
		CHARACTER*11	KEY		! DATE,SAMPLE NO. YYMMDDSSSRR
		INTEGER		BDAT(2)		! BINARY DATE-TIME (START)
		INTEGER		BDATE(2)	! BINARY DATE-TIME (STOP)
		REAL 		VALUES(100)     ! INDIVIDUAL SAMPLE VALUES
		INTEGER		NUM		! PELLET COUNT FOR THIS SAMPLE
		INTEGER*2	CODE		! 
C
	END STRUCTURE
C
C
	RECORD /CT/ CT,Z
C
C
	STRUCTURE /CT2/
C
		CHARACTER*9	KEY		!YYMMDD LINE SHIFT HALF
		REAL		VALUES(100)
C		REAL		MEAN,SD,CA200,CA300,A200,MIN,MAX,NUM
		REAL		MEAN,SD,CA200,A200,MIN,MAX,NUM
		INTEGER 	SAMPLE
		INTEGER 	REP
C
	END STRUCTURE
C
	RECORD /CT2/ CT2,ctz,ct2save
C
	INTEGER PASS
C
	CHARACTER AA*6,A12*12,A23*23,KY2*9,A7*7,OLDKEY*9,A234*23
C
C
C
	STRUCTURE /POTTS/
C
		CHARACTER KD*8
C
		INTEGER DAT(2),IS,IH
C
		DOUBLE PRECISION SCREEN(14,3,2,5),COMP(2,3,2,5),CAB(2,3,5),
	1		SIL(3,5),FILT2(5,3),FILT3(5,3),BL2,BL3,FLUXG,
	1		FCA(2,3),FMG(2,3),FAL(2,3),FSI(2,3),PA(3,5),
	1		PC(3,5),PM(3,5)
C
		INTEGER S4,H4
C
		DOUBLE PRECISION SCWT(3,2,5),TONS4(3,2,5),
	1	GHRS(3,2,5),COMP2(3,3,2,5)
C  	1	GHRS(3,2,5),COMP2(2,3,2,5)  !temp for testing
C
	END STRUCTURE
C
	RECORD /POTTS/ Q           !Write to screen.dat

C
	CHARACTER TDT*11,TK*6,aone/'1'/,TRKEY*8
	INTEGER BTDT(2),bone(2)
	integer*8 plus1,ACTUAL                      
	real m3,n3
C*************************************************                                             
	call sys$bintim (aone,bone)
	STRL=ES//POF
C
C
	WRITE (6,229)STRL
229	FORMAT (' ',A)
C******************************************************************************
C		!Terminal Control Area
C******************************************************************************
C
	CALL FDV$ATERM( %DESCR(TCA),12 ,2)
	CALL FDV$AWKSP( %DESCR(WORKSPACE), 2000 )
	CALL FDV$LOPEN( 'D_I:COMPT', 1 )
C	CALL FDV$SPADA( 0 )
C	CALL FDV$SSIGQ( 0 )
C
C
1111	DO 13 I=1,17
13		KFILE(I)='           '
C
C
	OPEN (UNIT=10,FILE='D_I:COMPTESTS.DAT',ORGANIZATION='INDEXED',
	1		FORM='UNFORMATTED',ACCESS='KEYED',RECL=128,
	1		RECORDTYPE='FIXED',SHARED,
	1		STATUS='OLD')
C
	OPEN (UNIT=11,FILE='D_I:COMPSAVE.DAT',ORGANIZATION='INDEXED',
	1		FORM='UNFORMATTED',ACCESS='KEYED',RECL=128,
	1		RECORDTYPE='FIXED',SHARED,
	1		STATUS='UNKNOWN',KEY=(1:9:CHARACTER))
C
	OPEN (88,FILE='MET_SCREEN:SCREEN.DAT',
 	1       ACCESS='KEYED',STATUS='OLD',shared,err=543)
	
c
	go to 279
C
C
544	Continue
543	CALL FDV$PUTL ('CANNOT OPEN SCREEN DATA, CALL 7490')
	CALL FDV$WAIT ()
	GO TO 99
C
C****************************************************
C	COMPT1
C****************************************************
279	OLDSAMPLE=0
C
C
	CALL FDV$CDISP ('COMPT1')

1	CALL FDV$GET (AA,X,'NUM')
C
	PASS=0

C	set pass=1 to indicate that all undated samples wanted.
	IF (AA(1:3) .EQ. '   ') THEN
		PASS=1
		GO TO 75
	END IF
c	check if valid sample number was entered 
	IF (AA(1:3) .GE. '100' .AND. AA(1:3) .LE. '399') GO TO 344                                             

c	check if 'a'or'A' entered, exit screen
	if (aa(1:1) .eq. 'a'  .or. aa(1:1) .eq. 'A') GO TO 99

c	Open screen compt3
	GO TO 30
C
C****************************************************
C	***specific sequence number requested***
C****************************************************
344	READ (AA(1:3),'(BN,I3)',ERR=1) NS

C****************************************************
C	***NO sequence number requested***
C****************************************************

75	IRP=0
	DO 80 III=1,2000

	IF (III .EQ. 1)  READ (10,KEYGT='00000000000',ERR=12) CT
11	IF (III .GT. 1)  READ (10,ERR=12) CT

		IF (PASS .EQ. 1) THEN 	! use this loop if 'ALL' requested.
			IRP=IRP+1
			KFILE(IRP)=CT.KEY
			GO TO 80
		END IF
C
       		IF (CT.KEY(7:9) .EQ. AA(1:3)) THEN ! use this loop for spec #
			IRP=IRP+1
			KFILE(IRP)=CT.KEY
		END IF
C
80	CONTINUE
C
12	GO TO 40		! go call COMPT2 to display
C
C
C****************************************************
c	COMPT1 > COMPT3
C	***user wants to enter line, shift, half***
C****************************************************
30	OLDSAMPLE=1	! SET TO SHOW THIS SAMPLE WAS ALREADY STORED
	CALL FDV$GET (AA,J,'DATE')
	KY2=AA(5:6)//AA(1:2)//AA(3:4)
	CALL FDV$GET (AA,J,'LINE')
		KY2(7:7)=AA(1:1)
	CALL FDV$GET (AA,J,'SHIFT')
		KY2(8:8)=AA(1:1)
	CALL FDV$GET (AA,J,'HALF')
		KY2(9:9)=AA(1:1)
	READ (KY2(7:7),'(I1)') LLN	!CONVERT LINE NO. TO INTEGER
	OLDKEY=KY2

	READ (11,KEY=KY2,IOSTAT=MM) CT2

	IF (MM .EQ. 36) THEN
		CALL FDV$PUTL ('SAMPLE DOES NOT EXIST')
		CALL LIB$WAIT (1.4)
		GO TO 99
	END IF

	IF (MM .NE. 0) GO TO 99

	CALL FDV$CDISP ('COMPT3')

	CALL FDV$PUT (CT2.KEY(3:4)//CT2.KEY(5:6)//CT2.KEY(1:2),'DATE')
	CALL FDV$PUT (CT2.KEY(7:7),'LINE')
	CALL FDV$PUT (CT2.KEY(8:8),'SHIFT')
	CALL FDV$PUT (CT2.KEY(9:9),'HALF')
C
	WRITE (A7,'(I3)')CT2.SAMPLE
	CALL FDV$PUT (A7,'TNO')
	WRITE (A7,'(I2)')CT2.REP
	IF (A7(1:1) .EQ. ' ') A7(1:1)='0'
	IF (A7(1:2) .GT. '01') CALL FDV$PUT (A7(1:2),'RPT')
C
	DO 808 M=1,100
		WRITE (A7,'(F7.2)')CT2.VALUES(M)
		CALL FDV$PUT(A7,'VAL',M)
808	CONTINUE

C****************************************************
c	COMPT1 > COMPT3  ***user entered line, shift, half***
C****************************************************
		WRITE (A7,'(F7.2)')CT2.MEAN
		CALL FDV$PUT(A7,'MEAN')
C
		WRITE (A7,'(F7.2)')CT2.SD
		CALL FDV$PUT(A7,'SD')
C
		WRITE (A7,'(F7.2)')CT2.CA200
		CALL FDV$PUT(A7,'CMINUS')
C
		WRITE (A7,'(F7.2)')CT2.MIN
		CALL FDV$PUT(A7,'MIN')
C
		WRITE (A7,'(F7.2)')CT2.MAX
		CALL FDV$PUT(A7,'MAX')

C****************************************************
c     	COMPT1 > COMPT3 **Count <300# (ACTUAL, not calculated value)**
C	SPA6635 Commented out ACTUAL < 300 only want to report CALCULATED
C****************************************************
	m3=0
	n3=0
C	do 881 i=1,100
C		if (ct2.values(i) .gt. 0) then
C		if (ct2.values(i) .lt. 300) then
C			IF (CT2.VALUES(i) .LE. 90) GO TO 881
C			n3=n3+1
C		end if
C		end if
881	continue
C	CALL FDV$PUT (A7,'MINUS3')

C****************************************************
C	COMPT1 > COMPT3 **make a % of < 300# (ACTUAL, not calculated value)
C	SPA6635 Commented out ACTUAL < 300 only want to report CALCULATED
C****************************************************
C	if (n3 .gt. 0) then
C		m3=n3/ct2.num*100.
C	else
C		m3=0
C	end if
c
C	WRITE (A7,'(F7.2)')m3
C	CALL FDV$PUT(A7,'minus3')
C
	GO TO 600	! GO ASK FOR CHANGES TO COMPT3 SCREEN
C
C****************************************************
C	COMPT1>COMPT2 ***seq # spec or all unseq samples wanted***
C****************************************************
C
40	CALL FDV$CDISP ('COMPT2')
C
	JJ=1
	DO 772 I=1,17
C
	IF (KFILE(JJ)(1:1) .EQ. ' ') THEN
		CALL FDV$PUT ('   ','SEQ',JJ)
		GO TO 772
	END IF
C
	READ (10,KEY=KFILE(JJ),ERR=99) CT
	CALL FDV$PUT (CT.KEY(7:9),'SEQ',JJ)
	CALL FDV$PUT (CT.KEY(10:11),'RPT',JJ)
C	*set BDAT by getting the system begin date/time from compsave file
	CALL SYS$ASCTIM (,A23,CT.BDAT,)

	A12=A23(1:6)//' '//A23(13:17)
	CALL FDV$PUT (A12,'START',JJ)
C	*set BDAT by getting the system end date/time from compsave file
	CALL SYS$ASCTIM (,A23,CT.BDATE,)
                      
	A12=A23(1:6)//' '//A23(13:17)
	CALL FDV$PUT (A12,'STOP',JJ)
	JJ=JJ+1
772	CONTINUE

	CALL FDV$GETAF (AA,J,A23,I)

C****************************************************
C	***COMPT2 - delete unwanted (unsequenced) samples***
C****************************************************
C
	IF ((AA(1:1) .EQ. 'D') .OR. (AA(1:1) .EQ. 'd')) then
c
c
		READ (10,KEY=KFILE(I),ERR=88)CT
		DELETE (10)
		CALL FDV$PUTL (' SPECIFIED RECORD DELETED')
		CALL LIB$WAIT (1.5)
		GO TO 279
c
88		call fdv$putl ('this record does not exist')
		call lib$wait (1.4)
		go to 99
C
C
	END IF
C
C****************************************************
C	***COMPT2 - read in requested seq from active file***
C****************************************************
C
101	READ (10,KEY=KFILE(I),iostat=J) CT
C
	IF (J  .EQ. 52) THEN
		CALL LIB$WAIT (2.)
		GO TO 101
	END IF
C
	IF (J .NE. 0) THEN
C
		CALL FDV$PUTL ('I CANT GET THIS RECORD')
		CALL LIB$WAIT (1.5)
		GO TO 99
C
	END IF
C
C****************************************************
C	***compt2>compt3 - display 100 values etc***
C****************************************************

C
	CALL FDV$CDISP ('COMPT3')
C
C
	CALL FDV$PUT (CT.KEY(7:9),'TNO')
	IF (CT.KEY(10:11) .GT. '01') CALL FDV$PUT (CT.KEY(10:11),'RPT')
C
C
C	highlight values less than 200

	DO 44 M=1,CT.NUM
		WRITE (A7,'(F7.2)')CT.VALUES(M)
C
		IF (CT.VALUES(M) .LT. 200.) THEN
			MMV=4
			CALL FDV$AFVA (MMV,'VAL',M)
		ELSE
			MMV=-2
			CALL FDV$AFVA (MMV,'VAL',M)
		END IF
C
		CALL FDV$PUT(A7,'VAL',M)
44	CONTINUE

C****************************************************
C	***compt2>compt3 Count <300# (actual, not calculated value)
C	SPA6635 Commented out ACTUAL < 300 only want to report CALCULATED
C****************************************************
C	m3=0
C	n3=0
C	do 8812 i=1,100
C		if (ct.values(i) .gt. 0) then
C		if (ct.values(i) .lt. 300) then
C			IF (CT.VALUES(i) .LE. 90) GO TO 8812
C			n3=n3+1
C		end if
C		end if
8812	continue
C	make a % < 300# (actual, not calculated value)
C	if (n3 .gt. 0) then
C		m3=n3/ct.num*100.
C	else
C		m3=0
C	end if
c
C		WRITE (A7,'(F7.2)')m3
C		CALL FDV$PUT(A7,'minus3')
C 
C****************************************************
C	***COMPT2>COMPT3***
C	Calculate mean,sd,-200, etc.***
C	IM322852 SPA6635 added c300 calculation      
C****************************************************        

	dsum=0
	n=0
	dsum2=0
	dmin=100000.
	dmax=0.
	d200=0.
	c200=0.
	c300=0.
	mean=0
	STDEV=0
	
	IF (CT.NUM .LE. 0) GO TO 600
	DO 444 M=1,CT.NUM
		IF (CT.VALUES(M) .LE. 90) GO TO 444
		N=N+1
		DSUM=DSUM+CT.VALUES(M)
		DSUM2=DSUM2+CT.VALUES(M)**2
		IF (CT.VALUES(M) .GT. DMAX) DMAX=CT.VALUES(M)
		IF (CT.VALUES(M) .LT. DMIN) DMIN=CT.VALUES(M)
		IF (CT.VALUES(M) .LT. 200.) D200=D200+1
444	CONTINUE
C	***avoid divide by 0 errors**
	IF (N .LT. 2) GO TO 600

	STDEV=DSQRT((N*DSUM2-DSUM**2)/(N*(N-1)))
	MEAN=DSUM/N
	D200=D200/N*100.

C	***COMPT2>COMPT3 calculated %<-200***
	ZJ=(MEAN-200.)/STDEV
	B6=2.*3.14159
	FJ=(1./DSQRT(B6))*(1./2.71828**(ZJ**2/2.))
	TJ=1./(1.+.2316419*ZJ)
	B1=.31938153
	B2=-.356563872
	B3=1.781477937
	B4=-1.821255978
	B5=1.330274429

	C200=FJ*(B1*TJ+B2*TJ**2+B3*TJ**3+B4*TJ**4+B5*TJ**5)*100.

C	***COMPT2>COMPT3 calculated %<-300***
C	IM322852 SPA6635 ADDED -300 calculation

	ZJ=(MEAN-300.)/STDEV
	B6=2.*3.14159
	FJ=(1./DSQRT(B6))*(1./2.71828**(ZJ**2/2.))
	TJ=1./(1.+.2316419*ZJ)
	C300=FJ*(B1*TJ+B2*TJ**2+B3*TJ**3+B4*TJ**4+B5*TJ**5)*100.

C	set values to write to compsave.dat
	ct2.mean=mean
	ct2.sd=stdev
 	ct2.ca200=c200
 	ct2.a200=d200
	ct2.min=dmin
	ct2.max=dmax
	ct2.num=n
C
	DO 38 I=1,100
38		CT2.VALUES(I)=CT.VALUES(I)
C
	WRITE (A7,'(F7.2)') MEAN
	CALL FDV$PUT (A7,'MEAN')
C
	WRITE (A7,'(F7.2)') STDEV
	CALL FDV$PUT (A7,'SD')
C
	WRITE (A7,'(F7.2)') C200
	CALL FDV$PUT (A7,'CMINUS')
C
	WRITE (A7,'(F7.2)') C300
	CALL FDV$PUT (A7,'C300')

C changed d200 to c300 (only on screen)
c	WRITE (A7,'(F7.2)') D200
c	CALL FDV$PUT (A7,'MINUS')
C
	WRITE (A7,'(F4.0)') DMIN
	CALL FDV$PUT (A7,'MIN')
C
	WRITE (A7,'(F5.0)') DMAX
	CALL FDV$PUT (A7,'MAX')
C
C****************************************************
C	***COMPT2>COMPT3 actual %<-300***
C	SPA6635 Commented out ACTUAL < 300 only want to report CALCULATED
C****************************************************
C	m3=0
C	n3=0
C	do 8813 i=1,100
C		if (ct.values(i) .gt. 0) then
C		if (ct.values(i) .lt. 300) then
C			IF (CT.VALUES(i) .LE. 90) GO TO 8813
C			n3=n3+1
C		end if
C		end if
8813	continue
c
C	if (n3 .gt. 0) then
C		m3=n3/n*100.
C	else
C		m3=0
C	end if
C	
C	WRITE (A7,'(F7.2)')m3
C	CALL FDV$PUT(A7,'minus3') 

C******************************************************************************
C	all paths Compt1>Compt2>compt3 or Compt1>compt3 lead to '600'
C******************************************************************************

C	*waiting for input of date
600	CALL FDV$GET (A23,J,'DATE')
C
C****************************************************
c	check input date and make sure it is .LE. today's date
C****************************************************
c
	call sys$gettim (btdt)		!binary
	CALL LIB$DAY (PLUS1,BTDT)	!integer
C
C
	READ (A23(1:2),'(I2)') IIZ
C	*Y2K rolling date
	IF (A23(5:6) .LT. '66') THEN
		TDT=A23(3:4)//'-'//MON(IIZ)//'-20'//A23(5:6)
	ELSE
		TDT=A23(3:4)//'-'//MON(IIZ)//'-19'//A23(5:6)
	END IF

	btdt(1)=0
	btdt(2)=0
	status= SYS$BINTIM (TDT,BTDT)
	if (.not. status) go to 600
	CALL LIB$DAY (ACTUAL,BTDT)
	if (actual .eq. 0) go to 600

C****************************************************
	CT2.KEY=A23(5:6)//A23(1:2)//A23(3:4)

	IF (ACTUAL .GT. PLUS1) THEN
		CALL FDV$PUTL ('YOU INPUT A DATE LATER THAN TODAY''S DATE')
		call FDV$wait ()
		GO TO 600
	END IF
CCCCCC
C	*waiting for input of line
601	CALL FDV$GET (A7,J,'LINE')

	READ (A7(1:1),'(I1)') LLN
        	CT2.KEY(7:7)=A7(1:1)
C		*if the prev field (F12) key pressed, back up to the prev field
		IF (J .EQ. FDV$K_FT_PRV) GO TO 600 
C	*waiting for input of shift
602	CALL FDV$GET (A7,J,'SHIFT')
        	CT2.KEY(8:8)=A7(1:1)
C		*if the prev field (F12) key pressed, back up to the prev field
		IF (J .EQ. FDV$K_FT_PRV) GO TO 601 
C	*waiting for input of half
603	CALL FDV$GET (A7,J,'HALF')
        	CT2.KEY(9:9)=A7(1:1)
C		*if the prev field (F12) key pressed, back up to the prev field
		IF (J .EQ. FDV$K_FT_PRV) GO TO 602 
C	*waiting for input of OK field
888	CALL FDV$GET (A7,J,'OK')
C		*if the prev field (F12) key pressed, back up to the prev field
	IF (J .EQ. FDV$K_FT_PRV) GO TO 603 

	IF (A7(1:1) .EQ. ' ') then
C		*check if this test has been written to compsave file before
		if (oldsample .eq. 0) then
C			*returns to open file and compt1 screen
			GO TO 1111
		else
C			*calculate 600 avg
			GO TO 6006
		end if
	END IF

        IF (A7(1:1).EQ.'Y' .OR. A7(1:1) .EQ. 'y') then
C		*check if this has been written to compsave file already
		IF (OLDSAMPLE .NE. 0) THEN
			if (oldkey .ne. ct2.key) then
				delete11=1
			else
				go to 6006
			end if
			READ (11,KEY=ct2.key,IOSTAT=MM) CT2save

			if (mm .eq. 0) then
	call fdv$putl ('this sample already exists, your change is not valid')
			call fdv$wait ()
				go to 99
			end if
c
			if (delete11 .ne. 0) then
				READ (11,KEY=oldkey,IOSTAT=MM) CT2save
				delete (11)
			end if

			READ (11,KEY=ct2.key,IOSTAT=MM) CT2SAVE

			IF (MM .NE. 36) THEN
		CALL FDV$PUTL ('SOMETHING IS VERY, VERY WRONG, CALL 7490')
		call fdv$wait()
				GO TO 99
			END IF
C
			WRITE (11) CT2
			GO TO 6006
		END IF		!OLDSAMPLE .NE. 0
C
C****************************************************
C		  save unassigned seq. to compsave file
C****************************************************
C	   check if line no. is valid
C
	IF (LLN .LT. 3 .OR. LLN .GT. 7) THEN
C
	ALN='LINE NO. '//A7(1:1)//' DOES NOT EXIST'
C
		CALL FDV$PUTL (ALN)
		call FDV$wait ()
		GO TO 601
C
	END IF
C
	IF (CT2.KEY(8:8) .LT. '1'  .OR. CT2.KEY(8:8) .GT. '3') GO TO 602
	IF (CT2.KEY(9:9) .LT. '1'  .OR. CT2.KEY(9:9) .GT. '2') GO TO 603
	IF (CT2.KEY(3:4) .LT. '01'  .OR. CT2.KEY(3:4) .GT. '12') GO TO 600
	IF (CT2.KEY(5:6) .LT. '01'  .OR. CT2.KEY(5:6) .GT. '31') GO TO 600
C
C
C******************************************************************************
C		SAVE HALF SHIFT COMPRESSION & -200 TO 'MET_P:SCREEN.DAT'
C******************************************************************************

628		IF (CT2.KEY(1:2) .LT. '66') THEN
			TRKEY='20'//CT2.KEY(1:6)
		ELSE
			TRKEY='19'//CT2.KEY(1:6)
		END IF
C
		READ (88,KEY=TRKEY,IOSTAT=MQ) Q
C		READ (89,KEY=TRKEY,IOSTAT=MQ2) Q2

		CALL FDV$PUTL ('                                       ')
C
C	iostat=36: attempt to access non-existent record
		IF (MQ .EQ. 36) THEN
			Q.KD=TRKEY
C			Q2.KD=TRKEY

C	iostat=52: file is locked
		ELSE IF (MQ .eq. 52) THEN
			call fdv$putl ('screen file locked, waiting to open')
 			call lib$wait (1.0)
			go to 628
		ELSE IF (MQ .NE. 0) THEN
			CALL FDV$PUTL ('AGLDAT FILE IS BAD, CALL 7490')
			GO TO 99
		END IF
C
		READ (CT2.KEY(7:7),'(I1)') LLLL
		LLLL=LLLL-2
		READ (CT2.KEY(8:8),'(I1)') LLLS
		READ (CT2.KEY(9:9),'(I1)') LLLH

C****************************************************
C Compt1>Compt2>compt3 or Compt1>compt3 
C****************************************************

		Q.COMP2(1,LLLS,LLLH,LLLL)=MEAN
		Q.COMP2(2,LLLS,LLLH,LLLL)=C200
                Q.COMP2(3,LLLS,LLLH,LLLL)=C300
C
		IF (MQ .EQ. 36) THEN
			WRITE (88,ERR=5522) Q
C			WRITE (89,ERR=5523) Q2
c		call fdv$putl ('WRITE (88,ERR=5522) Q -first')
		call lib$wait (2.0)

		ELSE
			REWRITE (88,ERR=5522) Q
C			REWRITE (89,ERR=5523) Q2
c		call fdv$putl ('REWRITE (88,ERR=5522) Q -first')
		call lib$wait (2.0)

		END IF

C****************************************************
C		WRITE DATA TO 'COMPSAVE.DAT'
C****************************************************

C
5523	CONTINUE
5522			read (11,key=ct2.key,iostat=mq) ctz
c
			if (mq .ne. 36) then
				call fdv$putl ('file exists already')
				call fdv$wait ()
				go to 99
			end if
c
C
C				save sample no. and repeat no.
c
			READ (CT.KEY(7:9),'(I3)') M1
			READ (CT.KEY(10:11),'(I2)') M2
C
			CT2.SAMPLE=M1
			CT2.REP=M2
C
			WRITE (11) CT2
			delete (10)
			GO TO 6006
	END IF                    !ends IF (A7(1:1).EQ.'Y' or 'y'
C
		GO TO 99

C****************************************************
C 	Bring in 600 avg -calculate
C****************************************************
6006	open (20,file='d_i:avg600.dat',form='unformatted',access='direct',
	1	recl=600,status='unknown')
c
	MMM=0
C
	READ (20,REC=LLN,ERR=1222) val6
C
797	dsum=0
	n=0
	dsum2=0
	dmin=100000.
	dmax=0.
	d200=0.
	c200=0.
	mean=0
	STDEV=0
C	
c
222	do 18 I=1,600
C
		IF (VAL6(I) .LE. 0) GO TO 18
		if (val6(i) .gt. 1450) go to 18
		N=N+1
		DSUM=DSUM+VAL6(I)
		DSUM2=DSUM2+VAL6(I)**2
		IF (VAL6(I) .LT. 200.) D200=D200+1
C
18	CONTINUE
	IF (N .LT. 2) GO TO 1222
C 
	STDEV=DSQRT((N*DSUM2-DSUM**2)/(N*(N-1)))
	MEAN=DSUM/N
	D200=D200/N*100.

C****************************************************
C 	***calculated -200 WITH 600 avg***
C****************************************************
	ZJ=(MEAN-200.)/STDEV
	B6=2.*3.14159
	FJ=(1./DSQRT(B6))*(1./2.71828**(ZJ**2/2.))
	TJ=1./(1.+.2316419*ZJ)
	B1=.31938153
	B2=-.356563872
	B3=1.781477937
	B4=-1.821255978
	B5=1.330274429

	C200=FJ*(B1*TJ+B2*TJ**2+B3*TJ**3+B4*TJ**4+B5*TJ**5)*100.

C	IM322852 SPA6635 ADDED C300 CALC
                 
 	ZJ=(MEAN-300.)/STDEV
 	FJ=(1./DSQRT(B6))*(1./2.71828**(ZJ**2/2.))
	TJ=1./(1.+.2316419*ZJ)

C	DON'T KNOW IF WE WANT TO INCLUDE 300 IN 600 AVG CALC
C	C300=FJ*(B1*TJ+B2*TJ**2+B3*TJ**3+B4*TJ**4+B5*TJ**5)*100.
       
	WRITE (A7,'(F7.2)') MEAN
	CALL FDV$PUT (A7,'M600')
C
	WRITE (A7,'(F7.2)') STDEV
	CALL FDV$PUT (A7,'MSD')
C
	WRITE (A7,'(F7.2)') C200
	CALL FDV$PUT (A7,'M200')
C
C 	WRITE (A7,'(F7.2)') C300
C	CALL FDV$PUT (A7,'C300')

C ***mmm gets assigned to 1 when a 'Y' is put in the 600 avg field (below)**
	if (mmm .eq. 1) then
		call fdv$putl ('data has been rolled into 600 average')
		call lib$wait (2.0)
	end if


C****************************************************
C	ask to add this test to the 600 average.
C****************************************************
1222	CALL FDV$GET (A7,J,'FILL')
C
	A234(1:1)=' '
	CALL FDV$PUT (A234(1:1),'FILL')
C
		IF (A7(1:1) .EQ. 'Y' .OR. A7(1:1) .EQ. 'y') then
			MMM=1
			DO 49 JJ=1,CT2.NUM
				DO 48 I=600,2,-1
48					VAL6(I)=VAL6(I-1)
C
49				CONTINUE
			DO 50 JJ=1,100
				IF (CT2.VALUES(JJ) .LE. 90.) GO TO 50
				VAL6(JJ)=CT2.VALUES(JJ)
50			CONTINUE
			GO TO 797
		end if
C
	IF (MMM .GE. 1) THEN
C	***PUT val6 (modified) to avg600.dat**
		WRITE (20,REC=LLN) val6

		IF (CT2.KEY(1:2) .LT. '66') THEN
			TRKEY='20'//CT2.KEY(1:6)
		ELSE
			TRKEY='19'//CT2.KEY(1:6)
		END IF

C	***PUT 600 AVG COMP,-200 AND -300 IN 'MET_P:SCREEN.DAT'***
		READ (88,KEY=TRKEY,IOSTAT=MQ) Q
C		READ (89,KEY=TRKEY,IOSTAT=MQ2) Q2
C
		IF (MQ .EQ. 52) THEN
			Q.KD=CT2.KEY(1:6)
C			Q2.KD=CT2.KEY(1:6)
		END IF
C
		READ (CT2.KEY(7:7),'(I1)') LLLL
		LLLL=LLLL-2		!ADJUST LLLL FOR ARRAY INDEX
		READ (CT2.KEY(8:8),'(I1)') LLLS
		READ (CT2.KEY(9:9),'(I1)') LLLH
C
		Q.COMP(1,LLLS,LLLH,LLLL)=MEAN
	  	Q.COMP(2,LLLS,LLLH,LLLL)=C200
C		Q2.COMP(1,LLLS,LLLH,LLLL)=MEAN
C	  	Q2.COMP(2,LLLS,LLLH,LLLL)=C200
 		                                                  
C		**if the record does not exist, write to screen.dat
		IF (MQ .EQ. 36) THEN
			WRITE (88,ERR=5522) Q
C			WRITE (89,ERR=5522) Q2
c		call fdv$putl ('WRITE (88... Q (2)') !for testing only
		call lib$wait (2.0)                     !for testing only 
C		**if the record already exists, overwrite the screen.dat record
		ELSE
			REWRITE (88,ERR=5522) Q
C			REWRITE (89,ERR=5522) Q2
c		call fdv$putl ('REWRITE (88... Q (2)') !for testing only
		call lib$wait (2.0)                     !for testing only
		END IF
	END IF
C
		IF (A7(1:1) .EQ. 'A' .OR. A7(1:1) .EQ. 'a') then
			GO TO 99
		else
			go to 1111
		end if

C****************************************************
99	CALL FDV$LCLOS
	CALL FDV$SPADA (0)
	CALL FDV$DEL ('D_I:COMPT')
	CALL FDV$DWKSP (%DESCR(WORKSPACE))
	CALL FDV$DTERM(%DESCR(TCA))
	STOP
	END
