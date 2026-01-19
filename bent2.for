C
	integer ufo_OPEN,ndat(2)
	character aone/'1'/
	integer bone(2),btim(2)
	character atim*23
	EXTERNAL UFO_OPEN
	INCLUDE 'SYS$LIBRARY:GKSDEFS.FOR'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	CHARACTER*4	BDOCK,BACCT,BGRADE
	CHARACTER MON(12)*3/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
	1	'AUG','SEP','OCT','NOV','DEC'/,ADAT*11,A2*2
	1,ASCD*11,LDAT*23,adate*8,key*6
	INTEGER	BDATEX,BLK,BCNTR,TIM(2),BADBOAT
	
C
	REAL XV(43)
	INTEGER DTX(2,43,6),ITIME
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	LOGICAL*1	ESC/"033/
	INTEGER*4	SCRLUN/6/
C
C
	CHARACTER CSI/155/,exp*4/'?43h'/,com*4/'?431'/,col*4/'?44h'/,
	1	prn*2/'?i'/,PCO*2/'5i'/,PCF*2/'4i'/,o5*5,o3*3,GDATE*23
CCCCCCCCCCCCC
	CHARACTER ES/27/,BSL/'\'/,ASQ,RSIDE*12,CPA*6,CPKA*6,AS(6)*5,
	1	RRSIDE*18,DX23*23,RSV*5,label*40
CCCCCCCCCCCCC
C
	REAL SD(5),SDA(5),SDR(5),SDRA(5),AVG(5)
	INTEGER	FC,BD,CPR,STAT,STATUS,FA
C
	REAL  TOPX,TOPR
	DATA 	TOPX /.81375/	!TOP OF X GRAPH 
	DATA	TOPR /.39375/	!TOP OF R GRAPH
C
	INTEGER*2 TDATA(7),STD(7),STD6(7),FILD(7)
C
C
	DOUBLE PRECISION T,TA,
	1		 SX,SX2,SUMB,SUMA,NC,SDN,SXR,SX2R,SDNR
	1		 SXA,SX2A,SDNA,SXRA,SX2RA,SDNRA
C
	REAL  YPB(42,3),YPA(21,5),YRB(42,3),X(2),PTBL(42)
c
C
C
	INTEGER IS,IH,DAT(2),PTNUM/21/,DTS(2,43)
C
	REAL PTX(21),PTY(21),AB(21),AA(21),R(21)
	CHARACTER MAT(2)*4/'FLUX','ACID'/,DTIM*23,DATT(30)*11,DX1*6,DAX*2,
	1	DX11*9
C
	INTEGER WS_ID,NUM_POINTS,NV,I,J
	CHARACTER ASC(3)/'1','2','3'/,RY(5)*3/'0.0','1.0','2.0','3.0','4.0'/
	CHARACTER AO*6
	REAL PX(2),PY(8),Y(2),XAX(42),YPB3(21),YRB3(21),AP3(21),
	1	AR3(21),xcc(2),ycc(2)
C
	character ascn*5,ascr*5,SPCA*27
c
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
	REAL YAXB/10/
	REAL YAXE/27.5/
	REAL YAXRB/0.00/
	REAL YAXRE/3.0/
C
	REAL XVAL(43,3)
	REAL RVAL(43,3)
C
	INTEGER DTM(2,43)
C
	REAL XBAR /2.51/
	REAL UCL /3.61/
	REAL LCL /1.41/
	REAL STDV /.46/
C
	REAL RBAR /.41/
	REAL UCLR /1.35/
	REAL LCLR /0.00/
	REAL STDVR /0.0/
C
	INTEGER CSH
C
C
	CHARACTER FILES(3)*2/'BT','AT','CM'/
C
	REAL CP,CPK
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
	DATA PX /0.1,.95/
	DATA PY /.225,.225,.39375,.39375,.42,.42,.81375,.81375/
	DATA WS_ID/1/,NUM_POINTS/2/,NV/20/
C
	character string*255
C
C
	STRUCTURE /FOREMAN/
C
		CHARACTER DATE*6        ! KEY 0  DATE (YYMMDD)
		CHARACTER  NAME(3,2)*10	! names by shift
		REAL TONS(5,3)          ! pellet tons
		REAL TIME(5,3)          ! grate hours
		CHARACTER FA(5,3)	! F=FLUX, A=ACID
		REAL MBTU(5,3)          ! btus/ton
		REAL BENT(5,3)          ! bent. lbs/ton
C
		REAL BT(5,3)		! before tumble +1/4
		REAL AT(5,3)		! after tumble +1/4
		REAL COMP(5,3)          ! compression
		REAL PBA(5,3)           ! filter b/a
		REAL PSIL(5,3)		! non-existent
		REAL FCR(2,3)           ! filter cake ratio
C
	END STRUCTURE
C
	RECORD /FOREMAN/ FOR,ZFOR
C
C
C
C
C
	OPEN   (UNIT=50,FILE='USER_D:[METREP.ASHIFT]FORFILE.DAT',
	1	STATUS='UNKNOWN',ORGANIZATION='INDEXED',ACCESS='KEYED',
	1	FORM='UNFORMATTED',RECL=256,KEY=(1:6:CHARACter),SHARED)
C
cccccccccccccccccccccccc
c
	MBA=0
c
C
	if (string(1:2) .eq. '16' .or. string(1:2) .eq. '17') then
c
	PRINTER=0	! set to 0 if output is to SCREEN
c
	o3=csi//pcf
	write (6,229)o3
c
	else if (string(1:2) .eq. '31') then
c
	o3=csi//pco
	write (6,229)o3
229	format (' ',a)
C
	PRINTER=1	! set to 1 if output is to LA324
C
	END IF
c
C
	WRITE (6,*) ' TREND CHART FOR BENT LBS/TON- STEP 2 AGGLOM'
333	WRITE (6,*) ' '
	WRITE (6,*) ' '
	WRITE (6,*) ' THRU WHAT DATE  ex. 08-31-93'
	read (5,55)m,adate
55	format (q,a)
c
	if (m .eq. 0) then
		call sys$gettim (btim)
		call sys$asctim (,atim,btim,)
		IF (ATIM(1:1) .EQ. ' ') ATIM(1:1)='0'
		do 371 i=1,12
			if (mon(i) .eq. ATIM(4:6)) GO TO 372
371		CONTINUE
		GO TO 333
c
c
372	WRITE (ADATE(1:2),'(I2)') I
	IF (ADATE(1:1) .EQ. ' ') ADATE(1:1)='0'
	ADATE(7:8)=ATIM(10:11)
	ADATE(4:5)=ATIM(1:2)
	GO TO 4373
c
c
	end if
c
C
C
C
	if (m .ne. 8) go to 333
c
C
4373	key=adate(7:8)//adate(1:2)//adate(4:5)

C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	I=0
	J=0
	K=0
	XXXX=0
	YYYY=0
	L=0
	M=0
	CALL GKS$OPEN_GKS ()
	CALL GKS$OPEN_WS (WS_ID,GKS$K_CONID_DEFAULT,GKS$K_WSTYPE_DEFAULT)
	CALL GKS$ACTIVATE_WS (WS_ID)
C
	CALL GKS$INQ_MAX_DS_SIZE(I,J,K,XXXX,YYYY,L,M)
	XMAX=MAX(XXXX,YYYY)
	IF((XXXX/XMAX).EQ.1.0) THEN 
	RATX=1.0
	RATY=YYYY/XMAX
	ELSE
	RATX=XXXX/XMAX
	RATY=1.0
	END IF
	LARGEST_VIEWPORT=1
	CALL GKS$SET_VIEWPORT(LARGEST_VIEWPORT,0.0,RATX,0.0,RATY)
	CALL GKS$SELECT_XFORM(LARGEST_VIEWPORT)
	CALL GKS$SET_WS_WINDOW(WS_ID,0.0,RATX,0.0,RATY)
	CALL GKS$SET_WS_VIEWPORT(WS_ID,0.0,XXXX,0.0,YYYY)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	call sys$bintim (aone,bone)
C
C	calc y-axis values for x-bar chart
C
	YJ=1/((YAXE-YAXB)/7)
	YRR=1/((YAXRE-YAXRB)/3)
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
	read (key(3:4),'(i2)') ll
	ldat=key(5:6)//'-'//mon(ll)//'-19'//key(1:2)
	call sys$bintim (ldat,ndat)
C
C
	iip=0
	I2=43
C
39	IE=0
398	READ (50,KEY=KEY,IOSTAT=M,ERR=3734) FOR
	GO TO 663
C
3734	IF (M .EQ. 52) THEN
		CALL LIB$WAIT(1.)
		IE=IE+1
		IF (IE .GT. 20) GO TO 1919
		GO TO 398
	ELSE
		X(1)=.3
		Y(1)=.5
C
		CALL GKS$TEXT (X(1),Y(1),'DATA IS BEING UPDATED-WAIT')
		CALL LIB$WAIT (10.)
		GO TO 1919
	END IF
c
c	 routine to pass shifts that don't exist yet for today
c
663	UNLOCK (50)
	if (iip .eq. 0) then
		if (for.BENT(1,3) .eq. 0 .and. for.bent(2,3) .eq. 0 .and.
	1		for.bent(3,3) .eq. 0) go to 521
	end if
c
C
	IF (FOR.BENT(1,3) .LT. 1 .OR. FOR.BENT(1,3) .GT. 1000) GO TO 112
	XVAL(I2,1)=FOR.BENT(1,3)
112	IF (FOR.BENT(2,3) .LT. 1 .OR. FOR.BENT(2,3) .GT. 1000) GO TO 113
	XVAL(I2,2)=FOR.BENT(2,3)
113	IF (FOR.BENT(3,3) .LT. 1 .OR. FOR.BENT(3,3) .GT. 1000) GO TO 114
	XVAL(I2,3)=FOR.BENT(3,3)
C
114	IF (I2-1 .LT. 1) GO TO 92
	DTM(1,I2-1)=NDAT(1)
	DTM(2,I2-1)=NDAT(2)
92	I2=I2-1
C
521	IF (I2 .LE. 1) GO TO 65 
C
c
c
	if (iip .eq. 0) then
		if (for.bent(1,2) .eq. 0 .and. for.bent(2,2) .eq. 0 .and.
	1		for.bent(3,2) .eq. 0) go to 522
	end if
c
c
	IF (FOR.BENT(1,2) .LT. 1 .OR. FOR.BENT(1,2) .GT. 1000) GO TO 115
	XVAL(I2,1)=FOR.BENT(1,2)
115	IF (FOR.BENT(2,2) .LT. 1 .OR. FOR.BENT(2,2) .GT. 1000) GO TO 116
	XVAL(I2,2)=FOR.BENT(2,2)
116	IF (FOR.BENT(3,2) .LT. 1 .OR. FOR.BENT(3,2) .GT. 1000) GO TO 117
	XVAL(I2,3)=FOR.BENT(3,2)
C
117	IF (I2-1 .LT. 1) GO TO 93
	DTM(1,I2-1)=NDAT(1)
	DTM(2,I2-1)=NDAT(2)
93	I2=I2-1
C
522	IF (I2 .LE. 1) GO TO 65 
C
c
	if (iip .eq. 0) then
		if (for.bent(1,1) .eq. 0 .and. for.bent(2,1) .eq. 0 .and.
	1		for.bent(3,1) .eq. 0) go to 523
	end if
c
c
	IF (FOR.BENT(1,1) .LT. 1 .OR. FOR.BENT(1,1) .GT. 1000) GO TO 118
	XVAL(I2,1)=FOR.BENT(1,1)
118	IF (FOR.BENT(2,1) .LT. 1 .OR. FOR.BENT(2,1) .GT. 1000) GO TO 119
	XVAL(I2,2)=FOR.BENT(2,1)
119	IF (FOR.BENT(3,1) .LT. 1 .OR. FOR.BENT(3,1) .GT. 1000) GO TO 120
	XVAL(I2,3)=FOR.BENT(3,1)
C
120	IF (I2-1 .LT. 1) GO TO 94
	DTM(1,I2-1)=NDAT(1)
	DTM(2,I2-1)=NDAT(2)
94	I2=I2-1
C
523	IF (I2 .LE. 1) GO TO 65 
C
	iip=1
373	call lib$sub_times (ndat,bone,ndat)
	call sys$asctim (,ldat,ndat,)
	IF (LDAT(1:1) .EQ. ' ') LDAT(1:1)='0'
	do 716 J=1,12
		IF (MON(J) .EQ. LDAT(4:6)) GO TO 717
716	CONTINUE
	GO TO 99
C
717	WRITE (KEY(3:4),'(i2)') J
	IF (KEY(3:3) .EQ. ' ') KEY(3:3)='0'
C
	KEY=LDAT(10:11)//KEY(3:4)//LDAT(1:2)
	GO TO 39
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
65	CLOSE (50)
C
C
	DO 363 I=43,2,-1
		RVAL(I-1,1)=XVAL(I,1)-XVAL(I-1,1)
		RVAL(I-1,2)=XVAL(I,2)-XVAL(I-1,2)
		RVAL(I-1,3)=XVAL(I,3)-XVAL(I-1,3)
363	CONTINUE
C
	DO 364 I=2,43
		XVAL(I-1,1)=XVAL(I,1)
		XVAL(I-1,2)=XVAL(I,2)
		XVAL(I-1,3)=XVAL(I,3)
364	CONTINUE
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	  convert xbar and r values to graph points
C	
C
	DO 22 LN=1,3
	DO 22 INDX=1,42
C
		YPB(INDX,LN)=(XVAL(INDX,LN)-YAXB)*.05625*YJ+.42
		IF (YPB(INDX,LN) .LT. .42)YPB(INDX,LN)=.42
		YRB(INDX,LN)=RVAL(INDX,LN)*.05625*YRR+.225
		IF (YRB(INDX,LN) .LT. .225)YRB(INDX,LN)=.225
C
C
22	CONTINUE
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCC	
C
23	ISTAT=STR$FIND_FIRST_SUBSTRING(LABEL,IX,IC,'BEFORE')
	LX=3
C
C
C	    look for BT or AT in label string and set target accordingly
C
	IF (IX .GT. 0) LX=5
C
C
C
	CALL GKS$SET_PLINE_LINETYPE (1)
C
	QW=2
	CALL GKS$SET _PLINE_LINEWIDTH (QW)
C
	mmx=2
	call gks$set_pline_color_index (mmx)
c
	CALL GKS$POLYLINE (NUM_POINTS,PX,PY(1))
	CALL GKS$POLYLINE (NUM_POINTS,PX,PY(3))
	CALL GKS$POLYLINE (NUM_POINTS,PX,PY(5))
	CALL GKS$POLYLINE (NUM_POINTS,PX,PY(7))
C
C	  generate vertical x lines 
c
	XAX(1)=.1
C
	DO 822 I=2,42
822		XAX(I)=XAX(I-1)+.020731707
C
C
C
	X(1)=.1
	X(2)=.1
	Y(1)=.225
	Y(2)=TOPR
C
	DO 33 I=1,42
C
	CALL GKS$POLYLINE (NUM_POINTS,X,Y)
C
	X(1)=X(1)+.020731707
	X(2)=X(1)
C
33	CONTINUE
C
C
	X(1)=.1
	X(2)=.1
	Y(1)=.42
	Y(2)=TOPX
C
	DO 133 I=1,42
C
	CALL GKS$POLYLINE (NUM_POINTS,X,Y)
C
	X(1)=X(1)+.020731707
	X(2)=X(1)
C
133	CONTINUE
C
C
C	DRAW THE HORIZ. LINES FOR RANGE
C	
	X(1)=.1
	X(2)=.95
	Y(1)=.225+.05625
	Y(2)=.225+.05625
C
	DO 4233 I=1,2
C
	CALL GKS$POLYLINE (NUM_POINTS,X,Y)
C
	Y(1)=Y(1)+.05625
	Y(2)=Y(1)
C
4233	CONTINUE
C
C
C
C
C
C	   DRAW THE HORIZ. LINES FOR X
C	
	X(1)=.1
	X(2)=.95
	Y(1)=.42+.05625
	Y(2)=.42+.05625
C
C
C
C
	mmx=2
	call gks$set_pline_color_index (mmx)
C
	DO 2333 I=1,6
C
	MMT=1
		CALL GKS$SET_PLINE_LINETYPE (MMT)
	ZW=2
		CALL GKS$SET_PLINE_LINEWIDTH (ZW)
C
C
	CALL GKS$POLYLINE (NUM_POINTS,X,Y)
C
	Y(1)=Y(1)+.05625
	Y(2)=Y(1)
C
2333	CONTINUE
C
CCCCCC
C	   DRAW THE HORIZ. LINES FOR X AT .2 INTERVALS
C
	QW=1
	CALL GKS$SET _PLINE_LINEWIDTH (QW)
C	
	X(1)=.1
	X(2)=.95
	Y(1)=.42+.01125
	Y(2)=.42+.01125
C
	mmx=12
	call gks$set_pline_color_index (mmx)
C
	DO 443 J=1,7
	DO 7333 I=1,4
C
	CALL GKS$POLYLINE (NUM_POINTS,X,Y)
C
	Y(1)=Y(1)+.01125
	Y(2)=Y(1)
C
7333	CONTINUE
C
	Y(1)=Y(1)+.01125
	Y(2)=Y(1)
C
443	CONTINUE
C	
C
C	DRAW THE HORIZ. LINES FOR RANGE AT .2 INTERVALS
C	
	X(1)=.1
	X(2)=.95
	Y(1)=.225+.01125
	Y(2)=.225+.01125
C
	DO 233 I=1,3
C
	DO 909	L=1,4
	CALL GKS$POLYLINE (NUM_POINTS,X,Y)
C
	Y(1)=Y(1)+.01125
	Y(2)=Y(1)
C
909	CONTINUE
C
	Y(1)=Y(1)+.01125
	Y(2)=Y(1)
C
233	CONTINUE
CCCCCC
C
C
C
C
C
	CALL GKS$SET_TEXT_HEIGHT (.013)	
C
C	    PRINT THE Y AXIS VALUES FOR R GRAPH
C
	mmx=7
	call gks$set_pline_color_index (mmx)
c
c
	X(1)=0
	Y(1)=.213
	J=1
	XXX=(YAXRE-YAXRB)/3
	YY=YAXRB
C
	DO 140 I=1,4
C
		WRITE (AO,'(F6.3)')YY
C
		CALL GKS$TEXT (X(1),Y(1),AO)
C
		YY=YY+XXX
C
		Y(1)=Y(1)+.05625
C
140	CONTINUE
C
C
C	    PRINT THE Y AXIS VALUES FOR THE X GRAPH
C
	X(1)=0
	Y(1)=.415
	J=1
	XXX=(YAXE-YAXB)/7
	YY=YAXB
	DO 240 I=1,8
C
		WRITE (AO,'(F6.1)')YY
C
		CALL GKS$TEXT (X(1),Y(1),AO)
C
		YY=YY+XXX
C
	Y(1)=Y(1)+.05625
C
240	CONTINUE
C
	CALL GKS$SET_TEXT_HEIGHT (.01)	
C
C
C
C	   PRINT THE DATES
C
	mmx=5
	call gks$set_pline_color_index (mmx)
	call gks$set_text_color_index (mmx)
c
	CALL GKS$SET_TEXT_HEIGHT (.01)	
C
C
C
	X(1)=.01
	Y(1)=.192  
C
	CALL GKS$TEXT(X(1),Y(1),'DATE')
C
C
C
	CALL GKS$SET_TEXT_PATH (3)
C
	X(1)=.099
	Y(1)=.21
C
	mmx=4
	mam=0
c
	DO 595 I=1,42
	CALL SYS$ASCTIM (,DX23,DTM(1,I),)
C
	mam=mam+1
	if (mam .le. 2) then
		mmx=5
 	else
		mmx=7
c
	end if
c
	if (mam .ge. 4) mam=0
c
	call gks$set_text_color_index (mmx)
C
	CALL GKS$TEXT(X(1),Y(1),DX23(1:2))
C
	X(1)=X(1)+.020731707
595	CONTINUE
C
C
	X(1)=.2375
	Y(1)=.21 	!day no.
C
c	DO 49 I=2,7
c	IF ((DATE(I)(2:2).LT.'0').OR.(DATE(I)(2:2).GT.'9')) GOTO49 
c	DX11=DATE(I)(1:6)//'-'//val.date(I)(10:11)
ccccc	DAX=DATE(I)(1:2)
Cc
c c	CALL GKS$TEXT(X(1),Y(1),DX11)
C
c	X(1)=X(1)+(.0425*3)
C
c	MPTCNT=MPTCNT+3 	!COUNT HOW MANY POINTS TO OUTPUT
C
c49	CONTINUE
C
C
C
C	  print month no. above range chart
C
C
	X(1)=.0993
	Y(1)=.43
C
	DO 3595 I=1,42
	CALL SYS$ASCTIM (,DX23,DTM(1,I),)
C
	DO 379 NR=1,12
C
		IF (DX23(4:6) .EQ. MON(NR)) GO TO 1144
C
379	CONTINUE
C
	GO TO 1919
C
1144	WRITE (A2,'(I2)') NR
C	
	CALL GKS$TEXT(X(1),Y(1),A2)
C
	X(1)=X(1)+.020731707 
3595	CONTINUE
C
	CALL GKS$SET_TEXT_PATH (0)
C
C
C
C
C
	MPTCNT=42
CCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	    headings
C
C
C
	CALL GKS$SET_TEXT_HEIGHT (.01)	
C
C
	LABEL=' agglom. STEPS 1 &2  BENT LBS./TON by shift'
	X(1)=.18
	Y(1)=.83
	CALL GKS$TEXT (X(1),Y(1),LABEL(1:40))
C
C
C	   get today's date and print at top
C
	CALL SYS$ASCTIM (,SPCA,DTM(1,42),)
C
	CALL GKS$SET_TEXT_HEIGHT (.01)	
	X(1)=.65
	Y(1)=.83
	CALL GKS$TEXT (X(1),Y(1),SPCA(1:17))
C
C
	CALL GKS$SET_TEXT_HEIGHT (.013)	
C
C
	mmx=5
	call gks$set_pline_color_index (mmx)
c
	X(1)=.24
	Y(1)=.88
	CALL GKS$TEXT (X(1),Y(1),'TREND CHART')
C
C
	mmx=5
	call gks$set_text_color_index (mmx)
C
	call gks$set_text_height (.017)
C
	X(1)=.26
	Y(1)=.93
	CALL GKS$TEXT (X(1),Y(1),'  USS MINNTAC AGGLOMERATOR')
C
	OLDE=1
	CALL GKS$SET_TEXT_FONTPREC (OLDE,GKS$K_TEXT_PRECISION_STROKE)
CCCCCCCCCCCCCCCCCCCCCCC
C
	CALL GKS$SET_TEXT_HEIGHT (.015)	
C
	X(1)=.1
	Y(1)=.83
	CALL GKS$TEXT (X(1),Y(1),'AVERAGES')
C
	X(1)=.1
	Y(1)=.38
	CALL GKS$TEXT (X(1),Y(1),'RANGES')
C
C
C
C
	mmx=2
	call gks$set_text_color_index (mmx)
C
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	    DRAW THE UCL,LCL,MEAN,TARGET
C
C
C		MEAN
C
361	X(1)=.1
	X(2)=.95
	Y(1)=(XBAR-YAXB)*.05625*YJ+.42
	Y(2)=Y(1)
C
	XXX=5.
C
	m11=14
	MAMBO=1
	CALL GKS$SET_PLINE_LINETYPE (MAMBO)
	CALL GKS$SET_PLINE_LINEWIDTH (XXX)
	call gks$set_pline_color_index (m11)
C
	N21=2
C
ccccc	CALL GKS$POLYLINE (N21,X,Y)
C
c
	m14=10
	call gks$set_text_color_index (m14)
	WRITE (RSIDE(1:6),'(F5.2)')XBAR
	RSIDE=RSIDE(1:6)
C
C
	Y(1)=Y(1)+.002
	X(2)=X(2)
ccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
C
	RSIDE=' MEAN'
	Y(1)=Y(1)-.014
	X(2)=X(2)
cccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
CCCCCCCCCCCCCCCCCC
C
C	      UCL
C
	X(1)=.1
	X(2)=.95
	Y(1)=(UCL-YAXB)*.05625*YJ+.42
C
	IF (Y(1) .GT. TOPX) Y(1)=TOPX
C
	Y(2)=Y(1)
C
	XXX=8
C
	m4=14
	MAMBO=2
	call gks$set_pline_color_index(m4)
	CALL GKS$SET_PLINE_LINETYPE (MAMBO)
	CALL GKS$SET_PLINE_LINEWIDTH (XXX)
C
	N21=2
C
ccccc	CALL GKS$POLYLINE (N21,X,Y)
C
	WRITE (RSIDE(1:6),'(F5.2)')UCL
	RSIDE=RSIDE(1:6)
C
	Y(1)=Y(1)+.002
	X(2)=X(2)
cccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
	RSIDE=' UCL'
C
	Y(1)=Y(1)-.014
	X(2)=X(2)
cccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
CCCCCCCCCCCCCCCCCC
C
C	      LCL
C
	X(1)=.1
	X(2)=.95
	Y(1)=(LCL-YAXB)*.05625*YJ+.42
	Y(2)=Y(1)
C
	XXX=8
C
	MAMBO=2
	CALL GKS$SET_PLINE_LINETYPE (MAMBO)
	CALL GKS$SET_PLINE_LINEWIDTH (XXX)
C
	N21=2
C
cccccc	CALL GKS$POLYLINE (N21,X,Y)
C
	WRITE (RSIDE(1:6),'(F5.2)')LCL
C
	Y(1)=Y(1)+.002
	X(2)=X(2)
cccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE(1:6))
	RSIDE=' LCL'
C
	Y(1)=Y(1)-.014
	X(2)=X(2)
ccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
CCCCCCCCCCCCCCCCCC
C
C	      TARGET
C
	MAMBO=3
	CALL GKS$SET_PLINE_LINETYPE (MAMBO)
	ABC=16.8
	X(1)=.1
	X(2)=.95
	Y(1)=(abc-YAXB)*.05625*YJ+.42
	Y(2)=Y(1)
	CALL GKS$POLYLINE (N21,X,Y)
C
	WRITE (RSIDE(6:9),'(F4.1)')ABC
	RSIDE='B.P. '//RSIDE(6:9)
C
C
	Y(1)=Y(1)+.002
	X(2)=.023
	CALL GKS$TEXT (X(2),Y(1),RSIDE)
CCCCCCCCCCCCCCCCCC
C
C	      UCL FOR R
C
	MAMBO=2
	CALL GKS$SET_PLINE_LINETYPE (MAMBO)
	X(1)=.1
	X(2)=.95
	Y(1)=(UCLR)*.05625*YRR+.225
	IF (Y(1) .GT. TOPR) Y(1)=TOPR
	Y(2)=Y(1)
C
C
	XXX=4
C
	MAMBO=2
	CALL GKS$SET_PLINE_LINETYPE (MAMBO)
	CALL GKS$SET_PLINE_LINEWIDTH (XXX)
C
	N21=2
C
ccccc	CALL GKS$POLYLINE (N21,X,Y)
C
C
	WRITE (RSIDE(1:6),'(F5.2)')UCLR
	Y(1)=Y(1)+.002
	X(2)=X(2)
ccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE(1:6))
C
	RSIDE=' UCL'
	Y(1)=Y(1)-.014
	X(2)=X(2)
cccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
CCCCCCCCCCCCCCCCCCCCCCCCC
C	LCL FOR R
CCCCCCCCCCCCCCCCCCCCCCCCC
C
	Y(1)=.225+.002
C
	RSIDE='0.00   LCL'
CCC	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
CCCCCCCCCCCCCCCCCCC
C
C	      R-BAR LINE AND LABEL
C
	X(1)=.1
	X(2)=.95
	Y(1)=(RBAR)*.05625*YRR+.225
	Y(2)=Y(1)
C
	XXX=5.
C
	MAMBO=1
	CALL GKS$SET_PLINE_LINETYPE (MAMBO)
	CALL GKS$SET_PLINE_LINEWIDTH (XXX)
C
C
	N21=2
C
ccccc	CALL GKS$POLYLINE (N21,X,Y)
C
	WRITE (RSIDE(1:6),'(F5.2)')RBAR
C
	Y(1)=Y(1)+.002
	X(2)=X(2)
cccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE(1:6))
C
	RSIDE=' MEAN'
C
	Y(1)=Y(1)-.014
	X(2)=X(2)
ccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
c
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
C
	CALL GKS$SET_TEXT_HEIGHT (.013)	   !patch
C
C	 print the X-bar and R values at the bottom
C
	X(1)=.079
	QA=.16  	!bt
	QB=0.14		!R
	LR=1
	J=1
C
	jjj=0
C
     	xhl=.012
	call gks$set_text_height (xhl)
	CALL GKS$SET_TEXT_EXPFAC(.79)
	DO 407 I=1,MPTCNT
C
	IF (LR .EQ. 1) THEN
cc		QA=.16
cc		QB=.14
		QA=.16
		QB=.135
		LR=0
	ELSE
cc		QA=.12
cc		QB=.10
		QA=.11
		QB=.085
		LR=1
	END IF
C
C
C
	jjj=jjj+1
	IF (JJJ .le. 2) THEN
		MMX=5
	ELSE 
		MMX=7
	end if
C
	if (jjj .ge. 4) jjj=0
C
C
	call gks$set_text_color_index (mmx)

c
C
C
C
	X(1)=X(1)+.020731707
C
407	CONTINUE
C
C
C
	X(1)=.03
	Y(1)=.14
cccccccc	CALL GKS$TEXT (X(1),Y(1),'R')
C
	X(1)=.03
	Y(1)=.16
cccc	CALL GKS$TEXT (X(1),Y(1),'X    ')
C
 	X(1)=.03
 	Y(1)=.12
cccc 	CALL GKS$TEXT (X(1),Y(1),'X    ')
C
 	X(1)=.03
 	Y(1)=.10
ccccc 	CALL GKS$TEXT (X(1),Y(1),'R    ')
C
C
c	X(1)=.03
c	Y(1)=.08
c	CALL GKS$TEXT (X(1),Y(1),'X(5) ')
C
c	X(1)=.03
c	Y(1)=.06
c	CALL GKS$TEXT (X(1),Y(1),'X(6) ')
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c	  PRINT THE TIMES AT BOTTOM OF GRAPH
C
	CALL GKS$SET_TEXT_EXPFAC(1.0)
	CALL GKS$SET_TEXT_PATH (3)
C
	X(1)=.099
	Y(1)=.08
C
	mam=0
c
	DO 5795 I=1,42
	CALL SYS$ASCTIM (,DX23,DTM(1,I),)
C
	mam=mam+1
	if (mam .le. 2) then
		mmx=5
 	else
		mmx=7
c
	end if
c
	if (mam .ge. 4) mam=0
c
	call gks$set_text_color_index (mmx)
C
ccccc	CALL GKS$TEXT(X(1),Y(1),DX23(13:14)//DX23(16:17))
C
	X(1)=X(1)+.020731707
5795	CONTINUE
C
	CALL GKS$SET_TEXT_PATH (0)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	    PLOT THE POINTS
C
	XXX=5.
C
	CALL GKS$SET_PLINE_LINEWIDTH (XXX)
C
C
	N21=MPTCNT
C
	DO 882 I=1,42
882		PTBL(I)=YPB(I,1)
C
	m4=4
c
	call gks$set_pline_color_index(m4)
	CALL GKS$POLYLINE (N21,XAX,PTBL)
C
	X(2)=.1
	Y(1)=.14
	CALL GKS$TEXT (X(2),Y(1),'line 3 ')
C
	n=2
	xcc(1)=.25
	xcc(2)=.35
	PTBL(1)=.14
	PTBL(2)=.14
C
	CALL GKS$POLYLINE (N,XCC,PTBL)
C
	DO 883 I=1,42
883		PTBL(I)=YPB(I,2)
C
	m4=5
c
	call gks$set_pline_color_index(m4)
	ML=2
	CALL GKS$SET_PLINE_LINETYPE (ML)
c
c	
	CALL GKS$POLYLINE (N21,XAX,PTBL)
C
	X(2)=.1
	Y(1)=.12
	CALL GKS$TEXT (X(2),Y(1),'line 4 ')
C
	n=2
	xcc(1)=.25
	xcc(2)=.35
	PTBL(1)=.12
	PTBL(2)=.12
C
	CALL GKS$POLYLINE (N,XCC,PTBL)
C
	DO 3882 I=1,42
3882		PTBL(I)=YPB(I,3)
C
	m4=3
c
	call gks$set_pline_color_index(m4)
	ML=3
	CALL GKS$SET_PLINE_LINETYPE (ML)
	CALL GKS$POLYLINE (N21,XAX,PTBL)
C
c
	X(2)=.1
	Y(1)=.1
	CALL GKS$TEXT (X(2),Y(1),'line 5 ')
c
	n=2
	xcc(1)=.25
	xcc(2)=.35
	PTBL(1)=.1
	PTBL(2)=.1
C
	CALL GKS$POLYLINE (N,XCC,PTBL)
c
ccccccccccccccccccccccccccccc
c
	ML=1
	CALL GKS$SET_PLINE_LINETYPE (ML)
C
C	m4=5
c
ccc	call gks$set_pline_color_index(m4)
CCC	CALL GKS$POLYLINE (N21,XAX,YRB)
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
c
c	   get std dev of values that appear on graph
c
c
	RN=0
	R22=0
C
	do 778 I=1,MPTCNT
C
C
cccccc		R22=R22+RVAL(I)
		RN=RN+1
C
665		CONTINUE
778	CONTINUE
C
	R22=R22/RN	!R BAR OF CHART VALUES
	SDH=R22/1.128
C
C
	XXX=0.
	XV2=0.
	XNC=0.
	DO 337 I=1,MPTCNT
C
cccccc	IF (XVAL(I) .LE. 0) GO TO 337
CCCCCC	XXX=XXX+XVAL(I)
	XNC=XNC+1.
C
337	CONTINUE
C
	XBARCHART=XXX/XNC
C
C
C
C
C	   calc cp and cpk WITH SCREEN VALUES
C
	CALL GKS$SET_TEXT_HEIGHT (.013)	
C
C
	if (SDH .le. 0) go to 9997
C
C
	XUSL=4.9
	XLSL=2.0
C
C
	CPK=MIN(((XBARCHART-XLSL)/(3*SDH)),((XUSL-XBARCHART)/(3*SDH)))
C
	CP=(XUSL-XLSL)/(6*SDH)
C
C
	WRITE (CPA,'(F6.2)')CP
	WRITE (CPKA,'(F6.2)')CPK
C
	RSIDE='  CP= '//CPA
C
	X(2)=.86
	Y(1)=.9
cccccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
	RSIDE='  CPK='//CPKA
C
	X(2)=.86
	Y(1)=.88
cccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
	WRITE (CPA,'(F6.4)') SDH
	RSIDE='S.D. '//CPA
	X(2)=.86
	Y(1)=.86
cccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
	WRITE (CPA,'(F6.2)') XBARCHART
	RSIDE='XBAR '//CPA
	X(2)=.86
	Y(1)=.84
cccc	CALL GKS$TEXT (X(2),Y(1),RSIDE)
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C
	IF (MBA .EQ. 0) GO TO 1919 !!!!!!!!!!!!!!!!!!!!!PATCH!!!!!!!!!!!!!!!
C
9997	CONTINUE
C
9998	CONTINUE
C
1919	call lib$wait (10.)
	CALL GKS$DEACTIVATE_WS (WS_ID)
	CALL GKS$CLOSE_WS (WS_ID)
	CALL GKS$CLOSE_GKS ()
	CALL LIB$WAIT (2.)
899	WRITE (6,445) ES//BSL
445	FORMAT (' ',A)
99	CONTINUE
	END
