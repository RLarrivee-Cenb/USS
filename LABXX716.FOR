CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CONFIDENTIAL
C       Property of United States Steel Corporation
C       Copyright 2010 United States Steel Corporation
C       All rights reserved.
C******************************************************************************
C               LABXX716.FOR
C******************************************************************************
C Revisions:
C       20100809 IM322852  SPA6635 changed COMP2(2,3,2,5) to COMP2(3,3,2,5)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	EXTERNAL UFO_OPEN
C
C
	INCLUDE 'FMS$EXAMPLES:FDVDEF'
Ccccccccccccccc
C
CCCCCCCCCCCCC
C
	INTEGER*2  KEYTABLE(24)/FDV$K_KF_NTR,FDV$K_KF_NONE,
	1		       	FDV$K_KF_NXT,FDV$K_KF_NONE,
	1		       	FDV$K_KF_PRV,FDV$K_KF_NONE,
	1			FDV$K_KF_SFW,FDV$K_KF_NONE,
	1			FDV$K_KF_SBK,FDV$K_KF_NONE,
	1		       	FDV$K_KF_NXT,107,
	1			fdv$k_kf_nxt,1037,
	1			fdv$k_kf_ntr,235,
	1			FDV$K_KF_NTR,269,
	1		       FDV$K_KF_NXT,FDV$K_AR_DOWN,
	1		       FDV$K_KF_PRV,FDV$K_AR_UP,	
	1		       FDV$K_KF_PRV,FDV$K_FK_F12/	
CCCCCCCCCCCCC
C
	INTEGER  WORKSPACE( 3 ),	!General workspace
	1	 CHECKWKSP( 3 ),	!Check workspace
	2	 TCA( 3 ),		!Terminal Control Area
    	3	 GASN_FORM(500),	!Storage for memory resident form
    	4	 GASD_FORM(750) 	!Storage for memory resident form
C
	integer flag/64/
C
 	DOUBLE PRECISION CT(18),FT(18),SCREEN(14,3,2,5),COMP(2,3,2,5),
	1  CAB(2,3,5),SIL(3,5),FILT2(5,3),BL2,FILT3(5,3),BL3,TOTAL,
	1  H2O,RMF,FTMF,BOIC(150),HOIC(150),CX(5),MX(5),AX(5),SX(5),
	1  FLUXG,P34,P12,FCA(2,3),FMG(2,3),FAL(2,3),FSI(2,3),
	1  PA(3,5),PC(3,5),PM(3,5),SCWT(3,2,5),
	1  TWT(5),BF(7,5),AF(7,5),TSW(5),TONS4(3,2,5),GHRS(3,2,5),
C	1  COMP2(2,3,2,5)
	1  COMP2(3,3,2,5)

C                                                                 
	INTEGER AMB(3),BMH(3),BD(2),YSTRDAY(2),NEW(3),LAST(3),
	1	IN,INO,DAT(2),INTV(2),IS,IS2,IS3,IH,STATUS,S4,H4
C
	DIMENSION ARD(8)
C
	CHARACTER PL(2)*2/'CN','AG'/,FN2*25,
	1	MO(12)*3/'JAN','FEB','MAR','APR',
	2	'MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
	3	,CAD*9,FILN*16,SH*2,TEMP*12,MINP,AINTV/'1'/,
	1	INAM(4)*6/'AMIOIC','BMIOIC',' AMBOP',' BMHOP'/,  
	1	PART(3)*16/'SCREEN FRACTIONS','       SIO2     ',
	2	'FILTER CAKE     '/,AYSTR*23,FILNAM*30,ASC2*3,
	1	PP1*29/'SCREEN FRACTIONS,COMPRESSIONS'/,KD*8,A5*5             
C
	CHARACTER SHED*32/'+5/8,+1/2,+3/8,+1/4,4M,+28M,-28M'/,
C
	2	CHED*38/'COMPRESSION AVG., -200 PCT.'/,
C
	2	BHED*18 /'AL203,CAO,MGO'/,
	2	BHEX*18 /'SIO2,AL203,CAO,MGO'/,
C
	3	FES*4/'SIO2'/,
C
	4	FILT*26/'SIO2,-270,-500,AL2O3,CAO,MGO'/,
C
	5	BAT(2)*6/'BEFORE','AFTER '/,
C
	1	MNTH(12)*3/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
	1'AUG','SEP','OCT','NOV','DEC'/,TDAY*11,YDAY*11,KY*8,AD*23,
	1 KY6*6
C
	INTEGER D2,EXIST,LS,LH
C
	CHARACTER ASC*10,FULLDATE*23
C
C
C	   structure for GRAD1
C
	STRUCTURE /SCREEN1/
		UNION
			MAP
C
				CHARACTER SC*2000
C
			END MAP
C
			MAP
				CHARACTER DATE*6,LINE,SHIFT,HALF
				CHARACTER TOTAL*5,BT(6)*5,AT(6)*5,
	1				  BTM*5,ATM*5,BTUM*6,ATUM*6,EXIT
			END MAP
C
		END UNION
C
	END STRUCTURE
C
C
	RECORD /SCREEN1/ MAP1
C******************************************************************************
C
464	CALL FDV$ATERM( %DESCR(TCA),12 ,2)
	CALL FDV$AWKSP( %DESCR(WORKSPACE), 2000 )
	CALL FDV$LOPEN( 'user_d:[METREP.LAB45]LABSUB2', 1 )
ccccc	call fdv$spada (0)
	CALL FDV$SPON ()
C
	CALL FDV$STIME (120)
C
	CALL FDV$DFKBD (%DESCR(KEYTABLE),12)
C                   
C
	CALL SYS$BINTIM (AINTV,INTV) !convert 24 hr interval to binary
C
C
	OPEN (11,FILE='MET_SCREEN:SCREEN.DAT',
	1	ACCESS='KEYED',STATUS='OLD',shared)
C
C
	AGL=0
C
C******************************************************************************
C	 bring in LAB4.DAT to get date,shift, half in ascii of last entry
C******************************************************************************
	OPEN (UNIT=10,FILE='USER_D:[METREP.LAB45]LAB4.DAT',STATUS='OLD',
	1ACCESS='DIRECT',FORM='FORMATTED',SHARED,ERR=255)
C
	GO TO 2655
C
255	CALL FDV$PUTL (' THE DATE,SHIFT,HALF FILE IS MISSING')
	CALL LIB$WAIT (30.)
	GO TO 99
C
2655	READ(10,55,REC=1)KY6,LS,LH
C
55	FORMAT (A6,I1,I1)
C
	KY='19'//KY6
	IF (KY6(1:2) .LT. '66') KY='20'//KY6
                                                 
C******************************************************************************
	IF ((LS .EQ. 3) .AND. (LH .EQ. 2)) THEN
C
		AD(7:11)='-'//KY(1:4)
		READ (KY(5:6),'(I2)')MV
		AD(3:6)='-'//MNTH(MV)
		AD(1:2)=KY(7:8)
		ad(12:23)='            '
C
		CALL SYS$BINTIM (AD,BD)
		CALL LIB$ADD_TIMES (BD,INTV,BD)
		CALL SYS$ASCTIM (,AD,BD,)
		IF (AD(1:1) .EQ. ' ') AD(1:1)='0'
C
		DO 91 I=1,12           
			IF (MNTH(I) .EQ. AD(4:6)) GO TO 191
91		CONTINUE
C
C
		STOP ' MONTH ERROR'
C
191		WRITE (KY(5:6),'(I2)') I
		IF (KY(5:5) .EQ. ' ') KY(5:5)='0'
C
		
		KY(1:4)=AD(08:11)
		KY(7:8)=AD(1:2)
C
		LS=1
		LH=1
C
		KY6=KY(3:8)
	ELSE
C
		IF (LH .EQ. 2) THEN
			LS=LS+1
			LH=1
		ELSE
			LH=2
		END IF
C
C
	END IF
C
C
	IREPEAT=0	!0 says that no changes have been made
C
C
1	CALL FDV$CDISP ('LAB1X2')
C
C
	AGL=1                           
C
	READ (11,KEY=KY,IOSTAT=MMM) KD,DAT,IS,IH,SCREEN,COMP,CAB,SIL,FILT2,
	1			    FILT3,BL2,BL3,FLUXG,FCA,FMG,FAL,FSI,   
	1			    PA,PC,PM,S4,H4,SCWT,TONS4,GHRS,COMP2
C                                   
	NEWNEW=0
C		iostat 36 - attempt to access non-existent record
	IF (MMM .EQ. 36) NEWNEW=1
		KD=KY
		AD(7:11)='-'//KY(1:4)
		READ (KY(5:6),'(I2)')MV
		AD(3:6)='-'//MNTH(MV)
		AD(1:2)=KY(7:8)
		ad(12:23)='            '
		CALL SYS$BINTIM (AD,DAT)
C
C		iostat 52 - attempt to access locked record
	IF (MMM .EQ. 52) THEN
C
		CALL FDV$PUTL (' RECORD LOCKED')
		CALL LIB$WAIT (3.)
		GO TO 99
C
	END IF                                  
C
5959	ASC(1:2)=KY(5:6)
	ASC(3:4)=KY(7:8)
	ASC(5:6)=KY(3:4)
C
	CALL FDV$PUT (ASC(1:6),'DATE')
	WRITE (ASC,'(I1)') LS
	CALL FDV$PUT (ASC(1:1),'SHIFT')
	WRITE (ASC,'(I1)') LH
	CALL FDV$PUT (ASC(1:1),'HALF')
C
	S4=LS
	H4=LH
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C		area to zero out data for first input of s 1  h 1
C
C		NEWNEW = 1 means create 0 filled records in screen.dat
	IF (NEWNEW .NE. 1) GO TO 59
	IF (.NOT.((S4 .EQ. 1) .AND. (H4 .EQ. 1))) GO TO 59
C
C
C
	DO 116 L=1,5
	DO 116 LLS=1,3
	DO 116 LLH=1,2
	DO 116 LL=1,14
116		SCREEN(LL,LLS,LLH,L)=0
C
	DO 117 L=1,5
	DO 117 LLS=1,3
	DO 117 LLH=1,2
		COMP(1,LLS,LLH,L)=0
117		COMP(2,LLS,LLH,L)=0
C
	DO 118 L=1,5
	DO 118 LLS=1,3
		CAB(1,LLS,L)=0
		CAB(2,LLS,L)=0
		SIL(LLS,L)=0
		FILT2(L,LLS)=0
		FILT3(L,LLS)=0
		PA(LLS,L)=0
		PC(LLS,L)=0
		PM(LLS,L)=0
118	CONTINUE
C
	BL2=0
	BL3=0
	FLUXG=0
C
C
	DO 119 L=1,3
	DO 119 LL=1,2
		FCA(LL,L)=0
		FMG(LL,L)=0
		FAL(LL,L)=0
		FSI(LL,L)=0
119	CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
59	DO 1000 LINE=1,5
C
C
	WRITE (ASC,'(I1)') LINE+2
	CALL FDV$PUT (ASC,'LINE')
C
640	ASC(1:5)='     '
	CALL FDV$PUT (ASC(1:5),'TOTAL')
C
	DO 48 M=1,6
	CALL FDV$PUT (ASC(1:5),'BT',M)
	CALL FDV$PUT (ASC(1:5),'AT',M)
48	CONTINUE
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C	see if changes were requested.  IREPEAT=1 if they were.
C
C
C
	IF (IREPEAT .EQ. 0) GO TO 3031
C
C
	if (twt(line) .le. 0) go to 3031
C
	IJ=TWT(LINE)
c
	WRITE (ASC,'(I5)') IJ
	CALL FDV$PUT (ASC,'TOTAL')
C
	DO 1911 MM=1,6
		IJ=SCREEN(MM,S4,H4,LINE)
		WRITE (ASC,'(I5)') IJ
		CALL FDV$PUT (ASC,'BT',MM)
		IJ=SCREEN(MM+7,S4,H4,LINE)
		WRITE (ASC,'(I5)') IJ
		CALL FDV$PUT (ASC,'AT',MM)
1911	CONTINUE
C
		IJ=SCREEN(7,S4,H4,LINE)
		WRITE (ASC,'(I5)') IJ
		CALL FDV$PUT (ASC,'BTM')
		IJ=SCREEN(14,S4,H4,LINE)
		WRITE (ASC,'(I5)') IJ
		CALL FDV$PUT (ASC,'ATM')
C
C
		XB=0
		XA=0
		XB4=0
		XA4=0
C
		DO 238 II=1,7
C
			XB=XB+SCREEN(II,S4,H4,LINE)
			XA=XA+SCREEN(II+7,S4,H4,LINE)
C
			IF (II .LE. 5) THEN
C
				XB4=XB4+SCREEN(II,S4,H4,LINE)
				XA4=XA4+SCREEN(II+7,S4,H4,LINE)
C
			END IF
C
238		CONTINUE
C
C
		XB4=XB4/XB*100.
		XA4=XA4/XA*100.
C
		WRITE (ASC,'(F6.2)') XB4
		CALL FDV$PUT (ASC,'BTUM')
		WRITE (ASC,'(F6.2)') XA4
		CALL FDV$PUT (ASC,'ATUM')
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
3031	LOOPCOUNT=0
C
	M28C=0
C
C		REQUEST TOTAL WEIGHT IN GRAMS
C
303	CALL FDV$GET (ASC,LX,'TOTAL')
C
	READ (ASC(1:5), '(F5.0)') TWT(LINE)
	TSW(LINE)=TWT(LINE)
C
	IF (TWT(LINE) .LE. 0) GO TO 1000
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	  read the rest of the screen
C
	I=1
C
304	DO WHILE (LX .NE. FDV$K_FT_PRV)
C
		IF (I .GT. 12) then
			lx=0
			GO TO 991
		end if
c
c
		IF (I .LE. 6) THEN
   			CALL FDV$GET (MAP1.BT(I),LX,'BT',I)
C
			READ (MAP1.BT(I),'(F5.0)') BF(I,LINE)
		ELSE
			J=I-6
   			CALL FDV$GET (MAP1.AT(J),LX,'AT',J)
C
			READ (MAP1.AT(J),'(F5.0)') AF(J,LINE)
		END IF
C
		I=I+1
C
	END DO
C
	I=I-2
C
	IF (I .LT. 1) THEN
		LX=0
		GO TO 303
	ELSE
		LX=0
		GO TO 304
	END IF
C
991		SUMAA=0
		SUMBT=0
		SUMAT=0
C
		DO 12 INDEX=1,6
			READ (MAP1.BT(INDEX),'(F5.0)') BF(INDEX,LINE)
			READ (MAP1.AT(INDEX),'(F5.0)') AF(INDEX,LINE)
C
			IF (INDEX .LE. 5)SUMAA=SUMAA+BF(INDEX,LINE)
			SUMBT=SUMBT+BF(INDEX,LINE)
			SUMAT=SUMAT+AF(INDEX,LINE)
12		CONTINUE
C
c
CCCCCCCCCCCC
C
C
c	   check if sum of after tumble is > bt +1/4
c
	IF (SUMAT .GT. SUMAA) THEN
	CALL FDV$BELL
	CALL FDV$BELL
	CALL FDV$PUTL ('AFTER TUMBLE WEIGHT TOO HIGH')
	GO TO 303
	END IF
C
C
CCCCCCCCCCCCCCCCC
c
c	   check if total b.t. weight is less than sum of indiv. weights
c
	IF ((TWT(LINE)-SUMBT) .LE. 0) THEN
C
		IF ((SUMBT-TWT(LINE)) .LT. 50) THEN
C
			TSW(LINE)=SUMBT+15.
C
		ELSE
C
		call fdv$bell
		call fdv$bell
	CALL FDV$PUTL ('SUM OF WEIGHTS HIGHER THAN TOTAL WEIGHT')
	CALL LIB$WAIT (2.)
	CALL FDV$PUTL ('                                            ')
C
		loopcount=loopcount+1
		if (loopcount .lt. 3) then
			GO TO 303 	!ask for reentry of data
c
		else if (loopcount .ge.3) then
c
		call fdv$bell
		call fdv$bell
		call fdv$putl ('weights still too high but I''ll continue')
		CALL LIB$WAIT (1.5)
	CALL FDV$PUTL ('                                            ')
c
			TSW(LINE)=SUMBT+15.
			loopcount=0
			go to 282
c
		end if
C
		END IF
C
	END IF
c
c
c
282	BF(7,LINE)=TSW(LINE)-SUMBT
	AF(7,LINE)=SUMAA-SUMAT
	IF (AF(7,LINE) .LT. 0) AF(7,LINE)=0
 
        KK=BF(7,LINE)           
        WRITE (ASC,'(I5)') KK   
        CALL FDV$PUT (ASC,'BTM')
C
        KK=AF(7,LINE)           
        WRITE (ASC,'(I5)') KK   
        CALL FDV$PUT (ASC,'ATM')
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	check if b.t. -28m is >500 grams.  if so, alarm once & then ok it
C
	IF (BF(7,LINE) .GT. 500) THEN
C
		IF (M28C .EQ. 0) THEN 
		call fdv$bell
		call fdv$bell
		CALL FDV$PUTL ('B.T. -28 MESH IS OVER 500 GRAMS')
		CALL LIB$WAIT (2.)
		M28C=M28C+1
		GO TO 303
		END IF
C
		IF (M28C .EQ. 1) THEN 
		call fdv$bell
		call fdv$bell
			CALL FDV$PUTL ('BT -28M IS STILL OVER 500 GRAMS, BUT
	1 WILL CONTINUE ANYWAY')
		END IF
	END IF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C		CALCULATE BT AND AT +1/4 FOR DISPLAY
C
C
		XB=0
		XA=0
		XB4=0
		XA4=0
C
		DO 2538 II=1,7
C
			XB=XB+BF(II,LINE)  
			XA=XA+AF(II,LINE)
C                             
			IF (II .LE. 5) THEN
C
				XB4=XB4+BF(II,LINE)           
				XA4=XA4+AF(II,LINE)          
C                                       
			END IF
C
2538		CONTINUE
C
C
		XB4=XB4/XB*100.
		XA4=XA4/XA*100.
C
		WRITE (ASC,'(F6.2)') XB4
		CALL FDV$PUT (ASC,'BTUM')
		WRITE (ASC,'(F6.2)') XA4
		CALL FDV$PUT (ASC,'ATUM')
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C	ASK IF INPUT IS OK OR NOT
C
		CALL FDV$GET (ASC,LX,'EXIT')
		IF (LX .NE. -2233) XXX=2
		IF ((ASC(1:1) .EQ. 'N').OR.(ASC(1:1) .EQ. 'n')) then
			CALL FDV$PUT (' ','EXIT')
			GO TO 3031
		else IF ((ASC(1:1) .eq. 'A').OR.(ASC(1:1) .eq. 'a')) then
			CALL FDV$PUT (' ','EXIT')
			GO TO 99
		else IF ((ASC(1:1) .EQ. 'Y').OR.(ASC(1:1) .EQ. 'y')) then
			CALL FDV$PUT (' ','EXIT')
			go to 1000
C
		END IF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                         C
1000	CONTINUE
C                                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
CCCCCCCC
C		store data for transfer to screenxx.dat file
C
	DO 200 L=1,5
		SCWT(S4,H4,L)=TWT(L)
	DO 200 I=1,7
		SCREEN (I,S4,H4,L)=BF(I,L)
		SCREEN (I+7,S4,H4,L)=AF(I,L)
C come back here
200	CONTINUE
C
C
CCCCCCCC
C
C
C
C
	CALL FDV$CDISP ('LABTX')
C
	DO 49 INDEX=1,6
C
	LQ=BF(INDEX,1)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'B3',INDEX)
C
	LQ=AF(INDEX,1)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'A3',INDEX)
C
	LQ=BF(INDEX,2)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'B4',INDEX)
C
	LQ=AF(INDEX,2)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'A4',INDEX)
C
	LQ=BF(INDEX,3)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'B5',INDEX)
C
	LQ=AF(INDEX,3)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'A5',INDEX)
C
	LQ=BF(INDEX,4)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'B6',INDEX)
C
	LQ=AF(INDEX,4)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'A6',INDEX)
C
	LQ=BF(INDEX,5)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'B7',INDEX)
C
	LQ=AF(INDEX,5)
	WRITE (A5,'(I5)') LQ
	CALL FDV$PUT (A5,'A7',INDEX)
C
C
49	CONTINUE
C
C
	DO 3191 LLL=1,5
		LJ=TWT(LLL)
		WRITE (A5,'(I5)') LJ
		CALL FDV$PUT (A5,'WEIGHT',LLL)
3191	CONTINUE
C
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	do 500 L=1,5
C
		XB=0
		XA=0
       		XB4=0
		XA4=0
		Q=0
C
		DO 499 J=1,7
C
			XB=XB+BF(J,L)
			XA=XA+AF(J,L)
			IF (J .GT. 5) GO TO 499
			XB4=XB4+BF(J,L)
			XA4=XA4+AF(J,L)
C
499		CONTINUE 
c
		IF (XB .LE. 0) GO TO 500
		XB=XB4/XB*100.
		IF (XA .LE. 0) GO TO 500
		XA=XA4/XA*100.
		Q=XB*XA/100.
C
		WRITE (ASC,'(F5.2)',ERR=500) XB
		CALL FDV$PUT (ASC,'BT4',L)
		WRITE (ASC,'(F5.2)',ERR=500) XA
		CALL FDV$PUT (ASC,'AT4',L)
		WRITE (ASC,'(F5.2)',ERR=500) Q
		CALL FDV$PUT (ASC,'Q',L)
c
C
		WRITE (ASC,'(F5.0)',ERR=500) BF(7,L)
		CALL FDV$PUT (ASC,'MB',L)
C
		WRITE (ASC,'(F5.0)',ERR=500) AF(7,L)
		CALL FDV$PUT (ASC,'MA',L)
C
500	CONTINUE
C
C
C
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c
c
	CALL FDV$STIME (240)
C
	IISTAT=FDV$GET (ASC,LX,'REPLY')
C
C
C		check for timeout
C
	IF (IISTAT .EQ. FDV$_TMO) GO TO 99
C
C
C
	IF ((ASC(1:1) .EQ. 'Y') .OR. (ASC(1:1) .EQ. 'y')) GO TO 17
C
C
C
	IF ((ASC(1:1) .EQ. 'A') .OR. (ASC(1:1) .EQ. 'a')) GO TO 99
C
C
	ASC(1:1)=' '
	CALL FDV$PUT (ASC,'REPLY')
C
	irepeat=1	! changes requested
C
C
	CALL FDV$CDISP ('LAB1X2')
C
C
	GO TO 5959
C
C******************************************************************************
17	IF (NEWNEW .EQ. 1) THEN
C
	WRITE (11) KD,DAT,IS,IH,SCREEN,COMP,CAB,SIL,FILT2,
	1			    FILT3,BL2,BL3,FLUXG,FCA,FMG,FAL,FSI,   
	1			    PA,PC,PM,S4,H4,SCWT,TONS4,GHRS,COMP2
C
	ELSE
C

	REWRITE (11) KD,DAT,IS,IH,SCREEN,COMP,CAB,SIL,FILT2,
	1			    FILT3,BL2,BL3,FLUXG,FCA,FMG,FAL,FSI,   
	1			    PA,PC,PM,S4,H4,SCWT,TONS4,GHRS,COMP2
C
C
	END IF
C
	WRITE(10,55,REC=1)KY6,LS,LH
C
	CLOSE (10)
	CLOSE (11)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	TURN ON AGGLOM GRAPHS
C
C
C
c	run labdat42n to generate data for [.gks]s2.for & s2fs3.for
	STATUS=LIB$SPAWN ('RUN USER_D:[METREP.GKS]LABDAT42N',,,flag)
ccccc	STATUS=LIB$SPAWN ('SUBMIT/QUE=AGGL2 USER_D:[METREP.GKS]SPC.COM')
CCCCC	STATUS=LIB$SPAWN ('SUBMIT/NOLOG USER_D:[METREP.GKS]SPC3.COM')
ccccc	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
	CALL FDV$CDISP ('LABEND')
C
	WRITE (ASC,'(I1)') S4
	CALL FDV$PUT (ASC,'SHIFT')
C
	WRITE (ASC,'(I1)') H4
	CALL FDV$PUT (ASC,'HALF')
C
	CALL SYS$ASCTIM (,FULLDATE,DAT,)
C
	ASC=FULLDATE(1:2)//FULLDATE(4:6)//FULLDATE(10:11)
C
	CALL FDV$PUT (ASC,'DATE')
C
C              SCREEN(14,3,2,5)
C
	
	DO 1300 LN=1,5
		TWT(LN)=0
		TSW(LN)=0
C
	DO 1300 IV=1,7
		TWT(LN)=TWT(LN)+SCREEN(IV,S4,H4,LN)
1300		TSW(LN)=TSW(LN)+SCREEN(IV+7,S4,H4,LN)
C
C
C
C
	DO 2330 LN=1,5
	SB=0
	SA=0
	DO 2320 IV=1,5
		SB=SB+SCREEN(IV,S4,H4,LN)
2320		SA=SA+SCREEN(IV+7,S4,H4,LN)
C
	sb2=sb
	sa2=sa
	IF (TWT(LN) .LE. 0) GO TO 644
	SB=SB/TWT(LN)*100.
644	IF (TSW(LN) .LE. 0) GO TO 2330
	SA=SA/TSW(LN)*100.
	WRITE (ASC,'(F5.2)')SB
	CALL FDV$PUT (ASC,'BT',LN) 
	WRITE (ASC,'(F5.2)')SA
	CALL FDV$PUT (ASC,'AT',LN)
C
	IF ((TWT(LN) .LE. 0) .OR. (TSW(LN) .LE. 0)) GO TO 2330
	Q=SB2/TWT(LN)*SA2/TSW(LN)*100.
	WRITE (ASC,'(F5.2)')Q
	CALL FDV$PUT (ASC,'Q',LN)
C
2330	CONTINUE
C 
	call lib$wait (300.)
C
C
99	CALL FDV$LCLOS
	CALL FDV$SPADA (0)
	CALL FDV$DEL ('LINEDATA')
	CALL FDV$DWKSP (%DESCR(WORKSPACE))
	CALL FDV$DTERM(%DESCR(TCA))
C
C
C
669	STOP
	END
