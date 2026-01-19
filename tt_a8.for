c
	EXTERNAL UFO_OPEN
	INCLUDE '($STRDEF)'
	INCLUDE '($OTSDEF)'
	CHARACTER A80*80,E80*80,RES(8)*5,DEL/','/,DIN(2)*2,DINY*4,
	1EADAY*11
	INTEGER EN,IV(8),BLNK/1/,FLAG/1/,BDATE(2),DAT(2),DAT2(2),
	1	INTC,INTF,EBDAY(2)
c
	CHARACTER MON(12)*3/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
	1'AUG','SEP','OCT','NOV','DEC'/,ADATE*23,FDATE*23,MMCKEY*7
c
	DOUBLE PRECISION CT(16,12),FT(7,3),SL(16,12),XL(7,3),
	1	AVG(16),NN,TSUM,NNC,CTA(3),FTA(3),NK(3),
	1	AVM(4),NM(4),AVY(4),NY(4),FLSIL,TLS(3),TSUX,DAV,
	1	AOIC(150),BOIC(150)
C
	INTEGER ROLLED/0/,DS(2),MM,SS(4),BONE(2)
C
	CHARACTER A1,FOREMAN(3)*10,AONE/'1'/,ASD*23
C
C
c
        structure /bozo2/
c
                character*4     key             ! yymm  (primary key)
                real            msil(3,31)      ! mine ind. sio2 by shift,day
                real            cr12(3,31)      ! crusher +1/2 by shift,day
                real            flfr(3,31)      ! flf davis tube recovery
                real            flfs(3,31)      ! flf davis tube sio2
                real            fls(3,31)       ! flf sio2
                real            flts(3,31)      ! flot tails sio2
                real            aeff(4,3,31)    ! amine efficiency by cell
c
c
        end structure
c
        record /bozo2/ hsd
C
C
C
c
C         this structure is in MMC.DAT, the key is a DESCENDING string
        STRUCTURE /XX/
                CHARACTER DATE*7/'       '/     !YYMMDDS    key 0
                REAL      FFDATA(4)     ! AL2O3,CAO,MGO,MNO
                REAL      FCDATA(7)     !SIL,AL,CAL,MG,MN,-270
                REAL      BENT(5)       !BENT. LBS/TON
                REAL      CTONS         !CRUSHER SCANNER TONS
                REAL      CSIL          !MINE IND. SILICA
                REAL      CMF           !MINE IND. MAG FE
                REAL      CWTR          !MINE PREDIC. RECOVERY
                REAL      CA            !MINE "A" FACTOR
                REAL      CT            !AVG. COARSE TAILS
                REAL      FT            !AVG. FINE TAILS
                REAL      FR            !FROTH REGRIND % OP TIME
                REAL      AM            !AMINE LBS/TON
        END STRUCTURE
C
        RECORD /XX/ MMC
c
C
	CHARACTER NAMES(4)*10,SDAT*23,WDAT*23,AINTV*7/'+:01'/
	INTEGER BSDAT(2),BWDAT(2),BINTV(2)
C
C
C	   FIXED FINE TAILS
C
	REAL L3FT/1.56/,L4FT/1.27/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
C	OPEN A FILE TO WRITE ERROR MESSAGES TO, TO FIND OUT WHY THE FINE
C	TAILS DOES NOT WORK
C
	OPEN (77,FILE='USER_D:[METREP.LAB45]TT.ERR',STATUS='UNKNOWN',
	1		ACCESS='APPEND',err=999)
	NNTT=0
	CALL SYS$BINTIM (AONE,BONE)
C
cc	CALL SYS$BINTIM (AINTV,BINTV)
C
cc	CALL SYS$SCHDWK (,,,BINTV)
C
C
C
C7777	OPEN (10,FILE='USER7:[METREP.LAB45.TAILS]CT.LIS',
C	1STATUS='OLD',FORM='FORMATTED',RECORDTYPE='STREAM',SHARED,
C	1READONLY,ERR=99)
7777	OPEN (10,FILE='USER7:[METREP.LAB45.TAILS]CT.LIS',
	1STATUS='OLD',SHARED,
	1READONLY,ERR=99)
C
C
C
	endmark=0
C
1	READ (10,20,ERR=66,IOSTAT=MM) A80
	GO TO 111
66	IF (MM .NE. 52) GO TO 99
	CALL LIB$WAIT (1.0)
	GO TO 1
20	FORMAT (A)
C
111	IF (A80(1:1) .eq. 'e' .OR. A80(1:1) .eq. 'E') THEN
		E80=' '
		JJ=1
		DO 688 KK=5,80
			E80(JJ:JJ)=A80(KK:KK)
			JJ=JJ+1
688		CONTINUE
C
		ENDMARK=1  !it's end of day
C
		CALL STR$ELEMENT(DIN(1),0,DEL,E80)
		CALL STR$ELEMENT(DIN(2),1,DEL,E80)
		CALL STR$ELEMENT(DINY,2,DEL,E80)
C
		IF (DIN(1)(2:2) .EQ. ' ')DIN(1)=' '//DIN(1)(1:1)
		IF (DIN(2)(2:2) .EQ. ' ')DIN(2)=' '//DIN(2)(1:1)
C
		READ (DIN(1),'(I2)') LL
C
		IF (DINY(3:4) .LT. '66') THEN
			ADATE=DIN(2)//'-'//MON(LL)//'-20'//DINY(3:4)
		ELSE
			ADATE=DIN(2)//'-'//MON(LL)//'-19'//DINY(3:4)
		END IF
C
		IF (ADATE(1:1) .EQ. ' ') ADATE(1:1)='0'
		CALL SYS$BINTIM (ADATE,BDATE) 
C
		GO TO 222
	END IF
c
	IF (A80(1:1) .LT. '1' .OR. A80(1:1) .GT. '9') GO TO 1
ccccccc111	IF (A80(1:1) .LT. '1' .OR. A80(1:1) .GT. '9') GO TO 1
C
	DO 70 I=0,5
C
		CALL STR$ELEMENT(RES(I+1),I,DEL,A80)
CCCC		IF (.NOT. J) CALL LIB$SIGNAL (%VAL(J))
C
70	CONTINUE
C
C
	DO 71 I=1,6
		IF ( I .NE. 2) THEN
			CALL OTS$CVT_TI_L (RES(I),IV(I),,%VAL(BLNK))
CCCCC			IF (.NOT. J) GO TO 71 
		ELSE
			CALL OTS$CVT_T_F (RES(I),CTL,%VAL(1),,)
CCCCC			IF (.NOT. J) GO TO 71 
		END IF			
71	CONTINUE
C
30	IF (IV(1) .LT. 3 .OR. IV(1) .GT. 18) GO TO 1
	IF (IV(3) .LT. 0 .OR. IV(3) .GT. 22) GO TO 1
C
	LINE=IV(1)
	INTV=IV(3)
C
	IF (RES(5)(2:2) .EQ. ' ') RES(5)(1:2)='0'//RES(5)(1:1)
	ADATE=RES(5)(1:2)//'-'//MON(IV(4))//'-'//RES(6)(1:4)
C
	CALL SYS$BINTIM (ADATE,BDATE)
C
C
C
c
C
CCCCCCCCCCCCCCCCCCCCCCCCC
C
222	OPEN (UNIT=20,FILE='USER_D:[METREP.LINEAT]TAILS2.LIS',
	1	STATUS='UNKNOWN',RECL=512,FORM='UNFORMATTED',ACCESS='DIRECT'
	2	,SHARED,USEROPEN=UFO_OPEN)
C
	DO 44 KDAY=1,2
75	READ (20,REC=KDAY,err=377) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 78
377	CALL LIB$WAIT (1.0)
	GO TO 75
C
78	CALL SYS$ASCTIM (,FDATE,DAT,)
	IF (FDATE(1:1) .EQ. ' ') FDATE(1:1)='0'
C
C
C
C	  see if the day has just changed due to an "end" in the data 
C	  stream 
	IF (KDAY .EQ. 1 .AND. ENDMARK .EQ. 1) THEN
C
		CALL LIB$DAY (ISD,BDATE,)
		CALL LIB$DAY (IRD,DAT)
C
		LLL=ISD-IRD
C
c		  if the end date is the same or .gt. the date of the
c		  latest file go roll the data into the met files 
		IF (LLL .GE. 0) THEN
			CALL LIB$ADD_TIMES (BDATE,BONE,BDATE)
			GO TO 77
		END IF
C
		ENDMARK=0
		GO TO 1
C
	END IF
C
C
C	   go thru this if kday .eq. 2
	IF (ENDMARK .EQ. 1) THEN
		ENDMARK=0
		GO TO 1
	END IF
C
	IF (FDATE(1:11) .EQ. ADATE(1:11)) GO TO 100
C
C
44	CONTINUE
CCCCCCCCCCCCCCCCCCCCC
C
	GO TO 1
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       THE CORRECT CT FILE HAS BEEN FOUND, DEPOSIT VALUE
C
100	L=LINE-2
	IF (L .LT. 1 .OR. L .GT. 16) GO TO 1
C
	I=INTV/2+1
C
	IF (I .LT. 1 .OR. I .GT. 12) GO TO 1
C
C
C
	CT(L,I)=CTL
C
	IF (I .GT. INTC) INTC=I
C
446	WRITE (20,REC=KDAY,err=22) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 23
22	CALL LIB$WAIT (1.0)
	GO TO 446
C
ccccc23	CLOSE (20)
23	Continue
C
	GO TO 1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	look for interval 0, if found, copy record 1
C	over to record 2, zero record 1, write data to record 1.
C	Then, send daily data to met reports and all other locations.
C
C
c
c
c
77	IF (KDAY .EQ. 1) THEN
C
	CLOSE (20)
C
	CALL LIB$WAIT(5.)
C
	OPEN (UNIT=20,FILE='USER_D:[METREP.LINEAT]TAILS2.LIS',
	1	STATUS='UNKNOWN',RECL=512,FORM='UNFORMATTED',ACCESS='DIRECT'
	2	,SHARED,USEROPEN=UFO_OPEN)
C
C
C            IT IS END OF DAY, ALL 12 VALUES ARE IN FOR COARSE TAILS
C
C
499	READ (20,REC=1,err=500) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 510
500	CALL LIB$WAIT (1.0)
	GO TO 499
510	READ (20,REC=2,err=501) DS,MM,SL,DAT2,INTF,FT
	GO TO 520
501	CALL LIB$WAIT (1.0)
	GO TO 510
520	INTC=12
	WRITE(20,REC=2,err=502) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 530
502	CALL LIB$WAIT(1.0)
	GO TO 520
C
C
530	READ (20,REC=1,err=503) DAT,MM,SL,DAT2,INTF,FT
	GO TO 540
503	CALL LIB$WAIT(1.0)
	GO TO 530
C
C	  zero out the coarse tails numbers for new day
540	do 342 i=1,16
	do 342 j=1,12
342		ct(i,j)=0
c
	INTC=0
541	WRITE(20,REC=1,ERR=504) BDATE,INTC,CT,DAT2,INTF,FT
	GO TO 550
504	CALL LIB$WAIT(1.0)
	GO TO 541
C
C
C
c	   bring in the full coarse tails for yesterday	
C
550	READ (20,REC=2,ERR=505) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 560
505	CALL LIB$WAIT(1.0)
	GO TO 550
C
C
560	CLOSE (20)
C
C
C
C
551	OPEN (UNIT=77,FILE='USER_D:[METREP]FNAMEFILE.DAT',STATUS='OLD',
	1	ACCESS='DIRECT',RECL=512,FORM='FORMATTED',ERR=552)
	GO TO 553
C
552	CALL LIB$WAIT (1.)
	GO TO 551
C
C
553	READ (77,207,REC=1,ERR=554)NAMES
207	FORMAT (4A10)
	GO TO 556
C
554	CALL LIB$WAIT (1.)
	GO TO 553
C
556	CLOSE (77)
C
C
C
C
C
C       get storage file for foremans' tails
C
561	OPEN (UNIT=33,FILE='user_d:[metrep]SAVED_TAILS.DAT',
	1STATUS='OLD',ACCESS='DIRECT',ERR=566,shared)
	GO TO 562
C
566	CALL LIB$WAIT (1.)
	GO TO 561
C
C
562	READ (33,REC=1,ERR=570)NUM      !kick up record counter by 1
	GO TO 572
C
570	CALL LIB$WAIT (1.)
	GO TO 562
C
572	NUM=NUM+1
        WRITE (33,REC=1,ERR=580)NUM
	GO TO 582
C
580	CALL LIB$WAIT (1.)
	GO TO 572
C
582	CALL PATTERN (DAT,SS) ! get shift no. for the crews
C
C
C
        DO 262 I=1,4
C
                IF (SS(I) .EQ. 0) GO TO 262
C
                FOREMAN(SS(I))=NAMES(I)
C
262     CONTINUE
C
C
        DO 877 I=1,3
                NK(I)=0
877             CTA(I)=0
C
C
        N=0
C
        DO 485 M=1,12,4
C
                N=N+1
C
        DO 485 L=M,M+3
        DO 485 K=1,16
C
                IF (CT(K,L) .LE. 0) GOTO 485
C
                CTA(N)=CTA(N)+CT(K,L)
                NK(N)=NK(N)+1.
C
485     CONTINUE
C
C
        DO 2486 M=1,3
                IF (NK(M) .LE. 0) GO TO 2486
                CTA(M)=CTA(M)/NK(M)
2486    CONTINUE
C
C
591	WRITE (33,REC=NUM,ERR=590)'C',DAT,FOREMAN,CTA
	GO TO 677
C
590	CALL LIB$WAIT (1.)
	GO TO 591
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C               section to calc. month and year avg coarse tails
C
C
677     READ (33,REC=1,ERR=620)NUM
	GO TO 621
C
620	CALL LIB$WAIT (1.)
	GO TO 677
C
C
621        NNN=0
C
        DO 300 I=NUM,2,-1
C
631		READ (33,REC=I,ERR=630)A1,DAT,FOREMAN,CTA
		GO TO 632
C
630		CALL LIB$WAIT (1.)
		GO TO 631
C
632		IF (A1 .NE. 'C') GO TO 300
C
		IF (NNN .NE. 0) GO TO 62
C
                        CALL SYS$ASCTIM(,SDAT,DAT,)!get beginning date in ascii
C
                        NNN=1
C
C
62              CALL SYS$ASCTIM(,WDAT,DAT,)
C
C
C
C                       loop thru all 3 shifts and get month & year avgs.
C
                DO 65 ISH=1,3
c
                        DO 55 M=1,4
C
	IF (FOREMAN(ISH) .EQ. NAMES(M)) GO TO 9122
C
55                      CONTINUE
C
                        GO TO 65        ! NO MATCH ON NAME
C
9122		IF (WDAT(4:11) .EQ. SDAT(4:11)) THEN
C                                                               MONTH AVG.
                        AVM(M)=AVM(M)+CTA(ISH)
                        NM(M)=NM(M)+1.
C
                END IF
C
C
                IF (WDAT(8:11) .EQ. SDAT(8:11)) THEN
C                                                               YEAR AVG.
                        AVY(M)=AVY(M)+CTA(ISH)
                        NY(M)=NY(M)+1.
C
                END IF
C
C
C
65              CONTINUE
C
C
300     CONTINUE
C
C
C               get month and year ct avg by foreman.
C
        DO 58 I=1,4
C
                IF (NM(I) .GT. 0) AVM(I)=AVM(I)/NM(I)
                IF (NY(I) .GT. 0) AVY(I)=AVY(I)/NY(I)
C
58      CONTINUE
C
        CLOSE (33)
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
        IF (FDATE(1:1) .EQ. ' ') FDATE(1:1)='0'
C
C               send avg. coarse tails to met files
C
C
291	OPEN (UNIT=42,FILE='MET_P:AMIOIC'//FDATE(1:2)//'.DAT',
	1       FORM='UNFORMATTED',STATUS='UNKNOWN',err=2928)
	go to 2936
C
2928	call lib$wait (1.)
	go to 291
C
2936	OPEN (UNIT=43,FILE='MET_P:BMIOIC'//FDATE(1:2)//'.DAT',
	1       FORM='UNFORMATTED',STATUS='UNKNOWN',err=294)
	go to 295
C
294	call lib$wait (1.)
	go to 2936
C
295	READ (42,ERR=178)AOIC
C
178	READ (43,ERR=979)BOIC
C
C
C
C
979     DO 293 J=1,16
        AVG(J)=0
        NN=0
        DO 292 K=1,12
C
                IF (CT(J,K) .LE. 0)  GOTO 292
C
                NN=NN+1
C
                AVG(J)=AVG(J)+CT(J,K)
C
292     CONTINUE
C
        IF (NN .EQ. 0) THEN
C
                AVG(J)=0
C
        ELSE
C
                AVG(J)=AVG(J)/NN
C
        END IF
C
C
C
293     CONTINUE
C
C
C
        L=3
        DO 89 M=1,10
        AOIC(L)=AVG(M)
C
89      L=L+1
C
C
        L=11

        DO 90 M=1,6
C
        BOIC(M)=AVG(L)
C
90      L=L+1
C
C
        REWIND (42)
        REWIND (43)
C
801	WRITE (42,ERR=802)AOIC
	GO TO 803
802	CALL LIB$WAIT (1.)
	GO TO 801
C
803	WRITE (43,ERR=804)BOIC
	GO TO 805
804	CALL LIB$WAIT (1.)
	GO TO 803
C
805	CLOSE (42)
	CLOSE (43)
C
C
CCCCCCCC
C
C       GET SHIFT AVERAGE FOR COARSE TAILS AND SEND IT TO MMC.DAT
C
C
C
C
	OPEN (UNIT=50,FILE='MET_P:MMC.DAT',ACCESS='KEYED',STATUS='OLD',
	1               SHARED)
C
        IF (FDATE(1:1) .EQ. ' ') FDATE(1:1)='0'
C
        DO 4253 M=1,12
                IF (FDATE(4:6) .EQ. MON(M)) GO TO 4254
4253    CONTINUE
C
C
4254    WRITE (MMCKEY(3:4),'(I2)') M
        IF (MMCKEY(3:3) .EQ. ' ') MMCKEY(3:3)='0'
C
        MMCKEY(1:6)=FDATE(10:11)//MMCKEY(3:4)//FDATE(1:2)
C
C
        DO 54 JS=1,12,4
                NC=0
                DAV=0
                DO 53 JS2=JS,JS+3
                DO 53 JL=1,16
C
                        IF (CT(JL,JS2) .LE. 0) GO TO 53
                        DAV=DAV+CT(JL,JS2)
                        NC=NC+1
C
53              CONTINUE
C
                IF (NC .GT. 0) THEN
C
                        RR7=DAV/NC
C
                        IF (JS .EQ. 1) THEN
                                MMCKEY(7:7)='1'
                        ELSE IF (JS .EQ. 5) THEN
                                MMCKEY(7:7)='2'
                        ELSE IF (JS .EQ. 9) THEN
                                MMCKEY(7:7)='3'
                        END IF
C
                        READ (50,KEY=MMCKEY,IOSTAT=JJJ) MMC
C
                        IF (JJJ .EQ. 52) THEN
                                GO TO 54
                        END IF
C
C
                        MMC.CT=RR7
C
                        IF (JJJ .EQ. 0) THEN
                                REWRITE (50) MMC
                        ELSE IF (JJJ .EQ. 36) THEN
                                MMC.DATE=MMCKEY
                                WRITE (50) MMC
                        END IF
C
C
                END IF
C
54      CONTINUE
C
C
        CLOSE (50)
C
C
C
        END IF
C
C
C
C
C
333	IF (ENDMARK .EQ. 1) ENDMARK=0
	GO TO 1
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C
C
C
C
C
C
C
99	CLOSE (10)
	CLOSE (20)
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C	FINE TAILS SECTION
C
C
C
c
	NNTT=0
	CALL SYS$BINTIM (AONE,BONE)
C
C
C
C413    	OPEN (80,FILE='USER7:[METREP.LAB45.TAILS]FT.LIS',
C	1STATUS='OLD',FORM='FORMATTED',RECORDTYPE='STREAM',SHARED,
C	1READONLY,IOSTAT=MJ,ERR=9992)
413    	OPEN (80,FILE='USER7:[METREP.LAB45.TAILS]FT.LIS',
	1STATUS='OLD',SHARED,
	1READONLY,IOSTAT=MJ,ERR=9992)
	GO TO 9993
C
9992	CALL LIB$WAIT (30.)
	call sys$asctim (,asd,,)
	WRITE (77,771) MJ,asd
771	FORMAT (1H ,'CANT OPEN FT.LIS',I6,2x,a23)
	GO TO 413
C
9993	endmark=0
C
1111	READ (80,20,ERR=766,IOSTAT=MM) A80
	GO TO 1112
766	IF (MJ .EQ. 0) GO TO 999
	CALL LIB$WAIT (15.0)
	WRITE (77,772) MM
772	FORMAT (1H ,'CANT READ FT.LIS',I6)
	GO TO 1111
C
1112	IF (A80(1:1) .eq. 'e' .OR. A80(1:1) .eq. 'E') THEN
		E80=' '
		JJ=1
		DO 6887 KK=5,80
			E80(JJ:JJ)=A80(KK:KK)
			JJ=JJ+1
6887		CONTINUE
C
		ENDMARK=1  !it's end of day
C
		CALL STR$ELEMENT(DIN(1),0,DEL,E80)
		CALL STR$ELEMENT(DIN(2),1,DEL,E80)
		CALL STR$ELEMENT(DINY,2,DEL,E80)
C
		IF (DIN(1)(2:2) .EQ. ' ')DIN(1)=' '//DIN(1)(1:1)
		IF (DIN(2)(2:2) .EQ. ' ')DIN(2)=' '//DIN(2)(1:1)
C
		READ (DIN(1),'(I2)') LL
C
		if (diny(3:4) .lt. '66') then
		ADATE=DIN(2)//'-'//MON(LL)//'-20'//DINY(3:4)
		else
		ADATE=DIN(2)//'-'//MON(LL)//'-19'//DINY(3:4)
		end if
		IF (ADATE(1:1) .EQ. ' ') ADATE(1:1)='0'
		CALL SYS$BINTIM (ADATE,BDATE) 
C
		GO TO 2223
	END IF
c
	IF (A80(1:1) .LT. '1' .OR. A80(1:1) .GT. '9') GO TO 1111
C
	DO 7094 I=0,5
C
		CALL STR$ELEMENT(RES(I+1),I,DEL,A80)
C
7094	CONTINUE
C
C
	DO 7194 I=1,6
		IF ( I .NE. 2) THEN
			CALL OTS$CVT_TI_L (RES(I),IV(I),,%VAL(BLNK))
		ELSE
			CALL OTS$CVT_T_F (RES(I),CTL,%VAL(1),,)
 		END IF			
7194	CONTINUE
C
	IF (IV(1) .LT. 1 .OR. IV(1) .GT. 17) GO TO 1111
	IF (IV(3) .LT. 0 .OR. IV(3) .GT. 3) GO TO 1111
C
	LINE=IV(1)
	INTV=IV(3)
C
	IF (RES(5)(2:2) .EQ. ' ') RES(5)(1:2)='0'//RES(5)(1:1)
	ADATE=RES(5)(1:2)//'-'//MON(IV(4))//'-'//RES(6)(1:4)
C
	CALL SYS$BINTIM (ADATE,BDATE)
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
2223	OPEN (UNIT=20,FILE='USER_D:[METREP.LINEAT]TAILS2.LIS',
	1	STATUS='UNKNOWN',RECL=512,FORM='UNFORMATTED',ACCESS='DIRECT'
	2	,SHARED,USEROPEN=UFO_OPEN)
C
	DO 4455 KDAY=1,2
7555	READ (20,REC=KDAY,IOSTAT=MJ,err=3775) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 785
3775	CALL LIB$WAIT (1.0)
	WRITE (77,773) MJ
773	FORMAT (1H ,'CANT READ TAILS2.LIS',I6)
	GO TO 7555
C
785	CALL SYS$ASCTIM (,FDATE,DAT2,)
	IF (FDATE(1:1) .EQ. ' ') FDATE(1:1)='0'
C
C
C
C	  see if the day has just changed due to an "end" in the data 
C	  stream 
	IF (KDAY .EQ. 1 .AND. ENDMARK .EQ. 1) THEN
C
		CALL LIB$DAY (ISD,BDATE,)
		CALL LIB$DAY (IRD,DAT2)
C
		LLL=ISD-IRD
C
c		  if the end date is the same or .gt. the date of the
c		  latest file go roll the data into the met files 
		IF (LLL .GE. 0) THEN
			CALL LIB$ADD_TIMES (BDATE,BONE,BDATE)
			GO TO 7711
		END IF
C
		ENDMARK=0
		GO TO 1111
C
	END IF
C
C
C	   go thru this if kday .eq. 2
	IF (ENDMARK .EQ. 1) THEN
		ENDMARK=0
		GO TO 1111
	END IF
C
	IF (FDATE(1:11) .EQ. ADATE(1:11)) GO TO 1009
C
C
4455	CONTINUE
CCCCCCCCCCCCCCCCCCCCC
C
	GO TO 1111
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       THE CORRECT FT FILE HAS BEEN FOUND, DEPOSIT VALUE
C
1009	L=(LINE/2)-1	!CONVERT SECTION NOS. 5 THRU 17 TO 1 THRU 7
	IF (L .LT. 1 .OR. L .GT. 7) GO TO 1111
C
	I=INTV
C
	IF (I .LT. 1 .OR. I .GT. 3) GO TO 1111
C
C
C
	FT(L,I)=CTL
C
	IF (I .GT. INTF) INTF=I
C
4465	WRITE (20,REC=KDAY,IOSTAT=MJ,err=2245) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 2345
2245	CALL LIB$WAIT (15.0)
	WRITE (77,774) MJ
774	FORMAT (1H ,'4465 CANT WRITE TAILS2.LIS',I6)
	GO TO 4465
C
CCCCCCC23	CLOSE (20)
2345	CONTINUE
C
	GO TO 1111
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	look for interval 0, if found, copy record 1
C	over to record 2, zero record 1, write data to record 1.
C	Then, send daily data to met reports and all other locations.
C
C
c
c
c
7711	IF (KDAY .EQ. 1) THEN
C
	CLOSE (20)
C
	CALL LIB$WAIT(5.)
C
	OPEN (UNIT=20,FILE='USER_D:[METREP.LINEAT]TAILS2.LIS',
	1	STATUS='UNKNOWN',RECL=512,FORM='UNFORMATTED',ACCESS='DIRECT'
	2	,SHARED,USEROPEN=UFO_OPEN)
C
C
C            IT IS END OF DAY, ALL 3 VALUES ARE IN FOR FINE TAILS
C
C
4999	READ (20,REC=1,IOSTAT=MJ,err=5009) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 5109
5009	CALL LIB$WAIT (15.0)
	WRITE (77,775) MJ
775	FORMAT (1H ,'4999 CANT WRITE TAILS2.LIS',I6)
	GO TO 4999
5109	READ (20,REC=2,IOSTAT=MJ,err=5019) DAT,INTC,CT,DS,MM,XL
	GO TO 5209
5019	CALL LIB$WAIT (15.0)
	WRITE (77,776) MJ
776	FORMAT (1H ,'5109 CANT READ TAILS2.LIS',I6)
	GO TO 5109
5209	INTF=3
	WRITE(20,REC=2,IOSTAT=MJ,err=5029) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 5309
5029	CALL LIB$WAIT(15.0)
	WRITE (77,777) MJ
777	FORMAT (1H ,'5209 CANT WRITE TAILS2.LIS',I6)
	GO TO 5209
C
C
5309	READ (20,REC=1,IOSTAT=MJ,err=5039) DAT,INTC,CT,DAT2,MM,XL
	GO TO 5409
5039	CALL LIB$WAIT(15.0)
	WRITE (77,778) MJ
778	FORMAT (1H ,'5309 CANT READ TAILS2.LIS',I6)
	GO TO 5309
C
C	  zero out the FINE tails numbers for new day
5409	do 3429 i=1,7
	do 3429 j=1,3
3429		FT(i,j)=0
c
	INTF=0
5419        WRITE(20,REC=1,IOSTAT=MJ,ERR=5049) DAT,INTC,CT,BDATE,INTF,FT
	GO TO 5509
5049	CALL LIB$WAIT(15.0)
	WRITE (77,779) MJ
779	FORMAT (1H ,'5419 CANT WRITE TAILS2.LIS',I6)
	GO TO 5419
C
C
C
c	   bring in the full FINE tails for yesterday	
C
5509	READ (20,REC=2,ERR=5059) DAT,INTC,CT,DAT2,INTF,FT
	GO TO 5609
5059	CALL LIB$WAIT(15.0)
	WRITE (77,780) MJ
780	FORMAT (1H ,'5509 CANT READ TAILS2.LIS',I6)
	GO TO 5509
C
C
5609	CLOSE (20)
C
C
C
C
5519	OPEN (UNIT=77,FILE='USER_D:[METREP]FNAMEFILE.DAT',STATUS='OLD',
	1	ACCESS='DIRECT',RECL=512,FORM='FORMATTED',IOSTAT=MJ,ERR=5529)
	GO TO 5539
C
5529	CALL LIB$WAIT (15.)
	WRITE (77,781) MJ
781	FORMAT (1H ,'5519 CANT OPEN FNAMEFILE.DAT',I6)
	GO TO 5519
C
C
5539	READ (77,2079,REC=1,IOSTAT=MJ,ERR=5549)NAMES
2079	FORMAT (4A10)
	GO TO 5569
C
5549	CALL LIB$WAIT (15.)
	WRITE (77,782) MJ
782	FORMAT (1H ,'5549 CANT READ FNAMEFILE.DAT',I6)
	GO TO 5539
C
5569	CLOSE (77)
C
C
C
C
C
C       get storage file for foremans' tails
C
5619	OPEN (UNIT=33,FILE='user_d:[metrep]SAVED_TAILS.DAT',
	1STATUS='OLD',ACCESS='DIRECT',IOSTAT=MJ,ERR=5669,shared)
	GO TO 5629
C
5669	CALL LIB$WAIT (15.)
	WRITE (77,783) MJ
783	FORMAT (1H ,'5619 CANT OPEN SAVED_TAILS.DAT',I6)
	GO TO 5619
C
C
5629	READ (33,REC=1,IOSTAT=MJ,ERR=5709)NUM      !kick up record counter by 1
	GO TO 5729
C
5709	CALL LIB$WAIT (15.)
	WRITE (77,784) MJ
784	FORMAT (1H ,'5629 CANT READ SAVED_TAILS.DAT',I6)
	GO TO 5629
C
5729	NUM=NUM+1
        WRITE (33,REC=1,IOSTAT=MJ,ERR=5809)NUM
	GO TO 5829
C
5809	CALL LIB$WAIT (15.)
	WRITE (77,1785) MJ
1785	FORMAT (1H ,'5729 CANT WRITE SAVED_TAILS.DAT',I6)
	GO TO 5729
C
5829	CALL PATTERN (DAT2,SS) ! get shift no. for the crews
C
C
C
        DO 2629 I=1,4
C
                IF (SS(I) .EQ. 0) GO TO 2629
C
                FOREMAN(SS(I))=NAMES(I)
C
2629     CONTINUE
C
C
        DO 8779 I=1,3
                NK(I)=0
8779             CTA(I)=0
C
C
C
        DO 4859 N=1,3
        DO 4859 K=1,7
C
                IF (FT(K,N) .LE. 0) GOTO 4859
C
                FTA(N)=FTA(N)+FT(K,N)
                NK(N)=NK(N)+1.
C
4859     CONTINUE
C
C
        DO 5486 M=1,3
		IF (NK(M) .LE. 0) GO TO 5486
		fta(m)=fta(m)+L3FT+L4FT
		nk(m)=nk(m)+2
		FTA(M)=FTA(M)/NK(M)
5486    CONTINUE
C
C
5919	WRITE (33,REC=NUM,IOSTAT=MJ,ERR=5909)'F',DAT2,FOREMAN,FTA
	GO TO 6779
C
5909	CALL LIB$WAIT (15.)
	WRITE (77,2785) MJ
2785	FORMAT (1H ,'5919 CANT WRITE SAVED_TAILS.DAT',I6)
	GO TO 5919
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C               section to calc. month and year avg FINE tails
C
C
6779     READ (33,REC=1,IOSTAT=MJ,ERR=6209)NUM
	GO TO 6219
C
6209	CALL LIB$WAIT (15.)
	WRITE (77,3785) MJ
3785	FORMAT (1H ,'6779 CANT READ SAVED_TAILS.DAT',I6)
	GO TO 6779
C
C
6219	NNN=0
C
        DO 3009 I=NUM,2,-1
C
6319		READ (33,REC=I,ERR=6309)A1,DAT2,FOREMAN,FTA
		GO TO 6329
C
6309		CALL LIB$WAIT (15.)
	WRITE (77,786) MJ
786	FORMAT (1H ,'6319 CANT READ SAVED_TAILS.DAT',I6)
		GO TO 6319
C
6329		IF (A1 .NE. 'F') GO TO 3009
C
                IF (NNN .NE. 0) GO TO 6298
C
                        CALL SYS$ASCTIM(,SDAT,DAT2,)!get beginning date in ascii
C
                        NNN=1
C
C
6298		CALL SYS$ASCTIM(,WDAT,DAT2,)
C
C
C
C                       loop thru all 3 shifts and get month & year avgs.
C
                DO 656 ISH=1,3
c
                        DO 7556 M=1,4
C
	IF (FOREMAN(ISH) .EQ. NAMES(M)) GO TO 9422
C
7556                      CONTINUE
C
                        GO TO 656        ! NO MATCH ON NAME
C
9422		IF (WDAT(4:11) .EQ. SDAT(4:11)) THEN
C                                                               MONTH AVG.
                        AVM(M)=AVM(M)+FTA(ISH)
                        NM(M)=NM(M)+1.
C
                END IF
C
C
                IF (WDAT(8:11) .EQ. SDAT(8:11)) THEN
C                                                               YEAR AVG.
                        AVY(M)=AVY(M)+FTA(ISH)
                        NY(M)=NY(M)+1.
C
                END IF
C
C
C
656              CONTINUE
C
C
3009     CONTINUE
C
C
C               get month and year ft avg by foreman.
C
        DO 5876 I=1,4
C
                IF (NM(I) .GT. 0) AVM(I)=AVM(I)/NM(I)
                IF (NY(I) .GT. 0) AVY(I)=AVY(I)/NY(I)
C
5876      CONTINUE
C
        CLOSE (33)
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
        IF (FDATE(1:1) .EQ. ' ') FDATE(1:1)='0'
C
C               send avg. coarse tails to met files
C
C
2917	OPEN (UNIT=42,FILE='MET_P:AMIOIC'//FDATE(1:2)//'.DAT',
	1       FORM='UNFORMATTED',STATUS='UNKNOWN',IOSTAT=MJ,err=7928)
	go to 7936
C
7928	call lib$wait (15.)
	WRITE (77,787) FDATE(1:2),MJ
787	FORMAT (1H ,'2917 CANT OPEN AMIOIC',A2,I6)
	go to 2917
C
7936	OPEN (UNIT=43,FILE='MET_P:BMIOIC'//FDATE(1:2)//'.DAT',
	1       FORM='UNFORMATTED',STATUS='UNKNOWN',IOSTAT=MJ,err=794)
	go to 795
C
794	call lib$wait (15.)
	WRITE (77,788) FDATE(1:2),MJ
788	FORMAT (1H ,'7936 CANT OPEN BMIOIC',A2,I6)
	go to 7936
C
795	READ (42,ERR=7178)AOIC
C
7178	READ (43,ERR=7979)BOIC
C
C
C
C
7979     DO 2293 J=1,7
        AVG(J)=0
        NN=0
        DO 7292 K=1,3
C
                IF (FT(J,K) .LE. 0)  GOTO 7292
C
                NN=NN+1
C
                AVG(J)=AVG(J)+FT(J,K)
C
7292     CONTINUE
C
        IF (NN .EQ. 0) THEN
C
                AVG(J)=0
C
        ELSE
C
                AVG(J)=AVG(J)/NN
C
        END IF
C
C
C
2293     CONTINUE
C
	AOIC(15)=L3FT	!FIXED FT FOR LINE 3
	AOIC(16)=L4FT	!FIXED FT FOR LINE 4
C
C
        L=17
        DO 789 M=1,4
        AOIC(L)=AVG(M)
        AOIC(L+1)=AVG(M)
C
789      L=L+2
C
C
        L=7

        DO 790 M=5,7
C
        BOIC(L)=AVG(M)
        BOIC(L+1)=AVG(M)
C
790      L=L+2
C
C
        REWIND (42)
        REWIND (43)
C
8017	WRITE (42,IOSTAT=MJ,ERR=8027)AOIC
	GO TO 8037
8027	CALL LIB$WAIT (15.)
	WRITE (77,1789) MJ
1789	FORMAT (1H ,'8017 CANT WRITE AMIOIC',I6)
	GO TO 8017
C
8037	WRITE (43,IOSTAT=MJ,ERR=8047)BOIC
   	GO TO 8057
8047	CALL LIB$WAIT (15.)
	WRITE (77,1790) MJ
1790	FORMAT (1H ,'8037 CANT WRITE BMIOIC',I6)
	GO TO 8037
C
8057	CLOSE (42)
	CLOSE (43)
C
C
CCCCCCCC
C
C       GET SHIFT AVERAGE FOR FINE TAILS AND SEND IT TO MMC.DAT
C
C
C
C
	OPEN (UNIT=50,FILE='MET_P:MMC.DAT',ACCESS='KEYED',STATUS='OLD',
	1               SHARED)
C
        IF (FDATE(1:1) .EQ. ' ') FDATE(1:1)='0'
C
        DO 4753 M=1,12
                IF (FDATE(4:6) .EQ. MON(M)) GO TO 4754
4753    CONTINUE
C
C
4754    WRITE (MMCKEY(3:4),'(I2)') M
        IF (MMCKEY(3:3) .EQ. ' ') MMCKEY(3:3)='0'
C
        MMCKEY(1:6)=FDATE(10:11)//MMCKEY(3:4)//FDATE(1:2)
C
C
	DO 477 JS=1,3
C
C
	IF (JS .EQ. 1) THEN
		MMCKEY(7:7)='1'
		RR7=FTA(1)
	ELSE IF (JS .EQ. 2) THEN
		MMCKEY(7:7)='2'
		RR7=FTA(2)
	ELSE IF (JS .EQ. 3) THEN
		MMCKEY(7:7)='3'
		RR7=FTA(3)
	END IF
C
	READ (50,KEY=MMCKEY,IOSTAT=JJJ) MMC
C
	IF (JJJ .EQ. 52) THEN
		GO TO 477
	END IF
C
C
	MMC.FT=RR7
C
	IF (JJJ .EQ. 0) THEN
		REWRITE (50) MMC
	ELSE IF (JJJ .EQ. 36) THEN
		MMC.DATE=MMCKEY
		WRITE (50) MMC
	END IF
C
C
477	CONTINUE
C
C
        CLOSE (50)
C
C
C
        END IF
C
C
C
C
	IF (ENDMARK .EQ. 1) ENDMARK=0
	GO TO 1112
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C
C
C
C
C
C
C
999	CLOSE (10)
	CLOSE (80)
	CLOSE (20)
	CLOSE (77)
9999	END
C
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE PATTERN (BTIM,SS)
C
	CHARACTER ADATE*11
	INTEGER SS(4),BTIM(2),NOFD,FNOFD(4),CTIM(2)
	INTEGER*2 NUM(7)
	INTEGER LIST(28)
	1	/1,1,1,1,1,1,1,0,0,0,0,3,3,3,3,3,3,3,0,2,2,2,2,2,2,2,0,0/
C
	CHARACTER*11   			                         
	1	FORD(4)/
	1	'02-MAR-1990',  	! #1  date that each of the four 
	2	'16-MAR-1990',      	! #2  crews start their midnight
	3	'23-MAR-1990',      	! #3   shift (shift 1)
	4	'09-MAR-1990'/           ! #4
C
C
	CALL LIB$DAY (NOFD,BTIM,)  !get day no. of passed time
C
C
	DO 44 I=1,4    	! get day no. of 4 starting dates
C
	CALL SYS$BINTIM (FORD(I),CTIM)  !convert sched. date to binary
C
44		CALL LIB$DAY (FNOFD(I),CTIM,)
C
C
	DO 45 I=1,4                	! calc shift no. of the day passed
C                                       ! to 'ADATE' for all 4 crews
C
	SS(I)=NOFD-FNOFD(I)+1  	!calc no. of days from base to now
C
	SS(I)=MOD(SS(I),28)             !get remainder after divide by 28
C
	IF (SS(I) .EQ. 0) SS(I)=28      !if remain.=0 then remain=28
C
	SS(I)=LIST(SS(I))               !get corresp. shift no. from 'list'
C
45	CONTINUE
C
C
	ICNT=0
	DO 46 I=1,4
46	IF (SS(I) .EQ. 0) ICNT=ICNT+1
C
C
	IF (ICNT .NE. 1) THEN
C
	WRITE (6,*) ' PATTERN SUBROUTINE DID NOT CALCULATE 3 SHIFTS'
	WRITE (6,*) ' CALL R.J. MAKI AT 7490'
	STOP
C
	END IF
C
C
C
	RETURN
C
C
	END
