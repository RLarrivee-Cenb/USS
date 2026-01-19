CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CONFIDENTIAL
C       Property of United States Steel Corporation
C       Copyright 2010 United States Steel Corporation
C       All rights reserved.
C******************************************************************************
C               ORACLE_FIX.FOR
C******************************************************************************
C Revisions:
C       20100809 IM322852  SPA6635 changed COMP2(2,3,2,5) to COMP2(3,3,2,5)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        DOUBLE PRECISION CT(18),FT(18),SCREEN(14,3,2,5),COMP(2,3,2,5),
     1  CAB(2,3,5),SIL(3,5),FILT2(5,3),BL2,FILT3(5,3),BL3,TOTAL,
     1  H2O,RMF,FTMF,BOIC(150),HOIC(150),CX(5),MX(5),AX(5),SX(5),       
     1  FLUXG,P34,P12,FCA(2,3),FMG(2,3),FAL(2,3),FSI(2,3),
     1  PA(3,5),PC(3,5),PM(3,5),SCWT(3,2,5),
     1  TWT(5),BF(7,5),AF(7,5),TSW(5),TONS4(3,2,5),GHRS(3,2,5),
C     1  COMP2(2,3,2,5)
     1  COMP2(3,3,2,5)
C                                                                 

      EXTERNAL oraclD
      INTEGER*4 SQLITR
      INTEGER*4 SQHSTV(28)
      INTEGER*4 SQHSTL(28)
      INTEGER*4 SQINDV(28)
      INTEGER*4 SQHARM(28)
      INTEGER*4 SQHARC(28)
        CHARACTER*198 SQ0001
        CHARACTER*370 SQ0002
      INTEGER*2 SQC000(295)
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC001(40)
      CHARACTER*14 SQLFNM
      COMMON / oraclI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000, SQC001
      COMMON / oraclC / 
     +SQLFNM, SQ0001, SQ0002
        INTEGER AMB(3),BMH(3),BD(2),YSTRDAY(2),NEW(3),LAST(3),
     1  IN,INO,DAT(2),INTV(2),IS,IS2,IS3,IH,STATUS,S4,H4
C
        DIMENSION ARD(8)
C
        CHARACTER PL(2)*2/'CN','AG'/,FN2*25,
     1  MO(12)*3/'JAN','FEB','MAR','APR',
     2  'MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
     3  ,CAD*9,FILN*16,SH*2,TEMP*12,MINP,AINTV/'1'/,
     1  INAM(4)*6/'AMIOIC','BMIOIC',' AMBOP',' BMHOP'/,  
     1  PART(3)*16/'SCREEN FRACTIONS','       SIO2     ',
     2  'FILTER CAKE     '/,AYSTR*23,FILNAM*30,ASC2*3,
     1  PP1*29/'SCREEN FRACTIONS,COMPRESSIONS'/,KD*8,A5*5             
C
        CHARACTER SHED*32/'+5/8,+1/2,+3/8,+1/4,4M,+28M,-28M'/,
C
     2  CHED*38/'COMPRESSION AVG., -200 PCT.'/,
C
     2  BHED*18 /'AL203,CAO,MGO'/,
     2  BHEX*18 /'SIO2,AL203,CAO,MGO'/,
C
     3  FES*4/'SIO2'/,
C
     4  FILT*26/'SIO2,-270,-500,AL2O3,CAO,MGO'/,
C
     5  BAT(2)*6/'BEFORE','AFTER '/,
C
     1  MNTH(12)*3/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     1  'AUG','SEP','OCT','NOV','DEC'/,TDAY*11,YDAY*11,KY*6,AD*23
C
        INTEGER D2,EXIST,LS,LH
C
        CHARACTER ASC*10,FULLDATE*23,asd*11
C
        real*4 PCT(14,3,2,5),totw,totw3
C
        character xnp*8
c
C       EXEC SQL INCLUDE SQLCA
C
C     
C      sqlca.for 
C     
C     *****************************************************************
C     *                                                               *
C     *               S  Q  L  C  A  .  F  O  R                       *
C     *                                                               *
C     *****************************************************************
 
C     *****************************************************************
C     *  Note: some compilers do  not  allow CHAR and INT data in the *
C     *  same common block;  therefore the CHAR vars are declared as  *
C     *  LOGICAL*1.  Sigh.                                            *
C     *****************************************************************
 
       LOGICAL*1   SQLAID  (8)
       INTEGER*4   SQLABC
       INTEGER*4   SQLCDE
C    SQLERRM
       INTEGER*2   SQLEML
       LOGICAL*1   SQLEMC (70)
       LOGICAL*1   SQLERP  (8)
       INTEGER*4   SQLERD  (6)
C    SQLWRN(8)
       LOGICAL*1   SQLWN0, SQLWN1, SQLWN2, SQLWN3,
     1             SQLWN4, SQLWN5, SQLWN6, SQLWN7
       LOGICAL*1   SQLEXT  (8)
C
       COMMON /SQLCA/
     1   SQLAID,
     2   SQLABC,
     3   SQLCDE,
     4   SQLEML,
     5   SQLEMC,
     6   SQLERP,
     7   SQLERD,
     8   SQLWN0, SQLWN1, SQLWN2, SQLWN3,
     9   SQLWN4, SQLWN5, SQLWN6, SQLWN7,
     1   SQLEXT
C
C     *****************************************************************
C     *  The following common block is provided for use by Dynamic    *
C     *  Method 4.  Along with the SQLCA, it must be present in any   *
C     *  subroutine which executes a dynamic SQL statement using      *
C     *  descriptors.                                                 *
C     *****************************************************************
C
       INTEGER *4   DSC2N
       INTEGER *4   DSC2V
       INTEGER *4   DSC2L
       INTEGER *4   DSC2T
       INTEGER *4   DSC2I
       INTEGER *4   DSC2F
       INTEGER *4   DSC2S
       INTEGER *4   DSC2M
       INTEGER *4   DSC2C
       INTEGER *4   DSC2X
       INTEGER *4   DSC2Y
       INTEGER *4   DSC2Z
C
       COMMON /DSC2/   DSC2N, DSC2V, DSC2L, DSC2T, DSC2I, DSC2F,
     1                 DSC2S, DSC2M, DSC2C, DSC2X, DSC2Y, DSC2Z
C
C       EXEC SQL BEGIN DECLARE SECTION
                CHARACTER*10    USERNAME
                CHARACTER*20    PASSWORD
                CHARACTER*400   DYNSTM
                CHARACTER*11    DATEX
                INTEGER         SHIFT
                INTEGER         HALF
                INTEGER         LINE
                REAL*4          B58
                REAL*4          B12
                REAL*4          B38
                REAL*4          B14
                REAL*4          B4M
                REAL*4          B28
                REAL*4          BM28
                REAL*4          A58
                REAL*4          A12
                REAL*4          A38
                REAL*4          A14
                REAL*4          A4M
                REAL*4          A28
                REAL*4          AM28
                REAL*4          ACOMP
                REAL*4          ACOMP200
                REAL*4          B916
                REAL*4          A916
                REAL*4          COMP600
                REAL*4          C600M200
                REAL*4          B716
                REAL*4          A716
                REAL*4          TONS
                REAL*4          HOURS
C       EXEC SQL END DECLARE SECTION
C
C       EXEC SQL WHENEVER SQLERROR GOTO 9000
C
        USERNAME='RJM'
        PASSWORD='MINNTAC'
C
C       EXEC SQL CONNECT :USERNAME IDENTIFIED BY :PASSWORD
C
C
C       DYNSTM='INSERT INTO AGGL_TUMBLES VALUES
C     1  VALUES(TO_DATE(:DATEX,'DD-MON-YYYY'),:SHIFT,:HALF,:LINE,
C     1  :B58,:B12,:B38,:B14,:B4M,:B28,:BM28,:A58,:A12,:A38,:A14,:A4M,
C     1  :A28,:AM28,:ACOMP,:ACOMP200,:B916,:A916,
C     1  :COMP600,:C600M200,:B716,:A716)'
C
C
        CALL SQLADR(%REF(USERNAME), SQHSTV(1))
        SQHSTL(1) = 10
        SQINDV(1) = 0
        SQHARM(1) = 0
        CALL SQLADR(%REF(PASSWORD), SQHSTV(2))
        SQHSTL(2) = 20
        SQINDV(2) = 0
        SQHARM(2) = 0
        CALL SQLFIN(14, %REF(SQLFNM))
        SQLITR = 10
        CALL SQLFX7(6, %REF(SQLCTX), 3, SQLITR, 2, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 9000
        write (6,*) ' enter yyyymmdd'
        read (5,221) xnp
221     format (a8)
        OPEN (11,FILE='MET_screen:SCREEN.DAT',
     1  ACCESS='KEYED',STATUS='old',shared)
c
        open (67,file='USER_d:[metrep.lab45]screen_oracle.dat',
     1  status='unknown',access='append',
     1  recl=200)
c
        READ (11,key=xnp) KD,DAT,IS,IH,SCREEN,COMP,
     1                      CAB,SIL,FILT2,
     1                      FILT3,BL2,BL3,FLUXG,FCA,FMG,FAL,FSI,   
     1                      PA,PC,PM,S4,H4,scwt,tons4,ghrs,comp2
c
        go to 20
C
c15     READ (11) KD,DAT,IS,IH,SCREEN,COMP,
c     1                     CAB,SIL,FILT2,
c     1                     FILT3,BL2,BL3,FLUXG,FCA,FMG,FAL,FSI,   
c     1                     PA,PC,PM,S4,H4,scwt,tons4,ghrs,comp2
C
c       IF (KD(1:8) .Lt. '20010101') GO TO 999
C
20      call sys$asctim (,asd,dat,)
C
        DO 100 NL=1,5
        DO 90  NS=1,3
        DO 80  NH=1,2
                TOTW=0
                TOTW3=0
                DO 70 I=1,7
                        TOTW=TOTW+SCREEN(I,NS,NH,NL)
                        TOTW3=TOTW3+SCREEN(I+7,NS,NH,NL)
                        PCT(I,NS,NH,NL)=0
                        PCT(I+7,NS,NH,NL)=0
70              CONTINUE
C
        IF (TOTW .GT. 0 .AND. TOTW3 .GT. 0) THEN
C
                DO 71 I=1,7
                        PCT(I,NS,NH,NL)=SCREEN(I,NS,NH,NL)/TOTW*100.
                        PCT(I+7,NS,NH,NL)=SCREEN(I+7,NS,NH,NL)/TOTW3*100.
71              CONTINUE
C
        END IF
C
C
C
C
C
C
        CALL SYS$ASCTIM (,DATEX,DAT,)
        SHIFT=NS
        HALF=NH
        LINE=NL+2
        B58=0
        B916=PCT(1,NS,NH,NL)
        B12=PCT(2,NS,NH,NL)
        B716=PCT(3,NS,NH,NL)
        B38=PCT(4,NS,NH,NL)
        B14=PCT(5,NS,NH,NL)
        B4M=0
        B28=PCT(6,NS,NH,NL)
        BM28=PCT(7,NS,NH,NL)
        A58=0
        A916=PCT(8,NS,NH,NL)
        A12=PCT(9,NS,NH,NL)
        A716=PCT(10,NS,NH,NL)
        A38=PCT(11,NS,NH,NL)
        A14=PCT(12,NS,NH,NL)
        A4M=0
        A28=PCT(13,NS,NH,NL)
        AM28=PCT(14,NS,NH,NL)
        ACOMP=COMP2(1,NS,NH,NL)
        ACOMP200=COMP2(2,NS,NH,NL)
        COMP600=COMP(1,NS,NH,NL)
        C600M200=COMP(2,NS,NH,NL)
        TONS=TONS4(NS,NH,NL)
        HOURS=GHRS(NS,NH,NL)
c
C
CC      EXEC SQL PREPARE S FROM :DYNSTM
C       EXEC SQL INSERT INTO AGGL_TUMBLES VALUES
C    1  (TO_DATE(:DATEX,'DD-MON-YYYY'),
C    1  :SHIFT,:HALF,:LINE,
C    1  :B58,:B12,:B38,:B14,:B4M,:B28,:BM28,:A58,:A12,:A38,
C    1  :A14,:A4M,:A28,
C    2  :AM28,:ACOMP,:ACOMP200,:B916,:A916,:COMP600,
C    3  :C600M200,:B716,:A716,:TONS,:HOURS)
C
Cccccc  WRITE (6,*) ' INSERT SECTION ',DATEX,SHIFT,HALF,LINE
        CALL SQLADR(%REF(DATEX), SQHSTV(1))
        SQHSTL(1) = 11
        SQINDV(1) = 0
        SQHARM(1) = 0
        CALL SQLADR(%REF(SHIFT), SQHSTV(2))
        SQHSTL(2) = 4
        SQINDV(2) = 0
        SQHARM(2) = 0
        CALL SQLADR(%REF(HALF), SQHSTV(3))
        SQHSTL(3) = 4
        SQINDV(3) = 0
        SQHARM(3) = 0
        CALL SQLADR(%REF(LINE), SQHSTV(4))
        SQHSTL(4) = 4
        SQINDV(4) = 0
        SQHARM(4) = 0
        CALL SQLADR(%REF(B58), SQHSTV(5))
        SQHSTL(5) = 4
        SQINDV(5) = 0
        SQHARM(5) = 0
        CALL SQLADR(%REF(B12), SQHSTV(6))
        SQHSTL(6) = 4
        SQINDV(6) = 0
        SQHARM(6) = 0
        CALL SQLADR(%REF(B38), SQHSTV(7))
        SQHSTL(7) = 4
        SQINDV(7) = 0
        SQHARM(7) = 0
        CALL SQLADR(%REF(B14), SQHSTV(8))
        SQHSTL(8) = 4
        SQINDV(8) = 0
        SQHARM(8) = 0
        CALL SQLADR(%REF(B4M), SQHSTV(9))
        SQHSTL(9) = 4
        SQINDV(9) = 0
        SQHARM(9) = 0
        CALL SQLADR(%REF(B28), SQHSTV(10))
        SQHSTL(10) = 4
        SQINDV(10) = 0
        SQHARM(10) = 0
        CALL SQLADR(%REF(BM28), SQHSTV(11))
        SQHSTL(11) = 4
        SQINDV(11) = 0
        SQHARM(11) = 0
        CALL SQLADR(%REF(A58), SQHSTV(12))
        SQHSTL(12) = 4
        SQINDV(12) = 0
        SQHARM(12) = 0
        CALL SQLADR(%REF(A12), SQHSTV(13))
        SQHSTL(13) = 4
        SQINDV(13) = 0
        SQHARM(13) = 0
        CALL SQLADR(%REF(A38), SQHSTV(14))
        SQHSTL(14) = 4
        SQINDV(14) = 0
        SQHARM(14) = 0
        CALL SQLADR(%REF(A14), SQHSTV(15))
        SQHSTL(15) = 4
        SQINDV(15) = 0
        SQHARM(15) = 0
        CALL SQLADR(%REF(A4M), SQHSTV(16))
        SQHSTL(16) = 4
        SQINDV(16) = 0
        SQHARM(16) = 0
        CALL SQLADR(%REF(A28), SQHSTV(17))
        SQHSTL(17) = 4
        SQINDV(17) = 0
        SQHARM(17) = 0
        CALL SQLADR(%REF(AM28), SQHSTV(18))
        SQHSTL(18) = 4
        SQINDV(18) = 0
        SQHARM(18) = 0
        CALL SQLADR(%REF(ACOMP), SQHSTV(19))
        SQHSTL(19) = 4
        SQINDV(19) = 0
        SQHARM(19) = 0
        CALL SQLADR(%REF(ACOMP200), SQHSTV(20))
        SQHSTL(20) = 4
        SQINDV(20) = 0
        SQHARM(20) = 0
        CALL SQLADR(%REF(B916), SQHSTV(21))
        SQHSTL(21) = 4
        SQINDV(21) = 0
        SQHARM(21) = 0
        CALL SQLADR(%REF(A916), SQHSTV(22))
        SQHSTL(22) = 4
        SQINDV(22) = 0
        SQHARM(22) = 0
        CALL SQLADR(%REF(COMP600), SQHSTV(23))
        SQHSTL(23) = 4
        SQINDV(23) = 0
        SQHARM(23) = 0
        CALL SQLADR(%REF(C600M200), SQHSTV(24))
        SQHSTL(24) = 4
        SQINDV(24) = 0
        SQHARM(24) = 0
        CALL SQLADR(%REF(B716), SQHSTV(25))
        SQHSTL(25) = 4
        SQINDV(25) = 0
        SQHARM(25) = 0
        CALL SQLADR(%REF(A716), SQHSTV(26))
        SQHSTL(26) = 4
        SQINDV(26) = 0
        SQHARM(26) = 0
        CALL SQLADR(%REF(TONS), SQHSTV(27))
        SQHSTL(27) = 4
        SQINDV(27) = 0
        SQHARM(27) = 0
        CALL SQLADR(%REF(HOURS), SQHSTV(28))
        SQHSTL(28) = 4
        SQINDV(28) = 0
        SQHARM(28) = 0
        CALL SQLFIN(14, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 28, SQLITR, 33, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0001),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 9000
80      CONTINUE
90      CONTINUE
100     CONTINUE
C
        GO TO 999
C
9000    if (sqlcde .eq. -1) THEN
c
c
c
cccccccccccccccccc
        DO 1001 NL=1,5
        DO 901  NS=1,3
        DO 801  NH=1,2
                TOTW=0
                TOTW3=0
                DO 701 I=1,7
                        TOTW=TOTW+SCREEN(I,NS,NH,NL)
                        TOTW3=TOTW3+SCREEN(I+7,NS,NH,NL)
                        PCT(I,NS,NH,NL)=0
                        PCT(I+7,NS,NH,NL)=0
701             CONTINUE
C
        IF (TOTW .GT. 0 .AND. TOTW3 .GT. 0) THEN
C
                DO 711 I=1,7
                        PCT(I,NS,NH,NL)=SCREEN(I,NS,NH,NL)/TOTW*100.
                        PCT(I+7,NS,NH,NL)=SCREEN(I+7,NS,NH,NL)/TOTW3*100.
711             CONTINUE
C
        END IF
C
C
C
C
C
C
        CALL SYS$ASCTIM (,DATEX,DAT,)
        IF (DATEX(1:1) .EQ. ' ') DATEX(1:1)='0'
        SHIFT=NS
        HALF=NH
        LINE=NL+2
        B58=0
        B916=PCT(1,NS,NH,NL)
        B12=PCT(2,NS,NH,NL)
        B716=PCT(3,NS,NH,NL)
        B38=PCT(4,NS,NH,NL)
        B14=PCT(5,NS,NH,NL)
        B4M=0
        B28=PCT(6,NS,NH,NL)
        BM28=PCT(7,NS,NH,NL)
        A58=0
        A916=PCT(8,NS,NH,NL)
        A12=PCT(9,NS,NH,NL)
        A716=PCT(10,NS,NH,NL)
        A38=PCT(11,NS,NH,NL)
        A14=PCT(12,NS,NH,NL)
        A4M=0
        A28=PCT(13,NS,NH,NL)
        AM28=PCT(14,NS,NH,NL)
        ACOMP=COMP2(1,NS,NH,NL)
        ACOMP200=COMP2(2,NS,NH,NL)
        COMP600=COMP(1,NS,NH,NL)
        C600M200=COMP(2,NS,NH,NL)
        TONS=TONS4(NS,NH,NL)
        HOURS=GHRS(NS,NH,NL)
c
C
C       EXEC SQL UPDATE AGGL_TUMBLES 
C    1  SET B58=:B58,B12=:B12,B38=:B38,B14=:B14,
C    1  B4M=:B4M,B28=:B28,BM28=:BM28,A58=:A58,
C    1  A12=:A12,A38=:A38,
C    1  A14=:A14,A4M=:A4M,A28=:A28,
C    2  AM28=:AM28,ACOMP=:ACOMP,ACOMP200=:ACOMP200,
C    1  B916=:B916,A916=:A916,COMP600=:COMP600,
C    3  C600M200=:C600M200,B716=:B716,A716=:A716,TONS=:TONS,HOURS=:HOURS
C    1  WHERE 
C    1  TRUNC(DATEX)=TRUNC(TO_DATE(:DATEX,'DD-MON-YYYY')) AND
C    1  SHIFT=:SHIFT AND HALF=:HALF AND LINE=:LINE
C
        CALL SQLADR(%REF(B58), SQHSTV(1))
        SQHSTL(1) = 4
        SQINDV(1) = 0
        SQHARM(1) = 0
        CALL SQLADR(%REF(B12), SQHSTV(2))
        SQHSTL(2) = 4
        SQINDV(2) = 0
        SQHARM(2) = 0
        CALL SQLADR(%REF(B38), SQHSTV(3))
        SQHSTL(3) = 4
        SQINDV(3) = 0
        SQHARM(3) = 0
        CALL SQLADR(%REF(B14), SQHSTV(4))
        SQHSTL(4) = 4
        SQINDV(4) = 0
        SQHARM(4) = 0
        CALL SQLADR(%REF(B4M), SQHSTV(5))
        SQHSTL(5) = 4
        SQINDV(5) = 0
        SQHARM(5) = 0
        CALL SQLADR(%REF(B28), SQHSTV(6))
        SQHSTL(6) = 4
        SQINDV(6) = 0
        SQHARM(6) = 0
        CALL SQLADR(%REF(BM28), SQHSTV(7))
        SQHSTL(7) = 4
        SQINDV(7) = 0
        SQHARM(7) = 0
        CALL SQLADR(%REF(A58), SQHSTV(8))
        SQHSTL(8) = 4
        SQINDV(8) = 0
        SQHARM(8) = 0
        CALL SQLADR(%REF(A12), SQHSTV(9))
        SQHSTL(9) = 4
        SQINDV(9) = 0
        SQHARM(9) = 0
        CALL SQLADR(%REF(A38), SQHSTV(10))
        SQHSTL(10) = 4
        SQINDV(10) = 0
        SQHARM(10) = 0
        CALL SQLADR(%REF(A14), SQHSTV(11))
        SQHSTL(11) = 4
        SQINDV(11) = 0
        SQHARM(11) = 0
        CALL SQLADR(%REF(A4M), SQHSTV(12))
        SQHSTL(12) = 4
        SQINDV(12) = 0
        SQHARM(12) = 0
        CALL SQLADR(%REF(A28), SQHSTV(13))
        SQHSTL(13) = 4
        SQINDV(13) = 0
        SQHARM(13) = 0
        CALL SQLADR(%REF(AM28), SQHSTV(14))
        SQHSTL(14) = 4
        SQINDV(14) = 0
        SQHARM(14) = 0
        CALL SQLADR(%REF(ACOMP), SQHSTV(15))
        SQHSTL(15) = 4
        SQINDV(15) = 0
        SQHARM(15) = 0
        CALL SQLADR(%REF(ACOMP200), SQHSTV(16))
        SQHSTL(16) = 4
        SQINDV(16) = 0
        SQHARM(16) = 0
        CALL SQLADR(%REF(B916), SQHSTV(17))
        SQHSTL(17) = 4
        SQINDV(17) = 0
        SQHARM(17) = 0
        CALL SQLADR(%REF(A916), SQHSTV(18))
        SQHSTL(18) = 4
        SQINDV(18) = 0
        SQHARM(18) = 0
        CALL SQLADR(%REF(COMP600), SQHSTV(19))
        SQHSTL(19) = 4
        SQINDV(19) = 0
        SQHARM(19) = 0
        CALL SQLADR(%REF(C600M200), SQHSTV(20))
        SQHSTL(20) = 4
        SQINDV(20) = 0
        SQHARM(20) = 0
        CALL SQLADR(%REF(B716), SQHSTV(21))
        SQHSTL(21) = 4
        SQINDV(21) = 0
        SQHARM(21) = 0
        CALL SQLADR(%REF(A716), SQHSTV(22))
        SQHSTL(22) = 4
        SQINDV(22) = 0
        SQHARM(22) = 0
        CALL SQLADR(%REF(TONS), SQHSTV(23))
        SQHSTL(23) = 4
        SQINDV(23) = 0
        SQHARM(23) = 0
        CALL SQLADR(%REF(HOURS), SQHSTV(24))
        SQHSTL(24) = 4
        SQINDV(24) = 0
        SQHARM(24) = 0
        CALL SQLADR(%REF(DATEX), SQHSTV(25))
        SQHSTL(25) = 11
        SQINDV(25) = 0
        SQHARM(25) = 0
        CALL SQLADR(%REF(SHIFT), SQHSTV(26))
        SQHSTL(26) = 4
        SQINDV(26) = 0
        SQHARM(26) = 0
        CALL SQLADR(%REF(HALF), SQHSTV(27))
        SQHSTL(27) = 4
        SQINDV(27) = 0
        SQHARM(27) = 0
        CALL SQLADR(%REF(LINE), SQHSTV(28))
        SQHSTL(28) = 4
        SQINDV(28) = 0
        SQHARM(28) = 0
        CALL SQLFIN(14, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 28, SQLITR, 164, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0002),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 9000
        WRITE (6,*) ' UPDATE SECTION ',DATEX,SHIFT,HALF,LINE
C
801     CONTINUE
901     CONTINUE
1001    CONTINUE
cccccccccccccccccc
c
c
c               
        else
                WRITE (6,*) SQLCDE
                write(6,'(70a)') SQLEMC
C               EXEC SQL WHENEVER SQLERROR CONTINUE
C               EXEC SQL ROLLBACK RELEASE
      CALL SQLFIN(14, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 2, 1, 256,
     +%REF(SQC001), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
                GO TO 99
        end if
C
  999 CONTINUE
C99     EXEC SQL COMMIT RELEASE
C
        CALL SQLFIN(14, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 21, 1, 256,
     +%REF(SQC001), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
99      STOP
        END

      BLOCK DATA oraclD
      INTEGER*2 SQCNT
        CHARACTER*198 SQ0001
        CHARACTER*370 SQ0002
      INTEGER*2 SQC000(295)
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC001(40)
      CHARACTER*14 SQLFNM
      COMMON / oraclI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000, SQC001
      COMMON / oraclC / 
     +SQLFNM, SQ0001, SQ0002
      DATA SQLCTX / 4848 /
      DATA IAPSUC / 0 /, IAPFAI / 1403 /, IAPFTL / 535 /
      DATA SQ0001 /'INSERT INTO AGGL_TUMBLES VALUES  (TO_DATE(:b1,''DD-M
     +ON-YYYY''),  :b2,:b3,:b4,  :b5,:b6,:b7,:b8,:b9,:b10,:b11,:b12,:b13
     +,:b14,  :b15,:b16,:b17,  :b18,:b19,:b20,:b21,:b22,:b23,  :b24,:b25
     +,:b26,:b27,:b28)'/
      DATA SQ0002 /'UPDATE AGGL_TUMBLES   SET B58=:b1,B12=:b2,B38=:b3,B1
     +4=:b4,  B4M=:b5,B28=:b6,BM28=:b7,A58=:b8,  A12=:b9,A38=:b10,  A14=
     +:b11,A4M=:b12,A28=:b13,  AM28=:b14,ACOMP=:b15,ACOMP200=:b16,  B916
     +=:b17,A916=:b18,COMP600=:b19,  C600M200=:b20,B716=:b21,A716=:b22,T
     +ONS=:b23,HOURS=:b24 WHERE   TRUNC(DATEX)=TRUNC(TO_DATE(:b25,''DD-M
     +ON-YYYY'')) AND  SHIFT=:b26 AND HALF=:b27 AND LINE=:b28 '/
      DATA SQLFNM /'oracle_fix.PFO'/
      DATA (SQC000(SQCNT), SQCNT=1, 295) /6,4130,2,8,0,0,0,0,0,0,0,0,
     +0,0,27,89,3,3,0,0,0,1,1,0,0,1,1,0,0,1,10,0,0,33,8,0,0,0,0,0,0,0,
     +0,1,198,3,182,28,28,0,1,0,1,1,0,0,1,3,0,0,1,3,0,0,1,3,0,0,1,4,0,
     +0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,
     +0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,
     +0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,164,8,
     +0,0,0,0,0,0,0,0,2,370,5,259,28,28,0,1,0,1,4,0,0,1,4,0,0,1,4,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,1,0,0,1,3,0,0,1,3,0,0,
     +1,3,0,0/
      DATA (SQC001(SQCNT), SQCNT=1, 40) /6,4130,2,8,0,0,0,0,0,0,0,0,2,
     +0,32,284,0,0,0,1,0,21,8,0,0,0,0,0,0,0,0,2,0,30,288,0,0,0,1,0/
      END
