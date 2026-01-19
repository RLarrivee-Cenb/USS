CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C               METINPUT.FOR
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
        INTEGER UFO_OPEN
C
        EXTERNAL UFO_OPEN
C

      EXTERNAL metinD
      INTEGER*4 SQLITR
      INTEGER*4 SQHSTV(7)
      INTEGER*4 SQHSTL(7)
      INTEGER*4 SQINDV(7)
      INTEGER*4 SQHARM(7)
      INTEGER*4 SQHARC(7)
        CHARACTER*172 SQ0001
        CHARACTER*186 SQ0002
        CHARACTER*148 SQ0003
        CHARACTER*162 SQ0004
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(270)
      CHARACTER*13 SQLFNM
      COMMON / metinI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / metinC / SQLFNM, SQ0001, SQ0002, SQ0003, SQ0004
        INCLUDE 'FMS$EXAMPLES:FDVDEF'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        INTEGER  WORKSPACE( 3 ),        !General workspace
     1           CHECKWKSP( 3 ),        !Check workspace
     1           TCA( 3 )               !Terminal Control Area
c
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
c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        DOUBLE PRECISION CT(18),FT(18),SCREEN(14,3,2,5),COMP(2,3,2,5),
     1    CAB(2,3,5),SIL(3,5),FILT2(5,3),BL2,FILT3(5,3),BL3,TOTAL,
     1    H2O,RMF,FTMF,BOIC(150),HOIC(150),CX(5),MX(5),AX(5),SX(5),
     1    FLUXG,P34,P12,FCA(2,3),FMG(2,3),FAL(2,3),FSI(2,3),SM,PP(2),
     1    DTR,DTS,PPx(3),
     1    H2O3,RMF3,DTR3,DTS3,P343,TS,TS3,PPX3(3),P123
C                                                                 
        INTEGER AMB(3),BMH(3),BD(2),YSTRDAY(2),NEW(3),LAST(3),
     1          IN,INO,DAT(2),INTV(2),IS,IS2,IS3,IH,INPDATE(2)
C
        DIMENSION ARD(8)
C
        CHARACTER PL(2)*2/'CN','AG'/,FN2*25,HSKEY*4,
     1          MO(12)*3/'JAN','FEB','MAR','APR',
     1          'MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
     1          ,CAD*9,FILN*16,SH*2,PS*3,TEMP*12,MINP,AINTV/'1'/,
     1          INAM(4)*6/'AMIOIC','BMIOIC',' AMBOP',' BMHOP'/,  
     1          PART(3)*16/'SCREEN FRACTIONS','       SIO2     ',
     1          'FILTER CAKE     '/,AYSTR*23,FILNAM*30,ASC,ASC2*3,
     1          PP1*29/'SCREEN FRACTIONS,COMPRESSIONS'/             
C
        CHARACTER SHED*32/'+5/8,+1/2,+3/8,+1/4,4M,+28M,-28M'/,
C
     1          CHED*38/'COMPRESSION AVG., -200 PCT.'/,
C
     1          BHED*18 /'AL203,CAO,MGO'/,
     1          BHEX*18 /'SIO2,AL203,CAO,MGO'/,
C
     1          FES*4/'SIO2'/,
C
     1          FILT*26/'SIO2,-270,-500,AL2O3,CAO,MGO'/,
C
     1          BAT(2)*6/'BEFORE','AFTER '/
C
        CHARACTER AA*20
C

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
                CHARACTER*12    DBSTR1
                CHARACTER*400   DYNSTM
                CHARACTER*11    DATEX
                INTEGER         DMYX
                REAL*4          OH2O
                REAL*4          ORMF
                REAL*4          OP34
                REAL*4          OP12
                REAL*4          OH2O3
                REAL*4          ORMF3
                REAL*4          OP343
                REAL*4          OP123
                REAL*4          OFDTR
                REAL*4          OFDTS
C       EXEC SQL END DECLARE SECTION
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C       EXEC SQL WHENEVER SQLERROR GOTO 9000
C
        USERNAME='TOLIVE'
        PASSWORD='MANAGER2'
C
C       EXEC SQL CONNECT :USERNAME IDENTIFIED BY :PASSWORD
C
C
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
        CALL SQLFIN(13, %REF(SQLFNM))
        SQLITR = 10
        CALL SQLFX7(6, %REF(SQLCTX), 3, SQLITR, 2, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 9000
        CALL SYS$BINTIM (AINTV,INTV) !convert 24 hr interval to binary
C
C
        CALL FDV$ATERM( %DESCR(TCA),12 ,2)
        CALL FDV$AWKSP( %DESCR(WORKSPACE), 2000 )
        CALL FDV$LOPEN( 'USER_D:[METREP]METINPUT', 1 )
ccccc   call fdv$spada (0)
        CALL FDV$SPON ()
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        CALL FDV$CDISP ('METINPUT2')
C
C       step 1 and 2 concentrator input
C
1       CONTINUE
C
        IBACK=0
C
C
        NNN=0
C
        OPEN (13,FILE='MET_P:AMIBMI.DAY',STATUS='UNKNOWN',
     1  FORM='UNFORMATTED',
     1  ACCESS='DIRECT',RECL=2)
C
        READ (13,REC=1)DAT




        call lib$sub_times (dat,intv,dat)





C
        CALL SYS$ASCTIM (,AYSTR,DAT,)           !cnvt time to ascii
C
                IF (AYSTR(1:1) .EQ. ' ') AYSTR(1:1)='0'
C
C
        CALL FDV$PUT (AYSTR(1:2)//AYSTR(4:6)//AYSTR(8:11),'DATE')
C
15      CALL FDV$GET (AA,I,'OK')
C
        IF (AA(1:1) .EQ. 'N' .OR. AA(1:1) .EQ. 'n') THEN
                NNN=1
                CALL FDV$PUT (' ','OK')
                CALL FDV$GET (AA,I,'DATE')
                IF (AA(2:2) .EQ. ' ') AA(1:2)='0'//AA(1:1)
                IF (AA(1:1) .EQ. ' ') AA(1:1)='0'
                AYSTR=AA(1:2)//'-'//AA(3:5)//'-'//AA(6:9)
                CALL SYS$BINTIM (AYSTR,INPDATE)
                GO TO 15
        END IF
C
        IF (.NOT. (AA(1:1) .EQ. 'Y' .OR. AA(1:1) .EQ. 'y')) go to 99
C
C
c
c
CCCC    IF (IBACK .NE. 0) GO TO 2


        close (13)
        go to 2




C
        OPEN (UNIT=42,FILE='MET_P:AMIOIC'//AYSTR(1:2)//'.DAT',
     1          FORM='UNFORMATTED',STATUS='UNKNOWN',ERR=2)
C
C
        OPEN (UNIT=43,FILE='MET_P:BMIOIC'//AYSTR(1:2)//'.DAT',
     1          FORM='UNFORMATTED',STATUS='UNKNOWN',ERR=2)
C
C
        READ (42,ERR=892)BOIC
C
892     READ (43,ERR=298)HOIC
C
C
298     CONTINUE                           
        REWIND (42)
        REWIND (43)
C
C
C
C                                            
C
C
C       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                       CONCENTRATOR INPUT
C
C
2       CONTINUE
C
C
200     CALL FDV$GET (AA,I,'H2O')
C
        READ (AA(1:4),'(F4.0)',ERR=200) H2O
C
        CALL FDV$GET (AA,I,'H2O3')
C
        READ (AA(1:4),'(F4.0)',ERR=200) H2O3
C
C
C
201     CALL FDV$GET (AA,I,'MAGFE')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 200
C
        READ (AA(1:4),'(F4.0)',ERR=201) RMF
C
        CALL FDV$GET (AA,I,'MAGFE3')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 201
C
        READ (AA(1:4),'(F4.0)',ERR=201) RMF3
C
C
202     CALL FDV$GET (AA,I,'RECOV')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 201
C
        READ (AA(1:4),'(F4.0)',ERR=202) DTR
C
        CALL FDV$GET (AA,I,'RECOV3')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 202
C
        READ (AA(1:4),'(F4.0)',ERR=202) DTR3
C
C
203     CALL FDV$GET (AA,I,'SIL')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 202
C
        READ (AA(1:5),'(F5.0)',ERR=203) DTS
C
        CALL FDV$GET (AA,I,'SIL3')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 203
C
        READ (AA(1:5),'(F5.0)',ERR=203) DTS3
C
C
204     CALL FDV$GET (AA,I,'INCH3')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 203
C
        READ (AA(1:6),'(F6.0)',ERR=204) P34
C
        CALL FDV$GET (AA,I,'INCH33')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 204
C
        READ (AA(1:6),'(F6.0)',ERR=204) P343
C
C
233     CALL FDV$GET (AA,I,'DTS')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 204
C
        READ (AA(1:6),'(F5.0)',ERR=233) TS
C
        CALL FDV$GET (AA,I,'DTS3')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 233
C
        READ (AA(1:6),'(F5.0)',ERR=233) TS3
C
C
205     CALL FDV$GET (AA,I,'HALF1')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 233
C
        READ (AA(1:4),'(F4.0)',ERR=205) PPX(1)
C
C
206     CALL FDV$GET (AA,I,'HALF2')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 205
C
        READ (AA(1:4),'(F4.0)',ERR=206) PPX(2)
C
C
207     CALL FDV$GET (AA,I,'HALF3')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 206
C
        READ (AA(1:4),'(F4.0)',ERR=207) PPX(3)
C
C
C
C
215     CALL FDV$GET (AA,I,'HALF13')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 207
C
        READ (AA(1:4),'(F4.0)',ERR=215) PPX3(1)
C
C
216     CALL FDV$GET (AA,I,'HALF23')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 205
C
        READ (AA(1:4),'(F4.0)',ERR=216) PPX3(2)
C
C
217     CALL FDV$GET (AA,I,'HALF33')
C
                IF (I .EQ. FDV$K_FT_PRV) GO TO 206
C
        READ (AA(1:4),'(F4.0)',ERR=217) PPX3(3)
C
C
        CALL FDV$GET (AA,I,'REPLY')
        CALL FDV$PUT (' ','REPLY')
C
        IF (AA(1:1) .EQ. 'Y' .OR. AA(1:1) .EQ. 'y') GO TO 9190
        IF (AA(1:1) .EQ. 'N' .OR. AA(1:1) .EQ. 'n') GO TO 200
        IF (AA(1:1) .EQ. 'A' .OR. AA(1:1) .EQ. 'a') GO TO 99
C
C
C         check if 3/4" is within limits
C
C
9190    IF (P34 .LT. 85 .OR. P34 .GT. 99
     1          .OR. P343 .LT. 85 .OR. P343 .GT. 99) THEN
          CALL FDV$PUT('THE %-3/4" IS OUT OF RANGE,
     1  IS THAT OK? (Y,N)','X')
                CALL FDV$GET (AA,I,'R2')
        CALL FDV$PUT('                             
     1                  ','X')
        IF (AA(1:1) .EQ. 'Y' .OR. AA(1:1) .EQ. 'y') GO TO 8765
                GO TO 204
        END IF
ccccccccccccccccccccccccccccc
c
c
c
c
8765    IJ=0
        IJ3=0
        SUMH=0
        SUMH3=0
C
        DO 699 I=1,3
C
                IF (PPx(I) .LE. 0) GO TO 689
C
                SUMH=SUMH+PPx(I)
                IJ=IJ+1
C
C
689             IF (PPx3(I) .LE. 0) GO TO 699
C
                SUMH3=SUMH3+PPx3(I)
                IJ3=IJ3+1
C
699     CONTINUE
C
        IF (SUMH .LE. 0) THEN
                P12=0
                GO TO 2671
        ELSE
                P12=SUMH/IJ
        END IF
C
        IF (P12 .LT. 65 .OR. P12 .GT. 90) THEN
         CALL FDV$PUT('THE AVG. %-1/2" IS OUT OF RANGE
     1  , IS THAT OK? (Y,N)','X')
                CALL FDV$GET (AA,I,'R2')
        CALL FDV$PUT('                         
     1                           ','X')
                IF (AA(1:1) .EQ. 'Y' .OR. AA(1:1) .EQ. 'y') GO TO 267
                GO TO (205,206,207),I
        END IF
C
C
C
2671    IF (SUMH3 .LE. 0) THEN
                P123=0
                GO TO 267
        ELSE
                P123=SUMH3/IJ3
        END IF
C
        IF (P123 .LT. 65 .OR. P123 .GT. 90) THEN
         CALL FDV$PUT('THE AVG. %-1/2" IS OUT OF RANGE
     1  , IS THAT OK? (Y,N)','X')
                CALL FDV$GET (AA,I,'R2')
        CALL FDV$PUT('                                     
     1               ','X')
                IF (AA(1:1) .EQ. 'Y' .OR. AA(1:1) .EQ. 'y') GO TO 267
                GO TO (205,206,207),I
        END IF
C
C
C
CCCCCCCCCCCCCCCCCCCC
C
C       send shift +1/2 to HALFSIL.DAT for use by in-house stds. program
C

        go to 1122


C
267     OPEN (UNIT=91,FILE='MET_P:HALFSIL2.DAT',STATUS='OLD',
     1          ACCESS='KEYED',SHARED)
C
c
        HSKEY(1:2)=AYSTR(10:11)
C
        DO 1011 I=1,12
                IF (MO(I) .EQ. AYSTR(4:6)) GO TO 313
1011    CONTINUE
C
        CALL FDV$PUTL('BO DATE')
        STOP
C
313     WRITE (HSKEY(3:4),'(I2)') I
        IF (HSKEY(3:3) .EQ. ' ') HSKEY(3:3)='0'
C
        READ (AYSTR(1:2),'(I2)') IID
C
        LMN=0
C
8333    READ (91,KEY=HSKEY,IOSTAT=M) HSD
C
        IF (.NOT.(M .EQ. 36 .OR. M .EQ. 0))THEN
                LMN=LMN+1
                IF (LMN .EQ. 6) GO TO 1798
                CALL LIB$WAIT (2.)
                GO TO 8333
                END IF
C
                DO 464 I=1,3
                        HSD.CR12(I,IID)=0
                        IF (PPX(I) .LE. 0 .AND. PPX3(I) .GT. 0)
     1                          HSD.CR12(I,IID)=PPX3(I)
                        IF (PPX(I) .GT. 0 .AND. PPX3(I) .LE. 0)
     1                          HSD.CR12(I,IID)=PPX(I)
                        IF (PPX(I) .GT. 0 .AND. PPX3(I) .GT. 0)
     1                          HSD.CR12(I,IID)=(PPX(I)+PPX3(I))/2
464             CONTINUE

C
C
C
332     IF (M .EQ. 36) THEN
                WRITE (91) HSD
        ELSE
                REWRITE (91) HSD
        END IF
C
        CLOSE (91)
C
C
CCCCCCCCCCCCCCCCCCCC
C
C
1798    IF (NNN .NE. 0) GO TO 909
C

        REWIND (42)
        REWIND (43)
C
1122    continue
        BOIC(31)=H2O
        BOIC(32)=RMF

        BOIC(41)=P34
        BOIC(42)=P12
        BOIC(52)=TS
        BOIC(45)=DTR
        BOIC(46)=DTS
C
        HOIC(19)=H2O3
        HOIC(21)=P343
        HOIC(22)=P123
        HOIC(20)=RMF3
        HOIC(27)=TS3
C
c       WRITE (42) BOIC
c       WRITE (43) HOIC
c
c       CLOSE (42)
c       CLOSE (43)
C
        OH2O=H2O
        OH2O3=H2O3
        ORMF=RMF
        ORMF3=RMF3
        OP34=P34
        OP343=P343
        OP12=P12
        OP123=P123
        OFDTR=DTR
        OFDTS=DTS
C
C
C
C
909     IF (NNN .EQ. 0) THEN                              
C
                CALL LIB$SUBX (DAT,INTV,DAT,) ! increase date by 1 day
C
cccccccc                WRITE (13,REC=1)DAT
cccccccc                close (13)
C
        ELSE
                CALL LIB$DAY (INPDAY,INPDATE,)
                CALL LIB$DAY (IFDAY,DAT,)
                IF (INPDAY .GE. IFDAY) THEN
                CALL LIB$ADD_TIMES (INPDATE,INTV,DAT,) ! increase date by 1 day
ccccccccc               WRITE (13,REC=1)DAT
ccccccccc               close (13)
                END IF
        END IF
C
C
C
C
C
C
C
C
        DATEX=AYSTR(1:11)
        DMYX=1
C       EXEC SQL INSERT INTO MET_CONC_PLANT2 (DATEX,DMY,RMF_H2O,
C    1  RMF_MAG_FE,PLANT_34_INCH,PLANT_12_INCH,DAV_TUBE_REC_FLOT) 
C    1  VALUES 
C    1  (TO_DATE(:DATEX,'DD-MON-YYYY'),
C    1  :DMYX,:OH2O,:ORMF,:OP34,:OP12,:OFDTR)
C
Cccccccccccc
        CALL SQLADR(%REF(DATEX), SQHSTV(1))
        SQHSTL(1) = 11
        SQINDV(1) = 0
        SQHARM(1) = 0
        CALL SQLADR(%REF(DMYX), SQHSTV(2))
        SQHSTL(2) = 4
        SQINDV(2) = 0
        SQHARM(2) = 0
        CALL SQLADR(%REF(OH2O), SQHSTV(3))
        SQHSTL(3) = 4
        SQINDV(3) = 0
        SQHARM(3) = 0
        CALL SQLADR(%REF(ORMF), SQHSTV(4))
        SQHSTL(4) = 4
        SQINDV(4) = 0
        SQHARM(4) = 0
        CALL SQLADR(%REF(OP34), SQHSTV(5))
        SQHSTL(5) = 4
        SQINDV(5) = 0
        SQHARM(5) = 0
        CALL SQLADR(%REF(OP12), SQHSTV(6))
        SQHSTL(6) = 4
        SQINDV(6) = 0
        SQHARM(6) = 0
        CALL SQLADR(%REF(OFDTR), SQHSTV(7))
        SQHSTL(7) = 4
        SQINDV(7) = 0
        SQHARM(7) = 0
        CALL SQLFIN(13, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 7, SQLITR, 33, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0001),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 9000
9000    if (sqlcde .eq. -1) THEN
c
C       EXEC SQL UPDATE MET_CONC_PLANT2 
C    1  SET RMF_H2O=:OH2O,RMF_MAG_FE=:ORMF,
C    1  PLANT_34_INCH=:OP34,PLANT_12_INCH=:OP12,
C    1  DAV_TUBE_REC_FLOT=:OFDTR
C    1  WHERE 
C    1  TRUNC(DATEX)=TRUNC(TO_DATE(:DATEX,'DD-MON-YYYY')) AND
C    1  DMY=:DMYX
C
        CALL SQLADR(%REF(OH2O), SQHSTV(1))
        SQHSTL(1) = 4
        SQINDV(1) = 0
        SQHARM(1) = 0
        CALL SQLADR(%REF(ORMF), SQHSTV(2))
        SQHSTL(2) = 4
        SQINDV(2) = 0
        SQHARM(2) = 0
        CALL SQLADR(%REF(OP34), SQHSTV(3))
        SQHSTL(3) = 4
        SQINDV(3) = 0
        SQHARM(3) = 0
        CALL SQLADR(%REF(OP12), SQHSTV(4))
        SQHSTL(4) = 4
        SQINDV(4) = 0
        SQHARM(4) = 0
        CALL SQLADR(%REF(OFDTR), SQHSTV(5))
        SQHSTL(5) = 4
        SQINDV(5) = 0
        SQHARM(5) = 0
        CALL SQLADR(%REF(DATEX), SQHSTV(6))
        SQHSTL(6) = 11
        SQINDV(6) = 0
        SQHARM(6) = 0
        CALL SQLADR(%REF(DMYX), SQHSTV(7))
        SQHSTL(7) = 4
        SQINDV(7) = 0
        SQHARM(7) = 0
        CALL SQLFIN(13, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 7, SQLITR, 80, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0002),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 9000
        else
                WRITE (6,*) SQLCDE
                write(6,'(70a)') SQLEMC
C               EXEC SQL WHENEVER SQLERROR CONTINUE
C               EXEC SQL ROLLBACK RELEASE
      CALL SQLFIN(13, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 127, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
                GO TO 99
        end if
c
CCCCCCCCCCC
C
C       EXEC SQL WHENEVER SQLERROR GOTO 9002
C
        OP12=OP123
C
        DATEX=AYSTR(1:11)
        DMYX=1
C       EXEC SQL INSERT INTO MET_CONC_PLANT3 (DATEX,DMY,RMF_H2O,
C    1  RMF_MAG_FE,PLANT_34_INCH,PLANT_12_INCH) VALUES 
C    1  (TO_DATE(:DATEX,'DD-MON-YYYY'),
C    1  :DMYX,:OH2O3,:ORMF3,:OP343,:OP12)
C
Cccccccccccc
        CALL SQLADR(%REF(DATEX), SQHSTV(1))
        SQHSTL(1) = 11
        SQINDV(1) = 0
        SQHARM(1) = 0
        CALL SQLADR(%REF(DMYX), SQHSTV(2))
        SQHSTL(2) = 4
        SQINDV(2) = 0
        SQHARM(2) = 0
        CALL SQLADR(%REF(OH2O3), SQHSTV(3))
        SQHSTL(3) = 4
        SQINDV(3) = 0
        SQHARM(3) = 0
        CALL SQLADR(%REF(ORMF3), SQHSTV(4))
        SQHSTL(4) = 4
        SQINDV(4) = 0
        SQHARM(4) = 0
        CALL SQLADR(%REF(OP343), SQHSTV(5))
        SQHSTL(5) = 4
        SQINDV(5) = 0
        SQHARM(5) = 0
        CALL SQLADR(%REF(OP12), SQHSTV(6))
        SQHSTL(6) = 4
        SQINDV(6) = 0
        SQHARM(6) = 0
        CALL SQLFIN(13, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 6, SQLITR, 146, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0003),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 9002
9002    if (sqlcde .eq. -1) THEN
c
C       EXEC SQL UPDATE MET_CONC_PLANT3 
C    1  SET RMF_H2O=:OH2O3,RMF_MAG_FE=:ORMF3,
C    1  PLANT_34_INCH=:OP343,PLANT_12_INCH=:OP12
C    1  WHERE 
C    1  TRUNC(DATEX)=TRUNC(TO_DATE(:DATEX,'DD-MON-YYYY')) AND
C    1  DMY=:DMYX
C
C
        CALL SQLADR(%REF(OH2O3), SQHSTV(1))
        SQHSTL(1) = 4
        SQINDV(1) = 0
        SQHARM(1) = 0
        CALL SQLADR(%REF(ORMF3), SQHSTV(2))
        SQHSTL(2) = 4
        SQINDV(2) = 0
        SQHARM(2) = 0
        CALL SQLADR(%REF(OP343), SQHSTV(3))
        SQHSTL(3) = 4
        SQINDV(3) = 0
        SQHARM(3) = 0
        CALL SQLADR(%REF(OP12), SQHSTV(4))
        SQHSTL(4) = 4
        SQINDV(4) = 0
        SQHARM(4) = 0
        CALL SQLADR(%REF(DATEX), SQHSTV(5))
        SQHSTL(5) = 11
        SQINDV(5) = 0
        SQHARM(5) = 0
        CALL SQLADR(%REF(DMYX), SQHSTV(6))
        SQHSTL(6) = 4
        SQINDV(6) = 0
        SQHARM(6) = 0
        CALL SQLFIN(13, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 6, SQLITR, 189, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0004),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 9002
        else
                WRITE (6,*) SQLCDE
                write(6,'(70a)') SQLEMC
C               EXEC SQL WHENEVER SQLERROR CONTINUE
C               EXEC SQL ROLLBACK RELEASE
      CALL SQLFIN(13, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 232, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
                GO TO 99
        end if
C




CCCCCCCCCCC
C
C       EXEC SQL COMMIT RELEASE
C
C
        CALL SQLFIN(13, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 251, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
99      CALL FDV$LCLOS
        CALL FDV$SPADA (0)
        CALL FDV$DEL ('METINPUT')
        CALL FDV$DWKSP (%DESCR(WORKSPACE))
        CALL FDV$DTERM(%DESCR(TCA))
c
        STOP
C
        END

      BLOCK DATA metinD
      INTEGER*2 SQCNT
        CHARACTER*172 SQ0001
        CHARACTER*186 SQ0002
        CHARACTER*148 SQ0003
        CHARACTER*162 SQ0004
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(270)
      CHARACTER*13 SQLFNM
      COMMON / metinI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / metinC / SQLFNM, SQ0001, SQ0002, SQ0003, SQ0004
      DATA SQLCTX / 4756 /
      DATA IAPSUC / 0 /, IAPFAI / 1403 /, IAPFTL / 535 /
      DATA SQ0001 /'INSERT INTO MET_CONC_PLANT2 (DATEX,DMY,RMF_H2O,  RMF
     +_MAG_FE,PLANT_34_INCH,PLANT_12_INCH,DAV_TUBE_REC_FLOT)   VALUES   
     +(TO_DATE(:b1,''DD-MON-YYYY''),  :b2,:b3,:b4,:b5,:b6,:b7)'/
      DATA SQ0002 /'UPDATE MET_CONC_PLANT2   SET RMF_H2O=:b1,RMF_MAG_FE=
     +:b2,  PLANT_34_INCH=:b3,PLANT_12_INCH=:b4,  DAV_TUBE_REC_FLOT=:b5 
     +WHERE   TRUNC(DATEX)=TRUNC(TO_DATE(:b6,''DD-MON-YYYY'')) AND  DMY=
     +:b7 '/
      DATA SQ0003 /'INSERT INTO MET_CONC_PLANT3 (DATEX,DMY,RMF_H2O,  RMF
     +_MAG_FE,PLANT_34_INCH,PLANT_12_INCH) VALUES   (TO_DATE(:b1,''DD-MO
     +N-YYYY''),  :b2,:b3,:b4,:b5,:b6)'/
      DATA SQ0004 /'UPDATE MET_CONC_PLANT3   SET RMF_H2O=:b1,RMF_MAG_FE=
     +:b2,  PLANT_34_INCH=:b3,PLANT_12_INCH=:b4 WHERE   TRUNC(DATEX)=TRU
     +NC(TO_DATE(:b5,''DD-MON-YYYY'')) AND  DMY=:b6 '/
      DATA SQLFNM /'metinputx.PFO'/
      DATA (SQC000(SQCNT), SQCNT=1, 270) /6,4130,2,8,0,0,0,0,0,0,0,0,0,
     +0,27,109,3,3,0,0,0,1,1,0,0,1,1,0,0,1,10,0,0,33,8,0,0,0,0,0,0,0,0,
     +1,172,3,553,7,7,0,1,0,1,1,0,0,1,3,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,
     +4,0,0,1,4,0,0,80,8,0,0,0,0,0,0,0,0,2,186,5,562,7,7,0,1,0,1,4,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,1,0,0,1,3,0,0,127,8,0,0,0,0,0,
     +0,0,0,2,0,32,574,0,0,0,1,0,146,8,0,0,0,0,0,0,0,0,3,148,3,586,6,6,
     +0,1,0,1,1,0,0,1,3,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,189,8,0,0,
     +0,0,0,0,0,0,4,162,5,594,6,6,0,1,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,
     +0,1,1,0,0,1,3,0,0,232,8,0,0,0,0,0,0,0,0,4,0,32,606,0,0,0,1,0,251,
     +8,0,0,0,0,0,0,0,0,4,0,30,616,0,0,0,1,0/
      END
