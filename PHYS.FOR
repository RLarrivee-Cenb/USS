*********************************************************************
* This program LOADS AN EMPTY SHIFT RECORD INTO THE ORACLE TABLE PHYS
*********************************************************************
 
      PROGRAM PHYS
C

      EXTERNAL physD
      INTEGER*4 SQLITR
      INTEGER*4 SQHSTV(14)
      INTEGER*4 SQHSTL(14)
      INTEGER*4 SQINDV(14)
      INTEGER*4 SQHARM(14)
      INTEGER*4 SQHARC(14)
      CHARACTER*235 SQ0001
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(170)
      CHARACTER*8 SQLFNM
      COMMON / physI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / physC / SQLFNM, SQ0001
        CHARACTER*23    DATX
C
        INTEGER*4       UFO_OPEN,I,J,K,L,M,N
C
C     EXEC SQL INCLUDE SQLCA
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
C     EXEC SQL BEGIN DECLARE SECTION
         CHARACTER*10   UID
         CHARACTER*10   PWD
         CHARACTER*10   DBSTR1
         REAL*4         S2BT12
         REAL*4         S2BT14
         REAL*4         S2AT14
         REAL*4         S2AT28
         REAL*4         S2COMP
         REAL*4         S2LCOMP
         REAL*4         S3BT12
         REAL*4         S3BT14
         REAL*4         S3AT14
         REAL*4         S3AT28
         REAL*4         S3COMP
         REAL*4         S3LCOMP
         REAL*4         SHIFT
         CHARACTER*11   PDATE
C     EXEC SQL END DECLARE SECTION
C
C
        EXTERNAL        UFO_OPEN
C
        CALL    LIB$DATE_TIME(DATX)
C
        PDATE=DATX(1:11)        !ASCII DATE FOR ORACLE TABLE PHYS
        SHIFT=1
        IF (DATX(13:14) .EQ. '22')SHIFT=3
        IF (DATX(13:14) .EQ. '14')SHIFT=2
        IF (DATX(14:14) .EQ. '6')SHIFT=1
C
C     EXEC SQL WHENEVER SQLERROR GOTO 9000
C     LOG ON TO ORACLE.
      UID = 'TOLIVE'
      PWD = 'MANAGER2'
      DBSTR1 = 'PROD2'
C
C     EXEC SQL DECLARE DBNAM1 DATABASE
C
C     EXEC SQL CONNECT :UID IDENTIFIED BY :PWD
C    1          AT DBNAM1 USING :DBSTR1
C
C
      CALL SQLADR(%REF(UID), SQHSTV(1))
      SQHSTL(1) = 10
      SQINDV(1) = 0
      SQHARM(1) = 0
      CALL SQLADR(%REF(PWD), SQHSTV(2))
      SQHSTL(2) = 10
      SQINDV(2) = 0
      SQHARM(2) = 0
      CALL SQLADR(%REF(DBSTR1), SQHSTV(3))
      SQHSTL(3) = 10
      SQINDV(3) = 0
      SQHARM(3) = 0
      CALL SQLFIN(8, %REF(SQLFNM))
      SQLITR = 10
      CALL SQLFX7(6, %REF(SQLCTX), 3, SQLITR, 2, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) GO TO 9000
C     EXEC SQL AT DBNAM1 INSERT INTO PHYS (PDATE,SHIFT,S2BT12,S2BT14,
C    1    S2AT14,S2AT28,S2COMP,S2LCOMP,S3BT12,S3BT14,
C    2    S3AT14,S3AT28,S3COMP,S3LCOMP)
C    3     VALUES(TO_DATE(:PDATE,'DD-MON-YYYY'),
C    4     :SHIFT,:S2BT12,:S2BT14,:S2AT14,:S2AT28,:S2COMP,
C    5     :S2LCOMP,:S3BT12,:S3BT14,:S3AT14,:S3AT28,
C    6     :S3COMP,:S3LCOMP)
C
      CALL SQLADR(%REF(PDATE), SQHSTV(1))
      SQHSTL(1) = 11
      SQINDV(1) = 0
      SQHARM(1) = 0
      CALL SQLADR(%REF(SHIFT), SQHSTV(2))
      SQHSTL(2) = 4
      SQINDV(2) = 0
      SQHARM(2) = 0
      CALL SQLADR(%REF(S2BT12), SQHSTV(3))
      SQHSTL(3) = 4
      SQINDV(3) = 0
      SQHARM(3) = 0
      CALL SQLADR(%REF(S2BT14), SQHSTV(4))
      SQHSTL(4) = 4
      SQINDV(4) = 0
      SQHARM(4) = 0
      CALL SQLADR(%REF(S2AT14), SQHSTV(5))
      SQHSTL(5) = 4
      SQINDV(5) = 0
      SQHARM(5) = 0
      CALL SQLADR(%REF(S2AT28), SQHSTV(6))
      SQHSTL(6) = 4
      SQINDV(6) = 0
      SQHARM(6) = 0
      CALL SQLADR(%REF(S2COMP), SQHSTV(7))
      SQHSTL(7) = 4
      SQINDV(7) = 0
      SQHARM(7) = 0
      CALL SQLADR(%REF(S2LCOMP), SQHSTV(8))
      SQHSTL(8) = 4
      SQINDV(8) = 0
      SQHARM(8) = 0
      CALL SQLADR(%REF(S3BT12), SQHSTV(9))
      SQHSTL(9) = 4
      SQINDV(9) = 0
      SQHARM(9) = 0
      CALL SQLADR(%REF(S3BT14), SQHSTV(10))
      SQHSTL(10) = 4
      SQINDV(10) = 0
      SQHARM(10) = 0
      CALL SQLADR(%REF(S3AT14), SQHSTV(11))
      SQHSTL(11) = 4
      SQINDV(11) = 0
      SQHARM(11) = 0
      CALL SQLADR(%REF(S3AT28), SQHSTV(12))
      SQHSTL(12) = 4
      SQINDV(12) = 0
      SQHARM(12) = 0
      CALL SQLADR(%REF(S3COMP), SQHSTV(13))
      SQHSTL(13) = 4
      SQINDV(13) = 0
      SQHARM(13) = 0
      CALL SQLADR(%REF(S3LCOMP), SQHSTV(14))
      SQHSTL(14) = 4
      SQINDV(14) = 0
      SQHARM(14) = 0
      CALL SQLFIN(8, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 14, SQLITR, 39, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0001),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) GO TO 9000
C     EXEC SQL AT DBNAM1 COMMIT WORK RELEASE 
      CALL SQLFIN(8, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 120, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) GO TO 9000
      GOTO 9999

9000  PRINT '(70A1)', SQLEMC
C     EXEC SQL WHENEVER SQLERROR CONTINUE
C     EXEC SQL AT DBNAM1 ROLLBACK RELEASE
C
      CALL SQLFIN(8, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 145, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
9999  CONTINUE
      END

      BLOCK DATA physD
      INTEGER*2 SQCNT
      CHARACTER*235 SQ0001
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(170)
      CHARACTER*8 SQLFNM
      COMMON / physI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / physC / SQLFNM, SQ0001
      DATA SQLCTX / 2446 /
      DATA IAPSUC / 0 /, IAPFAI / 1403 /, IAPFTL / 535 /
      DATA SQ0001 /'INSERT INTO PHYS (PDATE,SHIFT,S2BT12,S2BT14,    S2AT
     +14,S2AT28,S2COMP,S2LCOMP,S3BT12,S3BT14,    S3AT14,S3AT28,S3COMP,S3
     +LCOMP)     VALUES(TO_DATE(:b1,''DD-MON-YYYY''),     :b2,:b3,:b4,:b
     +5,:b6,:b7,     :b8,:b9,:b10,:b11,:b12,     :b13,:b14)'/
      DATA SQLFNM /'phys.PFO'/
      DATA (SQC000(SQCNT), SQCNT=1, 170) /6,4130,2,8,0,0,0,0,0,0,0,0,0,
     +0,27,52,3,3,0,0,6,68,66,78,65,77,49,1,1,0,0,1,1,0,0,1,1,0,0,39,8,
     +0,0,0,0,0,0,0,0,1,235,3,56,14,14,0,3,6,68,66,78,65,77,49,1,1,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,120,8,0,0,0,0,0,0,0,0,1,
     +0,30,64,0,0,0,3,6,68,66,78,65,77,49,145,8,0,0,0,0,0,0,0,0,1,0,32,
     +69,0,0,0,3,6,68,66,78,65,77,49/
      END
