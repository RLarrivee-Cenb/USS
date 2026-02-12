*********************************************************************
* This program LOADS PHYSICAL DATA INTO THE ORACLE TABLE PHYS
*********************************************************************
 
      PROGRAM BTUMO
C

      EXTERNAL btumoD
      INTEGER*4 SQLITR
      INTEGER*4 SQHSTV(16)
      INTEGER*4 SQHSTL(16)
      INTEGER*4 SQINDV(16)
      INTEGER*4 SQHARM(16)
      INTEGER*4 SQHARC(16)
      CHARACTER*227 SQ0001
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(178)
      CHARACTER*9 SQLFNM
      COMMON / btumoI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / btumoC / SQLFNM, SQ0001
        INTEGER*4       UFO_OPEN,I,J,K,L,M,N
C
        CHARACTER*12    DATEX
C
        REAL*8          PHY(14)
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
         REAL*4         S2BT916
         REAL*4         S2AT14
         REAL*4         S2AT28
         REAL*4         S2COMP
         REAL*4         S2LCOMP
         REAL*4         S3BT12
         REAL*4         S3BT14
         REAL*4         S3BT916
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
        OPEN    (UNIT=1,FILE='OREM:BTUMO.DAT',STATUS='OLD',
     +           ACCESS='DIRECT',RECORDTYPE='FIXED',SHARED,
     +           FORM='UNFORMATTED',RECL=31)
C
        READ    (1,REC=1)DATEX,PHY
C
        CLOSE   (UNIT=1)
C
        PDATE=DATEX(1:11)       !ASCII DATE FOR ORACLE TABLE PHYS
        IF (DATEX(12:12) .EQ. '3')SHIFT=3
        IF (DATEX(12:12) .EQ. '2')SHIFT=2
        IF (DATEX(12:12) .EQ. '1')SHIFT=1
C
        S2BT12=PHY(1)
        S2BT14=PHY(2)
        S2BT916=PHY(3)
        S2AT14=PHY(4)
        S2AT28=PHY(5)
        S2COMP=PHY(6)
        S2LCOMP=PHY(7)
C
        S3BT12=PHY(8)
        S3BT14=PHY(9)
        S3BT916=PHY(10)
        S3AT14=PHY(11)
        S3AT28=PHY(12)
        S3COMP=PHY(13)
        S3LCOMP=PHY(14)
C
C     EXEC SQL INCLUDE SQLCA
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
C     EXEC SQL WHENEVER SQLERROR GOTO 9000
C
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
      CALL SQLFIN(9, %REF(SQLFNM))
      SQLITR = 10
      CALL SQLFX7(6, %REF(SQLCTX), 3, SQLITR, 2, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) GO TO 9000
C     EXEC SQL AT DBNAM1 UPDATE PHYS SET S2BT12=:S2BT12,S2BT14=:S2BT14,
C    1    S2AT14=:S2AT14,S2AT28=:S2AT28,S2COMP=:S2COMP,
C    2    S2LCOMP=:S2LCOMP,S3BT12=:S3BT12,S3BT14=:S3BT14,
C    3    S3AT14=:S3AT14,S3AT28=:S3AT28,S3COMP=:S3COMP,
C    4    S3LCOMP=:S3LCOMP,S2BT916=:S2BT916,S3BT916=:S3BT916
C    5    WHERE SHIFT=:SHIFT AND PDATE=:PDATE
C
      CALL SQLADR(%REF(S2BT12), SQHSTV(1))
      SQHSTL(1) = 4
      SQINDV(1) = 0
      SQHARM(1) = 0
      CALL SQLADR(%REF(S2BT14), SQHSTV(2))
      SQHSTL(2) = 4
      SQINDV(2) = 0
      SQHARM(2) = 0
      CALL SQLADR(%REF(S2AT14), SQHSTV(3))
      SQHSTL(3) = 4
      SQINDV(3) = 0
      SQHARM(3) = 0
      CALL SQLADR(%REF(S2AT28), SQHSTV(4))
      SQHSTL(4) = 4
      SQINDV(4) = 0
      SQHARM(4) = 0
      CALL SQLADR(%REF(S2COMP), SQHSTV(5))
      SQHSTL(5) = 4
      SQINDV(5) = 0
      SQHARM(5) = 0
      CALL SQLADR(%REF(S2LCOMP), SQHSTV(6))
      SQHSTL(6) = 4
      SQINDV(6) = 0
      SQHARM(6) = 0
      CALL SQLADR(%REF(S3BT12), SQHSTV(7))
      SQHSTL(7) = 4
      SQINDV(7) = 0
      SQHARM(7) = 0
      CALL SQLADR(%REF(S3BT14), SQHSTV(8))
      SQHSTL(8) = 4
      SQINDV(8) = 0
      SQHARM(8) = 0
      CALL SQLADR(%REF(S3AT14), SQHSTV(9))
      SQHSTL(9) = 4
      SQINDV(9) = 0
      SQHARM(9) = 0
      CALL SQLADR(%REF(S3AT28), SQHSTV(10))
      SQHSTL(10) = 4
      SQINDV(10) = 0
      SQHARM(10) = 0
      CALL SQLADR(%REF(S3COMP), SQHSTV(11))
      SQHSTL(11) = 4
      SQINDV(11) = 0
      SQHARM(11) = 0
      CALL SQLADR(%REF(S3LCOMP), SQHSTV(12))
      SQHSTL(12) = 4
      SQINDV(12) = 0
      SQHARM(12) = 0
      CALL SQLADR(%REF(S2BT916), SQHSTV(13))
      SQHSTL(13) = 4
      SQINDV(13) = 0
      SQHARM(13) = 0
      CALL SQLADR(%REF(S3BT916), SQHSTV(14))
      SQHSTL(14) = 4
      SQINDV(14) = 0
      SQHARM(14) = 0
      CALL SQLADR(%REF(SHIFT), SQHSTV(15))
      SQHSTL(15) = 4
      SQINDV(15) = 0
      SQHARM(15) = 0
      CALL SQLADR(%REF(PDATE), SQHSTV(16))
      SQHSTL(16) = 11
      SQINDV(16) = 0
      SQHARM(16) = 0
      CALL SQLFIN(9, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 16, SQLITR, 39, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0001),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) GO TO 9000
C     EXEC SQL INCLUDE SQLCA
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
C     EXEC SQL AT DBNAM1 COMMIT WORK RELEASE 
      CALL SQLFIN(9, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 128, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) GO TO 9000
      GOTO 9999

9000  PRINT '(70A1)', SQLEMC
C     EXEC SQL INCLUDE SQLCA
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
C     EXEC SQL WHENEVER SQLERROR CONTINUE
C     EXEC SQL AT DBNAM1 ROLLBACK RELEASE
C
      CALL SQLFIN(9, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 153, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
9999  CONTINUE
      STOP
      END

      BLOCK DATA btumoD
      INTEGER*2 SQCNT
      CHARACTER*227 SQ0001
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(178)
      CHARACTER*9 SQLFNM
      COMMON / btumoI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / btumoC / SQLFNM, SQ0001
      DATA SQLCTX / 4670 /
      DATA IAPSUC / 0 /, IAPFAI / 1403 /, IAPFTL / 535 /
      DATA SQ0001 /'UPDATE PHYS SET S2BT12=:b1,S2BT14=:b2,    S2AT14=:b3
     +,S2AT28=:b4,S2COMP=:b5,    S2LCOMP=:b6,S3BT12=:b7,S3BT14=:b8,    S
     +3AT14=:b9,S3AT28=:b10,S3COMP=:b11,    S3LCOMP=:b12,S2BT916=:b13,S3
     +BT916=:b14 WHERE SHIFT=:b15 AND PDATE=:b16 '/
      DATA SQLFNM /'btumo.PFO'/
      DATA (SQC000(SQCNT), SQCNT=1, 178) /6,4130,2,8,0,0,0,0,0,0,0,0,0,
     +0,27,79,3,3,0,0,6,68,66,78,65,77,49,1,1,0,0,1,1,0,0,1,1,0,0,39,8,
     +0,0,0,0,0,0,0,0,1,227,5,82,16,16,0,3,6,68,66,78,65,77,49,1,4,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,1,0,0,128,8,0,
     +0,0,0,0,0,0,0,1,0,30,90,0,0,0,3,6,68,66,78,65,77,49,153,8,0,0,0,
     +0,0,0,0,0,1,0,32,96,0,0,0,3,6,68,66,78,65,77,49/
      END
