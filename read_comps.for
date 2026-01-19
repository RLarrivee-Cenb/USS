CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CONFIDENTIAL
C       Property of United States Steel Corporation
C       Copyright 2010 United States Steel Corporation
C       All rights reserved.
C******************************************************************************
C               READ_COMPS.FOR
C******************************************************************************
C Revisions:
C       20100809 IM322852  SPA6635 changed COMP2(2,3,2,5) to COMP2(3,3,2,5)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cc	EXTERNAL UFO_OPEN
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
     1	PP1*29/'SCREEN FRACTIONS,COMPRESSIONS'/,KD*8,A5*5,KDX*8             
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
     1  'AUG','SEP','OCT','NOV','DEC'/,TDAY*11,YDAY*11,KY*6,AD*23
C
	INTEGER D2,EXIST,LS,LH
C
	CHARACTER ASC*10,FULLDATE*23,asd*11
C
	real*4 PCT(14,3,2,5),totw,totw3
	LOGICAL FTNFFB,FTNFFH
	OPEN (11,FILE='MET_screen:SCREEN.DAT',
     1	ACCESS='KEYED',STATUS='old',shared)
c
	KDX='20050207'
c
28	READ (11,KEY=KDX,iostat=mm) KD,DAT,IS,IH,SCREEN,COMP,
     1		 	    CAB,SIL,FILT2,
     1			    FILT3,BL2,BL3,FLUXG,FCA,FMG,FAL,FSI,   
     1			    PA,PC,PM,S4,H4,scwt,tons4,ghrs,comp2
c
CCCC	if (mm .ne. 0) go to 99
C
C
20	call sys$asctim (,asd,dat,)
99	STOP
	END
