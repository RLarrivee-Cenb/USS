C
C
C		MATERIAL.FOR
C
C
C	include this code to determine if fluxed or non-fluxed material
C	 is being made in each plant.
C
C
	LOGICAL FTNFFB,FTNFFH
C
	OPEN (UNIT=99,FILE='MET_P:FNOFF.DAT',ACCESS='DIRECT',RECL=2,
	1	STATUS='UNKNOWN',FORM='UNFORMATTED',
	2	SHARED,USEROPEN=UFO_OPEN)
C
	READ (99,REC=1)FTNFFB,FTNFFH
C
	CLOSE (99)
