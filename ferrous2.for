c  internal program for request 1031, for debug and test purposes 08-18-2004
	EXTERNAL UFO_OPEN
	CHARACTER DAT*11,AA(10)*1
	INTEGER L,S,H,M,nod,onod,lcnt,jx,j2
	integer*8 svday,oday
	REAL VAL
	j2=0
15	OPEN (UNIT=10,FILE='USER_D:[METREP.LAB45]FEP.TXT',
	1	STATUS='unknown',iostat=jx)
c	
	if (jx .eq. 0) then
	 go to 165
	else
	j2=j2+1
	call lib$wait (5.)
	if (j2 .lt. 5) then
	go to 15
	else
	go to 199
	end if
	end if
        
165	j2=0
16	OPEN (UNIT=11,FILE='USER_D:[METREP.LAB45]FERROUS2.DAT',
	1	STATUS='UNKNOWN',ACCESS='APPEND',SHARED,iostat=jx)
c
	if (jx .eq. 0) then
	  go to 18 
	else
	j2=j2+1
	call lib$wait (5.)
	if (j2 .lt. 5) then
	go to 16
	else
	go to 199
	end if
	end if
        
18	lcnt=0
1	READ (10,1010,ERR=99) DAT,L,S,H,M,VAL
c		
1010	FORMAT (' ',A11,1X,I1,1X,I1,1X,I1,1X,I1,1X,F5.2)
	WRITE (11,1111) DAT,L-2,S,H,M,VAL
1111	FORMAT (1H ,A11,4I1,F5.2)
c
	call sys$bintim (dat,svday)
	GO TO 1
c
99	close (10,STATUS='DELETE',err=199)
c
c
199	CLOSE (11)
	END
