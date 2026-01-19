****************************************
	PROGRAM		REDUCTION
****************************************
C	This program must be compiled with FORT/OLD_F77/ALIGN=NONE/
C	FLOAT=D_FLOAT/
C	WARN=NOALIGNMENT and linked with SUBSTVCTR,BAP/OPT.
C	Must use INCLUDE 'REDFURN:KEYDEF' for keyboard definitions.
C
	IMPLICIT	INTEGER*4(A - Z)
	CHARACTER*4	PRINTON/' [0i'/,PRINTOF/' [4i'/
C
	EXTERNAL	UFO_OPEN
	INCLUDE		'REDFURN:KEYDEF'
C
C
	INTEGER WORKSPACE( 3 ),
	1	TCA( 3 ),FIELDT
C	
	OPEN (UNIT=20,FILE='SYS$COMMAND',STATUS='UNKNOWN')
C
	CALL FDV$ATERM( %DESCR(TCA),12 ,2)
	CALL FDV$AWKSP( %DESCR(WORKSPACE), 2000 )
	CALL FDV$LOPEN( 'REDD:SUBSET.FLB',1 )
        CALL FDV$SPADA( 1 )    !Allows keypad to be used as field term
	CALL FDV$SSIGQ( 0 )
C
	CALL MENU 
C
	CALL FDV$LCLOS
	CALL FDV$DWKSP(%DESCR(WORKSPACE))
	CALL FDV$DTERM(%DESCR(TCA))
	CALL FDV$SPADA(0)
C
	END



*******************************
	FUNCTION DIGIT3(VAR)
*******************************
	REAL*4		REALNUM
        CHARACTER*3	VAR
	CHARACTER*3	DIGIT3

	READ  (VAR,10,ERR=999) REALNUM
10	FORMAT(BN,F3.1)

	WRITE (DIGIT3,20,ERR=999) REALNUM
20	FORMAT(BN,F3.1)

999	END

******************************************
	FUNCTION DIGIT4(VAR)
******************************************
	REAL*4		REALNUM
        CHARACTER*4	VAR
	CHARACTER*4	DIGIT4

	READ  (VAR,10,ERR=999) REALNUM
10	FORMAT(BN,F4.1)

	WRITE (DIGIT4,20,ERR=999) REALNUM
20	FORMAT(BN,F4.1)

999	END

*******************************
	FUNCTION DIGIT5(VAR)
*******************************
	REAL*4		REALNUM
        CHARACTER*5	VAR
	CHARACTER*5	DIGIT5

	READ  (VAR,10,ERR=999) REALNUM
10	FORMAT(BN,F5.2)

	WRITE (DIGIT5,20,ERR=999) REALNUM
20	FORMAT(BN,F5.2)

999	END

*******************************
	FUNCTION DIG5(VAR)
*******************************
	REAL*4		REALNUM
        CHARACTER*5	VAR
	CHARACTER*5	DIG5

	READ  (VAR,10,ERR=999) REALNUM
10	FORMAT(BN,F5.1)

	WRITE (DIG5,20,ERR=999) REALNUM
20	FORMAT(BN,F5.1)

999	END


*******************************
	FUNCTION INTDIG2(VAR)
*******************************
	INTEGER*2	INTNUM
        CHARACTER*2	VAR
	CHARACTER*2	INTDIG2

	READ  (VAR,10,ERR=999) INTNUM
10	FORMAT(BN,I2)

	WRITE (INTDIG2,20,ERR=999) INTNUM
20	FORMAT(BN,I2)

999	END

*******************************
	FUNCTION INTDIG3(VAR)
*******************************
	INTEGER*2	INTNUM
        CHARACTER*3	VAR
	CHARACTER*3	INTDIG3

	READ  (VAR,10,ERR=999) INTNUM
10	FORMAT(BN,I3)

	WRITE (INTDIG3,20,ERR=999) INTNUM
20	FORMAT(BN,I3)

999	END

********************************
	FUNCTION VALID1
********************************
	CHARACTER*31	FRMNAM,FLDNAME
	CHARACTER*80	UARVAL
	CHARACTER*2	FVALUE
	INTEGER		VALID1,CURPOS,FLDTRM,INSOVR,FINDEX,HELPNUM
	
	INTEGER*2	FDV$K_UVAL_SUC /1000/,
	1		FDV$K_UVAL_FAIL/1001/
C
	CALL FDV$RETCX(%DESCR(TCA),%DESCR(WORKSPACE),	!Return current context
	1		FRMNAM,UARVAL,CURPOS,FLDTRM,    !of form driver
	1		INSOVR,HELPNUM)
C
	CALL FDV$RETFN(FLDNAME,FINDEX)         	!Return field name
	CALL FDV$RET(FVALUE,FLDNAME,FINDEX)     !Return field value

	IF (INDEX(UARVAL,FVALUE) .GT. 0) THEN   !If field value is also in UAR	
		VALID1 = FDV$K_UVAL_SUC         !value(defined on form) success
	ELSE
		CALL FDV$PUTL('Illegal value')
		VALID1 = FDV$K_UVAL_FAIL
	END IF
	END

**************************************
	INTEGER FUNCTION MULT_OF_10
**************************************
	CHARACTER*31	FRMNAM,FLDNAME
	CHARACTER*80	UARVAL
	CHARACTER*5	FVALUE
	INTEGER		CURPOS,FLDTRM,INSOVR,FINDEX,HELPNUM,INTNUM,WHOLE
	REAL*4		RESULT,REMAIN
C	
	INTEGER*2	FDV$K_UVAL_SUC /1000/,
	1		FDV$K_UVAL_FAIL/1001/
C
	CALL FDV$RETCX(%DESCR(TCA),%DESCR(WORKSPACE),	!Return current context
	1		FRMNAM,UARVAL,CURPOS,FLDTRM,    !of form driver
	1		INSOVR,HELPNUM)
C
	CALL FDV$RETFN(FLDNAME,FINDEX)         	!Return field name
	CALL FDV$RET(FVALUE,FLDNAME,FINDEX)     !Return field value
C
	READ  (FVALUE(4:5),10,ERR=999) INTNUM
10	FORMAT(BN,I2)
C
	RESULT = INTNUM/10.0
	WHOLE  = RESULT
	REMAIN = RESULT - WHOLE
C
	IF (REMAIN .EQ. 0.00) THEN  
		MULT_OF_10 = FDV$K_UVAL_SUC      
	ELSE
		CALL FDV$PUTL('Seconds must be a multiple of 10')
		MULT_OF_10 = FDV$K_UVAL_FAIL
	END IF
999	END


********************************
	FUNCTION MODIFY
********************************
	CHARACTER*31	FRMNAM,FLDNAME
	CHARACTER*80	UARVAL
	CHARACTER*11	FVALUE
	INTEGER		CURPOS,FLDTRM,INSOVR,FINDEX,HELPNUM
	
	INTEGER*2	FDV$K_UVAL_SUC /1000/,
	1		FDV$K_UVAL_FAIL/1001/



	CALL FDV$RETCX(%DESCR(TCA),%DESCR(WORKSPACE),	!Return current context
	1		FRMNAM,UARVAL,CURPOS,FLDTRM,    !of form driver
	1		INSOVR,HELPNUM)


	CALL FDV$RETFN(FLDNAME,FINDEX)         	!Return field name
	CALL FDV$RET(FVALUE,FLDNAME,FINDEX)     !Return field value

	IF (INDEX(UARVAL,FVALUE) .GT. 0) THEN   !If field value is also in UAR	
		CALL FDV$PUTL('Input Required')
		MODIFY = FDV$K_UVAL_FAIL        !value(defined on form) fail
	ELSE
		MODIFY = FDV$K_UVAL_SUC
	END IF
	END


***********************************
	SUBROUTINE AST_DISPLAY_HOT
***********************************
C	
  	IMPLICIT	INTEGER*4(A - Z)
C
	EXTERNAL UFO_OPEN
C 	
	STRUCTURE /DATA/
		CHARACTER*1     FURN      	                          
		CHARACTER*8     DATE                      
		CHARACTER*1     LINE                      
		CHARACTER*1     SHIFT                     
		CHARACTER*8     FROMDATE                  
		CHARACTER*8     TODATE                    
		CHARACTER*10    SEQ                       
		CHARACTER*29    OTHER                     
		CHARACTER*29    TESTER                    
                CHARACTER*5     M0                      
		CHARACTER*5     FE2                       
		CHARACTER*5     FE                     
		CHARACTER*5     MT                        
		CHARACTER*3     Z1                        
		CHARACTER*3     Z2                        
		CHARACTER*3     Z3                        
		CHARACTER*3     Z4                        
		CHARACTER*4     GASFLO                    
		CHARACTER*4     CO                        
		CHARACTER*4     N2                        
		CHARACTER*4     RT                        
		CHARACTER*23    TIME                      
		CHARACTER*1     PERIOD                    
		CHARACTER*3     Z1_SP                     
		CHARACTER*3     Z2_SP                     
		CHARACTER*3     Z3_SP                     
		CHARACTER*4     GASFLO_SP                 
		CHARACTER*4     CO_SP                     
		CHARACTER*4     N2_SP                     
		CHARACTER*3     Z1_OUT                    
		CHARACTER*3     Z2_OUT                    
		CHARACTER*3     Z3_OUT                    
		CHARACTER*4     GASFLO_OUT                
		CHARACTER*4     CO_OUT                    
		CHARACTER*4     N2_OUT                    
		CHARACTER*8	ELAPSED
		CHARACTER*5	M1
	END STRUCTURE
	RECORD /DATA/ HOT
C
	STRUCTURE /MORE_DATA/
		CHARACTER*4     IPURGE   	                         
		CHARACTER*10    GASTYPE                  
		CHARACTER*3     Z1                       
		CHARACTER*3     Z2                       
		CHARACTER*3     Z3                       
		CHARACTER*3     Z4                       
		CHARACTER*3     TEMP                     
		CHARACTER*4     FPURGE                   
		CHARACTER*5     PINT                     
		CHARACTER*3     MASS                     
		CHARACTER*5     FE2                       
		CHARACTER*5     FE                             		
		CHARACTER*4     CO_PCNT
		CHARACTER*4     N2_PCNT
		CHARACTER*4     GASRATE                  
		CHARACTER*3     RZ1                      
		CHARACTER*3     RZ2                      
		CHARACTER*3     RZ3                      
		CHARACTER*3     RZ4                      
		CHARACTER*5     RINT                     
		CHARACTER*5     LONG                     
		CHARACTER*5     FINT                     
		CHARACTER*2     STOP                     
		CHARACTER*3     IZ1                      
		CHARACTER*3     IZ2                      
		CHARACTER*3     IZ3                      
		CHARACTER*4     CO_FLOW
		CHARACTER*4     N2_FLOW
	END STRUCTURE           
	RECORD /MORE_DATA/ RED_PAR
C
	INTEGER*2	DISP_HEADING
C
	INTEGER*4	HELPNUM,TCA,WORKSPACE,CURPOS,FLDTRM,INSOVR
C
	CHARACTER*13 	PHASE4
  	CHARACTER*16	FURNACE1
	CHARACTER*40  	PHASE1,PHASE2,PHASE3
	CHARACTER*31	FORMNAME
	CHARACTER*80	UARVAL
C
        COMMON /RED_HOT/ HOT
	COMMON /RED_PARAMETER/ RED_PAR
	COMMON /RED_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	COMMON /RED_DISPLAY/ DISP_HEADING
C
	PHASE1 = 'Initial purge  '//RED_PAR.IPURGE//'  l/m - preheating'
	PHASE2 = 'Sample at  '//RED_PAR.TEMP//' C   Final purge '
	1	//RED_PAR.FPURGE//' l/m'
	PHASE3 = 'Sample at  '//RED_PAR.RZ4//' C  Reducing gas '
	1	//RED_PAR.GASRATE//' l/m'
	PHASE4 = 'Test complete'
C
C-------------------------------------------------------------------------------
C	There is a problem with updating the fields on the red_newtest form
C	and returning from the post help form at the same time (if this happens
C	the red_newtest diagram gets displayed twice and user gets bumped out
C	of entry mode - somehow a return gets issued to bust out of the get call
C	in the red_newtest subroutine).  If form displayed is rednewtest_help or
C	if fldtrm is 10 (help field terminator) then fields on red_newtest form
C	are not updated.
C-------------------------------------------------------------------------------
C
	STATUS = FDV$RETCX(%DESCR(TCA),%DESCR(WORKSPACE),
	1	FORMNAME,UARVAL,CURPOS,FLDTRM,
	1	INSOVR,HELPNUM)
	IF (FLDTRM .EQ. 10) GOTO 999
C
	CALL FDV$PUT (RED_PAR.GASTYPE,'GASTYPE')
C
	CALL FDV$PUT (HOT.FURN,'FURN')
	CALL FDV$PUT (HOT.SEQ,'SEQ')	
C
	IF (DISP_HEADING .EQ. 1) THEN
		CALL FDV$PUT (HOT.DATE,'DATE')
		CALL FDV$PUT (HOT.LINE,'LINE')
		CALL FDV$PUT (HOT.SHIFT,'SHIFT')
		CALL FDV$PUT (HOT.FROMDATE,'FROMDATE')
		CALL FDV$PUT (HOT.TODATE,'TODATE')
		CALL FDV$PUT (HOT.OTHER,'OTHER')
!       	CALL FDV$PUT (HOT.OTHER(30:58),'OTHER',2)
		CALL FDV$PUT (HOT.TESTER,'TESTER')
       		CALL FDV$PUT (HOT.M0,'MASS')
		CALL FDV$PUT (HOT.FE2,'FE2')
       		CALL FDV$PUT (HOT.FE,'FE')
        END IF
C
	CALL FDV$PUT (HOT.MT,'MT')
	CALL FDV$PUT (HOT.Z1,'Z1')
	CALL FDV$PUT (HOT.Z2,'Z2')
	CALL FDV$PUT (HOT.Z3,'Z3')
	CALL FDV$PUT (HOT.Z4,'Z4')
	CALL FDV$PUT (HOT.GASFLO,'GASFLO')
	CALL FDV$PUT (HOT.CO,'CO')
	CALL FDV$PUT (HOT.N2,'N2')
	CALL FDV$PUT (HOT.RT,'RT')
	CALL FDV$PUT (HOT.RT,'RT_BIG')
C
	CALL FDV$PUT (HOT.Z1_SP,'Z1_SP')
	CALL FDV$PUT (HOT.Z2_SP,'Z2_SP')
	CALL FDV$PUT (HOT.Z3_SP,'Z3_SP')
	CALL FDV$PUT (HOT.GASFLO_SP,'GASFLO_SP')
	CALL FDV$PUT (HOT.CO_SP,'CO_SP')
	CALL FDV$PUT (HOT.N2_SP,'N2_SP')
C
	CALL FDV$PUT (HOT.Z1_OUT,'Z1_OUT')
	CALL FDV$PUT (HOT.Z2_OUT,'Z2_OUT')
	CALL FDV$PUT (HOT.Z3_OUT,'Z3_OUT')
	CALL FDV$PUT (HOT.GASFLO_OUT,'GASFLO_OUT')
	CALL FDV$PUT (HOT.CO_OUT,'CO_OUT')
	CALL FDV$PUT (HOT.N2_OUT,'N2_OUT')
C       
	CALL FDV$PUT (PHASE1,'PHASE1')
	CALL FDV$PUT (PHASE2,'PHASE2')
	CALL FDV$PUT (PHASE3,'PHASE3')
	CALL FDV$PUT (PHASE4,'PHASE4')
C
	IF (START .EQ. 1) THEN
		IF (HOT.PERIOD .EQ. '1') THEN                                
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE2')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE3')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE4')                       
	                BOLDON = 1                                           
	                CALL FDV$AFVA(BOLDON,'PHASE1')                       
	        ELSE IF (HOT.PERIOD .EQ. '2') THEN                           
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE1')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE3')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE4')                       
	                BOLDON = 1                                           
	                CALL FDV$AFVA(BOLDON,'PHASE2')                       
	        ELSE IF ((HOT.PERIOD.EQ.'3').OR.(HOT.PERIOD.EQ.'4'))THEN  
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE1')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE2')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE4')                       
	                BOLDON = 1                                           
	                CALL FDV$AFVA(BOLDON,'PHASE3')                       
	        END IF                                                       
	ELSE IF ((HOT.PERIOD .EQ. '5').AND.(BEEP .EQ. 1)) THEN
	        BOLDON = 0
	        CALL FDV$AFVA(BOLDON,'PHASE1')                      
	        BOLDON = 0                                          
	        CALL FDV$AFVA(BOLDON,'PHASE2')                      
	        BOLDON = 0                                          
	        CALL FDV$AFVA(BOLDON,'PHASE3')                      
	        BOLDON = 1                                          
	        CALL FDV$AFVA(BOLDON,'PHASE4')                      
C
C-------------------------------------------------------------------------------
C	Start beeping until a return is pressed.
C-------------------------------------------------------------------------------
C
!		I = 1
!		DO WHILE (I .LT. 9)
!			CALL FDV$SIGOP
!			CALL LIB$WAIT(1.0)
!			I = I + 1
!		END DO
	ELSE                                                         
	        BOLDON = 0                                           
	        CALL FDV$AFVA(BOLDON,'PHASE1')                       
	        BOLDON = 0                                           
	        CALL FDV$AFVA(BOLDON,'PHASE2')                       
	        BOLDON = 0                                           
	        CALL FDV$AFVA(BOLDON,'PHASE3')                       
	        BOLDON = 0                                           
	        CALL FDV$AFVA(BOLDON,'PHASE4')                       
	END IF
C
	CALL FURN_STAT
C
C	Take this out eventually!!!!!!!!!!!!!
!	CALL FDV$PUTL ('THIS IS A TEST!')
C
999     CALL AST_RESET
C
	RETURN	
	END

******************************
	SUBROUTINE AST_RESET
******************************
C
  	IMPLICIT	INTEGER*4(A - Z)
	CHARACTER*9	DELAY/'0 ::10.00'/
	DIMENSION	BIN_DELAY(2)
	DATA		TIMER_ID/3/
C
        EXTERNAL	AST_DISPLAY_HOT
C	
  	STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
C
	CALL SYS$BINTIM(DELAY, BIN_DELAY)
	CALL SYS$SETIMR( ,BIN_DELAY,AST_DISPLAY_HOT, 
	1	%VAL(TIMER_ID),)
C
	RETURN
	END

****************************************
	SUBROUTINE AST_DISPLAY_HOT_LTD
****************************************
	STRUCTURE /LTD_DATA/
				CHARACTER*1	FURN
				CHARACTER*8	DATE
				CHARACTER*1	LINE
				CHARACTER*1	SHIFT
				CHARACTER*8	FROMDATE
				CHARACTER*8	TODATE
				CHARACTER*10	SEQ
				CHARACTER*29	OTHER
				CHARACTER*29	TESTER
                                CHARACTER*5	M0
				CHARACTER*5	M1
				CHARACTER*5	M2
				CHARACTER*5	M3
				CHARACTER*3	Z1
				CHARACTER*3	Z2
				CHARACTER*3	Z3
				CHARACTER*3	Z4
				CHARACTER*4	GASFLO
				CHARACTER*4	CO
				CHARACTER*4	N2
				CHARACTER*5	H2
				CHARACTER*4	CO2
				CHARACTER*23	TIME				
				CHARACTER*1	PERIOD
				CHARACTER*3	Z1_SP
				CHARACTER*3	Z2_SP
				CHARACTER*3	Z3_SP
				CHARACTER*4	GASFLO_SP
				CHARACTER*4	CO_SP
				CHARACTER*4	N2_SP
				CHARACTER*5	H2_SP
				CHARACTER*4	CO2_SP
				CHARACTER*3	Z1_OUT
				CHARACTER*3	Z2_OUT
				CHARACTER*3	Z3_OUT
				CHARACTER*3	GASFLO_OUT
				CHARACTER*3	CO_OUT
				CHARACTER*3	N2_OUT
				CHARACTER*3	H2_OUT
				CHARACTER*3	CO2_OUT
				CHARACTER*8	ELAPSED
	END STRUCTURE
	RECORD /LTD_DATA/ HOT
C
	STRUCTURE /MORE_LTD_DATA/
				CHARACTER*4	IPURGE
       				CHARACTER*10	GASTYPE
       				CHARACTER*3	Z1
       				CHARACTER*3	Z2
       				CHARACTER*3	Z3
       				CHARACTER*3	Z4
       				CHARACTER*3	PTIME
				CHARACTER*5	PINT
				CHARACTER*4	CO_PCNT
				CHARACTER*4	N2_PCNT
	  			CHARACTER*4	H2_PCNT				
       	       			CHARACTER*4	CO2_PCNT
	       			CHARACTER*4	GASRATE
	       			CHARACTER*3	RZ1
	       			CHARACTER*3	RZ2
	       			CHARACTER*3	RZ3
	       			CHARACTER*3	RZ4
	       			CHARACTER*3	RTIME
	       			CHARACTER*5	RINT
	       			CHARACTER*4	FPURGE
	       			CHARACTER*3	STOP_TEMP
	       			CHARACTER*5	FINT
	       			CHARACTER*3	IZ1
	       			CHARACTER*3	IZ2
	       			CHARACTER*3	IZ3
       	       			CHARACTER*4	CO_FLOW
	       			CHARACTER*4	N2_FLOW
       	       			CHARACTER*5	H2_FLOW
	       			CHARACTER*4	CO2_FLOW
	END STRUCTURE
	RECORD /MORE_LTD_DATA/ LTD_PAR
C
	INTEGER*2	DISP_HEADING
	INTEGER*4	START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	INTEGER*4	HELPNUM,TCA,WORKSPACE,CURPOS,FLDTRM,INSOVR
	INTEGER*4 	BOLDON
C
	CHARACTER*31	FORMNAME
	CHARACTER*40	PHASE1,PHASE2,PHASE3,PHASE4
	CHARACTER*80	UARVAL
C
	COMMON /LTD_HOT/ HOT
	COMMON /LTD_PARAMETER/ LTD_PAR
	COMMON /LTD_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	COMMON /LTD_DISPLAY/ DISP_HEADING
C
	PHASE1 = 'Initial purge  '//LTD_PAR.IPURGE//'  l/m - preheating'
	PHASE2 = 'Sample at  '//LTD_PAR.RZ4//' C  Reducing gas '
	1	//LTD_PAR.GASRATE//' l/m'
	PHASE3 = 'Test complete'
	PHASE4 = ' '
C
C-------------------------------------------------------------------------------
C	There is a problem with updating the fields on the ltb_newtest form
C	and returning from the post help form at the same time (if this happens
C	the ltb_newtest diagram gets displayed twice and then the menu form is 
C	displayed - somehow a return gets issued to bust out of the get call
C	in the ltb_newtest subroutine).  If form displayed is ltbnewtest_help or
C	if fldtrm is 10 (help field terminator) then fields on ltb_newtest form
C	are not updated.
C-------------------------------------------------------------------------------
C
	CALL FDV$RETCX(%DESCR(TCA),%DESCR(WORKSPACE),
	1	FORMNAME,UARVAL,CURPOS,FLDTRM,
	1	INSOVR,HELPNUM)
	IF (FLDTRM .EQ. 10) GOTO 999
C
	CALL FDV$PUT (LTD_PAR.GASTYPE,'GASTYPE')
C
	CALL FDV$PUT (HOT.FURN,'FURN')
	CALL FDV$PUT (HOT.SEQ,'SEQ')	
C
	IF (DISP_HEADING .EQ. 1) THEN
		CALL FDV$PUT (HOT.DATE,'DATE')
		CALL FDV$PUT (HOT.LINE,'LINE')
		CALL FDV$PUT (HOT.SHIFT,'SHIFT')
		CALL FDV$PUT (HOT.FROMDATE,'FROMDATE')
		CALL FDV$PUT (HOT.TODATE,'TODATE')
		CALL FDV$PUT (HOT.OTHER,'OTHER')
!       	CALL FDV$PUT (HOT.OTHER(30:58),'OTHER',2)
		CALL FDV$PUT (HOT.TESTER,'TESTER')
	END IF
C
	CALL FDV$PUT (HOT.Z1,'Z1')
	CALL FDV$PUT (HOT.Z2,'Z2')
	CALL FDV$PUT (HOT.Z3,'Z3')
	CALL FDV$PUT (HOT.Z4,'Z4')
	CALL FDV$PUT (HOT.GASFLO,'GASFLO')
	CALL FDV$PUT (HOT.CO,'CO')
	CALL FDV$PUT (HOT.N2,'N2')
	CALL FDV$PUT (HOT.H2,'H2')
	CALL FDV$PUT (HOT.CO2,'CO2')
C
	CALL FDV$PUT (HOT.Z1_SP,'Z1_SP')
	CALL FDV$PUT (HOT.Z2_SP,'Z2_SP')
	CALL FDV$PUT (HOT.Z3_SP,'Z3_SP')
	CALL FDV$PUT (HOT.GASFLO_SP,'GASFLO_SP')
	CALL FDV$PUT (HOT.CO_SP,'CO_SP')
	CALL FDV$PUT (HOT.N2_SP,'N2_SP')
	CALL FDV$PUT (HOT.H2_SP,'H2_SP')
	CALL FDV$PUT (HOT.CO2_SP,'CO2_SP')
C
	CALL FDV$PUT (HOT.Z1_OUT,'Z1_OUT')
	CALL FDV$PUT (HOT.Z2_OUT,'Z2_OUT')
	CALL FDV$PUT (HOT.Z3_OUT,'Z3_OUT')
	CALL FDV$PUT (HOT.GASFLO_OUT,'GASFLO_OUT')
	CALL FDV$PUT (HOT.CO_OUT,'CO_OUT')
	CALL FDV$PUT (HOT.N2_OUT,'N2_OUT')
	CALL FDV$PUT (HOT.H2_OUT,'H2_OUT')
	CALL FDV$PUT (HOT.CO2_OUT,'CO2_OUT')
C       
	CALL FDV$PUT (PHASE1,'PHASE1')
	CALL FDV$PUT (PHASE2,'PHASE2')
	CALL FDV$PUT (PHASE3,'PHASE3')
C
	IF (START .EQ. 1) THEN
		IF (HOT.PERIOD .EQ. '1') THEN                                
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE2')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE3')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE4')                       
	                BOLDON = 1                                           
	                CALL FDV$AFVA(BOLDON,'PHASE1')                       
	        ELSE IF (HOT.PERIOD .EQ. '2') THEN                           
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE1')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE3')                       
	                BOLDON = 0                                           
	                CALL FDV$AFVA(BOLDON,'PHASE4')                       
	                BOLDON = 1                                           
	                CALL FDV$AFVA(BOLDON,'PHASE2')                       
	        END IF                                                       
	ELSE IF ((HOT.PERIOD .EQ. '3').AND.(BEEP .EQ. 1)) THEN
	        BOLDON = 0
	        CALL FDV$AFVA(BOLDON,'PHASE1')                      
	        BOLDON = 0                                          
	        CALL FDV$AFVA(BOLDON,'PHASE2')                      
	        BOLDON = 0                                          
	        CALL FDV$AFVA(BOLDON,'PHASE4')                      
	        BOLDON = 1                                          
	        CALL FDV$AFVA(BOLDON,'PHASE3')                      
C
C-------------------------------------------------------------------------------
C	Start beeping until a return is pressed.
C-------------------------------------------------------------------------------
C
!		I = 1
!		DO WHILE (I .LT. 9)
!			CALL FDV$SIGOP
!			CALL LIB$WAIT(1.0)
!			I = I + 1
!		END DO
	ELSE                                                         
	        BOLDON = 0                                           
	        CALL FDV$AFVA(BOLDON,'PHASE1')                       
	        BOLDON = 0                                           
	        CALL FDV$AFVA(BOLDON,'PHASE2')                       
	        BOLDON = 0                                           
	        CALL FDV$AFVA(BOLDON,'PHASE3')                       
	        BOLDON = 0                                           
	        CALL FDV$AFVA(BOLDON,'PHASE4')                       
	END IF
C
	CALL FURN_STAT
C
999     CALL AST_RESET_LTD
C
	RETURN	
	END

**********************************
	SUBROUTINE AST_RESET_LTD
**********************************
C
  	IMPLICIT	INTEGER*4(A - Z)
	CHARACTER*9	DELAY/'0 ::10.00'/
	DIMENSION	BIN_DELAY(2)
	DATA		TIMER_ID/6/
C
        EXTERNAL	AST_DISPLAY_HOT_LTD
C	
  	STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
C
	CALL SYS$BINTIM(DELAY, BIN_DELAY)
	CALL SYS$SETIMR( ,BIN_DELAY,AST_DISPLAY_HOT_LTD, 
	1	%VAL(TIMER_ID),)
C
	RETURN
	END
	
***********************************
	SUBROUTINE AST_FURN_RUN
***********************************
C	
  	IMPLICIT	INTEGER*4(A - Z)
C
	CHARACTER*16	FURNACE1
C
        COMMON /RED_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
C
	CALL FURN_STAT
C
C	Take this out eventually!!!!!!!!!!!!!
!	CALL FDV$PUTL ('THIS IS A TEST FOR FURN RUN!')
C
        CALL AST_RESET_FURN
C
	RETURN	
	END



**********************************
	SUBROUTINE AST_RESET_FURN
**********************************
C
  	IMPLICIT	INTEGER*4(A - Z)
	CHARACTER*9	DELAY/'0 ::10.00'/
	DIMENSION	BIN_DELAY(2)
	DATA		TIMER_ID/1/
C
        EXTERNAL	AST_FURN_RUN
C	
  	STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
C
	CALL SYS$BINTIM(DELAY, BIN_DELAY)
	CALL SYS$SETIMR( ,BIN_DELAY,AST_FURN_RUN, 
	1	%VAL(TIMER_ID),)
C
	RETURN
	END





******************************
	SUBROUTINE MENU
******************************
!	1  => Exit
!	2  => RED_PARAMETER 	Reducibility parameter table
!	3  => RED_NEWTEST	Reducibility new test furnace #1
!	4  => RED_STATUS    	Reducibility test status furnace #1
!	5  => RED IDLE
!	6  => RED_REPORT 	Iso reducibility report
!	7  => LTD_PARAMETER	LTD parameter table
!	8  => LTD_NEWTEST	LTD new test furnace #1
!	9  => LTD_STATUS	LTD test status furnace #1
!	10 => LTD IDLE
!	11 => LTD_REPORT	Iso LTD report
C
	IMPLICIT	INTEGER*4(A - Z)
C
	INCLUDE 	'($PRCDEF)'
	INCLUDE         '($SSDEF)'
	INCLUDE		'REDFURN:KEYDEF'
C
	INTEGER*4	RED_START,RED_STOP,RED_FIRST_PASS,
	1		RED_DETACH,RED_BEEP,RED_TOGGLE               
	INTEGER*4    	LTD_START,LTD_STOP,LTD_FIRST_PASS,
	1		LTD_DETACH,LTD_BEEP,LTD_TOGGLE                
	INTEGER*4	OPTION_INT,FIELDT
	DIMENSION	BIN_DELAY(2)
C
	CHARACTER*1	DREPLY
	CHARACTER*3	OPTION_CHAR
	CHARACTER*9	DELAY/'0 ::10.00'/
	CHARACTER*10	OLDPASSWORD/'GARY'/,PASSWORD
	CHARACTER*11	PRCNAM_RED/'COLLECT_RED'/
	CHARACTER*11	PRCNAM_LTD/'COLLECT_LTD'/
	CHARACTER*16	FURNACE1
	CHARACTER*16	FIND_DISPLAY_RED/'REDD:COLLECT_RED'/
	CHARACTER*20	ERROR_RED	/'REDD:COLLECT_RED.ERR'/
	CHARACTER*16	FIND_DISPLAY_LTD/'REDD:COLLECT_LTD'/
	CHARACTER*20	ERROR_LTD	/'REDD:COLLECT_LTD.ERR'/
	CHARACTER*44	DATAOK
	CHARACTER*80	LINE
C
	DATA 		TIMER_ID/1/
	EXTERNAL	AST_FURN_RUN
C
	COMMON /RED_COM/ RED_START,RED_STOP,RED_FIRST_PASS,
	1		RED_DETACH,RED_BEEP,RED_TOGGLE               
	COMMON /LTD_COM/ LTD_START,LTD_STOP,LTD_FIRST_PASS,
	1		LTD_DETACH,LTD_BEEP,LTD_TOGGLE                
C
	DREPLY = ' '
	DATAOK = '                                            '
C
	DO WHILE (OPTION_INT .NE. 1)
       		CALL FDV$CDISP ('MENU')
        	CALL FDV$SPADA( 0 )   
		CALL FDV$DFKBD (%DESCR(KEY),8)
		CALL FDV$SPON
C
		CALL SYS$BINTIM (DELAY,BIN_DELAY)
		CALL SYS$SETIMR ( ,BIN_DELAY,AST_FURN_RUN,
	1      			%VAL(TIMER_ID),)
C
		CALL FURN_STAT
C
10		CALL FDV$PUT ( '  ','OPTION' )
C
		CALL FDV$GET (OPTION_CHAR,FIELDT,'OPTION')
C
		IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
			CALL STOP_CHECK (FIELDT)
			GOTO 10
		END IF
C
                READ  (OPTION_CHAR,'(BN,I2)') OPTION_INT
		GOTO  (200,20,30,40,50,60,70,80,90,100,110) OPTION_INT
                GOTO 10
C		
20              CALL FDV$SPOFF
		CALL FDV$PUT ( 'Password:','PASS_MES' )
		CALL FDV$GET ( PASSWORD,FIELDT,'PASSWORD' )
		IF (PASSWORD .NE. OLDPASSWORD) THEN
			CALL FDV$SIGOP
			CALL FDV$PUT ( '         ','PASS_MES' )
			CALL FDV$PUT ( '          ','PASSWORD')
			CALL FDV$PUTL ()
			CALL FDV$PUTL ( 'Invalid password!' )
			CALL FDV$SPON
			CALL FDV$PUT ( '  ','OPTION' )
			GOTO 10
		END IF
  		STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
  		CALL RED_PARAMETER 
		GOTO 200
C
30		IF (RED_START .EQ. 1) THEN
			CALL FDV$SIGOP
			LINE = ' '
			LINE(1:47) = 'FURNACE #1 is already running '// 
	1		'a reduction test!'
			CALL FDV$PUTL(LINE(1:47))
			CALL LIB$WAIT(3.0)
			CALL FDV$PUTL()
			GOTO 10
		ELSE IF (LTD_START .EQ. 1) THEN
			CALL FDV$SIGOP
			LINE = ' '
			LINE(1:34)='FURNACE #1 is running an LTD test!'
			CALL FDV$PUTL(LINE(1:34))
			CALL LIB$WAIT(3.0)
			CALL FDV$PUTL()
			GOTO 10
	        ELSE
			CALL SYS$DELPRC( ,PRCNAM_LTD)
			LTD_STOP = 1
			LTD_START = 0
			LTD_DETACH = 0
			IF (RED_DETACH .NE. 1) THEN
				STATUS=SYS$CREPRC(,FIND_DISPLAY_RED,,,
	1			ERROR_RED ,,,PRCNAM_RED,%VAL(4),,,
	1			%VAL(PRC$M_DETACH))	
	      		END IF
  			STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
			CALL LIB$WAIT(1.0)
   			CALL RED_NEWTEST 
		END IF
		GOTO 200
C 
40		STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
  		CALL RED_STATUS
		GOTO 200
C
50		IF (RED_START .EQ. 1) THEN
			CALL FDV$SIGOP
			LINE = ' '
			LINE(1:39) = 'FURNACE #1 is running '//
	1		'a reduction test!'
			CALL FDV$PUTL(LINE(1:39))
			CALL LIB$WAIT(3.0)
			CALL FDV$PUTL()
			GOTO 10
		ELSE IF (LTD_START .EQ. 1) THEN
			CALL FDV$SIGOP
			LINE = ' '
			LINE(1:42) = 'FURNACE #1 is already running '// 
	1		'an LTD test!'
			CALL FDV$PUTL(LINE(1:42))
			CALL LIB$WAIT(3.0)
			CALL FDV$PUTL()
			GOTO 10
	        ELSE
			CALL SYS$DELPRC( ,PRCNAM_LTD)
			LTD_STOP = 1
			LTD_START = 0
			LTD_DETACH = 0
			IF (RED_DETACH .NE. 1) THEN
				STATUS=SYS$CREPRC(,FIND_DISPLAY_RED,,,
	1			ERROR_RED ,,,PRCNAM_RED,%VAL(4),,,
	1			%VAL(PRC$M_DETACH))	
	      		END IF
 			RED_STOP = 0
			RED_START = 0			
			RED_TOGGLE = 0
		END IF
		GOTO 10
C		
60		STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
  		CALL RED_REPORT
		GOTO 200
C
70              CALL FDV$SPOFF
		CALL FDV$PUT ( 'Password:','PASS_MES' )
		CALL FDV$GET ( PASSWORD,FIELDT,'PASSWORD' )
		IF (PASSWORD .NE. OLDPASSWORD) THEN
			CALL FDV$SIGOP
			CALL FDV$PUT ( '         ','PASS_MES' )
			CALL FDV$PUT ( '          ','PASSWORD')
			CALL FDV$PUTL ()
			CALL FDV$PUTL ( 'Invalid password!' )
			CALL FDV$SPON
			CALL FDV$PUT ( '  ','OPTION' )
			GOTO 10
		END IF
  		STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
  		CALL LTD_PARAMETER
		GOTO 200
C
80		IF (RED_START .EQ. 1) THEN
			CALL FDV$SIGOP
			LINE = ' '
			LINE(1:39) = 'FURNACE #1 is running '//
	1		'a reduction test!'
			CALL FDV$PUTL(LINE(1:39))
			CALL LIB$WAIT(3.0)
			CALL FDV$PUTL()
			GOTO 10
		ELSE IF (LTD_START .EQ. 1) THEN
			CALL FDV$SIGOP
			LINE = ' '
			LINE(1:42) = 'FURNACE #1 is already running '// 
	1		'an LTD test!'
			CALL FDV$PUTL(LINE(1:42))
			CALL LIB$WAIT(3.0)
			CALL FDV$PUTL()
			GOTO 10
	        ELSE
			CALL SYS$DELPRC( ,PRCNAM_RED)
			RED_STOP = 1
			RED_START = 0
			RED_DETACH = 0
			IF (LTD_DETACH .NE. 1) THEN
				STATUS=SYS$CREPRC(,FIND_DISPLAY_LTD,,,
	1			ERROR_LTD ,,,PRCNAM_LTD,%VAL(4),,,
	1			%VAL(PRC$M_DETACH))	
	      		END IF
  			STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
			CALL LIB$WAIT(1.0)
   			CALL LTD_NEWTEST 
		END IF
		GOTO 200
C
90		STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
  		CALL LTD_STATUS
		GOTO 200
C
100		IF (RED_START .EQ. 1) THEN
			CALL FDV$SIGOP
			LINE = ' '
			LINE(1:39) = 'FURNACE #1 is running '//
	1		'a reduction test!'
			CALL FDV$PUTL(LINE(1:39))
			CALL LIB$WAIT(3.0)
			CALL FDV$PUTL()
			GOTO 10
		ELSE IF (LTD_START .EQ. 1) THEN
			CALL FDV$SIGOP
			LINE = ' '
			LINE(1:42) = 'FURNACE #1 is already running '// 
	1		'an LTD test!'
			CALL FDV$PUTL(LINE(1:42))
			CALL LIB$WAIT(3.0)
			CALL FDV$PUTL()
			GOTO 10
	        ELSE
			CALL SYS$DELPRC( ,PRCNAM_RED)
			RED_STOP = 1
			RED_START = 0
			RED_DETACH = 0
			IF (LTD_DETACH .NE. 1) THEN
				STATUS=SYS$CREPRC(,FIND_DISPLAY_LTD,,,
	1			ERROR_LTD ,,,PRCNAM_LTD,%VAL(4),,,
	1			%VAL(PRC$M_DETACH))	
	      		END IF
			LTD_STOP = 0
			LTD_START = 0
			LTD_TOGGLE = 0
		END IF
		GOTO 10
C
110		STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
   		CALL LTD_REPORT 
C
200	END DO
C
	CALL FDV$SPOFF
	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'DATAOK')
300	CALL FDV$PUT ('Stop detached process for FURNACE #1 <Y/N>? ',
	1		'DATAOK')
	UNDERLINE = 8
	CALL FDV$AFVA (UNDERLINE,'DREPLY')
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 300
	END IF
	IF ((DREPLY .EQ. 'Y').OR.(DREPLY .EQ. 'y')) THEN
 		RED_DETACH = 0
 		LTD_DETACH = 0
	END IF
C
	STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
	CALL LIB$WAIT (11.0)
  	RETURN
	END

		
**********************************
	SUBROUTINE RED_PARAMETER
**********************************
C
        IMPLICIT	INTEGER*4(A - Z)
	STRUCTURE /DATA/
		UNION
			MAP
				CHARACTER*4	IPURGE
				CHARACTER*10	GASTYPE
				CHARACTER*3	Z1
				CHARACTER*3	Z2
				CHARACTER*3	Z3
				CHARACTER*3	Z4
				CHARACTER*3	TEMP
				CHARACTER*4	FPURGE
				CHARACTER*5	PINT
				CHARACTER*3	MASS
				CHARACTER*5	FE2
				CHARACTER*5	FE				
				CHARACTER*4     CO_PCNT
				CHARACTER*4     N2_PCNT
				CHARACTER*4	GASRATE
				CHARACTER*3	RZ1
				CHARACTER*3	RZ2
				CHARACTER*3	RZ3
				CHARACTER*3	RZ4
				CHARACTER*5	RINT
				CHARACTER*5	LONG
				CHARACTER*5	FINT
				CHARACTER*2	STOP
				CHARACTER*3     IZ1                      
				CHARACTER*3     IZ2                      
				CHARACTER*3     IZ3                      
		     		CHARACTER*4     CO_FLOW
	       			CHARACTER*4     N2_FLOW
			END MAP
			MAP
				CHARACTER*109	STRING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /DATA/ RED_PAR
C
	INCLUDE		'REDFURN:KEYDEF'
	INTEGER*2	BOLDON,ATTROF,UNDERLINE,NONE
	INTEGER*4	TRUSTED/64/
	REAL*4		CO,N2,GASRATE,COFLOW,N2FLOW
	CHARACTER*1	DREPLY
	CHARACTER*2	INTDIG2
	CHARACTER*3	INTDIG3,DIGIT3
	CHARACTER*4	DIGIT4,C_COFLOW,C_N2FLOW
	CHARACTER*5	DIGIT5	
	CHARACTER*9	DELAY/'0 ::10.00'/
	CHARACTER*16	FURNACE1
	CHARACTER*17	DATAOK
	CHARACTER*23	ASCII_TIME,FIRST_TIME     	!Used for testing
!	CHARACTER*200	STRING                          !detached process
	DATA 		TIMER_ID/2/
	EXTERNAL	AST_FURN_RUN
	DIMENSION	BIN_DELAY(2)
C
	COMMON /RED_PARAMETER/ RED_PAR
	COMMON /RED_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
C
	DREPLY = ' '
	DATAOK = '                 '
C
	CALL SYS$BINTIM (DELAY,BIN_DELAY)
	CALL SYS$SETIMR ( ,BIN_DELAY,AST_FURN_RUN,
	1      			%VAL(TIMER_ID),)
C
10	OPEN (UNIT=1,FILE='REDFURN:RED_PAR.DAT',
	1	STATUS='OLD',
	1	ORGANIZATION='RELATIVE',
	1	FORM='UNFORMATTED',
	1	RECORDTYPE='FIXED',
	1	ACCESS='DIRECT',SHARED,
	1	ERR=20)
C
	GOTO 30
C
20	CALL LIB$SPAWN ('CREATE/FDL=RED_PAR REDFURN:RED_PAR.DAT',,,
	1	TRUSTED)
	GOTO 10
C
30	READ  (UNIT=1,REC=1,IOSTAT=IOS,ERR=35)RED_PAR.STRING
        UNLOCK(UNIT=1)
35      IF (IOS .EQ. 52) THEN
	        UNLOCK(UNIT=1)
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('Record locked!')
		CALL LIB$WAIT(3.0)
		GOTO 30
	END IF
C
!!!!!!!!!!!!	May want to take out	!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	WRITE (UNIT=1,REC=1)RED_PAR.STRING(1:109)
C	
        CALL FDV$SPON
        CALL FDV$CDISP ('REDPAR')
C
	CALL FDV$PUTAL (RED_PAR.STRING(1:109))
C
	CALL FURN_STAT
C
	CALL FDV$SPADA(0)
40	CALL FDV$DFKBD (%DESCR(KEYTABLE),14)
C
50      CALL FDV$GETAL(RED_PAR.STRING,FIELDT,'IPURGE')
C	
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 50
	END IF
C	
        RED_PAR.IPURGE 	= DIGIT4(RED_PAR.IPURGE)
	RED_PAR.Z1    	= INTDIG3(RED_PAR.Z1)
	RED_PAR.Z2	= INTDIG3(RED_PAR.Z2)
	RED_PAR.Z3	= INTDIG3(RED_PAR.Z3)
	RED_PAR.Z4	= INTDIG3(RED_PAR.Z4) 
	RED_PAR.TEMP    = INTDIG3(RED_PAR.TEMP)
	RED_PAR.FPURGE	= DIGIT4(RED_PAR.FPURGE)
	RED_PAR.PINT(3:3)= ':'
	RED_PAR.MASS	= DIGIT3(RED_PAR.MASS)
	RED_PAR.FE2	= DIGIT5(RED_PAR.FE2)
	RED_PAR.FE	= DIGIT5(RED_PAR.FE)
C
        RED_PAR.CO_PCNT = DIGIT4(RED_PAR.CO_PCNT)
        RED_PAR.N2_PCNT = DIGIT4(RED_PAR.N2_PCNT)
        RED_PAR.GASRATE = DIGIT4(RED_PAR.GASRATE)
        RED_PAR.RZ1     = INTDIG3(RED_PAR.RZ1) 
        RED_PAR.RZ2    	= INTDIG3(RED_PAR.RZ2) 
        RED_PAR.RZ3     = INTDIG3(RED_PAR.RZ3) 
        RED_PAR.RZ4     = INTDIG3(RED_PAR.RZ4) 
        RED_PAR.RINT(3:3)= ':'
        RED_PAR.LONG(3:3)= ':'
        RED_PAR.FINT(3:3)= ':'
        RED_PAR.STOP    = INTDIG2(RED_PAR.STOP)
        RED_PAR.IZ1     = INTDIG3(RED_PAR.IZ1) 
        RED_PAR.IZ2    	= INTDIG3(RED_PAR.IZ2) 
        RED_PAR.IZ3     = INTDIG3(RED_PAR.IZ3) 
C
	CALL FDV$PUTAL (RED_PAR.STRING(1:109))
C
	CALL FURN_STAT
C
C-------------------------------------------------------------------------------
C	Calculate CO & N2 flows based on percentages entered by operator
C-------------------------------------------------------------------------------
C
	READ (RED_PAR.CO_PCNT,'(F4.1,BN)') CO
	READ (RED_PAR.N2_PCNT,'(F4.1,BN)') N2
C
	IF (CO + N2 .NE. 100.0) THEN
		CALL FDV$SIGOP
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('CO % and N2 % must add up to 100.0')
		GOTO 40
	ELSE 
		READ (RED_PAR.GASRATE,'(F4.1,BN)') GASRATE
		COFLOW = GASRATE * CO / 100.0
		N2FLOW = GASRATE * N2 / 100.0
		WRITE (C_COFLOW,'(F4.1,BN)') COFLOW
		WRITE (C_N2FLOW,'(F4.1,BN)') N2FLOW
		CALL FDV$PUT (C_COFLOW,'COFLOW')
		CALL FDV$PUT (C_N2FLOW,'N2FLOW')
		RED_PAR.CO_FLOW = C_COFLOW
		RED_PAR.N2_FLOW = C_N2FLOW
	END IF
C
        CALL FDV$SPOFF
C
        BOLDON = 1
       	CALL FDV$AFVA (BOLDON,'DATAOK')
	CALL FDV$PUT ('Is data ok <Y/N>? ','DATAOK')
C
	CALL FDV$DFKBD (%DESCR(KEY),8)
C
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
70	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 70
	END IF
C	
	IF (DREPLY .EQ. 'Y') THEN
	ELSE
		NONE = 0
		ATTROF = -1
        	CALL FDV$AFVA (NONE,'DATAOK')
        	CALL FDV$AFVA (ATTROF,'DREPLY')
		CALL FDV$PUT ('                 ','DATAOK')
		CALL FDV$PUT (' ','DREPLY')
		CALL FDV$SPON		
		GOTO 40
	END IF
C
	DELETE (UNIT=1,REC=1)
	WRITE (UNIT=1,REC=1)RED_PAR.STRING(1:109)
	CLOSE (UNIT=1)
C
	STATUS = SYS$CANTIM (%VAL(TIMER_ID),)
	END

*******************************************
	SUBROUTINE RED_NEWTEST
*******************************************
C
	IMPLICIT	INTEGER*4(A - Z)
	INCLUDE '($PRCDEF)'
C
	EXTERNAL UFO_OPEN
C
	STRUCTURE /DATA/
		UNION
			MAP
				CHARACTER*1	FURN
				CHARACTER*8	DATE
				CHARACTER*1	LINE
				CHARACTER*1	SHIFT
				CHARACTER*8	FROMDATE
				CHARACTER*8	TODATE
				CHARACTER*10	SEQ
				CHARACTER*29	OTHER
				CHARACTER*29	TESTER
                                CHARACTER*5	M0
				CHARACTER*5	FE2
				CHARACTER*5	FE
				CHARACTER*5	MT
				CHARACTER*3	Z1
				CHARACTER*3	Z2
				CHARACTER*3	Z3
				CHARACTER*3	Z4
				CHARACTER*4	GASFLO
				CHARACTER*4	CO
				CHARACTER*4	N2
				CHARACTER*4	RT
				CHARACTER*23	TIME
				CHARACTER*1	PERIOD
				CHARACTER*3	Z1_SP
				CHARACTER*3	Z2_SP
				CHARACTER*3	Z3_SP
				CHARACTER*4	GASFLO_SP
				CHARACTER*4	CO_SP
				CHARACTER*4	N2_SP
				CHARACTER*3	Z1_OUT
				CHARACTER*3	Z2_OUT
				CHARACTER*3	Z3_OUT
				CHARACTER*4	GASFLO_OUT
				CHARACTER*4	CO_OUT
				CHARACTER*4	N2_OUT
				CHARACTER*8	ELAPSED
				CHARACTER*5	M1
			END MAP
			MAP
				CHARACTER*115	STRING
			END MAP
			MAP
				CHARACTER*95	HEADING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /DATA/ HOT
C
	STRUCTURE /MORE_DATA/
		UNION
			MAP
				CHARACTER*4     IPURGE
		                CHARACTER*10    GASTYPE
		                CHARACTER*3     Z1
		                CHARACTER*3     Z2
		                CHARACTER*3     Z3
		                CHARACTER*3     Z4
		                CHARACTER*3     TEMP
		                CHARACTER*4     FPURGE
		                CHARACTER*5     PINT
		                CHARACTER*3     MASS
		                CHARACTER*5     FE2
		                CHARACTER*5     FE
		                CHARACTER*4     CO_PCNT
		                CHARACTER*4     N2_PCNT
		                CHARACTER*4     GASRATE
		                CHARACTER*3     RZ1
		                CHARACTER*3     RZ2
		                CHARACTER*3     RZ3
		                CHARACTER*3     RZ4
		                CHARACTER*5     RINT
		                CHARACTER*5     LONG
		                CHARACTER*5     FINT
		                CHARACTER*2     STOP
		                CHARACTER*3     IZ1
		                CHARACTER*3     IZ2
		                CHARACTER*3     IZ3
		                CHARACTER*4     CO_FLOW
		                CHARACTER*4     N2_FLOW
			END MAP
			MAP
				CHARACTER*109	PARAMETER
			END MAP
		END UNION
	END STRUCTURE           
	RECORD /MORE_DATA/ RED_PAR
C
	INCLUDE		'REDFURN:KEYDEF'
C
        INTEGER*2	COUNT1,DISP_HEADING
	INTEGER*2	BOLDON,ATTROF,UNDERLINE,NONE	
	INTEGER*4	CONC_CONTACT(100)
	INTEGER*4	CONC_POINT/1758/,CONC_NEWBIT
	INTEGER*4	TRUSTED/64/
C
	CHARACTER*1	DREPLY
	CHARACTER*2	COUNT
	CHARACTER*5	DIG5,DIGIT5
	CHARACTER*6	DATE_6
	CHARACTER*8	DATE
	CHARACTER*9	DELAY/'0 ::10.00'/
	CHARACTER*10    OLD_SEQ
	CHARACTER*16	FURNACE1
	CHARACTER*17	DATAOK
	EXTERNAL 	AST_DISPLAY_HOT
	DIMENSION	BIN_DELAY(2)
	DATA		TIMER_ID/3/
C
	COMMON /RED_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	COMMON /RED_HOT/ HOT
	COMMON /RED_PARAMETER/ RED_PAR
	COMMON /CONTACTSCNC/ CONC_CONTACT
	COMMON /RED_DISPLAY/ DISP_HEADING	!only used in AST_DISPLAY_HOT
C
	DREPLY = ' '
	DATAOK = '                 '
	HOT.RT = '    '
	HOT.ELAPSED = '        '
C
	COUNT1 = 0
	N = 1
	DISP_HEADING = 0
C
C	Set timer to turn on AST which updates screen every 10 sec
	CALL SYS$BINTIM(DELAY, BIN_DELAY)
	CALL SYS$SETIMR( ,BIN_DELAY, AST_DISPLAY_HOT,
	1	 %VAL(TIMER_ID),)
C
C	Get today's date in the form YYMMDD
	CALL DATE_YYMMDD(DATE_6)
C
C	This file is used to calculate the sequence number
10	OPEN (UNIT=3,FILE='REDFURN:RED_NEWTEST.DAT',
	1	STATUS='OLD',ACCESS='KEYED',SHARED,
	1	ERR=20)
C
	GOTO 30
C
20	CALL LIB$SPAWN('CREATE/FDL=RED_NEWTEST REDFURN:RED_NEWTEST.DAT'
	1	,,,TRUSTED)
	GOTO 10
C
30	READ  (UNIT=3,KEYGE=DATE_6//'    ',KEYID=0,IOSTAT=IOS,ERR=50)
	1	HOT.HEADING
	UNLOCK(UNIT=3)
C
C1/11/00	IF (HOT.SEQ(1:1) .NE. '9') GOTO 50
	OLD_SEQ = HOT.SEQ
C
40	READ  (UNIT=3,END=54)HOT.HEADING 
	UNLOCK(UNIT=3)
C
C1/11/00	IF (HOT.SEQ(1:1) .NE. '9') THEN
C1/11/00		HOT.SEQ = OLD_SEQ
C1/11/00		GOTO 54
C1/11/00	END IF
	OLD_SEQ = HOT.SEQ
C	
	GOTO 40
C
50	IF (IOS .EQ. 52) THEN
		CALL FDV$PUTL ('Record locked!')
		CALL LIB$WAIT (3.0)
		GOTO 30
	END IF
C
	HOT.SEQ = DATE_6//'100R'
	GOTO 55
C
54	READ (HOT.SEQ(8:9),'(I2.2)') COUNT1
	COUNT1 = COUNT1 + 1
	WRITE (HOT.SEQ(8:9),'(I2.2)') COUNT1
C
55	HOT.FURN = '1'
	HOT.SEQ(7:7) = '1'
	CALL FDV$SPADA(0)
	CALL FDV$DFKBD (%DESCR(KEYTABLE),14)
        CALL FDV$CDISP ('REDNEWTEST')
C
	CALL FDV$PUT (RED_PAR.FE2,'FE2')
	CALL FDV$PUT (RED_PAR.FE,'FE')
C
C-------------------------------------------------------------------------------
C	Display hot values first time through
C-------------------------------------------------------------------------------
C
	CALL AST_DISPLAY_HOT
C
	CALL FURN_STAT
C
60	CALL FDV$SPADA(0)
	CALL FDV$DFKBD (%DESCR(KEYTABLE),14)
C 
	CALL FDV$SPON
	CALL FDV$GETAL (HOT.STRING,FIELDT,'DATE')
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 60
	END IF
C	
	HOT.M0  = DIG5(HOT.M0)
	HOT.FE2   = DIGIT5(HOT.FE2)
	HOT.FE = DIGIT5(HOT.FE)
C
	CALL FDV$PUT (HOT.M0,'MASS')
	CALL FDV$PUT (HOT.FE2,'FE2')
	CALL FDV$PUT (HOT.FE,'FE')
C
C-------------------------------------------------------------------------------
C 	Allows heading information to be updated in AST_DISPLAY_HOT.
C-------------------------------------------------------------------------------
C
	DISP_HEADING = 1
C
        CALL FDV$SPOFF
C
        BOLDON = 1
       	CALL FDV$AFVA (BOLDON,'DATAOK')
70	CALL FDV$PUT ('Is data ok <Y/N>? ','DATAOK')
C
	CALL FDV$DFKBD (%DESCR(KEY),8)    	!ENTER or RETURN are field 
C                                               !terminators
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 70
	END IF
C	
	IF (DREPLY .EQ. 'Y') THEN
	ELSE
		NONE = 0
		ATTROF = -1
        	CALL FDV$AFVA (NONE,'DATAOK')
        	CALL FDV$AFVA (ATTROF,'DREPLY')
		CALL FDV$PUT ('                 ','DATAOK')
 		CALL FDV$PUT (' ','DREPLY')
 		CALL FDV$SPON		
		GOTO 60
	END IF
C
80 	CALL FDV$PUT (' ','DREPLY')
C 
       	BOLDON = 1
       	CALL FDV$AFVA (BOLDON,'DATAOK')
	CALL FDV$PUT ('Begin test <Y/N>? ','DATAOK')
C
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 80
	END IF
C
	IF ((DREPLY .NE. 'Y').AND.(DREPLY .NE. 'N')) THEN
		CALL FDV$SIGOP
		CALL FDV$PUTL ()
		CALL FDV$PUTL ( 'Invalid entry!' )
		GOTO 80
	END IF
C	
	IF (DREPLY .NE. 'Y') GOTO 999
C
C-------------------------------------------------------------------------------
C 	If test was stopped by a PF1 TOGGLE equals 3 which stops the contact
C	in the VAX from toggling thus forcing all outputs from WDPF to zero.
C	To try starting test again toggle contact from VAX must be started.
C-------------------------------------------------------------------------------
C
	IF (TOGGLE .EQ. 3) THEN
		TOGGLE = 1
		CALL LIB$WAIT (10.0)
	END IF
C
C-------------------------------------------------------------------------------
C 	Check WDPF digital point to see if all permissives are satisfied.
C-------------------------------------------------------------------------------
C
	J = CONC_POINT / 32 + 1
	K = MOD (CONC_POINT,32)
	CONC_NEWBIT = JIBITS(CONC_CONTACT(J),K,1)
C
	IF (CONC_NEWBIT .EQ. 0) THEN
		CALL FDV$SIGOP 
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('Cannot start test, furnace not ready!')
		GOTO 85
	END IF


	CALL LIB$SPAWN('CREATE/FDL=REDDATA_COLLECT '//
	1	'REDFURN:'//HOT.SEQ//'.DAT',,,TRUSTED)

C	
C-------------------------------------------------------------------------------
C	Starts writing data to a file "SEQ#".DAT from the 
C	detached process COLLECT_RED
C-------------------------------------------------------------------------------
C
	START = 1
	STOP = 0
	FIRST_PASS = 1
!	BEEP = 1
        HOT.HEADING = HOT.STRING
	WRITE (UNIT=3)HOT.HEADING//RED_PAR.PARAMETER
C
85 	CALL FDV$PUT (' ','DREPLY')
C 
       	BOLDON = 1
       	CALL FDV$AFVA (BOLDON,'DATAOK')
90	CALL FDV$PUT ('To continue <RET> ','DATAOK')
C
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	
C
C06/28/01	IF (HOT.PERIOD .EQ. '5') BEEP = 0
	BEEP = 0
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 90
	END IF
C	
C-------------------------------------------------------------------------------
C	Cancels timer which stops AST from updating screen
C-------------------------------------------------------------------------------
C
999     STATUS = SYS$CANTIM( %VAL(TIMER_ID),)
        IF (.NOT. STATUS) CALL LIB$STOP(%VAL(STATUS))
C
	END


******************************
	SUBROUTINE RED_STATUS
******************************
	IMPLICIT	INTEGER*4(A - Z)
C
	INTEGER*2	FIELDT,BOLDON,ATTROF
 	CHARACTER*1	DREPLY
	CHARACTER*9	DELAY/'0 ::10.00'/
	DIMENSION	BIN_DELAY(2)
	DATA		TIMER_ID/3/
C
	COMMON /RED_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
C
	EXTERNAL	AST_DISPLAY_HOT	
C
	INCLUDE		'($PRCDEF)'
	INCLUDE		'REDFURN:KEYDEF'
C
	CALL SYS$BINTIM(DELAY,BIN_DELAY)
	CALL SYS$SETIMR(,BIN_DELAY,AST_DISPLAY_HOT,
	1	%VAL(TIMER_ID),)
C
	CALL FDV$SPADA (0) 		!Numeric mode
	CALL FDV$DFKBD(%DESCR(KEY),8)
        CALL FDV$CDISP ('REDSTATUS')
C
	CALL AST_DISPLAY_HOT
C
	CALL FURN_STAT
C
        BOLDON = 1
 	CALL FDV$PUT (' ','DREPLY')
C 
       	BOLDON = 1
       	CALL FDV$AFVA (BOLDON,'DATAOK')
90	CALL FDV$PUT ('To continue <RET> ','DATAOK')
C
	CALL FDV$SPOFF
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
C
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	
C
	BEEP = 0
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 90
	END IF
C	
C	Cancels timer which stops AST from updating screen
999     STATUS = SYS$CANTIM( %VAL(TIMER_ID),)
        IF (.NOT. STATUS) CALL LIB$STOP(%VAL(STATUS))
C
	END

******************************
	SUBROUTINE RED_REPORT
******************************
C
	IMPLICIT	INTEGER*4(A - Z)
	INCLUDE		'REDFURN:KEYDEF'
C
	STRUCTURE /DATA/
		UNION
			MAP
				CHARACTER*1	FURN
                                CHARACTER*8	DATE
				CHARACTER*1	LINE
				CHARACTER*1	SHIFT
				CHARACTER*8	FROMDATE
				CHARACTER*8	TODATE
				CHARACTER*10	SEQ
				CHARACTER*29	OTHER
				CHARACTER*29	TESTER
				CHARACTER*4	IPURGE
				CHARACTER*10	GASTYPE
				CHARACTER*3	Z1
				CHARACTER*3	Z2
				CHARACTER*3	Z3
				CHARACTER*3	Z4
				CHARACTER*3 	TEMP
				CHARACTER*4 	FPURGE
				CHARACTER*5 	PINT
				CHARACTER*3 	MASS
				CHARACTER*5 	FE2
				CHARACTER*5 	FE
				CHARACTER*4 	CO
				CHARACTER*4 	N2
				CHARACTER*4 	GASRATE
				CHARACTER*3 	RZ1
				CHARACTER*3 	RZ2
				CHARACTER*3 	RZ3
				CHARACTER*3 	RZ4
				CHARACTER*5 	RINT
				CHARACTER*5 	LONG
				CHARACTER*5 	FINT
				CHARACTER*2 	STOP
				CHARACTER*3 	IZ1
				CHARACTER*3 	IZ2
				CHARACTER*3 	IZ3
				CHARACTER*5 	T30
				CHARACTER*5 	T60
				CHARACTER*4 	RATE
			END MAP
			MAP
				CHARACTER*240	STRING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /DATA/ REP(26),NEW_RECORD
C
	STRUCTURE /MORE_DATA/
		UNION
			MAP
				CHARACTER*10	SEQ
                                CHARACTER*5	M0
				CHARACTER*5	FE2
				CHARACTER*5	FE
				CHARACTER*5	MT
				CHARACTER*3	Z1
				CHARACTER*3	Z2
				CHARACTER*3	Z3
				CHARACTER*3	Z4
				CHARACTER*4	GASFLO
				CHARACTER*4	CO
				CHARACTER*4	N2
				CHARACTER*4	RT
				CHARACTER*23	TIME
				CHARACTER*1	PERIOD
				CHARACTER*3	Z1_SP
				CHARACTER*3	Z2_SP
				CHARACTER*3	Z3_SP
				CHARACTER*4	GASFLO_SP
				CHARACTER*4	CO_SP
				CHARACTER*4	N2_SP
				CHARACTER*3	Z1_OUT
				CHARACTER*3	Z2_OUT
				CHARACTER*3	Z3_OUT
				CHARACTER*4	GASFLO_OUT
				CHARACTER*4	CO_OUT
				CHARACTER*4	N2_OUT
				CHARACTER*8 	ELAPSED
				CHARACTER*5	M1
			END MAP
			MAP
				CHARACTER*137	STRING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /MORE_DATA/ REC(2200),SET

C
	INTEGER*4	WORKSPACE( 3 ),REDRECALC( 3 ),IOS
	INTEGER*4	PRINTER_STAT,S,I
	INTEGER*4	FLAG_30,FLAG_60
	INTEGER*4	MINWINDOW,MAXWINDOW,CURLINE,LASTREG,FIELDT
	INTEGER*4	SFW/6/,
	1		SBK/7/,
	1		NEXT/38/,
	1		PREV/37/
	INTEGER*4	TRUSTED/64/
	REAL*4		TIME_30,TIME_60,RATE
	REAL*4		R_FE2,R_FE,R_PARMASS,R_INITMASS,R_MASS,R_RT
	REAL*4		R_CONSTANTMASS
	REAL*4		HOUR,MIN,SEC,CUR_SEC,INTERVAL_SEC
	CHARACTER*1	OPTION,BEGIN,FAKE
	CHARACTER*3	RED_PAR_MASS
        CHARACTER*4	PRINTSCREEN/' [0i'/,PRINTON/' [5i'/
	CHARACTER*4	PRINTOF/' [4i'/
	CHARACTER*5	FE2RECALC,FERECALC,DIGIT5
	CHARACTER*5	CMASS
	CHARACTER*8	RTIME
	CHARACTER*9	DELAY/'0 ::10.00'/
	CHARACTER*10	SEQNUMBERS,SEQNUM,SEQ_RECALC 
	CHARACTER*16	FURNACE1		
	CHARACTER*24	MESSAGE
	CHARACTER*80 	TEXT,V(300)
	EXTERNAL 	AST_FURN_RUN
	DIMENSION	BIN_DELAY(2)
	DATA		TIMER_ID/1/
C
	COMMON /RED_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	COMMON /SCROLL/ MINWINDOW,MAXWINDOW,CURLINE,LASTREG,FIELDT,V
C
	PRINTSCREEN(1:1) = CHAR(27)
	PRINTON(1:1)	= CHAR(27)
	PRINTOF(1:1)	= CHAR(27)
C
4	FORMAT ('+',A4)
C	
	FE2RECALC = ' '
	FERECALC = ' '
	RTIME = '00:00:00'
	ONCE_PER2 = 0
	ONCE_PER4 = 0
C
C-------------------------------------------------------------------------------
C	Set timer to turn on AST which updates screen every 10 sec
C-------------------------------------------------------------------------------
C
1	CALL SYS$BINTIM(DELAY, BIN_DELAY)
	CALL SYS$SETIMR( ,BIN_DELAY, AST_FURN_RUN,
	1	 %VAL(TIMER_ID),)
C
	CALL FILL_RED_SEQNUM_ARRAY(REP)
C
	CALL FDV$SPADA(0)
	CALL FDV$DFKBD (%DESCR(KEYREPORT1),7)
        CALL FDV$CDISP ('REDREPORT1')
	CALL FURN_STAT
C
2			CALL FDV$SPOFF
			NONE = 0
	       		CALL FDV$AFVA (NONE,'FETEXT')
	      		CALL FDV$PUT ('                 ','FETEXT')
			NONE = 0
			CALL FDV$AFVA (NONE,'FE2TEXT')
      			CALL FDV$PUT ('             ','FE2TEXT')
C
			NONE = 0
	       		CALL FDV$AFVA (NONE,'FE')
			CALL FDV$PUT ('     ','FE')
C
	       		NONE = 0
	 		CALL FDV$AFVA (NONE,'FE2')
			CALL FDV$PUT ('     ','FE2')
C
	    		NONE = 0
			CALL FDV$AFVA (NONE,'BEGINTEXT')
			CALL FDV$PUT ('                           ',
	1		'BEGINTEXT')
C
	 		NONE = 0
			CALL FDV$AFVA ( NONE,'BEGIN' )
	       		CALL FDV$PUT (' ','BEGIN')
C
	       		CALL FDV$PUTL (  )
C
	CALL FDV$SPON
	N = 1
60	CALL FDV$PUT (REP(N).SEQ,'SEQNUMBERS',N)
	N = N + 1
	IF (N .LE. 26) GOTO 60
C
	CALL READ_ALL_FIELDS (SEQNUMBERS)
C
	N = 1
70	IF ((REP(N).SEQ .EQ. SEQNUMBERS).AND.
	1	(SEQNUMBERS .NE. '          ')) GOTO 80
	N = N + 1
	IF (N .LE. 26) GOTO 70
C
	CALL FDV$SIGOP
	CALL FDV$PUTL ()
	CALL FDV$PUTL ('Sequence number does not exist!')
	CALL LIB$WAIT (3.0)
	GOTO 888
C

80 	REP_NUM = N
	CALL FDV$SPOFF
	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'SEQMES')
	CALL FDV$PUT ('Sequence number: ','SEQMES')
C
	UNDERLINE = 8
	CALL FDV$AFVA (UNDERLINE,'SEQNUM')
	CALL FDV$PUT (SEQNUMBERS,'SEQNUM')
C
100	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'OPTMES')
110	CALL FDV$PUT ('Choose option (1-5): ','OPTMES')	
C
	UNDERLINE = 8
	CALL FDV$AFVA (UNDERLINE,'OPTION')
120	CALL FDV$GET (OPTION,FIELDT,'OPTION')
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 110
	END IF
C
	IF (FIELDT .EQ. 2) GOTO 2
C
	IF (OPTION .EQ. '1') THEN
		STATUS = SYS$CANTIM ( %VAL(TIMER_ID), )
		OPEN (UNIT=30,FILE='REDFURN:'//SEQNUMBERS//'.PRN',
	1	STATUS='UNKNOWN',SHARED)		
C
		I = 1
		READ (30,510,END=520,ERR=515,IOSTAT=IOS)V(I)
500		READ (30,510,END=520,ERR=515,IOSTAT=IOS)V(I)
		UNLOCK (UNIT=30)
510		FORMAT (A80)
		I = I + 1
		GOTO 500
C
515		IF (IOS .EQ. 52) THEN
			UNLOCK (UNIT=30)
			CALL FDV$SIGOP
			CALL FDV$PUTL ()
			CALL FDV$PUTL ( 'Record is locked press <RET>' )
			CALL FDV$WAIT
			GOTO 100
		END IF 
C
520		CLOSE (UNIT=30)
		LASTREG = I - 1
		MINWINDOW = 1
		CURLINE = 1
C
		CALL FDV$DISP ( 'VIEW' )
		CALL FDV$PUTSC ( 'RECORD',V(CURLINE) )
C
		DO 530 CURLINE = 2,23
			CALL FDV$PFT ( SFW,'RECORD' )
			CALL FDV$PUTSC ( 'RECORD',V(CURLINE) )
530		CONTINUE
C
		CURLINE = CURLINE - 1
		MAXWINDOW = CURLINE
C		
		CALL FDV$GET ( FAKE,FIELDT,'FAKE' )
		DO WHILE ((FIELDT .NE. FDV$K_FT_NTR).AND.
	1		(FIELDT .NE. 106))
			IF (FIELDT .EQ. SFW) THEN
				CALL FORWARD
			ELSE IF (FIELDT .EQ. SBK) THEN
				CALL BACK
			ELSE IF (FIELDT .EQ. NEXT) THEN
				CALL PAGE_FORWARD
			ELSE IF (FIELDT .EQ. PREV) THEN
				CALL PAGE_BACK
			END IF
			CALL FDV$GET ( FAKE,FIELDT,'FAKE' )
		END DO
C
		GOTO 1
C
	ELSE IF ((OPTION .EQ. '2').OR.(OPTION .EQ. '3')) THEN
		STATUS = SYS$CANTIM ( %VAL(TIMER_ID), )
C
 		S = PRINTER_STAT ()
C
		IF (.NOT. S) THEN
			CALL FDV$SIGOP
			IF (S .EQ. 2) THEN
				CALL FDV$PUTL ()
				CALL FDV$PUTL ( 'Printer not ready!' )
			ELSE IF (S .EQ. 4) THEN 
				CALL FDV$PUTL ()
				CALL FDV$PUTL ( 'Turn printer on!' )
	       		ELSE IF (S .EQ. 6) THEN 
				CALL FDV$PUTL ()
				CALL FDV$PUTL ( 'Printer busy!' )
	      		ELSE IF (S .EQ. 8) THEN 
				CALL FDV$PUTL ()
				CALL FDV$PUTL ( 'Printer fault!' )
	       		END IF
			GOTO 100
		END IF
C
		WRITE (6,4) PRINTON
		CALL LIB$SPAWN ( 'TYPE REDFURN:'//SEQNUMBERS//'.PRN'
	1		,,,TRUSTED )		


		CALL LIB$WAIT(6.0)	!to be able to print from x-terminal

		WRITE (6,4) PRINTOF
C
                IF (OPTION .EQ. '3') THEN



		OPEN (UNIT=4,FILE='REDFURN:'//SEQNUMBERS//'.DAT',
	1		STATUS='OLD',ACCESS='KEYED',SHARED,ERR=888)
		N = 1
		READ  (UNIT=4,KEYGE='         ',KEYID=0,IOSTAT=IOS
	1		) REC(N).STRING
		UNLOCK (UNIT=4)
		N = N + 1
C
		FIRST2 = 1
		FIRST3 = 1
200		READ  (UNIT=4,END=250) REC(N).STRING
		UNLOCK (UNIT=4)
		IF ((REC(N).PERIOD .EQ. '2').AND.(FIRST2.EQ.1)) THEN
			RTIME = REC(N).TIME(13:20)
			FIRST2 = 0
		END IF			
C
		IF ((REC(N).PERIOD .EQ. '3').AND.(FIRST3.EQ.1)) THEN
			CMASS = REC(N).M1
			FIRST3 = 0
		END IF			

	        N = N + 1
		IF (N .EQ. 2200) GOTO 250
		GOTO 200
C
250		CLOSE (UNIT=4)
		TOT_REC = N - 1
		DO WHILE (N .LE. 2200)
			REC(N).STRING = ' '
			N = N + 1
260		END DO
C		
		WRITE (6,4) PRINTON
C
		N = 1
		READ (REP(REP_NUM).PINT(1:2),'(F4.0,BN)',IOSTAT=IOS)MIN
		READ (REP(REP_NUM).PINT(4:5),'(F4.0,BN)',IOSTAT=IOS)SEC
		INTERVAL_SEC = MIN * 60.0 + SEC
300		SET.STRING = REC(N).STRING
		READ (SET.ELAPSED(1:2),'(F4.0,BN)',IOSTAT=IOS)HOUR
		READ (SET.ELAPSED(4:5),'(F4.0,BN)',IOSTAT=IOS)MIN
		READ (SET.ELAPSED(7:8),'(F4.0,BN)',IOSTAT=IOS)SEC
		CUR_SEC = HOUR*3600 + MIN * 60.0 + SEC
		IF ((SET.PERIOD .EQ. '3').AND.(ONCE_PER2 .EQ. 0)) THEN
			WRITE (6,305)
305			FORMAT(' REDUCTION PERIOD')
			WRITE (6,306)
306			FORMAT(' ----------------')
			ONCE_PER2 = 1
			READ (REP(REP_NUM).RINT(1:2),'(F4.0,BN)',
	1			IOSTAT=IOS)MIN
			READ (REP(REP_NUM).RINT(4:5),'(F4.0,BN)',
	1			IOSTAT=IOS)SEC
			INTERVAL_SEC = MIN * 60.0 + SEC
		ELSE IF ((SET.PERIOD.EQ.'4').AND.(ONCE_PER4.EQ.0)) THEN
			WRITE (6,307)
307			FORMAT(' FINAL PERIOD')
			WRITE (6,308)
308			FORMAT(' ------------')
			ONCE_PER4 = 1
	       		READ (REP(REP_NUM).FINT(1:2),'(F4.0,BN)',
	1			IOSTAT=IOS)MIN
			READ (REP(REP_NUM).FINT(4:5),'(F4.0,BN)',
	1			IOSTAT=IOS)SEC
			INTERVAL_SEC = MIN * 60.0 + SEC
		END IF
C
		IF (N .EQ. 1) THEN
			WRITE (6,310)
310			FORMAT(//,' ',T51,'GAS CONDITIONS')
			WRITE (6,315)
315			FORMAT(' ','Minutes',T25,'ZONE TEMPERATURES',
	1		T49,'-------------------')
			WRITE (6,320)
320			FORMAT(' ','Elapsed',T13,'Rt',T19,'Mt',T25,
	1		'#1',T31,'#2',T37,'#3',T43,'#4',T49,'CO%',T56,
	1		'N2%',T63,'L/min')
			WRITE (6,325)
325			FORMAT(' ','---------------------------------',
	1			   '---------------------------------')
			WRITE (6,330)
330			FORMAT(' ','PRE-REDUCTION PERIOD:')
			WRITE (6,335)
335			FORMAT(' ','---------------------')
			WRITE (6,340)SET.ELAPSED,SET.RT,SET.MT,
	1			SET.Z1,SET.Z2,SET.Z3,SET.Z4,
	1			SET.CO,SET.N2,SET.GASFLO
          	ELSE IF (MOD(CUR_SEC,INTERVAL_SEC) .EQ. 0) THEN
			WRITE (6,340)SET.ELAPSED,SET.RT,SET.MT,
	1			SET.Z1,SET.Z2,SET.Z3,SET.Z4,
	1			SET.CO,SET.N2,SET.GASFLO
                END IF
340		FORMAT(' ',A8,3X,A4,2X,A5,1X,A3,3X,A3,3X,A3,3X,A3,3X,
	1		A4,3X,A4,3X,A4)

		N = N + 1
		IF (N .LE. TOT_REC) GOTO 300
C
 		WRITE (6,4) PRINTOF		
C
		END IF
C-------------------------------------------------------------------------------
C	Turn timer for AST on
C-------------------------------------------------------------------------------
		CALL SYS$BINTIM(DELAY, BIN_DELAY)
		CALL SYS$SETIMR( ,BIN_DELAY, AST_FURN_RUN,
	1		 %VAL(TIMER_ID),)
		GOTO 2
C
	ELSE IF (OPTION .EQ. '4') THEN
		OPEN (UNIT=3,FILE='REDFURN:RED_NEWTEST.DAT',
	1 			STATUS='OLD',ACCESS='KEYED',SHARED)
		READ (UNIT=3,KEY=SEQNUMBERS,KEYID=0)
		DELETE (UNIT=3)
		CLOSE (UNIT=3)
                CALL LIB$SPAWN ('COPY REDFURN:'//SEQNUMBERS//
	1	'.DAT.1 REDSAVE',,,TRUSTED)
		CALL LIB$SPAWN ('DELETE REDFURN:'//SEQNUMBERS
	1				//'.DAT.1',,,TRUSTED)


                CALL LIB$SPAWN ('COPY REDFURN:'//SEQNUMBERS//
	1	'.PRN.1 REDSAVE',,,TRUSTED)
		CALL LIB$SPAWN ('DELETE REDFURN:'//SEQNUMBERS
	1				//'.PRN.1',,,TRUSTED)
		CALL FILL_RED_SEQNUM_ARRAY(REP)
		NONE = 0 
		CALL FDV$AFVA (NONE,'OPTION')
		CALL FDV$PUT (' ','OPTION')
		CALL FDV$SPON
		GOTO 2
	ELSE IF (OPTION .EQ. '5') THEN
		GOTO 888
        ELSE
            	CALL FDV$SIGOP
		CALL FDV$PUTL ()
		CALL FDV$PUTL ( 'Invalid option!' )
		CALL FDV$PUT ( ' ','OPTION' )
		GOTO 120
	END IF


C
888     END

*************************************************
	SUBROUTINE READ_ALL_FIELDS (SEQNUMBERS)
*************************************************
	IMPLICIT	INTEGER*4(A - Z)
	INTEGER*4	FMSSTATUS
	CHARACTER*10	SEQNUMBERS
	CHARACTER*15	FIELDNAME
C
	INCLUDE		'REDFURN:KEYDEF'
C
1       CALL FDV$GET ( SEQNUMBERS, FIELDT, '*' )
	CALL FDV$RETFN ( FIELDNAME, FIELDINDEX )
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK ( FIELDT )
		GOTO 1
	END IF
C
	DO WHILE (.TRUE.)

		CALL FDV$PFT ( FIELDT )
		IF (FMSSTATUS .LT. 0) RETURN

		IF (FIELDT .EQ. FDV$K_FT_NTR) THEN
			IF (FMSSTATUS .NE. 2) THEN 
				RETURN
			ELSE
				CALL FDV$PUTL ()
		        	CALL FDV$PUTL ( 'Input required!' )
				CALL FDV$BELL
			END IF
		END IF
		CALL FDV$GETAF ( SEQNUMBERS, FIELDT, FIELDNAME,
	1			FIELDINDEX )
C
		IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
			CALL STOP_CHECK ( FIELDT )
		END IF
	END DO
	RETURN
	END
	
**********************************
	SUBROUTINE LTD_PARAMETER
**********************************
	INCLUDE		'REDFURN:KEYDEF'
C
	STRUCTURE /LTD_DATA/
		UNION
			MAP
				CHARACTER*4	IPURGE
       				CHARACTER*10	GASTYPE
       				CHARACTER*3	Z1
       				CHARACTER*3	Z2
       				CHARACTER*3	Z3
       				CHARACTER*3	Z4
       				CHARACTER*3	PTIME
				CHARACTER*5	PINT
				CHARACTER*4	CO_PCNT
				CHARACTER*4	N2_PCNT
	  			CHARACTER*4	H2_PCNT				
       	       			CHARACTER*4	CO2_PCNT
	       			CHARACTER*4	GASRATE
	       			CHARACTER*3	RZ1
	       			CHARACTER*3	RZ2
	       			CHARACTER*3	RZ3
	       			CHARACTER*3	RZ4
	       			CHARACTER*3	RTIME
	       			CHARACTER*5	RINT
	       			CHARACTER*4	FPURGE
	       			CHARACTER*3	STOP_TEMP
	       			CHARACTER*5	FINT
	       			CHARACTER*3	IZ1
	       			CHARACTER*3	IZ2
	       			CHARACTER*3	IZ3
       	       			CHARACTER*4	CO_FLOW
	       			CHARACTER*4	N2_FLOW
       	       			CHARACTER*5	H2_FLOW
	       			CHARACTER*4	CO2_FLOW
			END MAP
			MAP
				CHARACTER*112	STRING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /LTD_DATA/ LTD_PAR
C
	EXTERNAL	AST_FURN_RUN
C	
	INTEGER*2	BOLDON,ATTROF,UNDERLINE
	INTEGER*4	START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	INTEGER*4	RESULT,STATUS,BIN_DELAY(2),TIMER_ID/5/
	INTEGER*4	FIELDT
	INTEGER*4	TRUSTED/64/
	REAL*4		CO,N2,H2,CO2,GASRATE
	REAL*4		COFLOW,N2FLOW,H2FLOW,CO2FLOW
	CHARACTER*1	DREPLY
	CHARACTER*2	INTDIG2
	CHARACTER*3	INTDIG3,DIGIT3
	CHARACTER*4	DIGIT4
	CHARACTER*4	C_COFLOW,C_N2FLOW,C_CO2FLOW
	CHARACTER*5	DIG5,C_H2FLOW	
	CHARACTER*9	DELAY/'0 ::10.00'/
	CHARACTER*17	DATAOK
C
	COMMON /LTD_PARAMETER/ LTD_PAR
	COMMON /LTD_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
C
	DREPLY = ' '
	DATAOK = '                 '
C
	CALL SYS$BINTIM (DELAY,BIN_DELAY)
	CALL SYS$SETIMR (,BIN_DELAY,AST_FURN_RUN,
	1		%VAL(TIMER_ID),)
C
10	OPEN (UNIT=2,FILE='REDFURN:LTD_PAR.DAT',
	1	STATUS='OLD',
	1	ORGANIZATION='RELATIVE',
	1	FORM='UNFORMATTED',
	1	RECORDTYPE='FIXED',
	1	ACCESS='DIRECT',SHARED,
	1	ERR=20)
C
	GOTO 30
C
20	CALL LIB$SPAWN ('CREATE/FDL=LTD_PAR REDFURN:LTD_PAR.DAT'
	1		,,,TRUSTED)
	GOTO 10
C
30	READ  (UNIT=2,REC=1,IOSTAT=IOS,ERR=35)LTD_PAR.STRING
	UNLOCK(UNIT=2)
35      IF (IOS .EQ. 52) THEN
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('Record locked!')
		CALL LIB$WAIT(3.0)
		GOTO 30
	END IF
	WRITE (UNIT=2,REC=1)LTD_PAR.STRING(1:95)
C
        CALL FDV$SPON
        CALL FDV$CDISP ('LTDPAR')
C
	CALL FDV$PUTAL (LTD_PAR.STRING(1:95))
C
	CALL FURN_STAT
C
	CALL FDV$SPADA(0)
40	CALL FDV$DFKBD (%DESCR(KEYTABLE),14)
C
50    	CALL FDV$GETAL(LTD_PAR.STRING,FIELDT,'IPURGE')
C	
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 50
	END IF
C
      	LTD_PAR.IPURGE    	= DIGIT4(LTD_PAR.IPURGE)
	LTD_PAR.Z1        	= INTDIG3(LTD_PAR.Z1) 
	LTD_PAR.Z2       	= INTDIG3(LTD_PAR.Z2)    
	LTD_PAR.Z3        	= INTDIG3(LTD_PAR.Z3)    
	LTD_PAR.Z4        	= INTDIG3(LTD_PAR.Z4)    
	LTD_PAR.PTIME     	= INTDIG3(LTD_PAR.PTIME) 
	LTD_PAR.PINT(3:3) 	= ':'
C          
	LTD_PAR.CO_PCNT 	= DIGIT4(LTD_PAR.CO_PCNT)
	LTD_PAR.N2_PCNT		= DIGIT4(LTD_PAR.N2_PCNT)
	LTD_PAR.H2_PCNT		= DIGIT4(LTD_PAR.H2_PCNT)
	LTD_PAR.CO2_PCNT	= DIGIT4(LTD_PAR.CO2_PCNT)
        LTD_PAR.GASRATE    	= DIGIT4(LTD_PAR.GASRATE)
        LTD_PAR.RZ1        	= INTDIG3(LTD_PAR.RZ1) 
        LTD_PAR.RZ2        	= INTDIG3(LTD_PAR.RZ2) 
        LTD_PAR.RZ3        	= INTDIG3(LTD_PAR.RZ3) 
        LTD_PAR.RZ4        	= INTDIG3(LTD_PAR.RZ4) 
        LTD_PAR.RTIME    	= INTDIG3(LTD_PAR.RTIME)
        LTD_PAR.RINT(3:3)	= ':'
C
        LTD_PAR.FPURGE     	= DIGIT4(LTD_PAR.FPURGE)
        LTD_PAR.STOP_TEMP	= INTDIG3(LTD_PAR.STOP_TEMP)
	LTD_PAR.FINT(3:3)       = ':'	
        LTD_PAR.IZ1        	= INTDIG3(LTD_PAR.IZ1) 
        LTD_PAR.IZ2        	= INTDIG3(LTD_PAR.IZ2) 
        LTD_PAR.IZ3        	= INTDIG3(LTD_PAR.IZ3) 
C
	CALL FDV$PUTAL (LTD_PAR.STRING(1:95))
C
	CALL FURN_STAT
C
C-------------------------------------------------------------------------------
C	Calculate CO, N2, H2, & CO2 flows based on percentages entered 
C	by operator
C-------------------------------------------------------------------------------
C
	READ (LTD_PAR.CO_PCNT,'(F4.1,BN)') CO
	READ (LTD_PAR.N2_PCNT,'(F4.1,BN)') N2
	READ (LTD_PAR.H2_PCNT,'(F4.1,BN)') H2
	READ (LTD_PAR.CO2_PCNT,'(F4.1,BN)') CO2
C
	IF (CO + N2 + H2 + CO2 .NE. 100.0) THEN
		CALL FDV$SIGOP
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('CO,N2,H2 and CO2 must add up to 100.0')
		GOTO 40
	ELSE 
		CALL FDV$PUTL ( )
		READ (LTD_PAR.GASRATE,'(F4.1,BN)') GASRATE
		COFLOW = GASRATE * CO / 100.0
		N2FLOW = GASRATE * N2 / 100.0
		H2FLOW = GASRATE * H2 * 10.0
		CO2FLOW = GASRATE * CO2 / 100.0
		WRITE (C_COFLOW,'(F4.1,BN)') COFLOW
		WRITE (C_N2FLOW,'(F4.1,BN)') N2FLOW
		WRITE (C_H2FLOW,'(F5.1,BN)') H2FLOW
		WRITE (C_CO2FLOW,'(F4.1,BN)') CO2FLOW
		CALL FDV$PUT (C_COFLOW,'CO_FLOW')
		CALL FDV$PUT (C_N2FLOW,'N2_FLOW')
		CALL FDV$PUT (C_H2FLOW,'H2_FLOW')
		CALL FDV$PUT (C_CO2FLOW,'CO2_FLOW')
		LTD_PAR.CO_FLOW = C_COFLOW
		LTD_PAR.N2_FLOW = C_N2FLOW
		LTD_PAR.H2_FLOW = C_H2FLOW
		LTD_PAR.CO2_FLOW = C_CO2FLOW
	END IF
C
        CALL FDV$SPOFF
C
        BOLDON = 1
        CALL FDV$AFVA (BOLDON,'DATAOK')
	CALL FDV$PUT ('Is data ok <Y/N>? ','DATAOK')
C
	CALL FDV$DFKBD (%DESCR(KEY),8)
C
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	

	IF ((DREPLY .EQ. 'Y').OR.(DREPLY .EQ. ' ')) THEN
	ELSE
		NONE = 0
		ATTROF = -1
        	CALL FDV$AFVA (NONE,'DATAOK')
        	CALL FDV$AFVA (ATTROF,'DREPLY')
		CALL FDV$PUT ('                 ','DATAOK')
		CALL FDV$PUT (' ','DREPLY')
		CALL FDV$SPON		
		GOTO 40
	END IF

	WRITE (UNIT=2,REC=1)LTD_PAR.STRING
        CLOSE (UNIT=2)

	END



*******************************
	SUBROUTINE LTD_NEWTEST
*******************************
	STRUCTURE /LTD_DATA/
		UNION
			MAP
				CHARACTER*1	FURN
				CHARACTER*8	DATE
				CHARACTER*1	LINE
				CHARACTER*1	SHIFT
				CHARACTER*8	FROMDATE
				CHARACTER*8	TODATE
				CHARACTER*10	SEQ
				CHARACTER*29	OTHER
				CHARACTER*29	TESTER
                                CHARACTER*5	M0
				CHARACTER*5	M1
				CHARACTER*5	M2
				CHARACTER*5	M3
				CHARACTER*3	Z1
				CHARACTER*3	Z2
				CHARACTER*3	Z3
				CHARACTER*3	Z4
				CHARACTER*4	GASFLO
				CHARACTER*4	CO
				CHARACTER*4	N2
				CHARACTER*5	H2
				CHARACTER*4	CO2
				CHARACTER*23	TIME				
				CHARACTER*1	PERIOD
				CHARACTER*3	Z1_SP
				CHARACTER*3	Z2_SP
				CHARACTER*3	Z3_SP
				CHARACTER*4	GASFLO_SP
				CHARACTER*4	CO_SP
				CHARACTER*4	N2_SP
				CHARACTER*5	H2_SP
				CHARACTER*4	CO2_SP
				CHARACTER*3	Z1_OUT
				CHARACTER*3	Z2_OUT
				CHARACTER*3	Z3_OUT
				CHARACTER*3	GASFLO_OUT
				CHARACTER*3	CO_OUT
				CHARACTER*3	N2_OUT
				CHARACTER*3	H2_OUT
				CHARACTER*3	CO2_OUT
				CHARACTER*8	ELAPSED
			END MAP
			MAP
				CHARACTER*95	HEADING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /LTD_DATA/ HOT
C
	STRUCTURE /MORE_LTD_DATA/
		UNION
			MAP
				CHARACTER*4	IPURGE
       				CHARACTER*10	GASTYPE
       				CHARACTER*3	Z1
       				CHARACTER*3	Z2
       				CHARACTER*3	Z3
       				CHARACTER*3	Z4
       				CHARACTER*3	PTIME
				CHARACTER*5	PINT
				CHARACTER*4	CO_PCNT
				CHARACTER*4	N2_PCNT
	  			CHARACTER*4	H2_PCNT				
       	       			CHARACTER*4	CO2_PCNT
	       			CHARACTER*4	GASRATE
	       			CHARACTER*3	RZ1
	       			CHARACTER*3	RZ2
	       			CHARACTER*3	RZ3
	       			CHARACTER*3	RZ4
	       			CHARACTER*3	RTIME
	       			CHARACTER*5	RINT
	       			CHARACTER*4	FPURGE
	       			CHARACTER*3	STOP_TEMP
	       			CHARACTER*5	FINT
	       			CHARACTER*3	IZ1
	       			CHARACTER*3	IZ2
	       			CHARACTER*3	IZ3
       	       			CHARACTER*4	CO_FLOW
	       			CHARACTER*4	N2_FLOW
       	       			CHARACTER*5	H2_FLOW
	       			CHARACTER*4	CO2_FLOW
			END MAP
			MAP
				CHARACTER*112	STRING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /MORE_LTD_DATA/ LTD_PAR
C
	INCLUDE		'REDFURN:KEYDEF'
C
	INTEGER*2	COUNT,DISP_HEADING
C
	INTEGER*4	CONC_CONTACT(100),J,K
	INTEGER*4	CONC_POINT/1758/,CONC_NEWBIT
	INTEGER*4	BIN_DELAY,TIMER_ID
	INTEGER*4	FIELDT,BOLDON,UNDERLINE,NONE,ATTROF
	INTEGER*4	START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	INTEGER*4	TRUSTED/64/
C
	CHARACTER*1	DREPLY
	CHARACTER*6	DATE_6
	CHARACTER*8	DELAY/'0 0:0:10'/
	CHARACTER*10	OLD_SEQ
	CHARACTER*23	RED_ELAPSED
C
	DIMENSION	BIN_DELAY(2)
	DATA		TIMER_ID/6/
	EXTERNAL	AST_DISPLAY_HOT_LTD
C
	COMMON /LTD_HOT/ HOT
	COMMON /LTD_PARAMETER/ LTD_PAR
	COMMON /CONTACTSCNC/ CONC_CONTACT
	COMMON /LTD_DISPLAY/ DISP_HEADING
	COMMON /LTD_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	COMMON /LTD_TIME/ RED_ELAPSED
C
	HOT.ELAPSED = '        '
	RED_ELAPSED(13:20) = '        '
	DISP_HEADING = 0
C
	OPEN (UNIT=2,FILE='REDFURN:LTD_PAR.DAT',
	1	STATUS='OLD',
	1	ORGANIZATION='RELATIVE',
	1	FORM='UNFORMATTED',
	1	RECORDTYPE='FIXED',
	1	ACCESS='DIRECT',SHARED,
	1	ERR=999)
C
1	READ  (UNIT=2,REC=1,IOSTAT=IOS,ERR=2)LTD_PAR.STRING
	UNLOCK(UNIT=2)
2       IF (IOS .EQ. 52) THEN
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('Record locked!')
		CALL LIB$WAIT(3.0)
		GOTO 1
	END IF
	CLOSE (UNIT=2)
C
C-------------------------------------------------------------------------------
C	Set timer to turn on AST which updates screen every 10 sec
C-------------------------------------------------------------------------------
C
	CALL SYS$BINTIM(DELAY, BIN_DELAY)
	CALL SYS$SETIMR( ,BIN_DELAY, AST_DISPLAY_HOT_LTD,
	1	 %VAL(TIMER_ID),)
C
C-------------------------------------------------------------------------------
C	Get today's date in the form YYMMDD
C-------------------------------------------------------------------------------
C
	CALL DATE_YYMMDD(DATE_6)
C
C-------------------------------------------------------------------------------
C	This file is used to calculate the sequence number
C-------------------------------------------------------------------------------
C
10	OPEN (UNIT=7,FILE='REDFURN:LTD_NEWTEST.DAT',
	1	STATUS='OLD',ACCESS='KEYED',SHARED,
	1	ERR=20)
C
	GOTO 30
C
20	CALL LIB$SPAWN('CREATE/FDL=LTD_NEWTEST REDFURN:LTD_NEWTEST.DAT'
	1	,,,TRUSTED)
	GOTO 10
C
30	READ  (UNIT=7,KEYGE=DATE_6//'    ',KEYID=0,IOSTAT=IOS,ERR=50)
	1	HOT.HEADING
	UNLOCK(UNIT=7)
C
C1/11/00	IF (HOT.SEQ(1:1) .NE. '9') GOTO 50
	OLD_SEQ = HOT.SEQ
C
40	READ  (UNIT=7,END=54)HOT.HEADING 
	UNLOCK(UNIT=7)
C
C1/11/00	IF (HOT.SEQ(1:1) .NE. '9') THEN
C1/11/00		HOT.SEQ = OLD_SEQ
C1/11/00		GOTO 54
C1/11/00	END IF
	OLD_SEQ = HOT.SEQ
C	
	GOTO 40
C
50	IF (IOS .EQ. 52) THEN
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('Record locked!')
		CALL LIB$WAIT (3.0)
		GOTO 30
	END IF
C
	HOT.SEQ = DATE_6//'100L'
	GOTO 55
C
54	READ (HOT.SEQ(8:9),'(I2.2)') COUNT
	COUNT = COUNT + 1
	WRITE (HOT.SEQ(8:9),'(I2.2)') COUNT
C
55	HOT.FURN = '1'
	HOT.SEQ(7:7) = '1'
	CALL FDV$SPADA (0)
	CALL FDV$DFKBD (%DESCR(KEYTABLE),14)
	CALL FDV$CDISP ('LTDNEWTEST')
C
C-------------------------------------------------------------------------------
C	Display hot values first time through
C-------------------------------------------------------------------------------
C
	CALL AST_DISPLAY_HOT_LTD
C
	CALL FURN_STAT
C
60	CALL FDV$SPADA(0)
	CALL FDV$DFKBD (%DESCR(KEYTABLE),14)
C 
	CALL FDV$SPON
	CALL FDV$GETAL (HOT.HEADING,FIELDT,'DATE')
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 60
	END IF
C
	DISP_HEADING = 1
C
        CALL FDV$SPOFF
C
        BOLDON = 1
       	CALL FDV$AFVA (BOLDON,'DATAOK')
70	CALL FDV$PUT ('Is data ok <Y/N>? ','DATAOK')
C
	CALL FDV$DFKBD (%DESCR(KEY),8)    	!ENTER or RETURN are field 
C                                               !terminators
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 70
	END IF
C	
	IF (DREPLY .EQ. 'Y') THEN
	ELSE
		NONE = 0
		ATTROF = -1
        	CALL FDV$AFVA (NONE,'DATAOK')
        	CALL FDV$AFVA (ATTROF,'DREPLY')
		CALL FDV$PUT ('                 ','DATAOK')
 		CALL FDV$PUT (' ','DREPLY')
 		CALL FDV$SPON		
		GOTO 60
	END IF
C
80 	CALL FDV$PUT (' ','DREPLY')
C 
       	BOLDON = 1
       	CALL FDV$AFVA (BOLDON,'DATAOK')
	CALL FDV$PUT ('Begin test <Y/N>? ','DATAOK')
C
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 80
	END IF
C
	IF ((DREPLY .NE. 'Y').AND.(DREPLY .NE. 'N')) THEN
		CALL FDV$SIGOP
		CALL FDV$PUTL ()
		CALL FDV$PUTL ( 'Invalid entry!' )
		GOTO 80
	END IF
C	
	IF (DREPLY .NE. 'Y') GOTO 999
C
C-------------------------------------------------------------------------------
C 	If test was stopped by a PF1 TOGGLE equals 3 which stops the contact
C	in the VAX from toggling thus changing all outputs from WDPF to zero.
C	To try starting test again toggle contact from VAX must be started.
C-------------------------------------------------------------------------------
C
	IF (TOGGLE .EQ. 3) THEN
		TOGGLE = 1
		CALL LIB$WAIT (10.0)
	END IF
C
C-------------------------------------------------------------------------------
C 	Check WDPF digital point to see if all permissives are satisfied.
C-------------------------------------------------------------------------------
C
	J = CONC_POINT / 32 + 1
	K = MOD (CONC_POINT,32)
	CONC_NEWBIT = JIBITS(CONC_CONTACT(J),K,1)
C
	IF (CONC_NEWBIT .EQ. 0) THEN
		CALL FDV$SIGOP 
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('Cannot start test, wdpf not ready!')
		GOTO 85
	END IF


	CALL LIB$SPAWN('CREATE/FDL=LTDDATA_COLLECT '//
	1	'REDFURN:'//HOT.SEQ//'.DAT',,,TRUSTED)

C	
C-------------------------------------------------------------------------------
C	Starts writing data to a file "SEQ#".DAT from the 
C	detached process COLLECT_LTD
C-------------------------------------------------------------------------------
C
	FIRST_PASS = 1
!	BEEP = 1
	STOP = 0
	START = 1
	WRITE (UNIT=7)HOT.HEADING//LTD_PAR.GASTYPE//LTD_PAR.PTIME//
	1	LTD_PAR.PINT//LTD_PAR.CO_PCNT//LTD_PAR.N2_PCNT//
	1	LTD_PAR.H2_PCNT//LTD_PAR.CO2_PCNT//LTD_PAR.RTIME//
	1	LTD_PAR.RINT//LTD_PAR.FINT
	CLOSE (UNIT=7)
C
85 	CALL FDV$PUT (' ','DREPLY')
C 
       	BOLDON = 1
       	CALL FDV$AFVA (BOLDON,'DATAOK')
90	CALL FDV$PUT ('To continue <RET> ','DATAOK')
C
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	
C
C06/28/01	IF (HOT.PERIOD .EQ. '3') BEEP = 0
	BEEP = 0
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 90
	END IF
C	
C-------------------------------------------------------------------------------
C	Cancels timer which stops AST from updating screen
C-------------------------------------------------------------------------------
C
999     CALL SYS$CANTIM( %VAL(TIMER_ID),)
C
	END
******************************
	SUBROUTINE LTD_STATUS
******************************
	INTEGER*4	START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	INTEGER*4	BIN_DELAY,TIMER_ID
	INTEGER*4	BOLDON,UNDERLINE,FIELDT
C
	CHARACTER*1	DREPLY
	CHARACTER*9	DELAY/'0 ::10.00'/
C
	DIMENSION	BIN_DELAY(2)
	DATA		TIMER_ID/6/
C
	COMMON /LTD_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
C
	EXTERNAL	AST_DISPLAY_HOT_LTD	
C
	INCLUDE		'REDFURN:KEYDEF'
C
	CALL SYS$BINTIM(DELAY,BIN_DELAY)
	CALL SYS$SETIMR(,BIN_DELAY,AST_DISPLAY_HOT_LTD,
	1	%VAL(TIMER_ID),)
C
	CALL FDV$SPADA (0) 		!Numeric mode
	CALL FDV$DFKBD(%DESCR(KEY),8)
        CALL FDV$CDISP ('LTDSTATUS')
C
	CALL AST_DISPLAY_HOT_LTD
C
	CALL FURN_STAT
C
        BOLDON = 1
 	CALL FDV$PUT (' ','DREPLY')
C 
       	BOLDON = 1
       	CALL FDV$AFVA (BOLDON,'DATAOK')
90	CALL FDV$PUT ('To continue <RET> ','DATAOK')
C
	CALL FDV$SPOFF
	UNDERLINE = 8
        CALL FDV$AFVA (UNDERLINE,'DREPLY')
C
	CALL FDV$GET (DREPLY,FIELDT,'DREPLY')	
C
	BEEP = 0
C
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 90
	END IF
C	
C	Cancels timer which stops AST from updating screen
999     CALL SYS$CANTIM( %VAL(TIMER_ID),)
C
	END

******************************
	SUBROUTINE LTD_REPORT
******************************
	STRUCTURE /DATA/
		UNION
			MAP
				CHARACTER*1	FURN
				CHARACTER*8	DATE
				CHARACTER*1	LINE
				CHARACTER*1	SHIFT
				CHARACTER*8	FROMDATE
				CHARACTER*8	TODATE
				CHARACTER*10	SEQ
				CHARACTER*29	OTHER
				CHARACTER*29	TESTER
				CHARACTER*10	GASTYPE
				CHARACTER*3	PTIME
				CHARACTER*5	PINT
				CHARACTER*4	CO_PCNT
				CHARACTER*4	N2_PCNT
				CHARACTER*4	H2_PCNT
				CHARACTER*4	CO2_PCNT
				CHARACTER*3	RTIME				
				CHARACTER*5	RINT
				CHARACTER*5	FINT
                                CHARACTER*5	M0
				CHARACTER*5	M1
				CHARACTER*5	M2
				CHARACTER*5	M3
				CHARACTER*4	LTD63
				CHARACTER*4	LTD315
				CHARACTER*4	LTD500
			END MAP
			MAP
				CHARACTER*174	STRING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /DATA/ REP(26),NEW_RECORD
C
	STRUCTURE /MORE_DATA/
		UNION
			MAP
				CHARACTER*10	SEQ
				CHARACTER*3	Z1
				CHARACTER*3	Z2
				CHARACTER*3	Z3
				CHARACTER*3	Z4
				CHARACTER*4	GASFLO
				CHARACTER*4	CO
				CHARACTER*4	N2
				CHARACTER*5	H2
				CHARACTER*4	CO2
				CHARACTER*23	TIME				
				CHARACTER*1	PERIOD
				CHARACTER*3	Z1_SP
				CHARACTER*3	Z2_SP
				CHARACTER*3	Z3_SP
				CHARACTER*4	GASFLO_SP
				CHARACTER*4	CO_SP
				CHARACTER*4	N2_SP
				CHARACTER*5	H2_SP
				CHARACTER*4	CO2_SP
				CHARACTER*3	Z1_OUT
				CHARACTER*3	Z2_OUT
				CHARACTER*3	Z3_OUT
				CHARACTER*3	GASFLO_OUT
				CHARACTER*3	CO_OUT
				CHARACTER*3	N2_OUT
				CHARACTER*3	H2_OUT
				CHARACTER*3	CO2_OUT
				CHARACTER*8	ELAPSED
			END MAP
			MAP
				CHARACTER*129	STRING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /MORE_DATA/ REC(2200),SET
C
	INTEGER*2	FIRST,TOT_REC,ONCE_PER2
C
	INTEGER*4	I,N,REP_NUM,BOLDON,UNDERLINE,IOS
	INTEGER*4	BIN_DELAY(2),TIMER_ID/1/
	INTEGER*4	PRINTER_STAT,S,RESULT,STATUS
        INTEGER*4       MINWINDOW,MAXWINDOW,CURLINE,LASTREG,FIELDT
	INTEGER*4	SFW/6/,SBK/7/,NEXT/38/,PREV/37/        
	INTEGER*4	START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
	INTEGER*4	TRUSTED/64/
C
	REAL*4		M0,M1,M2,M3,LTD63,LTD315,LTD500
	REAL*4		HOUR,MIN,SEC,INTERVAL_SEC,CUR_SEC
C
	CHARACTER*1	OPTION,FAKE
        CHARACTER*4	PRINTSCREEN/' [0i'/,PRINTON/' [5i'/
	CHARACTER*4	PRINTOF/' [4i'/
	CHARACTER*8	RTIME
	CHARACTER*9	DELAY/'0 ::10.00'/
	CHARACTER*10	SEQNUMBERS
	CHARACTER*16	M0_TEXT/'Total weight (g)'/
	CHARACTER*21	M1_TEXT/'Weight of +6.30mm (g)'/
	CHARACTER*21	M2_TEXT/'Weight of +6.15mm (g)'/
	CHARACTER*20	M3_TEXT/'Weight of +500um (g)'/
        CHARACTER*80    V(300)
C
	EXTERNAL	AST_FURN_RUN
	INCLUDE		'REDFURN:KEYDEF'
C
        COMMON /LTD_COM/ START,STOP,FIRST_PASS,DETACH,BEEP,TOGGLE
        COMMON /SCROLL/ MINWINDOW,MAXWINDOW,CURLINE,LASTREG,FIELDT,V
C
	PRINTSCREEN(1:1) = CHAR(27)
	PRINTON(1:1)     = CHAR(27)
	PRINTOF(1:1)     = CHAR(27)
C
4	FORMAT ('+',A4)
C
C-------------------------------------------------------------------------------
C	Set timer to turn on AST which updates screen every 10 sec
C-------------------------------------------------------------------------------
C
1	CALL SYS$BINTIM(DELAY, BIN_DELAY)
	CALL SYS$SETIMR( ,BIN_DELAY, AST_FURN_RUN,
	1	 %VAL(TIMER_ID),)
C
	CALL FDV$SPADA(0)
	CALL FDV$DFKBD (%DESCR(KEYREPORT1),7)
        CALL FDV$CDISP ('LTDREPORT1')
	CALL FURN_STAT
C
2	DO N = 1,26
		REP(N).STRING = ' '
		REP(N).LTD63 = ' '
		REP(N).LTD315 = ' '
		REP(N).LTD500 = ' '
	END DO
C
	DO N = 1,300
		V(N) = ' '
	END DO
C
	OPEN (UNIT=7,FILE='REDFURN:LTD_NEWTEST.DAT',
	1	STATUS='OLD',ACCESS='KEYED',SHARED)
C
	N = 1
30	READ (UNIT=7,KEYGE='          ',KEYID=0,IOSTAT=IOS,
	1	ERR=50) REP(N).STRING
	UNLOCK (UNIT=7)
	N = N + 1
C
40	READ (UNIT=7,END=50) REP(N).STRING
	UNLOCK (UNIT=7)
	N = N + 1
	IF (N .LE. 26) GOTO 40
C
50	IF (IOS .EQ. 52) THEN
		UNLOCK (UNIT=7)
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('Record locked!')
		CALL LIB$WAIT (3.0)
		GOTO 30
	END IF
C
	CLOSE (UNIT=7)
C
	CALL FDV$SPON
	N = 1
60	CALL FDV$PUT (REP(N).SEQ,'SEQNUMBERS',N)
	N = N + 1
	IF (N .LE. 26) GOTO 60
C
	CALL READ_ALL_FIELDS (SEQNUMBERS)
C
	N = 1
70	IF ((REP(N).SEQ .EQ. SEQNUMBERS).AND.
	1	(SEQNUMBERS .NE. '          ')) GOTO 80
	N = N + 1
	IF (N .LE. 26) GOTO 70
C
	CALL FDV$SIGOP
	CALL FDV$PUTL ()
	CALL FDV$PUTL ('Sequence number does not exist!')
	CALL LIB$WAIT (3.0)
	GOTO 888
C

80 	REP_NUM = N
	CALL FDV$SPOFF
	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'SEQMES')
	CALL FDV$PUT ('Sequence number: ','SEQMES')
C
	UNDERLINE = 8
	CALL FDV$AFVA (UNDERLINE,'SEQNUM')
	CALL FDV$PUT (SEQNUMBERS,'SEQNUM')
C
	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'M0_TEXT')
	CALL FDV$PUT (M0_TEXT,'M0_TEXT')
	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'M1_TEXT')
	CALL FDV$PUT (M1_TEXT,'M1_TEXT')
	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'M2_TEXT')
	CALL FDV$PUT (M2_TEXT,'M2_TEXT')
	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'M3_TEXT')
	CALL FDV$PUT (M3_TEXT,'M3_TEXT')
C
	READ (REP(N).M0,'(BN,F5.1)',IOSTAT=IOS) M0
	READ (REP(N).M1,'(BN,F5.1)',IOSTAT=IOS) M1
	READ (REP(N).M2,'(BN,F5.1)',IOSTAT=IOS) M2
	READ (REP(N).M3,'(BN,F5.1)',IOSTAT=IOS) M3
C
	WRITE (REP(N).M0,'(BN,F5.1)',IOSTAT=IOS) M0
	WRITE (REP(N).M1,'(BN,F5.1)',IOSTAT=IOS) M1
	WRITE (REP(N).M2,'(BN,F5.1)',IOSTAT=IOS) M2
	WRITE (REP(N).M3,'(BN,F5.1)',IOSTAT=IOS) M3
C
	UNDERLINE = 8
	CALL FDV$AFVA (UNDERLINE,'M0')
	CALL FDV$PUT (REP(N).M0,'M0')
	UNDERLINE = 8
	CALL FDV$AFVA (UNDERLINE,'M1')
	CALL FDV$PUT (REP(N).M1,'M1')
	UNDERLINE = 8
	CALL FDV$AFVA (UNDERLINE,'M2')
	CALL FDV$PUT (REP(N).M2,'M2')
	UNDERLINE = 8
	CALL FDV$AFVA (UNDERLINE,'M3')
	CALL FDV$PUT (REP(N).M3,'M3')
C
100	CALL FDV$GET (REP(N).M0,FIELDT,'M0')
	IF (FIELDT .EQ. 2) GOTO 2
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 100
	END IF
110	CALL FDV$GET (REP(N).M1,FIELDT,'M1')
	IF (FIELDT .EQ. 2) GOTO 100
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 110
	END IF
120	CALL FDV$GET (REP(N).M2,FIELDT,'M2')
	IF (FIELDT .EQ. 2) GOTO 110
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 120
	END IF
130    	CALL FDV$GET (REP(N).M3,FIELDT,'M3')
	IF (FIELDT .EQ. 2) GOTO 120
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 130
	END IF
C
	READ (REP(N).M0,'(BN,F5.1)',IOSTAT=IOS) M0
	READ (REP(N).M1,'(BN,F5.1)',IOSTAT=IOS) M1
	READ (REP(N).M2,'(BN,F5.1)',IOSTAT=IOS) M2
	READ (REP(N).M3,'(BN,F5.1)',IOSTAT=IOS) M3
C
	WRITE (REP(N).M0,'(BN,F5.1)',IOSTAT=IOS) M0
	WRITE (REP(N).M1,'(BN,F5.1)',IOSTAT=IOS) M1
	WRITE (REP(N).M2,'(BN,F5.1)',IOSTAT=IOS) M2
	WRITE (REP(N).M3,'(BN,F5.1)',IOSTAT=IOS) M3
C
	CALL FDV$PUT (REP(N).M0,'M0')
	CALL FDV$PUT (REP(N).M1,'M1')
	CALL FDV$PUT (REP(N).M2,'M2')
	CALL FDV$PUT (REP(N).M3,'M3')
C	
	IF (M0 .NE. 0.0) THEN
		LTD63 = M1 / M0 * 100.0
		LTD315 = (M1 + M2) / M0 * 100.0
       		LTD500 = (M0 - (M1 + M2 + M3)) / M0 * 100.0
		WRITE (REP(N).LTD63,'(BN,F4.1)',IOSTAT=IOS) LTD63
		WRITE (REP(N).LTD315,'(BN,F4.1)',IOSTAT=IOS) LTD315
      		WRITE (REP(N).LTD500,'(BN,F4.1)',IOSTAT=IOS) LTD500
	END IF
C	
	OPEN (UNIT=7,FILE='REDFURN:LTD_NEWTEST.DAT',
	1	STATUS='OLD',ACCESS='KEYED',SHARED)
C
	READ (UNIT=7,KEY=SEQNUMBERS,KEYID=0,IOSTAT=IOS) NEW_RECORD
	DELETE (UNIT=7)
C
	WRITE (UNIT=7) REP(N).STRING
C
200	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'OPTMES')
210	CALL FDV$PUT ('Choose option (1-5): ','OPTMES')	
C
	UNDERLINE = 8
	CALL FDV$AFVA (UNDERLINE,'OPTION')
220	CALL FDV$GET (OPTION,FIELDT,'OPTION')
C
	IF (FIELDT .EQ. 2) GOTO 130
	IF ((FIELDT .EQ. 103).OR.(FIELDT .EQ. 104)) THEN
		CALL STOP_CHECK (FIELDT)
		GOTO 220
	END IF
C
  	OPEN (UNIT=30,FILE='REDFURN:'//SEQNUMBERS//'.PRN',
	1	STATUS='UNKNOWN',SHARED)
C
        I = 1
250 	READ (30,255,END=265,ERR=260,IOSTAT=IOS)V(I)
        UNLOCK (UNIT=30)
255     FORMAT (A80)
        I = I + 1
        GOTO 250
C
260     IF (IOS .EQ. 52) THEN
                UNLOCK (UNIT=30)
                CALL FDV$SIGOP
                CALL FDV$PUTL ()
                CALL FDV$PUTL ( 'Record is locked press <RET>' )
                CALL FDV$WAIT
                GOTO 100
        END IF
C
265     LASTREG = I - 1
     	REWIND (UNIT=30)
C
	V(18)(23:26) = REP(N).LTD63
	V(19)(23:26) = REP(N).LTD315
	V(20)(23:26) = REP(N).LTD500
	V(25)(42:46) = REP(N).M0
	V(26)(42:46) = REP(N).M1
	V(27)(42:46) = REP(N).M2
	V(28)(42:46) = REP(N).M3
C
	DO I = 1,LASTREG
		WRITE (30,255) V(I)
	END DO
C
     	CLOSE (UNIT=30)
C
       	IF (OPTION .EQ. '1') THEN
                STATUS = SYS$CANTIM ( %VAL(TIMER_ID), )
  		OPEN (UNIT=30,FILE='REDFURN:'//SEQNUMBERS//'.PRN',
	1	STATUS='UNKNOWN',SHARED)
C
                I = 1
                READ (30,510,END=520,ERR=515,IOSTAT=IOS)V(I)
500             READ (30,510,END=520,ERR=515,IOSTAT=IOS)V(I)
                UNLOCK (UNIT=30)
510             FORMAT (A80)
                I = I + 1
                GOTO 500
C
515             IF (IOS .EQ. 52) THEN
                        UNLOCK (UNIT=30)
                        CALL FDV$SIGOP
                        CALL FDV$PUTL ()
                       	CALL FDV$PUTL ( 'Record is locked press <RET>' )
                        CALL FDV$WAIT
                        GOTO 100
                END IF
C
520             CLOSE (UNIT=30)
                LASTREG = I - 1
                MINWINDOW = 1
                CURLINE = 1
C
                CALL FDV$DISP ( 'VIEW' )
                CALL FDV$PUTSC ( 'RECORD',V(CURLINE) )
C
                DO 530 CURLINE = 2,23
                        CALL FDV$PFT ( SFW,'RECORD' )
                        CALL FDV$PUTSC ( 'RECORD',V(CURLINE) )
530             CONTINUE
C

                CURLINE = CURLINE - 1
                MAXWINDOW = CURLINE
C
                CALL FDV$GET ( FAKE,FIELDT,'FAKE' )
		DO WHILE ((FIELDT .NE. 0).AND.
	1	(FIELDT .NE. 106))
                        IF (FIELDT .EQ. SFW) THEN
                                CALL FORWARD
                        ELSE IF (FIELDT .EQ. SBK) THEN
                                CALL BACK
                        ELSE IF (FIELDT .EQ. NEXT) THEN
                                CALL PAGE_FORWARD
                        ELSE IF (FIELDT .EQ. PREV) THEN
                                CALL PAGE_BACK
                        END IF
                        CALL FDV$GET ( FAKE,FIELDT,'FAKE' )
                END DO
C
		GOTO 1
C
	ELSE IF ((OPTION .EQ. '2').OR.(OPTION .EQ. '3')) THEN
                STATUS = SYS$CANTIM ( %VAL(TIMER_ID), )
C
 		S = PRINTER_STAT ()
C
		IF (.NOT. S) THEN
			CALL FDV$SIGOP
			IF (S .EQ. 2) THEN
				CALL FDV$PUTL ()
				CALL FDV$PUTL ( 'Printer not ready!' )
			ELSE IF (S .EQ. 4) THEN 
				CALL FDV$PUTL ()
				CALL FDV$PUTL ( 'Turn printer on!' )
	       		ELSE IF (S .EQ. 6) THEN 
				CALL FDV$PUTL ()
				CALL FDV$PUTL ( 'Printer busy!' )
	      		ELSE IF (S .EQ. 8) THEN 
				CALL FDV$PUTL ()
				CALL FDV$PUTL ( 'Printer fault!' )
	       		END IF
			GOTO 200
		END IF
C
		WRITE (6,4) PRINTON
		CALL LIB$SPAWN ( 'TYPE REDFURN:'//SEQNUMBERS//'.PRN'
	1		,,,TRUSTED )
		CALL LIB$WAIT(6.0)	!to be able to print from x-terminal
		WRITE (6,4) PRINTOF
C
		IF (OPTION .EQ. '3') THEN
C
		OPEN (UNIT=5,FILE='REDFURN:'//SEQNUMBERS//'.DAT',
	1		STATUS='OLD',ACCESS='KEYED',SHARED,ERR=888)
		N = 1
		READ  (UNIT=5,KEYGE='         ',KEYID=0,IOSTAT=IOS
	1		) REC(N).STRING
		UNLOCK (UNIT=5)
		N = N + 1
C
		FIRST = 1
300		READ  (UNIT=5,END=350) REC(N).STRING
		UNLOCK (UNIT=5)
		IF ((REC(N).PERIOD .EQ. '2').AND.(FIRST.EQ.1)) THEN
			RTIME = REC(N).TIME(13:20)
			FIRST = 0
		END IF			

	        N = N + 1
		IF (N .EQ. 2200) GOTO 350
		GOTO 300
C
350		CLOSE (UNIT=5)
		TOT_REC = N - 1
		DO WHILE (N .LE. 2200)
			REC(N).STRING = ' '
			N = N + 1
		END DO
C		
		WRITE (6,4) PRINTON
C
		ONCE_PER2 = 0
		N = 1
C
		READ (REP(REP_NUM).PINT(1:2),'(F4.0,BN)',IOSTAT=IOS)MIN
		READ (REP(REP_NUM).PINT(4:5),'(F4.0,BN)',IOSTAT=IOS)SEC
		INTERVAL_SEC = MIN * 60.0 + SEC
400		SET.STRING = REC(N).STRING
		READ (SET.ELAPSED(1:2),'(F4.0,BN)',IOSTAT=IOS)HOUR
		READ (SET.ELAPSED(4:5),'(F4.0,BN)',IOSTAT=IOS)MIN
		READ (SET.ELAPSED(7:8),'(F4.0,BN)',IOSTAT=IOS)SEC
		CUR_SEC = HOUR*3600 + MIN * 60.0 + SEC
		IF ((SET.PERIOD .EQ. '2').AND.(ONCE_PER2 .EQ. 0)) THEN
			WRITE (6,405)
405			FORMAT(' REDUCTION PERIOD')
			WRITE (6,406)
406			FORMAT(' ----------------')
			ONCE_PER2 = 1
			READ (REP(REP_NUM).RINT(1:2),'(F4.0,BN)',
	1			IOSTAT=IOS)MIN
			READ (REP(REP_NUM).RINT(4:5),'(F4.0,BN)',
	1			IOSTAT=IOS)SEC
			INTERVAL_SEC = MIN * 60.0 + SEC
		END IF
C
		IF (N .EQ. 1) THEN
			WRITE (6,410)
410			FORMAT(//,' ',T39,'GAS FLOWS l/min ',
	1		'(H2 sccm/min)')
			WRITE (6,415)
415			FORMAT(' ','Minutes',T13,'ZONE TEMPERATURES',
	1		T37,'----------------------------------')
			WRITE (6,420)
420			FORMAT(' ','Elapsed',T13,'#1',T19,'#2',T25,'#3',
	1		T31,'#4',T38,'CO',T45,'N2',T53,'H2',T60,'CO2',
	1		T66,'TOTAL')
			WRITE (6,425)
425			FORMAT(' ','-----------------------------------'
	1		'----------------------------------')
			WRITE (6,430)
430			FORMAT(' ','PRE-REDUCTION PERIOD')
			WRITE (6,435)
435			FORMAT(' ','--------------------')
			WRITE (6,440)SET.ELAPSED,SET.Z1,SET.Z2,SET.Z3,
	1			SET.Z4,SET.CO,SET.N2,SET.H2,SET.CO2,
	1			SET.GASFLO
          	ELSE IF (MOD(CUR_SEC,INTERVAL_SEC) .EQ. 0) THEN
			WRITE (6,440)SET.ELAPSED,SET.Z1,SET.Z2,SET.Z3,
	1			SET.Z4,SET.CO,SET.N2,SET.H2,SET.CO2,
	1			SET.GASFLO
                END IF
440		FORMAT(' ',A8,3X,A3,3X,A3,3X,A3,3X,A3,3X,A4,3X,A4,3X,
	1		A5,3X,A4,3X,A4)
C
		N = N + 1
		IF (N .LE. TOT_REC) GOTO 400
C
		WRITE (6,4) PRINTOF		
C
		END IF
C-------------------------------------------------------------------------------
C	Turn timer for AST on
C-------------------------------------------------------------------------------
		CALL SYS$BINTIM (DELAY, BIN_DELAY)
		CALL SYS$SETIMR ( ,BIN_DELAY, AST_FURN_RUN,
	1	%VAL(TIMER_ID), )
C
		GOTO 2
	ELSE IF (OPTION .EQ. '4') THEN
		OPEN (UNIT=7,FILE='REDFURN:LTD_NEWTEST.DAT',
	1	STATUS='OLD',ACCESS='KEYED',SHARED)
C
		READ (UNIT=7,KEY=SEQNUMBERS,KEYID=0,IOSTAT=IOS)
		DELETE (UNIT=7)
		CLOSE (UNIT=7)

                CALL LIB$SPAWN ('COPY REDFURN:'//SEQNUMBERS//
	1	'.DAT.1 REDSAVE',,,TRUSTED)
		CALL LIB$SPAWN ('DELETE REDFURN:'//SEQNUMBERS//
	1	'.DAT.1',,,TRUSTED)

                CALL LIB$SPAWN ('COPY REDFURN:'//SEQNUMBERS//
	1	'.PRN.1 REDSAVE',,,TRUSTED)
		CALL LIB$SPAWN ('DELETE REDFURN:'//SEQNUMBERS//
	1	'.PRN.1',,,TRUSTED)
		NONE = 0 
		CALL FDV$AFVA (NONE,'OPTION')
		CALL FDV$PUT (' ','OPTION')
		CALL FDV$SPON
		GOTO 2	
        ELSE IF (OPTION .EQ. '5') THEN
		GOTO 888		
	ELSE 
		CALL FDV$SIGOP
		CALL FDV$PUTL ()
		CALL FDV$PUTL ( 'Invalid option!' )
		CALL FDV$PUT ( ' ','OPTION' )
		GOTO 220
        END IF
	CALL FDV$WAIT	


888     END


******************************************
	SUBROUTINE DATE_YYMMDD (DATE)
******************************************
C
 	INCLUDE '($LIBDTDEF)'
C        INTEGER*4       LIB$INIT_DATE_TIME_CONTEXT,LIB$FORMAT_DATE_TIME,
C     	1               LIB$SIGNAL
       	INTEGER*4       USER_CONTEXT, COMPONENT, STATUS
	CHARACTER*6 	DATE     
       	CHARACTER*80    INIT_STRING, DATE_STRING

C Table of format mnemonics is in the VMS RTL Library (LIB$) Manual,
C pp. 3-5 TO 3-6.

       	INIT_STRING = '|!Y2!MN0!D0 !H04:!M0:!S0.!C2|'
       	USER_CONTEXT = 0
       	COMPONENT = LIB$K_OUTPUT_FORMAT

C  Pass only the initialized portion of the init_string variable
C  to the routine.  Otherwise, you get an invalid initialization
C  string error.

       	STATUS = LIB$INIT_DATE_TIME_CONTEXT (USER_CONTEXT,
     	1                                      COMPONENT,
     	2                                      INIT_STRING(1:29))

       	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))

C  If you default the user_context argument, it uses the logical
C  name LIB$DT_FORMAT for the format.

       	STATUS = LIB$FORMAT_DATE_TIME (DATE_STRING,
     	1                                ,
     	2                                USER_CONTEXT,
     	3                                ,)
       	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))

	DATE = DATE_STRING
	RETURN
       	END

******************************************
	SUBROUTINE DATE_DEF (DATE)
******************************************
C	This subroutine passes back the date in the form MM-DD-YY
C
 	INCLUDE '($LIBDTDEF)'
C        INTEGER*4       LIB$INIT_DATE_TIME_CONTEXT,LIB$FORMAT_DATE_TIME,
C     	1               LIB$SIGNAL
       	INTEGER*4       USER_CONTEXT, COMPONENT, STATUS
	CHARACTER*8 	DATE     
       	CHARACTER*80    INIT_STRING, DATE_STRING

C Table of format mnemonics is in the VMS RTL Library (LIB$) Manual,
C pp. 3-5 TO 3-6.

       	INIT_STRING = '|!MN0-!D0-!Y2 !H04:!M0:!S0.!C2|'
       	USER_CONTEXT = 0
       	COMPONENT = LIB$K_OUTPUT_FORMAT

C  Pass only the initialized portion of the init_string variable
C  to the routine.  Otherwise, you get an invalid initialization
C  string error.

       	STATUS = LIB$INIT_DATE_TIME_CONTEXT (USER_CONTEXT,
     	1                                      COMPONENT,
     	2                                      INIT_STRING(1:31))

       	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))

C  If you default the user_context argument, it uses the logical
C  name LIB$DT_FORMAT for the format.

       	STATUS = LIB$FORMAT_DATE_TIME (DATE_STRING,
     	1                                ,
     	2                                USER_CONTEXT,
     	3                                ,)
       	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))

	DATE = DATE_STRING
	RETURN
       	END


******************************************
	SUBROUTINE STOP_CHECK (FIELDT)
******************************************
C
C	
	INTEGER*4	RED_START,RED_STOP,RED_FIRST_PASS,
	1		RED_DETACH,RED_BEEP,RED_TOGGLE
	INTEGER*4	LTD_START,LTD_STOP,LTD_FIRST_PASS,
	1		LTD_DETACH,LTD_BEEP,LTD_TOGGLE
	INTEGER*4	FIELDT
C
	COMMON /RED_COM/ RED_START,RED_STOP,RED_FIRST_PASS,
	1	RED_DETACH,RED_BEEP,RED_TOGGLE
	COMMON /LTD_COM/ LTD_START,LTD_STOP,LTD_FIRST_PASS,
	1	LTD_DETACH,LTD_BEEP,LTD_TOGGLE
C
	INCLUDE		'REDFURN:KEYDEF'
C
	IF (FIELDT .EQ. 103) THEN
		IF ((RED_STOP .EQ. 1).AND.(LTD_STOP .EQ. 1)) THEN
			CALL FDV$PUTL ()
			CALL FDV$PUTL ('FURNACE #1 is not running')
		ELSE
			RED_STOP = 1
			RED_START = 0
			RED_TOGGLE = 3
			LTD_STOP = 1
			LTD_START = 0
			LTD_TOGGLE = 3
			CALL FDV$PUTL ()
			CALL FDV$PUTL ('Stopping FURNACE #1')
		END IF
	ELSE IF (FIELDT .EQ. 104) THEN
		IF ((RED_START .NE. 1).AND.(LTD_START .NE. 1)) THEN
			CALL FDV$PUTL ()
			CALL FDV$PUTL ('No test running')
		ELSE
		   	RED_START = 0
		   	LTD_START = 0
			CALL FDV$PUTL ()
		      CALL FDV$PUTL ('Stopping test, idling FURNACE #1')
		END IF
	END IF
C
	END

********************************************************************************
	SUBROUTINE FILL_RED_SEQNUM_ARRAY(REP)
********************************************************************************
C
	INTEGER		N,IOS
C
	STRUCTURE /DATA/
		UNION
			MAP
				CHARACTER*1	FURN
                                CHARACTER*8	DATE
				CHARACTER*1	LINE
				CHARACTER*1	SHIFT
				CHARACTER*8	FROMDATE
				CHARACTER*8	TODATE
				CHARACTER*10	SEQ
				CHARACTER*29	OTHER
				CHARACTER*29	TESTER
				CHARACTER*4	IPURGE
				CHARACTER*10	GASTYPE
				CHARACTER*3	Z1
				CHARACTER*3	Z2
				CHARACTER*3	Z3
				CHARACTER*3	Z4
				CHARACTER*3 	TEMP
				CHARACTER*4 	FPURGE
				CHARACTER*5 	PINT
				CHARACTER*3 	MASS
				CHARACTER*5 	FE2
				CHARACTER*5 	FE
				CHARACTER*4 	CO
				CHARACTER*4 	N2
				CHARACTER*4 	GASRATE
				CHARACTER*3 	RZ1
				CHARACTER*3 	RZ2
				CHARACTER*3 	RZ3
				CHARACTER*3 	RZ4
				CHARACTER*5 	RINT
				CHARACTER*5 	LONG
				CHARACTER*5 	FINT
				CHARACTER*2 	STOP
				CHARACTER*3 	IZ1
				CHARACTER*3 	IZ2
				CHARACTER*3 	IZ3
				CHARACTER*5 	T30
				CHARACTER*5 	T60
				CHARACTER*4 	RATE
			END MAP
			MAP
				CHARACTER*240	STRING
			END MAP
		END UNION
	END STRUCTURE
	RECORD /DATA/ REP(26)
C
	N = 1
	DO WHILE (N .LE. 26)
		REP(N).STRING = ' '
		N = N + 1
10	END DO
	
C
	OPEN  (UNIT=3,FILE='REDFURN:RED_NEWTEST.DAT',
	1	STATUS='OLD',ACCESS='KEYED',SHARED)
C
	N = 1
30	READ  (UNIT=3,KEYGE='         ',KEYID=0,IOSTAT=IOS,
	1	ERR=50) REP(N).STRING
	UNLOCK (UNIT=3)
	N = N + 1
C
40	READ  (UNIT=3,END=50) REP(N).STRING
	UNLOCK (UNIT=3)
	N = N + 1
	IF (N .LE. 26) GOTO 40
C
50	IF (IOS .EQ. 52) THEN
		CALL FDV$PUTL ()
		CALL FDV$PUTL ('Record locked!')
		CALL LIB$WAIT (3.0)
		GOTO 30
  	END IF
C
	CLOSE (UNIT=3)
	END

********************************************************************************
	SUBROUTINE FURN_STAT
********************************************************************************
C
	STRUCTURE /RED_DATA/
		CHARACTER*1     FURN      	                          
		CHARACTER*8     DATE                      
		CHARACTER*1     LINE                      
		CHARACTER*1     SHIFT                     
		CHARACTER*8     FROMDATE                  
		CHARACTER*8     TODATE                    
		CHARACTER*10    SEQ                       
		CHARACTER*29    OTHER                     
		CHARACTER*29    TESTER                    
                CHARACTER*5     M0                      
		CHARACTER*5     FE2                       
		CHARACTER*5     FE                    
		CHARACTER*5     MT                        
		CHARACTER*3     Z1                        
		CHARACTER*3     Z2                        
		CHARACTER*3     Z3                        
		CHARACTER*3     Z4                        
		CHARACTER*4     GASFLO                    
		CHARACTER*4     CO                        
		CHARACTER*4     N2                        
		CHARACTER*4     RT                        
		CHARACTER*23    TIME                      
		CHARACTER*1     PERIOD                    
		CHARACTER*3     Z1_SP                     
		CHARACTER*3     Z2_SP                     
		CHARACTER*3     Z3_SP                     
		CHARACTER*4     GASFLO_SP                 
		CHARACTER*4     CO_SP                     
		CHARACTER*4     N2_SP                     
		CHARACTER*3     Z1_OUT                    
		CHARACTER*3     Z2_OUT                    
		CHARACTER*3     Z3_OUT                    
		CHARACTER*4     GASFLO_OUT                
		CHARACTER*4     CO_OUT                    
		CHARACTER*4     N2_OUT                    
		CHARACTER*8	ELAPSED
		CHARACTER*5	M1
	END STRUCTURE
	RECORD /RED_DATA/ RED_HOT
C
	STRUCTURE /LTD_DATA/
		CHARACTER*1	FURN
		CHARACTER*8     DATE                        
		CHARACTER*1     LINE                        
		CHARACTER*1     SHIFT                       
		CHARACTER*8     FROMDATE                    
		CHARACTER*8     TODATE                      
		CHARACTER*10    SEQ                         
		CHARACTER*29    OTHER                       
		CHARACTER*29    TESTER                      
                CHARACTER*5     M0                          
		CHARACTER*5     M1                          
		CHARACTER*5     M2                          
		CHARACTER*5     M3                          
		CHARACTER*3     Z1                          
		CHARACTER*3     Z2                          
		CHARACTER*3     Z3                          
		CHARACTER*3     Z4                          
		CHARACTER*4     GASFLO                      
		CHARACTER*4     CO                          
		CHARACTER*4     N2                          
		CHARACTER*5     H2                          
		CHARACTER*4     CO2                         
		CHARACTER*23    TIME                            		
		CHARACTER*1     PERIOD                      
		CHARACTER*3     Z1_SP                       
		CHARACTER*3     Z2_SP                       
		CHARACTER*3     Z3_SP                       
		CHARACTER*4     GASFLO_SP                   
		CHARACTER*4     CO_SP                       
		CHARACTER*4     N2_SP                       
		CHARACTER*5     H2_SP                       
		CHARACTER*4     CO2_SP                      
		CHARACTER*3     Z1_OUT                      
		CHARACTER*3     Z2_OUT                      
		CHARACTER*3     Z3_OUT                      
		CHARACTER*3     GASFLO_OUT                  
		CHARACTER*3     CO_OUT                      
		CHARACTER*3     N2_OUT                      
		CHARACTER*3     H2_OUT                      
		CHARACTER*3     CO2_OUT                     
		CHARACTER*8     ELAPSED                     
	END STRUCTURE           
	RECORD /LTD_DATA/ LTD_HOT
C
	INTEGER*4	BOLDON
	INTEGER*4	RED_START,RED_STOP,RED_FIRST_PASS,
	1		RED_DETACH,RED_BEEP,RED_TOGGLE
	INTEGER*4	LTD_START,LTD_STOP,LTD_FIRST_PASS,
	1		LTD_DETACH,LTD_BEEP,LTD_TOGGLE
C	
	CHARACTER*23	RED_ELAPSED
	CHARACTER*34	MESSAGE1/'        FURNACE#1 is off          '/
	CHARACTER*34	MESSAGE2/'FURNACE#1 is on - IDLING (RED PAR)'/
	CHARACTER*34	MESSAGE3/' FURNACE#1 is on - REDUCTION TEST '/
	CHARACTER*34	MESSAGE4/'FURNACE#1 is on - IDLING (LTD PAR)'/
	CHARACTER*34	MESSAGE5/'    FURNACE#1 is on - LTD TEST    '/
C                                                  
	COMMON /RED_COM/ RED_START,RED_STOP,RED_FIRST_PASS,
	1		RED_DETACH,RED_BEEP,RED_TOGGLE
	COMMON /LTD_COM/ LTD_START,LTD_STOP,LTD_FIRST_PASS,
	1		LTD_DETACH,LTD_BEEP,LTD_TOGGLE
	COMMON /RED_HOT/ RED_HOT
	COMMON /LTD_HOT/ LTD_HOT
	COMMON /LTD_TIME/ RED_ELAPSED
C
       	BOLDON = 1
	CALL FDV$AFVA (BOLDON,'FURNACE1')
C
	IF (RED_START .EQ. 1) THEN
		CALL FDV$PUT (MESSAGE3,'FURNACE1')
	ELSE IF (LTD_START .EQ. 1) THEN
		CALL FDV$PUT (MESSAGE5,'FURNACE1')
	ELSE IF ((RED_START .EQ. 0).AND.(RED_STOP .EQ. 0)) THEN
		CALL FDV$PUT (MESSAGE2,'FURNACE1')
	ELSE IF ((LTD_START .EQ. 0).AND.(LTD_STOP .EQ. 0)) THEN
		CALL FDV$PUT (MESSAGE4,'FURNACE1')
	ELSE IF ((RED_STOP .EQ. 1).AND.(LTD_STOP .EQ. 1)) THEN
		CALL FDV$PUT (MESSAGE1,'FURNACE1')
	END IF
C
	IF (RED_START .EQ. 1) THEN
		CALL FDV$PUT (RED_HOT.ELAPSED,'ELAPSED')
	ELSE IF (LTD_START .EQ. 1) THEN
		CALL FDV$PUT (LTD_HOT.ELAPSED,'ELAPSED')
		CALL FDV$PUT (RED_ELAPSED(13:20),'RED_ELAPSED')
	ELSE
!		CALL FDV$PUT ('        ','ELAPSED')
	END IF
C
	END	
C
********************************************************************************
	INTEGER*4	FUNCTION PRINTER_STAT()
********************************************************************************
C
C-------------------------------------------------------------------------------
C	This function was copied from Bob W.  It checks to see if the printer
C	is running.
C-------------------------------------------------------------------------------
C
	INCLUDE		'($IODEF)'
	INCLUDE		'($SSDEF)'
C
	INTEGER*2	CHAN,IOSB(4)
	INTEGER*4	PROMPT_LEN/6/,BUF_LEN/20/,STATUS
	INTEGER*4	FUNC
C
	CHARACTER*2	DEV/'TT'/,PTEMP2
	CHARACTER*20	BUF
	CHARACTER*5	PROMPT1/'[?15n'/
	CHARACTER*6	PROMPT
	CHARACTER*1	ESC/27/
C
	FUNC=IO$_READPROMPT.OR.IO$M_PURGE.OR.IO$M_ESCAPE
	1	.OR.IO$M_NOECHO
	PROMPT=ESC//PROMPT1
C
	STATUS=SYS$ASSIGN(DEV,CHAN,,)
C
	STATUS=SYS$QIO(,%VAL(CHAN),
	1	%VAL(FUNC),
	1	%REF(IOSB),,,
	1	%REF(BUF),
	1	%VAL(BUF_LEN),,,
	1	%REF(PROMPT),%VAL(PROMPT_LEN))
C
	CALL LIB$WAIT(1.0)
C
	PTEMP2=BUF(4:5)
C
	IF (PTEMP2 .EQ. '10') THEN
		PRINTER_STAT=1		!Ready
	ELSE IF (PTEMP2 .EQ. '11') THEN
		PRINTER_STAT=2		!Not ready
	ELSE IF (PTEMP2 .EQ. '13') THEN
		PRINTER_STAT=4		!No printer
	ELSE IF (PTEMP2 .EQ. '18') THEN
		PRINTER_STAT=6		!Printer busy
	ELSE 
		PRINTER_STAT=8		!Don't know
	END IF
C
	S=SYS$DASSGN (CHAN)
C
	RETURN
	END	

***************************************************
	SUBROUTINE MINS (ELAPSED_TIME,REAL_TIME)
****************************************************
C
	CHARACTER*2	HRS
	CHARACTER*2	MIN
	CHARACTER*2	SEC	
	CHARACTER*8	ELAPSED_TIME
C
	REAL*4		REAL_TIME
	REAL*4		RHRS
	REAL*4		RMIN
	REAL*4		RSEC
C
	HRS = ELAPSED_TIME (1:2)
	MIN = ELAPSED_TIME (4:5)
	SEC = ELAPSED_TIME (7:8)
C
	READ (HRS,'(F5.0,BN)') RHRS
	READ (MIN,'(F5.0,BN)') RMIN
	READ (SEC,'(F5.0,BN)') RSEC
C
	REAL_TIME = RHRS * 60
	REAL_TIME = REAL_TIME + RMIN + RSEC/60.0
C
	END

*****************************************************************
	SUBROUTINE FORWARD
*****************************************************************
	IMPLICIT	INTEGER(A - Z)
	INTEGER*4	FDV$K_FT_SFW/6/
	PARAMETER	(LIMIT = 300)	  
	CHARACTER*80	V(LIMIT)
C
	COMMON /SCROLL/ MINWINDOW,MAXWINDOW,CURLINE,LASTREG,FIELDT,V
C
	IF (CURLINE .GE. LASTREG) THEN
		CALL FDV$PUTL ( 'Last line of file', 0 )
		RETURN
	END IF
C
	IF (CURLINE .NE. MAXWINDOW) THEN
		CALL FDV$PFT( FDV$K_FT_SFW, 'FAKE' )
	ELSE
		MINWINDOW = MINWINDOW + 1
		MAXWINDOW = MAXWINDOW + 1
		CALL FDV$PFT ( FDV$K_FT_SFW, 'FAKE',V(MAXWINDOW) )	
	END IF
	CURLINE = CURLINE + 1
C
	RETURN
	END

*********************************************************************
	SUBROUTINE PAGE_FORWARD
*********************************************************************
	IMPLICIT	INTEGER(A - Z)
	INTEGER*4	FDV$K_FT_SFW/8/
	PARAMETER	(LIMIT = 300)	  
	CHARACTER*80	V(LIMIT)
C
	COMMON /SCROLL/ MINWINDOW,MAXWINDOW,CURLINE,LASTREG,FIELDT,V
C
	IF (CURLINE .GE. LASTREG) THEN
		CALL FDV$PUTL ( 'Last line of file', 0 )
		RETURN
	END IF
C
	IF (MAXWINDOW .LT. LASTREG) THEN
		I = 1
		DO WHILE ((I .LE. 23).AND.
	1			(MAXWINDOW .LT. LASTREG)) 
			MINWINDOW = MINWINDOW + 1
			MAXWINDOW = MAXWINDOW + 1
			CALL FDV$PFT (FDV$K_FT_SFW, 'FAKE',
	1				V(MAXWINDOW) )	
			CURLINE = CURLINE + 1
			I = I + 1
		END DO
	ELSE
		CALL FDV$PUTL ( 'Last line of file', 0 )
	END IF
C
	RETURN
	END

***********************************************************************
	SUBROUTINE BACK
***********************************************************************
	IMPLICIT	INTEGER(A - Z)
	INTEGER*4	FDV$K_FT_SBK/9/
	PARAMETER	(LIMIT = 300)	  
	CHARACTER*80	V(LIMIT)
C
	COMMON /SCROLL/ MINWINDOW,MAXWINDOW,CURLINE,LASTREG,FIELDT,V
C
	IF (CURLINE .EQ. 1) THEN
		CALL FDV$PUTL ( 'First line of file', 0 )
		RETURN
	END IF
C
	IF (CURLINE .NE. MINWINDOW) THEN
		CALL FDV$PFT( FDV$K_FT_SBK, 'FAKE' )
	ELSE
		MINWINDOW = MINWINDOW - 1
		MAXWINDOW = MAXWINDOW - 1
		CALL FDV$PFT ( FDV$K_FT_SBK, 'FAKE',V(MINWINDOW) )
	END IF
	CURLINE = CURLINE - 1
C
	RETURN
	END

***********************************************************************
	SUBROUTINE PAGE_BACK
***********************************************************************
	IMPLICIT	INTEGER(A - Z)
	INTEGER*4	FDV$K_FT_SBK/9/
	PARAMETER	(LIMIT = 300)	  
	CHARACTER*80	V(LIMIT)
C
	COMMON /SCROLL/ MINWINDOW,MAXWINDOW,CURLINE,LASTREG,FIELDT,V
C
	IF (MINWINDOW .LE. 1) THEN
		CALL FDV$PUTL ( 'First line of file', 0 )
		RETURN
	END IF
C
	IF (MINWINDOW - 23 .LE. 1) THEN
		DO WHILE (MINWINDOW .GT. 1)
			MINWINDOW = MINWINDOW - 1
			MAXWINDOW = MAXWINDOW - 1
			CALL FDV$PFT ( FDV$K_FT_SBK,'FAKE',
	1				V(MINWINDOW) )	
			CURLINE = CURLINE - 1
		END DO
	ELSE
		DO 300 I = 1,23
			MINWINDOW = MINWINDOW - 1
			MAXWINDOW = MAXWINDOW - 1
			CALL FDV$PFT ( FDV$K_FT_SBK,'FAKE',
	1				V(MINWINDOW) )	
			CURLINE = CURLINE - 1
300		CONTINUE
	END IF
C
	RETURN
	END
