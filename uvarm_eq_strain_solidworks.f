      SUBROUTINE UVARM(UVAR,DIRECT,T,TIME,DTIME,CMNAME,ORNAME,
     1 NUVARM,NOEL,NPT,LAYER,KSPT,KSTEP,KINC,NDI,NSHR,COORD,
     2 JMAC,JMATYP,MATLAYO,LACCFLA) 
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3 FLGRAY(15)
      DIMENSION UVAR(NUVARM),DIRECT(3,3),T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),JMAC(*),JMATYP(*),COORD(*)
C      Parameter(zero=0.d0)
      REAL Estar, Eone, Etwo
C
C
C Error counter:
      JERROR = 0
C
C Get strain tensor:
      CALL GETVRM('E',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,
     1 MATLAYO,LACCFLA)
      JERROR = JERROR + JRCD
C
C Double index components are returned in the order 11, 22, 33, 12, 13, 23 for symmetric tensors
C
C Solidworks equivalent strain
      Estar = (ARRAY(1)+ARRAY(2)+ARRAY(3))/3
      Eone = 0.5*((ARRAY(1)-Estar)**2 + (ARRAY(2)-Estar)**2 + (ARRAY(3)-Estar)**2)
      Etwo = (ARRAY(4)**2 + ARRAY(5)**2 + ARRAY(6)**2)/4
      UVAR(1) = 2*sqrt((Eone+Etwo)/3)
C
C when error appears, write comment to .DAT file
      IF(JERROR.NE.0)THEN
        WRITE(6,*) 'REQUEST ERROR IN UVARM FOR ELEMENT NUMBER ',
     1      NOEL,'INTEGRATION POINT NUMBER ',NPT
      ENDIF
      RETURN
      END