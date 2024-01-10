INTERFACE
  SUBROUTINE HL2FL_OPENACC (KIDIA, KFDIA, KLON, KTDIAT, KLEV, PAPRS, PAPRSF, PXHL, KINI, PXFL, YDSTACK)
!$acc routine( HL2FL_OPENACC ) seq
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KTDIAT
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    REAL(KIND=JPRB), INTENT(IN) :: PAPRS(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PXHL(KLON, KINI:KLEV)
    INTEGER(KIND=JPIM), INTENT(IN) :: KINI
    REAL(KIND=JPRB), INTENT(OUT) :: PXFL(KLON, KLEV)
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE HL2FL_OPENACC
END INTERFACE
