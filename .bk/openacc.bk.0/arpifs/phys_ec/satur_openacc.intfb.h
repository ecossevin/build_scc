INTERFACE
  SUBROUTINE SATUR_OPENACC (YDTHF, YDCST, KIDIA, KFDIA, KLON, KTDIA, KLEV, LDPHYLIN, PAPRSF, PT, PQSAT, KFLAG, YDSTACK)
!$acc routine( SATUR_OPENACC ) seq
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMCST, ONLY: TCST
    USE YOETHF, ONLY: TTHF
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TTHF), INTENT(IN) :: YDTHF
    TYPE(TCST), INTENT(IN) :: YDCST
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    LOGICAL, INTENT(IN) :: LDPHYLIN
    REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PQSAT(KLON, KLEV)
    INTEGER(KIND=JPIM), INTENT(IN) :: KFLAG
    
    
    !DIR$ VFUNCTION EXPHF
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE SATUR_OPENACC
END INTERFACE
