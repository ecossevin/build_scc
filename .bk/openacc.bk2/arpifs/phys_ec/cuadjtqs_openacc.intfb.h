INTERFACE
  SUBROUTINE CUADJTQS_OPENACC (YDTHF, YDCST, KIDIA, KFDIA, KLON, KLEV, KK, PSP, PT, PQ, LDFLAG, KCALL, YDSTACK)
!$acc routine( CUADJTQS_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMCST, ONLY: TCST
    USE YOETHF, ONLY: TTHF
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    TYPE(TTHF), INTENT(IN) :: YDTHF
    TYPE(TCST), INTENT(IN) :: YDCST
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KK
    REAL(KIND=JPRB), INTENT(IN) :: PSP(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PQ(KLON, KLEV)
    LOGICAL, INTENT(IN) :: LDFLAG(KLON)
    INTEGER(KIND=JPIM), INTENT(IN) :: KCALL
    
    
    
    !DIR$ VFUNCTION EXPHF
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE CUADJTQS_OPENACC
END INTERFACE