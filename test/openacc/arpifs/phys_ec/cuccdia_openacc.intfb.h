INTERFACE
  SUBROUTINE CUCCDIA_OPENACC (YDERAD, YDEPHLI, YDEPHY, KIDIA, KFDIA, KLON, KLEV, KSTEP, KCBOT, KCTOP, LDCUM, PQU, PLU, PMFU,  &
  & PRAIN, PARPRC, KTOPC, KBASEC, YDSTACK)
!$acc routine( CUCCDIA_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOEPHY, ONLY: TEPHY
    USE YOERAD, ONLY: TERAD
    USE YOEPHLI, ONLY: TEPHLI
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TEPHLI), INTENT(IN) :: YDEPHLI
    TYPE(TEPHY), INTENT(IN) :: YDEPHY
    TYPE(TERAD), INTENT(IN) :: YDERAD
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KSTEP
    INTEGER(KIND=JPIM), INTENT(IN) :: KCBOT(KLON)
    INTEGER(KIND=JPIM), INTENT(IN) :: KCTOP(KLON)
    LOGICAL, INTENT(IN) :: LDCUM(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PQU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PLU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PMFU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PRAIN(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PARPRC(KLON)
    INTEGER(KIND=JPIM), INTENT(INOUT) :: KTOPC(KLON)
    INTEGER(KIND=JPIM), INTENT(INOUT) :: KBASEC(KLON)
    !     ------------------------------------------------------------------
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE CUCCDIA_OPENACC
END INTERFACE
