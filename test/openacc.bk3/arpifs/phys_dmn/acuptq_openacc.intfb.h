INTERFACE
  SUBROUTINE ACUPTQ_OPENACC (YDCST, KLON, KIDIA, KFDIA, KFLEV, LDPTQ, PFRSO, PFRTH, PDIFCQ, PDIFCS, PDIFTQ, PDIFTS, PFCCQL,  &
  & PFCCQN, PFPLCL, PFPLCN, PFECL, PFECN, PFACL, PFACN, PRDELP, PT, PQ, PTS, PTENDH, PTENDQ, YDSTACK)
!$acc routine( ACUPTQ_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMCST, ONLY: TCST
    
    !-----------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TCST), INTENT(IN) :: YDCST
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFLEV
    LOGICAL, INTENT(IN) :: LDPTQ
    REAL(KIND=JPRB), INTENT(IN) :: PFRSO(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFRTH(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PDIFCQ(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PDIFCS(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PDIFTQ(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PDIFTS(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFCCQL(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFCCQN(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFPLCL(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFPLCN(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFECL(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFECN(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFACL(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFACN(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PRDELP(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PT(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQ(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PTS(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PTENDH(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PTENDQ(KLON, KFLEV)
    !-----------------------------------------------------------------------
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE ACUPTQ_OPENACC
END INTERFACE
