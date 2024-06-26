INTERFACE
  SUBROUTINE GPHPRE_EXPL_VERTFE1_OPENACC (YDCVER, TOPPRES, YDCST, KPROMA, KFLEV, KST, KEND, YDVAB, PRESH, PRESF, LHSET, LDELP,  &
  & LALPHA, LRTGR, LRPP, PDELP, PLNPR, PRDELP, PALPH, PRTGR, PRPRE, PRPP, YDSTACK)
!$acc routine( GPHPRE_EXPL_VERTFE1_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, JPHOOK, DR_HOOK
    
    USE YOMCST, ONLY: TCST
    USE YOMVERT, ONLY: TVAB
    USE YOMCVER, ONLY: TCVER
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TCVER), INTENT(IN) :: YDCVER
    REAL(KIND=JPRB), INTENT(IN) :: TOPPRES
    TYPE(TCST), INTENT(IN) :: YDCST
    INTEGER(KIND=JPIM), INTENT(IN) :: KPROMA, KFLEV, KST, KEND
    TYPE(TVAB), INTENT(IN) :: YDVAB
    REAL(KIND=JPRB), INTENT(INOUT) :: PRESH(KPROMA, 0:KFLEV)
    REAL(KIND=JPRB), OPTIONAL, TARGET, INTENT(OUT) :: PRESF(KPROMA, KFLEV)
    LOGICAL, OPTIONAL, INTENT(IN) :: LHSET, LDELP, LALPHA, LRTGR, LRPP
    REAL(KIND=JPRB), OPTIONAL, TARGET, INTENT(OUT) :: PDELP(KPROMA, KFLEV), PLNPR(KPROMA, KFLEV), PRDELP(KPROMA, KFLEV),  &
    & PALPH(KPROMA, KFLEV)
    REAL(KIND=JPRB), OPTIONAL, TARGET, INTENT(OUT) :: PRTGR(KPROMA, KFLEV), PRPRE(KPROMA, KFLEV), PRPP(KPROMA, KFLEV)
    
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE GPHPRE_EXPL_VERTFE1_OPENACC
END INTERFACE
