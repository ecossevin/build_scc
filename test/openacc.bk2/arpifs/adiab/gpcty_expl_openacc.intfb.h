INTERFACE
  SUBROUTINE GPCTY_EXPL_OPENACC (YDVFE, YDCVER, KPROMA, KST, KEND, KFLEV, LDRUBC, YDVAB, YDVETA, PU, PV, PD, PEVT, PSPL, PSPM,  &
  & PRPREF, PDPHYCTY, PDELP, PLNPR, PRDELP, PALPH, PRTGR, PRPRE, PRPP, PEVEL, PVVEL, PPSDIV, PPSDVBC, PDIVDP, YDSTACK)
!$acc routine( GPCTY_EXPL_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, JPHOOK, DR_HOOK
    
    USE YOMVERT, ONLY: TVFE, TVAB, TVETA
    USE YOMCVER, ONLY: TCVER
    
    
    
    !     ------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TVFE), INTENT(IN) :: YDVFE
    TYPE(TCVER), INTENT(IN) :: YDCVER
    INTEGER(KIND=JPIM), INTENT(IN) :: KPROMA
    INTEGER(KIND=JPIM), INTENT(IN) :: KST
    INTEGER(KIND=JPIM), INTENT(IN) :: KEND
    INTEGER(KIND=JPIM), INTENT(IN) :: KFLEV
    LOGICAL, INTENT(IN) :: LDRUBC
    TYPE(TVAB), INTENT(IN) :: YDVAB
    TYPE(TVETA), INTENT(IN) :: YDVETA
    REAL(KIND=JPRB), INTENT(IN) :: PU(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PV(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PD(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PEVT(KPROMA)
    REAL(KIND=JPRB), INTENT(IN) :: PSPL(KPROMA)
    REAL(KIND=JPRB), INTENT(IN) :: PSPM(KPROMA)
    REAL(KIND=JPRB), INTENT(IN) :: PRPREF(KPROMA, KFLEV)
    REAL(KIND=JPRB), OPTIONAL, INTENT(IN) :: PDPHYCTY(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PDELP(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PLNPR(KPROMA, KFLEV), PRDELP(KPROMA, KFLEV), PALPH(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PRTGR(KPROMA, KFLEV), PRPRE(KPROMA, KFLEV), PRPP(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PEVEL(KPROMA, 0:KFLEV), PVVEL(KPROMA, 0:KFLEV), PPSDIV(KPROMA, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PPSDVBC(KPROMA, 0:KFLEV), PDIVDP(KPROMA, 0:KFLEV)
    
    !     ------------------------------------------------------------------
    
    
    
    !     ------------------------------------------------------------------
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE GPCTY_EXPL_OPENACC
END INTERFACE
