INTERFACE
  SUBROUTINE ACDRAG_OPENACC (YDCST, YDML_PHY_MF, KIDIA, KFDIA, KLON, KTDIA, KLEV, PAPRS, PAPRSF, PDELP, PNBVNO, PRDELP, PU, PV,  &
  & PRCORI, PGETRL, PGWDCS, PVRLAN, PVRLDI, PSTRDU, PSTRDV, PRAPTRAJ, YDSTACK)
!$acc routine( ACDRAG_OPENACC )
    
    USE MODEL_PHYSICS_MF_MOD, ONLY: MODEL_PHYSICS_MF_TYPE
    USE PARKIND1, ONLY: JPIM, JPRB, JPRD
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMCST, ONLY: TCST
    
    !-----------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TCST), INTENT(IN) :: YDCST
    TYPE(MODEL_PHYSICS_MF_TYPE), INTENT(IN) :: YDML_PHY_MF
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA
    REAL(KIND=JPRB), INTENT(IN) :: PAPRS(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PDELP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PNBVNO(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PRDELP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PV(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PRCORI(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PGETRL(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PGWDCS(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PVRLAN(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PVRLDI(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PSTRDU(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PSTRDV(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PRAPTRAJ(KLON, 0:KLEV)
    
    
    !-----------------------------------------------------------------------
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE ACDRAG_OPENACC
END INTERFACE
