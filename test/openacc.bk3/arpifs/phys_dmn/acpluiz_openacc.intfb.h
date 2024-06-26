INTERFACE
  SUBROUTINE ACPLUIZ_OPENACC (YDCST, YDML_PHY_MF, KIDIA, KFDIA, KLON, KTDIA, KLEV, PT, PQ, PQL, PQI, PQR, PQS, PDELP, PAPRSF,  &
  & PCP, PR, PNEBS, PQCS, PNEB_CVPP, PQLI_CVPP, PQC_DET_PCMT, PTENDH, PTENDQ, LDADJCLD, PAPHI, PTS, PNEIJ, PLSM, PGM, YDSTA,  &
  & PFCSQL, PFCSQN, PFPLSL, PFPLSN, PFPEVPL, PFPEVPN, PFPFPL, PFPFPN, PSEDIQL, PSEDIQN, YDSTACK)
!$acc routine( ACPLUIZ_OPENACC )
    
    USE MODEL_PHYSICS_MF_MOD, ONLY: MODEL_PHYSICS_MF_TYPE
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMCST, ONLY: TCST
    USE YOMLSFORC, ONLY: LMUSCLFA, NMUSCLFA
    USE YOMSTA, ONLY: TSTA
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TCST), INTENT(IN) :: YDCST
    TYPE(MODEL_PHYSICS_MF_TYPE), INTENT(IN) :: YDML_PHY_MF
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    REAL(KIND=JPRB), INTENT(IN) :: PT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQ(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQL(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQI(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQR(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQS(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PDELP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PCP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PR(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PNEBS(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PQCS(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PNEB_CVPP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQLI_CVPP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQC_DET_PCMT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PTENDH(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PTENDQ(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPHI(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PTS(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PNEIJ(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PLSM(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PGM(KLON)
    TYPE(TSTA), INTENT(IN) :: YDSTA
    
    LOGICAL, INTENT(IN) :: LDADJCLD
    
    REAL(KIND=JPRB), INTENT(INOUT) :: PFCSQL(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFCSQN(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPLSL(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPLSN(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPEVPL(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPEVPN(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPFPL(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPFPN(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSEDIQL(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSEDIQN(KLON, 0:KLEV)
    
    
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE ACPLUIZ_OPENACC
END INTERFACE
