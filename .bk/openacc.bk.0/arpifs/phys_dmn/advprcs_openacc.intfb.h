INTERFACE
  SUBROUTINE ADVPRCS_OPENACC (YDCST, YDML_PHY_MF, KIDIA, KFDIA, KLON, KTDIA, KFLEV, PT, PQ, PQL, PQI, PAUTOL, PAUTOI, PQR, PQS,  &
  & PNEB, PCP, PR, PAPHI, PAPRSF, PDELP, PFPLSL, PFPLSN, PFPEVPL, PFPEVPN, PFPFPL, PFPFPN, PSEDIQL, PSEDIQN, YDSTACK)
!$acc routine( ADVPRCS_OPENACC ) seq
    
    USE MODEL_PHYSICS_MF_MOD, ONLY: MODEL_PHYSICS_MF_TYPE
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMCST, ONLY: TCST
    
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TCST), INTENT(IN) :: YDCST
    TYPE(MODEL_PHYSICS_MF_TYPE), INTENT(IN) :: YDML_PHY_MF
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFLEV
    REAL(KIND=JPRB), INTENT(IN) :: PT(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQ(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQL(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQI(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAUTOL(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAUTOI(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQR(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQS(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PNEB(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PCP(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PR(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPHI(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PDELP(KLON, KFLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPLSL(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPLSN(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPEVPL(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPEVPN(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPFPL(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PFPFPN(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSEDIQL(KLON, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSEDIQN(KLON, 0:KFLEV)
    
    !REAL(KIND=JPRB), EXTERNAL :: FCGENERALIZED_GAMMA
    
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE ADVPRCS_OPENACC
END INTERFACE