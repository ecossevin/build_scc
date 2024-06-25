INTERFACE
  SUBROUTINE ACTQSAT_OPENACC (YDCST, YDPHY, KIDIA, KFDIA, KLON, KTDIA, KLEV, PAPRSF, PCP, PQ, PT, PGEOSLC, PLH, PLSCPE, PQSAT,  &
  & PQW, PRH, PTW, YDSTACK)
!$acc routine( ACTQSAT_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMPHY, ONLY: TPHY
    USE YOMCST, ONLY: TCST
    
    !-----------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TCST), INTENT(IN) :: YDCST
    TYPE(TPHY), INTENT(IN) :: YDPHY
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA
    REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PCP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQ(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PGEOSLC(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PLH(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PLSCPE(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PQSAT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PQW(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PRH(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PTW(KLON, KLEV)
    
    !-----------------------------------------------------------------------
    
    
    
    
    !-----------------------------------------------------------------------
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE ACTQSAT_OPENACC
END INTERFACE
