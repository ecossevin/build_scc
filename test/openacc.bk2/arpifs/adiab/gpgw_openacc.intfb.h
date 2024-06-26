INTERFACE
  SUBROUTINE GPGW_OPENACC (YDGEOMETRY, LDNHDYN, KFLEV, KPROMA, KST, KEND, LDGWF, LDGDWI, POROGL, POROGM, PLNPR, PALPH, PUS, PVS,  &
  & PRT, PDVER, PGWH, PGWF, LDVFE, PRNHPPI, PGDW, YDSTACK)
!$acc routine( GPGW_OPENACC )
    
    USE GEOMETRY_MOD, ONLY: GEOMETRY
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    ! -----------------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(GEOMETRY), INTENT(IN) :: YDGEOMETRY
    LOGICAL, INTENT(IN) :: LDNHDYN
    INTEGER(KIND=JPIM), INTENT(IN) :: KFLEV, KPROMA, KST, KEND
    LOGICAL, INTENT(IN) :: LDGWF, LDGDWI
    REAL(KIND=JPRB), INTENT(IN) :: POROGL(KPROMA)
    REAL(KIND=JPRB), INTENT(IN) :: POROGM(KPROMA)
    REAL(KIND=JPRB), INTENT(IN) :: PLNPR(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PALPH(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PUS(KPROMA)
    REAL(KIND=JPRB), INTENT(IN) :: PVS(KPROMA)
    REAL(KIND=JPRB), INTENT(IN) :: PRT(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PDVER(KPROMA, KFLEV)
    REAL(KIND=JPRB), TARGET, INTENT(OUT) :: PGWH(KPROMA, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PGWF(KPROMA, KFLEV)
    LOGICAL, OPTIONAL, INTENT(IN) :: LDVFE
    REAL(KIND=JPRB), OPTIONAL, INTENT(IN) :: PRNHPPI(KPROMA, KFLEV)
    REAL(KIND=JPRB), OPTIONAL, TARGET, INTENT(OUT) :: PGDW(KPROMA, KFLEV)
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE GPGW_OPENACC
END INTERFACE
