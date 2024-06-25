INTERFACE
  SUBROUTINE GPGEO_EXPL_OPENACC (KPROMA, KST, KEND, KFLEV, PHI, PHIF, PT, PR, PLNPR, PALPH, YDVGEOM, YDSTACK)
!$acc routine( GPGEO_EXPL_OPENACC ) seq
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, JPHOOK, DR_HOOK
    
    USE YOMVERT, ONLY: TVERTICAL_GEOM
    
    !     ------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    INTEGER(KIND=JPIM), INTENT(IN) :: KPROMA
    INTEGER(KIND=JPIM), INTENT(IN) :: KST
    INTEGER(KIND=JPIM), INTENT(IN) :: KEND
    INTEGER(KIND=JPIM), INTENT(IN) :: KFLEV
    REAL(KIND=JPRB), INTENT(INOUT) :: PHI(KPROMA, 0:KFLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PHIF(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PT(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PR(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PLNPR(KPROMA, KFLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PALPH(KPROMA, KFLEV)
    TYPE(TVERTICAL_GEOM), INTENT(IN) :: YDVGEOM
    
    !     ------------------------------------------------------------------
    
    
    !     ------------------------------------------------------------------
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE GPGEO_EXPL_OPENACC
END INTERFACE
