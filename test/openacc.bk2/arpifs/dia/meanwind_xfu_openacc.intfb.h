INTERFACE
  SUBROUTINE MEANWIND_XFU_OPENACC (KLON, KIDIA, KFDIA, KMEANSTEPS, PXFU, PXFV, PXU, PXV, PMWINDCALC, ZUM, ZVM, YDSTACK)
!$acc routine( MEANWIND_XFU_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: DR_HOOK, LHOOK, JPHOOK
    
    ! The algorithm is the same as the one used to compute observed mean wind
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KMEANSTEPS
    REAL(KIND=JPRB), INTENT(IN) :: PXFU(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PXFV(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PXU(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PXV(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PMWINDCALC(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: ZUM(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: ZVM(KLON)
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE MEANWIND_XFU_OPENACC
END INTERFACE
