INTERFACE
  SUBROUTINE DPRECIPS_XFU_OPENACC (KIDIA, KFDIA, KLON, KDTPREC, KSTATS, PDPRECIPS, PXPTYPE, LDRESET, YDSTACK)
!$acc routine( DPRECIPS_XFU_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    !     ------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KDTPREC
    INTEGER(KIND=JPIM), INTENT(IN) :: KSTATS
    REAL(KIND=JPRB), INTENT(IN) :: PDPRECIPS(KLON, KDTPREC)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXPTYPE(KLON)
    LOGICAL, INTENT(IN) :: LDRESET
    
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE DPRECIPS_XFU_OPENACC
END INTERFACE