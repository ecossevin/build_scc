INTERFACE
  SUBROUTINE GPTF2_EXPL_2TL_OPENACC (YDGEOMETRY, KST, KEND, LDFSTEP, P0U, P0V, P9U, P9V, YDSTACK)
!$acc routine( GPTF2_EXPL_2TL_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: DR_HOOK, LHOOK, JPHOOK
    
    USE GEOMETRY_MOD, ONLY: GEOMETRY
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(GEOMETRY), INTENT(IN) :: YDGEOMETRY
    INTEGER(KIND=JPIM), INTENT(IN) :: KST
    INTEGER(KIND=JPIM), INTENT(IN) :: KEND
    LOGICAL, INTENT(IN) :: LDFSTEP
    REAL(KIND=JPRB), OPTIONAL, INTENT(INOUT) :: P0U(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), OPTIONAL, INTENT(INOUT) :: P0V(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), OPTIONAL, INTENT(INOUT) :: P9U(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), OPTIONAL, INTENT(INOUT) :: P9V(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE GPTF2_EXPL_2TL_OPENACC
END INTERFACE