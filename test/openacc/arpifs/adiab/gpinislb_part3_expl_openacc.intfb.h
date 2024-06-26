INTERFACE
  SUBROUTINE GPINISLB_PART3_EXPL_OPENACC (YDGEOMETRY, YDDYNA, KST, KEND, PVVEL0, PZPRE0F, PB2VVEL, PB2GWF, PB2GDW, PB2GWS,  &
  & PGWFT0, PGDW0, PGWS0, YDSTACK)
!$acc routine( GPINISLB_PART3_EXPL_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: DR_HOOK, LHOOK, JPHOOK
    
    USE GEOMETRY_MOD, ONLY: GEOMETRY
    USE YOMDYNA, ONLY: TDYNA
    
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(GEOMETRY), INTENT(IN) :: YDGEOMETRY
    TYPE(TDYNA), INTENT(IN) :: YDDYNA
    INTEGER(KIND=JPIM), INTENT(IN) :: KST
    INTEGER(KIND=JPIM), INTENT(IN) :: KEND
    REAL(KIND=JPRB), INTENT(IN) :: PVVEL0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(IN) :: PZPRE0F(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PB2VVEL(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PB2GWF(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PB2GDW(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PB2GWS(YDGEOMETRY%YRDIM%NPROMA)
    REAL(KIND=JPRB), OPTIONAL, INTENT(IN) :: PGWFT0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), OPTIONAL, INTENT(IN) :: PGDW0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), OPTIONAL, INTENT(IN) :: PGWS0(YDGEOMETRY%YRDIM%NPROMA)
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE GPINISLB_PART3_EXPL_OPENACC
END INTERFACE
