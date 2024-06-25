INTERFACE
  SUBROUTINE LATTEX_TNT_OPENACC (YDGEOMETRY, YDLDDH, YDDYN, KST, KEND, KXLAG, PESGP, PESGM, PXT9, PMOY1X, PXSI9, PXSI0, PXT1,  &
  & PXL0, PXL9, PXLF9, PSIDDHXT1, PSIDDHXL0, YDSTACK)
!$acc routine( LATTEX_TNT_OPENACC ) seq
    
    USE GEOMETRY_MOD, ONLY: GEOMETRY
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    USE YOMDYN, ONLY: TDYN
    USE YOMLDDH, ONLY: TLDDH
    
    !     ------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(GEOMETRY), INTENT(IN) :: YDGEOMETRY
    TYPE(TDYN), INTENT(IN) :: YDDYN
    TYPE(TLDDH), INTENT(IN) :: YDLDDH
    INTEGER(KIND=JPIM), INTENT(IN) :: KST
    INTEGER(KIND=JPIM), INTENT(IN) :: KEND
    INTEGER(KIND=JPIM), INTENT(IN) :: KXLAG
    REAL(KIND=JPRB), INTENT(IN) :: PESGP
    REAL(KIND=JPRB), INTENT(IN) :: PESGM
    REAL(KIND=JPRB), INTENT(IN) :: PXT9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(IN) :: PMOY1X(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(IN) :: PXSI9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXSI0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXT1(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXL0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXL9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXLF9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSIDDHXT1(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSIDDHXL0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
    
    !     ------------------------------------------------------------------
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE LATTEX_TNT_OPENACC
END INTERFACE
