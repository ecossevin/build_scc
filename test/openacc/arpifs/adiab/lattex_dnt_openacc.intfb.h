INTERFACE
  SUBROUTINE LATTEX_DNT_OPENACC (KSTEP, YDGEOMETRY, YDLDDH, YDRIP, YDDYN, YDDYNA, KST, KEND, LDSETTLS, KXLAG, PESGP, PESGM,  &
  & PXT0, PXT9, PMOY1X, PMIXNL, PXSI, PXNLT9, PXT1, PXL0, PXL9, PXLF9, PCXNLT9, PSIDDHXT1, PSIDDHXT9, PSIDDHXL0, PXLF0, LDNESC,  &
  & YDSTACK)
!$acc routine( LATTEX_DNT_OPENACC ) seq
    
    USE GEOMETRY_MOD, ONLY: GEOMETRY
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: DR_HOOK, JPHOOK, LHOOK
    USE YOMDYNA, ONLY: TDYNA
    
    USE YOMDYN, ONLY: TDYN
    USE YOMRIP, ONLY: TRIP
    USE YOMLDDH, ONLY: TLDDH
    
    !     ------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    INTEGER(KIND=JPIM), INTENT(IN) :: KSTEP
    TYPE(GEOMETRY), INTENT(IN) :: YDGEOMETRY
    TYPE(TLDDH), INTENT(IN) :: YDLDDH
    TYPE(TRIP), INTENT(IN) :: YDRIP
    TYPE(TDYN), INTENT(IN) :: YDDYN
    TYPE(TDYNA), INTENT(IN) :: YDDYNA
    INTEGER(KIND=JPIM), INTENT(IN) :: KST
    INTEGER(KIND=JPIM), INTENT(IN) :: KEND
    LOGICAL, INTENT(IN) :: LDSETTLS
    INTEGER(KIND=JPIM), INTENT(IN) :: KXLAG
    REAL(KIND=JPRB), INTENT(IN) :: PESGP
    REAL(KIND=JPRB), INTENT(IN) :: PESGM
    REAL(KIND=JPRB), INTENT(IN) :: PXT0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(IN) :: PXT9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(IN) :: PMOY1X(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(IN) :: PMIXNL(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXSI(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXNLT9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXT1(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXL0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXL9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
    REAL(KIND=JPRB), INTENT(INOUT) :: PXLF9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
    REAL(KIND=JPRB), INTENT(INOUT) :: PCXNLT9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSIDDHXT1(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSIDDHXT9(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSIDDHXL0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
    REAL(KIND=JPRB), INTENT(OUT) :: PXLF0(YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
    LOGICAL, OPTIONAL, INTENT(IN) :: LDNESC
    
    !     ------------------------------------------------------------------
    
    
    !     * ZXNLT0 (resp. ZXNLT1) the non linear term at t (resp. t+dt).
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE LATTEX_DNT_OPENACC
END INTERFACE
