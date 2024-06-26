INTERFACE
  SUBROUTINE CUFLXN_OPENACC (YDTHF, YDCST, YDEPHLI, YDECUMF, KIDIA, KFDIA, KLON, KLEV, PTSPHY, PTEN, PQEN, PQSEN, PTENH, PQENH,  &
  & PAPH, PAP, PGEOH, LDLAND, LDCUM, LDTDKMF, KCBOT, KCTOP, KDTOP, KTOPM2, KTYPE, LDDRAF, PMFU, PMFD, PMFUS, PMFDS, PMFUQ,  &
  & PMFDQ, PMFUL, PLUDE, PLUDELI, PLRAIN, PSNDE, PDMFUP, PDMFDP, PDPMEL, PLGLAC, PMFLXR, PMFLXS, PRAIN, PMFUDE_RATE,  &
  & PMFDDE_RATE, YDSTACK)
!$acc routine( CUFLXN_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMCST, ONLY: TCST
    USE YOETHF, ONLY: TTHF
    USE YOEPHLI, ONLY: TEPHLI
    USE YOECUMF, ONLY: TECUMF
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TTHF), INTENT(IN) :: YDTHF
    TYPE(TCST), INTENT(IN) :: YDCST
    TYPE(TECUMF), INTENT(IN) :: YDECUMF
    TYPE(TEPHLI), INTENT(IN) :: YDEPHLI
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    REAL(KIND=JPRB), INTENT(IN) :: PTSPHY
    REAL(KIND=JPRB), INTENT(IN) :: PTEN(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQEN(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PQSEN(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PTENH(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQENH(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPH(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(IN) :: PAP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PGEOH(KLON, KLEV + 1)
    LOGICAL, INTENT(IN) :: LDLAND(KLON)
    LOGICAL, INTENT(IN) :: LDCUM(KLON)
    LOGICAL, INTENT(IN) :: LDTDKMF
    INTEGER(KIND=JPIM), INTENT(IN) :: KCBOT(KLON)
    INTEGER(KIND=JPIM), INTENT(IN) :: KCTOP(KLON)
    INTEGER(KIND=JPIM), INTENT(IN) :: KDTOP(KLON)
    INTEGER(KIND=JPIM), INTENT(OUT) :: KTOPM2
    INTEGER(KIND=JPIM), INTENT(INOUT) :: KTYPE(KLON)
    LOGICAL, INTENT(INOUT) :: LDDRAF(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PMFU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PMFD(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PMFUS(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PMFDS(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PMFUQ(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PMFDQ(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PMFUL(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PLUDE(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PLUDELI(KLON, KLEV, 2)
    REAL(KIND=JPRB), INTENT(IN) :: PLRAIN(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PSNDE(KLON, KLEV, 2)
    REAL(KIND=JPRB), INTENT(INOUT) :: PDMFUP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PDMFDP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PDPMEL(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PLGLAC(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PMFLXR(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PMFLXS(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PRAIN(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PMFUDE_RATE(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PMFDDE_RATE(KLON, KLEV)
    
    
    ! Numerical fit to wet bulb temperature
    
    !DIR$ VFUNCTION EXPHF
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE CUFLXN_OPENACC
END INTERFACE
