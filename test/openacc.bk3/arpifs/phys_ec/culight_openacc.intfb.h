INTERFACE
  SUBROUTINE CULIGHT_OPENACC (PPLDARE, PPLRG, YDTHF, YDCST, YDEPHY, YGFL, YDECUMF, KIDIA, KFDIA, KLON, KLEV, PGAW, PGELAT, PAP,  &
  & PAPH, PAPHI, PAPHIF, LDLAND, PT, PLU, PMFU, PCAPE, PFPLCL, PFPLCN, PQPFROZ, LDCUM, KCBOT, KCTOP, LDLINOX, PLIGH_TOT,  &
  & PLIGH_CTG, PCTOPH, PPRECMX, PICE, PCDEPTH, PWMFU, PCHARGE, YDSTACK)
!$acc routine( CULIGHT_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMCST, ONLY: TCST
    USE YOETHF, ONLY: TTHF
    USE YOEPHY, ONLY: TEPHY
    USE YOM_YGFL, ONLY: TYPE_GFLD
    USE YOECUMF, ONLY: TECUMF
    ! USE YOMLUN    , ONLY : NULOUT
    
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    REAL(KIND=JPRB), INTENT(IN) :: PPLDARE
    REAL(KIND=JPRB), INTENT(IN) :: PPLRG
    TYPE(TTHF), INTENT(IN) :: YDTHF
    TYPE(TCST), INTENT(IN) :: YDCST
    TYPE(TEPHY), INTENT(IN) :: YDEPHY
    TYPE(TYPE_GFLD), INTENT(IN) :: YGFL
    TYPE(TECUMF), INTENT(IN) :: YDECUMF
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    REAL(KIND=JPRB), INTENT(IN) :: PGAW(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PGELAT(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPH(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPHIF(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPHI(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PLU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PMFU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PCAPE(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PFPLCL(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PFPLCN(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQPFROZ(KLON, KLEV)
    LOGICAL, INTENT(IN) :: LDCUM(KLON)
    LOGICAL, INTENT(IN) :: LDLAND(KLON)
    INTEGER(KIND=JPIM), INTENT(IN) :: KCBOT(KLON)
    INTEGER(KIND=JPIM), INTENT(IN) :: KCTOP(KLON)
    LOGICAL, INTENT(OUT) :: LDLINOX(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PLIGH_TOT(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PLIGH_CTG(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PCTOPH(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PPRECMX(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PICE(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PCDEPTH(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PWMFU(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PCHARGE(KLON)
    
    
    !             LOCAL STORAGE
    !             ----- -------
    
    
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE CULIGHT_OPENACC
END INTERFACE