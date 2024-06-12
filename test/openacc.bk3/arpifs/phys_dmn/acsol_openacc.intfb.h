INTERFACE
  SUBROUTINE ACSOL_OPENACC (YDCLI, YDCST, YDPHY, YDPHY1, KIDIA, KFDIA, KLON, PARG, PD2, PGZ0F, PGZ0HF, PGZ0RLF, PLSM, PIVEG,  &
  & PLAI, PALBNS, PRHONS, PSAB, PSNS, PTS, PVEG0, PWP, PWPI, PWS, PWSI, LDHMT, PC1, PC2, PC3, PCG, PCN, PCT, PNEIJG, PNEIJV,  &
  & PWFC, PWPMX, PWSEQ, PWSMX, PWWILT, YDSTACK)
!$acc routine( ACSOL_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMPHY1, ONLY: TPHY1
    USE YOMPHY, ONLY: TPHY
    USE YOMCST, ONLY: TCST
    USE YOMCLI, ONLY: TCLI
    
    !-----------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TCLI), INTENT(IN) :: YDCLI
    TYPE(TCST), INTENT(IN) :: YDCST
    TYPE(TPHY), INTENT(IN) :: YDPHY
    TYPE(TPHY1), INTENT(IN) :: YDPHY1
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    REAL(KIND=JPRB), INTENT(IN) :: PARG(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PD2(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PGZ0F(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PGZ0HF(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PGZ0RLF(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PLSM(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PIVEG(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PLAI(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PALBNS(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PRHONS(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PSAB(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PSNS(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PTS(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PVEG0(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PWP(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PWPI(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PWS(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PWSI(KLON)
    LOGICAL, INTENT(IN) :: LDHMT
    REAL(KIND=JPRB), INTENT(INOUT) :: PC1(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PC2(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PC3(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PCG(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PCN(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PCT(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PNEIJG(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PNEIJV(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PWFC(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PWPMX(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PWSEQ(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PWSMX(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PWWILT(KLON)
    
    
    
    !-----------------------------------------------------------------------
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE ACSOL_OPENACC
END INTERFACE
