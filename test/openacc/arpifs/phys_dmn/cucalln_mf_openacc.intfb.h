INTERFACE
  SUBROUTINE CUCALLN_MF_OPENACC (PPLDARE, PPLRG, KSTEP, YDTHF, YDCST, YDERAD, YDML_PHY_SLIN, YDML_PHY_EC, YGFL, YDCHEM,  &
  & YDSPP_CONFIG, YDPERTPAR, KIDIA, KFDIA, KLON, KSMAX, KLEV, PDX, KSPPN2D, LDMCAPEA, LDLAND, LDSLPHY, PTSPHY, PVDIFTS, PTM1,  &
  & PQM1, PUM1, PVM1, PLITOT, PVERVEL, PQHFL, PAHFS, PAPHM1, PAP, PAPH, PGEO, PGEOH, PGAW, PCUCONVCA, PGP2DSPP, PTENT, PTENQ,  &
  & PTENU, PTENV, PTENTA, PTENQA, PARPRC, KTOPC, KBASEC, KTYPE, KCBOT, KCTOP, KBOTSC, LDCUM, LDSC, KCBOT_LIG, KCTOP_LIG,  &
  & LDCUM_LIG, LDSHCV, PLCRIT_AER, PLU, PLUDE, PLUDELI, PSNDE, PMFU, PMFD, PLGLAC, PDIFCQ, PDIFCS, PFHPCL, PFHPCN, PFPLCL,  &
  & PFPLCN, PLRAIN, PRSUD, PSTRCU, PSTRCV, PFCQLF, PFCQIF, PMFUDE_RATE, PMFDDE_RATE, PCAPE, PWU, PWMEAN, PVDISCU, PDISS, KTRAC,  &
  & PCM1, PTENC, PSCAV, PSCAV0, YDSTACK)
!$acc routine( CUCALLN_MF_OPENACC ) seq
    
    USE MODEL_PHYSICS_ECMWF_MOD, ONLY: MODEL_PHYSICS_ECMWF_TYPE
    USE MODEL_PHYSICS_SIMPLINEAR_MOD, ONLY: MODEL_PHYSICS_SIMPLINEAR_TYPE
    USE YOERAD, ONLY: TERAD
    USE YOM_YGFL, ONLY: TYPE_GFLD
    USE YOMCHEM, ONLY: TCHEM
    USE SPP_MOD, ONLY: TSPP_CONFIG
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    
    USE YOMCST, ONLY: TCST
    USE YOETHF, ONLY: TTHF
    USE YOMPERTPAR, ONLY: TPERTPAR
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    
    REAL(KIND=JPRB), INTENT(IN) :: PPLDARE
    REAL(KIND=JPRB), INTENT(IN) :: PPLRG
    INTEGER(KIND=JPIM), INTENT(IN) :: KSTEP
    TYPE(TTHF), INTENT(IN) :: YDTHF
    TYPE(TCST), INTENT(IN) :: YDCST
    TYPE(TERAD), INTENT(IN) :: YDERAD
    TYPE(MODEL_PHYSICS_SIMPLINEAR_TYPE), INTENT(IN) :: YDML_PHY_SLIN
    TYPE(MODEL_PHYSICS_ECMWF_TYPE), INTENT(IN) :: YDML_PHY_EC
    TYPE(TYPE_GFLD), INTENT(IN) :: YGFL
    TYPE(TCHEM), INTENT(IN) :: YDCHEM
    TYPE(TSPP_CONFIG), INTENT(IN) :: YDSPP_CONFIG
    TYPE(TPERTPAR), INTENT(IN) :: YDPERTPAR
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KSMAX
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    INTEGER(KIND=JPIM), INTENT(IN) :: KSPPN2D
    INTEGER(KIND=JPIM), INTENT(IN) :: KTRAC
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    LOGICAL, INTENT(IN) :: LDMCAPEA
    LOGICAL, INTENT(IN) :: LDLAND(KLON)
    LOGICAL, INTENT(IN) :: LDSLPHY
    REAL(KIND=JPRB), INTENT(IN) :: PTSPHY
    REAL(KIND=JPRB), INTENT(IN) :: PVDIFTS
    REAL(KIND=JPRB), INTENT(IN) :: PLCRIT_AER(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PTM1(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQM1(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PUM1(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PVM1(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PCM1(KLON, KLEV, KTRAC)
    REAL(KIND=JPRB), INTENT(IN) :: PLITOT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PVERVEL(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PQHFL(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(IN) :: PAHFS(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(IN) :: PAPHM1(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(IN) :: PAP(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPH(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(IN) :: PGEO(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PGEOH(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(INOUT) :: PTENT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PTENQ(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PTENU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PTENV(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PTENTA(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(INOUT) :: PTENQA(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PGAW(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PCUCONVCA(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PGP2DSPP(KLON, KSPPN2D)
    REAL(KIND=JPRB), INTENT(IN) :: PSCAV(KTRAC)
    REAL(KIND=JPRB), INTENT(IN) :: PSCAV0(KTRAC)
    REAL(KIND=JPRB), INTENT(IN) :: PDX(KLON)
    REAL(KIND=JPRB), INTENT(INOUT) :: PTENC(KLON, KLEV, KTRAC)
    REAL(KIND=JPRB), INTENT(OUT) :: PARPRC(KLON)
    INTEGER(KIND=JPIM), INTENT(OUT) :: KTOPC(KLON)
    INTEGER(KIND=JPIM), INTENT(OUT) :: KBASEC(KLON)
    INTEGER(KIND=JPIM), INTENT(OUT) :: KTYPE(KLON)
    INTEGER(KIND=JPIM), INTENT(OUT) :: KCBOT(KLON)
    INTEGER(KIND=JPIM), INTENT(OUT) :: KCTOP(KLON)
    INTEGER(KIND=JPIM), INTENT(OUT) :: KCBOT_LIG(KLON)
    INTEGER(KIND=JPIM), INTENT(OUT) :: KCTOP_LIG(KLON)
    INTEGER(KIND=JPIM), INTENT(OUT) :: KBOTSC(KLON)
    LOGICAL, INTENT(OUT) :: LDCUM(KLON)
    LOGICAL, INTENT(OUT) :: LDCUM_LIG(KLON)
    LOGICAL, INTENT(OUT) :: LDSC(KLON)
    LOGICAL, INTENT(IN) :: LDSHCV(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PLU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PLUDE(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PLUDELI(KLON, KLEV, 4)
    REAL(KIND=JPRB), INTENT(OUT) :: PSNDE(KLON, KLEV, 2)
    REAL(KIND=JPRB), INTENT(OUT) :: PMFU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PMFD(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PLGLAC(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PDIFCQ(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PDIFCS(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PFHPCL(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PFHPCN(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PFPLCL(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PFPLCN(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PLRAIN(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PRSUD(KLON, KLEV, 2)
    REAL(KIND=JPRB), INTENT(OUT) :: PSTRCU(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PSTRCV(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PFCQLF(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PFCQIF(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PMFUDE_RATE(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PMFDDE_RATE(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PCAPE(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PWMEAN(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PVDISCU(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PWU(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PDISS(KLON, KLEV)
    
    
    
    !-----------------------------------------------------------------------
    
    
    
    
    
    
    !DIR$ VFUNCTION EXPHF
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE CUCALLN_MF_OPENACC
END INTERFACE
