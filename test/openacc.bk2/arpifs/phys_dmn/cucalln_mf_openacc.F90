SUBROUTINE CUCALLN_MF_OPENACC (PPLDARE, PPLRG, KSTEP, YDTHF, YDCST, YDERAD, YDML_PHY_SLIN, YDML_PHY_EC, YGFL, YDCHEM,  &
& YDSPP_CONFIG, KIDIA, KFDIA, KLON, KSMAX, KLEV, PDX, KSPPN2D, LDMCAPEA, LDLAND, LDSLPHY, PTSPHY, PVDIFTS, PTM1, PQM1, PUM1,  &
& PVM1, PLITOT, PVERVEL, PQHFL, PAHFS, PAPHM1, PAP, PAPH, PGEO, PGEOH, PGAW, PCUCONVCA, PGP2DSPP, PTENT, PTENQ, PTENU, PTENV,  &
& PTENTA, PTENQA, PARPRC, KTOPC, KBASEC, KTYPE, KCBOT, KCTOP, KBOTSC, LDCUM, LDSC, KCBOT_LIG, KCTOP_LIG, LDCUM_LIG, LDSHCV,  &
& PLCRIT_AER, PLU, PLUDE, PLUDELI, PSNDE, PMFU, PMFD, PDIFCQ, PDIFCS, PFHPCL, PFHPCN, PFPLCL, PFPLCN, PLRAIN, PRSUD, PSTRCU,  &
& PSTRCV, PFCQLF, PFCQIF, PMFUDE_RATE, PMFDDE_RATE, PCAPE, PWMEAN, PVDISCU, PDISS, KTRAC, PCM1, PTENC, PSCAV, PSCAV0, YDSTACK)
  
  !          *CUCALL* - MASTER ROUTINE - PROVIDES INTERFACE FOR:
  !                     *CUMASTR* (CUMULUS PARAMETERIZATION)
  !                     *CUCCDIA* (CUMULUS CLOUDS FOR RADIATION)
  !                     *CUSTRAT* (PBL_STRATOCUMULUS)
  
  !**   PURPOSE.
  !     --------
  
  !          *CUCALL* - INTERFACE FOR *CUMASTR*,*CUCCDIA* AND *CUSTRAT*:
  !                     PROVIDES INPUT FOR CUMASTR, CUCCDIA AND CUSTRAT.
  !                     RECEIVES UPDATED TENDENCIES, PRECIPITATION
  !                     AND CONVECTIVE CLOUD PARAMETERS FOR RADIATION.
  
  !**   INTERFACE.
  !     ----------
  
  !          *CUCALL* IS CALLED FROM *CALLPAR*
  
  !     PARAMETER     DESCRIPTION                                   UNITS
  !     ---------     -----------                                   -----
  !     INPUT PARAMETERS (INTEGER):
  
  !    *KIDIA*        START POINT
  !    *KFDIA*        END POINT
  !    *KLON*         NUMBER OF GRID POINTS PER PACKET
  !    *KSMAX*        HORIZONTAL SPECTRAL TRUNCATION
  !    *KLEV*         NUMBER OF LEVELS
  !    *KSPPN2D*      Number of 2D patterns in SPP scheme
  !    *KTRAC*        NUMBER OF CHEMICAL TRACERS
  
  !     INPUT PARAMETERS (LOGICAL)
  
  !    *LDLAND*       LAND SEA MASK (.TRUE. FOR LAND)
  
  !     INPUT PARAMETERS (REAL)
  
  !    *PTSPHY*       TIME STEP FOR THE PHYSICS                     S
  !    *PTM1*         TEMPERATURE (T-1)                             K
  !    *PQM1*         SPECIFIC HUMIDITY (T-1)                       KG/KG
  !    *PUM1*         X-VELOCITY COMPONENT (T-1)                    M/S
  !    *PVM1*         Y-VELOCITY COMPONENT (T-1)                    M/S
  !    *PCM1*         CHEMICAL TRACERS (T-1)                        KG/KG
  !    *PLITOT*       GRID MEAN LIQUID WATER + ICE CONTENT          KG/KG
  !    *PVERVEL*      VERTICAL VELOCITY                             PA/S
  !    *PQHFL*        MOISTURE FLUX (EXCEPT FROM SNOW EVAP.)     KG/(SM2)
  !    *PAHFS*        SENSIBLE HEAT FLUX                            W/M2
  !    *PAPHM1*       PRESSURE ON HALF LEVELS                       PA
  !    *PAP*          PROVISIONAL PRESSURE ON FULL LEVELS           PA
  !    *PAPH*         PROVISIONAL PRESSURE ON HALF LEVELS           PA
  !    *PGEO*         GEOPOTENTIAL                                  M2/S2
  !    *PGEOH*        GEOPOTENTIAL ON HALF LEVELS                   M2/S2
  !    *PGAW*       NORMALISED GAUSSIAN QUADRATURE WEIGHT / NUMBER OF LONGITUDE POINTS
  !                           LOCAL SUB-AREA == 4*RPI*RA**2 * PGAW
  !    *PLCRIT_AER*   CRITICAL LIQUID MMR FOR AUTOCONVERSION PROCESS KG/KG
  !    *PCUCONVCA*    COUPLING TO CELL AUTOMATON OR 2D ADVECT FIELD ( )
  !    *PGP2DSPP*     Standard stochastic variable (mean=0, SD=1)
  
  !    UPDATED PARAMETERS (REAL):
  
  !     tendency_cml    cumulative tendency used for final output
  !     tendency_tmp    cumulative tendency used as input
  !     tendency_loc    local tendency from convection
  
  !    *PTENC*        TENDENCY OF CHEMICAL TRACERS                 1/S
  !    *PARPRC*       ACCUMULATED PRECIPITATION AMMOUNT           KG/(M2*S)
  !                   FOR RADIATION CALCULATION
  
  !    UPDATED PARAMETERS (INTEGER):
  
  !    *KTOPC*        UPDATED CONVECTIVE CLOUD TOP LEVEL FOR RADIATION
  !    *KBASEC*       UPDATED CONVECTIVE CLOUD BASE LEVEL FOR RADIATION
  
  !    OUTPUT PARAMETERS (INTEGER):
  
  !    *KTYPE*        TYPE OF CONVECTION
  !                       1 = PENETRATIVE CONVECTION
  !                       2 = SHALLOW CONVECTION
  !                       3 = MIDLEVEL CONVECTION
  !    *KCBOT*        CLOUD BASE LEVEL
  !    *KCTOP*        CLOUD TOP LEVEL
  !    *KBOTSC*       CLOUD BASE LEVEL FOR SC-CLOUDS
  !    *KCBOT_LIG*    CLOUD BASE LEVEL (FOR LIGHTNING PARAM)
  !    *KCTOP_LIG*    CLOUD TOP LEVEL (FOR LIGHTNING PARAM)
  
  !    OUTPUT PARAMETERS (LOGICAL):
  
  !    *LDCUM*        FLAG: .TRUE. FOR CONVECTIVE POINTS
  !    *LDCUM_LIG*    FLAG: .TRUE. FOR CONVECTIVE POINTS (FOR LIGHTNING PARAM)
  !    *LDSC*         FLAG: .TRUE. FOR SC-POINTS
  
  !    OUTPUT PARAMETERS (REAL):
  
  !    *PLU*          LIQUID WATER CONTENT IN UPDRAFTS            KG/KG
  !    *PLUDE*        DETRAINED TOTAL CONDENSATE                  KG/(M2*S)
  !    *PLUDELI*      DETRAINED LIQUID, ICE                       KG/(M2*S)
  !    *PSNDE*        DETRAINED SNOW/RAIN                         KG/(M2*S)
  !    *PMFU*         MASSFLUX UPDRAFTS                           KG/(M2*S)
  !    *PMFD*         MASSFLUX DOWNDRAFTS                         KG/(M2*S)
  !    *PDIFCQ*       CONVECTIVE FLUX OF SPECIFIC HUMIDITY        KG/(M2*S)
  !    *PDIFCS*       CONVECTIVE FLUX OF HEAT                      J/(M2*S)
  !    *PFHPCL*       ENTHALPY FLUX DUE TO CONVECTIVE RAIN         J/(M2*S)
  !    *PFHPCN*       ENTHALPY FLUX DUE TO CONVECTIVE SNOW         J/(M2*S)
  !    *PFPLCL*       CONVECTIVE RAIN FLUX                        KG/(M2*S)
  !    *PFPLCN*       CONVECTIVE SNOW FLUX                        KG/(M2*S)
  !    *PLRAIN*       RAIN+SNOW CONTENT IN UPDRAFTS                 KG/KG
  !    *PRSUD*        CONVECT MEAN RAIN AND SNOW CONTENT IN COLUMN  KG/KG
  !    *PSTRTU*       CONVECTIVE FLUX OF U-MOMEMTUM         (M/S)*KG/(M2*S)
  !    *PSTRTV*       CONVECTIVE FLUX OF V-MOMEMTUM         (M/S)*KG/(M2*S)
  !    *PFCQLF*       CONVECTIVE CONDENSATION FLUX LIQUID         KG/(M2*S)
  !    *PFCQIF*       CONVECTIVE CONDENSATION FLUX ICE            KG/(M2*S)
  
  !               Diagnostics:
  !    *PMFUDE_RATE* UPDRAFT DETRAINMENT RATE                     KG/(M3*S)
  !    *PMFDDE_RATE* DOWNDRAFT DETRAINMENT RATE                   KG/(M3*S)
  !    *PCAPE*       MAXIMUM pseudoadiabat CAPE                   (J/KG)
  !    *PWMEAN*      VERTICALLY AVERAGED UPDRAUGHT VELOCITY        M/S
  !    *PVDISCU*     VERT INTEGRATED CONVECTIVE DISSIPATION RATE   W/m2
  !    *PDISS*       ESTIMATE FOR DISSIPATION BT DEEP CONVCTION    J/m2
  
  !     EXTERNALS.
  !     ----------
  
  !          CUMASTR
  !          CUCCDIA
  !          CUSTRAT
  
  !     AUTHOR.
  !     -------
  !      M.TIEDTKE      E.C.M.W.F.     12/1989
  
  !     MODIFICATIONS.
  !     --------------
  !      15-10-01 : FULLIMP mods D.Salmond
  !      11-02-04 : Enable tracer transport P. Bechtold
  !      M.Hamrud      01-Oct-2003 CY28 Cleaning
  !      D.Salmond     22-Nov-2005 Mods for coarser/finer physics
  !      F.Vana        18-May-2012 cleaning
  !     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
  !----------------------------------------------------------------------
  
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
  
  USE STACK_MOD
#include "stack.h"
  
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
  REAL(KIND=JPRB), INTENT(OUT) :: PDISS(KLON, KLEV)
  
  temp (REAL (KIND=JPRB), ZTP1, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZQP1, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZUP1, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZVP1, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZTU, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZQU, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZQSAT, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZRAIN, (KLON))
  
  temp (REAL (KIND=JPRB), ZCP1, (KLON, KLEV, KTRAC))
  
  !-----------------------------------------------------------------------
  
  temp (REAL (KIND=JPRB), ZQEA, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZTEA, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZVEA, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZUEA, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZENTHD, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZENTHS, (KLON, KLEV))
  REAL(KIND=JPRB) :: ZCONDFLL
  REAL(KIND=JPRB) :: ZCONDFLN
  
  INTEGER(KIND=JPIM) :: IFLAG
  INTEGER(KIND=JPIM) :: JK
  INTEGER(KIND=JPIM) :: JL
  INTEGER(KIND=JPIM) :: JN
  
  REAL(KIND=JPRB) :: ZCP
  REAL(KIND=JPRB) :: ZGDPH
  REAL(KIND=JPRB) :: ZEPS
  
  LOGICAL :: LLTDKMF
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  
#include "cuccdia_openacc.intfb.h"
#include "cumastrn_openacc.intfb.h"
#include "satur_openacc.intfb.h"
  
  !DIR$ VFUNCTION EXPHF
#include "fcttre.func.h"
#include "fcttrm.func.h"
  INTEGER(KIND=    JPIM) :: JLON
  TYPE(STACK), INTENT(IN) :: YDSTACK
  TYPE(STACK) :: YLSTACK
  YLSTACK = YDSTACK
  IF (KIND (ZTP1) == 8) THEN
    alloc8 (ZTP1)
  ELSE
    IF (KIND (ZTP1) == 4) THEN
      alloc4 (ZTP1)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZQP1) == 8) THEN
    alloc8 (ZQP1)
  ELSE
    IF (KIND (ZQP1) == 4) THEN
      alloc4 (ZQP1)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZUP1) == 8) THEN
    alloc8 (ZUP1)
  ELSE
    IF (KIND (ZUP1) == 4) THEN
      alloc4 (ZUP1)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZVP1) == 8) THEN
    alloc8 (ZVP1)
  ELSE
    IF (KIND (ZVP1) == 4) THEN
      alloc4 (ZVP1)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZTU) == 8) THEN
    alloc8 (ZTU)
  ELSE
    IF (KIND (ZTU) == 4) THEN
      alloc4 (ZTU)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZQU) == 8) THEN
    alloc8 (ZQU)
  ELSE
    IF (KIND (ZQU) == 4) THEN
      alloc4 (ZQU)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZQSAT) == 8) THEN
    alloc8 (ZQSAT)
  ELSE
    IF (KIND (ZQSAT) == 4) THEN
      alloc4 (ZQSAT)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZRAIN) == 8) THEN
    alloc8 (ZRAIN)
  ELSE
    IF (KIND (ZRAIN) == 4) THEN
      alloc4 (ZRAIN)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZCP1) == 8) THEN
    alloc8 (ZCP1)
  ELSE
    IF (KIND (ZCP1) == 4) THEN
      alloc4 (ZCP1)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZQEA) == 8) THEN
    alloc8 (ZQEA)
  ELSE
    IF (KIND (ZQEA) == 4) THEN
      alloc4 (ZQEA)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZTEA) == 8) THEN
    alloc8 (ZTEA)
  ELSE
    IF (KIND (ZTEA) == 4) THEN
      alloc4 (ZTEA)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZVEA) == 8) THEN
    alloc8 (ZVEA)
  ELSE
    IF (KIND (ZVEA) == 4) THEN
      alloc4 (ZVEA)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZUEA) == 8) THEN
    alloc8 (ZUEA)
  ELSE
    IF (KIND (ZUEA) == 4) THEN
      alloc4 (ZUEA)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZENTHD) == 8) THEN
    alloc8 (ZENTHD)
  ELSE
    IF (KIND (ZENTHD) == 4) THEN
      alloc4 (ZENTHD)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZENTHS) == 8) THEN
    alloc8 (ZENTHS)
  ELSE
    IF (KIND (ZENTHS) == 4) THEN
      alloc4 (ZENTHS)
    ELSE
      STOP 1
    END IF
  END IF
  JLON = KIDIA
  
  !-----------------------------------------------------------------------
  
  !*    0.1          STORE T,Q,X,Y TENDENCIES FOR FLUX COMPUTATIONS
  !*                 ----------------------------------------------
  
  
  ! Setup of tendencies
  DO JK=1,KLEV
    ZQEA(JLON, JK) = PTENQ(JLON, JK)
    ZTEA(JLON, JK) = PTENT(JLON, JK)
    ZVEA(JLON, JK) = PTENV(JLON, JK)
    ZUEA(JLON, JK) = PTENU(JLON, JK)
    ZENTHD(JLON, JK) = 0.0_JPRB
    ZENTHS(JLON, JK) = 0.0_JPRB
  END DO
  
  DO JK=1,KLEV + 1
    PFPLCL(JLON, JK) = 0.0_JPRB
    PFPLCN(JLON, JK) = 0.0_JPRB
  END DO
  
  !-----------------------------------------------------------------------
  
  !*    1.           UPDATE PROGN. VALUES DEPENDING ON
  !                  TIME INTEGRATION AND CALCULATE QS
  !*                 ---------------------------------
  !-------------------------------------------------
  ! Protection in division. ZEPS is choosen as 1000 times lower
  ! than the actual values of qv (1.e-7 kg/kg) met in the high atmosphere.
  ! This ZEPS value, used in multiplication only, is relevant for both simple and
  ! double precision purpose.
  !-------------------------------------------------
  ZEPS = 1.0E-10_JPRB
  DO JK=1,KLEV
    ZUP1(JLON, JK) = PUM1(JLON, JK) + PTENU(JLON, JK)*PTSPHY
    ZVP1(JLON, JK) = PVM1(JLON, JK) + PTENV(JLON, JK)*PTSPHY
    ZTP1(JLON, JK) = PTM1(JLON, JK) + PTENT(JLON, JK)*PTSPHY
    ZQP1(JLON, JK) = PQM1(JLON, JK) + PTENQ(JLON, JK)*PTSPHY
    ZQSAT(JLON, JK) = ZQP1(JLON, JK)
    IF (ZQSAT(JLON, JK) <= 0.0_JPRB) ZQSAT(JLON, JK) = ZEPS
  END DO
  
  IF (LDSLPHY) THEN
    IF (KTRAC > 0 .and. YDML_PHY_EC%YREPHY%LMFTRAC) THEN
      DO JN=1,KTRAC
        DO JK=1,KLEV
          ! attention: transport is for positive definit quantities only
          ! ZCP1(JL,JK,JN)=MAX(0._JPRB,PCM1(JL,JK,JN))
          ZCP1(JLON, JK, JN) = PCM1(JLON, JK, JN)
        END DO
      END DO
    END IF
  ELSE
    IF (KTRAC > 0 .and. YDML_PHY_EC%YREPHY%LMFTRAC) THEN
      DO JN=1,KTRAC
        DO JK=1,KLEV
          ! attention: transport is for positive definit quantities only
          ! ZCP1(JL,JK,JN)=MAX(0._JPRB,PCM1(JL,JK,JN)+PTENC(JL,JK,JN)*PTSPHY)
          ZCP1(JLON, JK, JN) = PCM1(JLON, JK, JN) + PTENC(JLON, JK, JN)*PTSPHY
        END DO
      END DO
    END IF
  END IF
  
  IFLAG = 1
  CALL SATUR_OPENACC(YDTHF, YDCST, KIDIA, KFDIA, KLON, YDML_PHY_EC%YRECUMF%NJKT2, KLEV, YDML_PHY_SLIN%YREPHLI%LPHYLIN, PAP,  &
  & ZTP1, ZQSAT, IFLAG, YDSTACK=YLSTACK)
  
  ZRAIN(JLON) = 0.0_JPRB
  
  !-----------------------------------------------------------------------
  
  !*    2.     CALL 'CUMASTR'(MASTER-ROUTINE FOR CUMULUS PARAMETERIZATION)
  !*           -----------------------------------------------------------
  
  LLTDKMF = .true.
  CALL CUMASTRN_OPENACC(PPLDARE, PPLRG, YDTHF, YDCST, YDML_PHY_SLIN, YDML_PHY_EC, YGFL, YDCHEM, YDSPP_CONFIG, KIDIA, KFDIA,  &
  & KLON, KLEV, PDX, LLTDKMF, LDMCAPEA, LDLAND, PTSPHY, ZTP1, ZQP1, ZUP1, ZVP1, PLITOT, PVERVEL, ZQSAT, PQHFL, PAHFS, PAP, PAPH,  &
  & PGEO, PGEOH, PGAW, PCUCONVCA, PGP2DSPP, PTENT, PTENQ, PTENU, PTENV, PTENTA, PTENQA, LDCUM, KTYPE, KCBOT, KCTOP, LDCUM_LIG,  &
  & KCBOT_LIG, KCTOP_LIG, KBOTSC, LDSC, LDSHCV, PLCRIT_AER, ZTU, ZQU, PLU, PLUDE, PLUDELI, PSNDE, ZENTHD, PFPLCL, PFPLCN, ZRAIN,  &
  & PLRAIN, PRSUD, PMFU, PMFD, PMFUDE_RATE, PMFDDE_RATE, PCAPE, PWMEAN, PVDISCU, PDISS, KTRAC, ZCP1, PTENC, PSCAV, PSCAV0,  &
  & YDSTACK=YLSTACK)
  !----------------------------------------------------------------------
  
  !*    3.0       CALL 'CUCCDIA' TO UPDATE CLOUD PARAMETERS FOR RADIATION
  !               -------------------------------------------------------
  
  CALL CUCCDIA_OPENACC(YDERAD, YDML_PHY_SLIN%YREPHLI, YDML_PHY_EC%YREPHY, KIDIA, KFDIA, KLON, KLEV, KSTEP, KCBOT, KCTOP, LDCUM,  &
  & ZQU, PLU, PMFU, ZRAIN, PARPRC, KTOPC, KBASEC, YDSTACK=YLSTACK)
  
  
  !---------------------------------------------------------------------
  
  !*    5.           FLUX COMPUTATIONS
  !                  -----------------
  
  PDIFCQ(JLON, 1) = 0.0_JPRB
  PDIFCS(JLON, 1) = 0.0_JPRB
  PSTRCU(JLON, 1) = 0.0_JPRB
  PSTRCV(JLON, 1) = 0.0_JPRB
  PFCQLF(JLON, 1) = 0.0_JPRB
  PFCQIF(JLON, 1) = 0.0_JPRB
  PFHPCL(JLON, 1) = 0.0_JPRB
  PFHPCN(JLON, 1) = 0.0_JPRB
  PDIFCS(JLON, 1) = 0.0_JPRB
  PDIFCQ(JLON, 1) = 0.0_JPRB
  ZCONDFLL = 0.0_JPRB
  ZCONDFLN = 0.0_JPRB
  
  DO JK=1,KLEV
    
    ZGDPH = -YDCST%RG / (PAPHM1(JLON, JK + 1) - PAPHM1(JLON, JK))
    !...increment in dry static energy is converted to flux of d.s.e.
    ZCP = YDCST%RCPD*(1 + YDTHF%RVTMP2*ZQP1(JLON, JK))
    PDIFCS(JLON, JK + 1) = (PTENT(JLON, JK) - ZTEA(JLON, JK)) / ZGDPH*ZCP + PDIFCS(JLON, JK)
    !...increments in U,V
    PSTRCU(JLON, JK + 1) = (PTENU(JLON, JK) - ZUEA(JLON, JK)) / ZGDPH + PSTRCU(JLON, JK)
    PSTRCV(JLON, JK + 1) = (PTENV(JLON, JK) - ZVEA(JLON, JK)) / ZGDPH + PSTRCV(JLON, JK)
    
    !...increment in Q
    PDIFCQ(JLON, JK + 1) = (PTENQ(JLON, JK) - ZQEA(JLON, JK)) / ZGDPH + PDIFCQ(JLON, JK)
    
  END DO
  
  DO JK=1,KLEV
    !DEC$ IVDEP
    !... enthalpy flux due to precipitations
    
    PFHPCL(JLON, JK + 1) = -PFPLCL(JLON, JK + 1)*YDCST%RLVTT
    PFHPCN(JLON, JK + 1) = -PFPLCN(JLON, JK + 1)*YDCST%RLSTT
    ZCONDFLL = ZCONDFLL + PLUDELI(JLON, JK, 1)
    ZCONDFLN = ZCONDFLN + PLUDELI(JLON, JK, 2)
    PDIFCS(JLON, JK + 1) =  &
    & PDIFCS(JLON, JK + 1) - PFHPCL(JLON, JK + 1) - PFHPCN(JLON, JK + 1) + YDCST%RLVTT*ZCONDFLL + YDCST%RLSTT*ZCONDFLN
    PDIFCQ(JLON, JK + 1) = PDIFCQ(JLON, JK + 1) - PFPLCL(JLON, JK + 1) - PFPLCN(JLON, JK + 1) - ZCONDFLL - ZCONDFLN
    PFCQLF(JLON, JK + 1) = ZCONDFLL
    PFCQIF(JLON, JK + 1) = ZCONDFLN
  END DO
  !---------------------------------------------------------------------
  
END SUBROUTINE CUCALLN_MF_OPENACC
