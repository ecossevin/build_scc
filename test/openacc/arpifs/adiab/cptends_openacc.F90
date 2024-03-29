SUBROUTINE CPTENDS_OPENACC (YDCST, YDML_PHY_MF, KLON, KIDIA, KFDIA, KFLEV, KCSS, PDT, PFPLCL, PFPLSL, PFPLCN, PFPLSN, PFRSO,  &
& PFRTH, PALBNS1, PCT, PC1, PC2, PFCHSP, PFCLL, PFCLN, PFCS, PFEVI, PFEVL, PFEVN, PFEVV, PFGEL, PFGELS, PFLWSP, PFONTE, PFTR,  &
& PLSM, PRHONS1, PRUISL, PRUISP, PRUISS, PSNS1, PVEG, PTDTS, PTDTP, PTDWS, PTDWSI, PTDWP, PTDWPI, PTDWL, PTDSNS, PTDALBNS,  &
& PTDRHONS, YDSTACK)
  !-------------------------------------------------------------------------------
  ! - INPUT 2D .
  ! - INPUT 1D .
  ! - OUTPUT 1D .
  !****
  !     ------------------------------------------------------------------
  
  !     ACTION DES PROCESSUS PHYSIQUES SUR LES CHAMPS AU SOL
  !     INTERFACE AVEC LES PARAMETRISATIONS PHYSIQUES  (IALPP)
  !     ------------------------------------------------------------------
  
  !     BUT
  !     ---
  !      CALCUL DES TENDANCES PHYSIQUES
  !      SUR - LA TEMPERATURE DU SOL
  !          - LA TEMPERATURE PROFONDE
  !          - LE RESERVOIR SUPERFICIEL
  !          - LE RESERVOIR SUPERFICIEL DE GLACE ( DANS LE CAS LSOLV )
  !          - LE RESERVOIR PROFOND
  !          - LE RESERVOIR PROFOND DE GLACE ( DANS LE CAS LSOLV )
  !          - LE RESERVOIR D'INTERCEPTION ( DANS LE CAS LSOLV )
  !          - LE RESERVOIR DE NEIGE
  !          - L'ALBEDO DE LA NEIGE
  !          - LA DENSITE DE LA NEIGE
  
  !     ARGUMENTS D ENTREE
  !     ------------------
  !       KLON   : DIMENSION HORIZONTALE
  !       KIDIA  : BORNE INITIALE HORIZONTALE DES CALCULS
  !       KFDIA : BORNE FINALE HORIZONTALE DES CALCULS
  !       KFLEV : DIMENSION ET BORNE VERTICALE
  !       KCSS  : NBRE DE COUCHES DANS LE SOL
  !       PDT   : PAS DE TEMPS EFFECTIF
  
  ! --- INPUT 2D
  !     --------
  !       PFPLCL, PFPLSL, PFPLCN, PFPLSN (KLON,0:KFLEV): FLUX DE PRECIPITATIONS
  !       PFRSO, PFRTH (IDEM): FLUX DE RAYONNEMENT
  
  ! --- INPUT 1D
  !     --------
  !       PALBNS1(KLON): ALBEDO DE LA NEIGE
  !       PC1: COEFF. HYDRIQUE REPRESENTANT L'INTENSITE AVEC LAQUELLE
  !               LES FLUX DE SURFACE PARTICIPENT A L'EVOLUTION DE WS (LSOLV).
  !       PC2   : COEFF. HYDRIQUE TRADUISANT LA RAPIDITE DES TRANSFERTS D'EAU
  !               ENTRE LES DEUX RESERVOIRS (LSOLV).
  !       PCT   : COEFFICIENT THERMIQUE DU MILIEU SOL-VEGETATION.
  !       PFCHSP (IDEM): FLUX DE CHALEUR DANS LE SOL
  !       PFCLL, PFCLN (IDEM): FLUX DE CHALEUR LATENTE
  !       PFCS  : FLUX DE CHALEUR SENSIBLE
  !                     (DANS L HYPOTHSE OU PS NE VARIE PAS)
  !       PFEVI,PFEVL,PFEVN : EVAPORATION DES RESERVOIRS DE GLACE, EAU ET
  !                           NEIGE (Y COMPRIS PFEVI).
  !       PFEVV  : FLUX D'EVAPOTRANSPIRATION (LSOLV).
  !       PFGEL  : FLUX DE GEL DE L EAU DU SOL (LSOLV et LFGEL).
  !       PFGELS : FLUX DE GEL DE L EAU DE SURFACE (LSOLV et LFGELS).
  !       PFLWSP (IDEM): FLUX D EAU DANS LE SOL
  !       PFONTE (KLON): FLUX DE FONTE DE NEIGE
  !       PFTR   : FLUX DE TRANSPIRATION (LSOLV).
  !       PLSM (IDEM): INDICATEUR TERRE(1.)/MER(0.)
  !       PRHONS1 (IDEM) : DENSITE DE LA NEIGE
  !       PRUISL (IDEM):RUISSELLEMENT DU RESERVOIR D EAU SUPERFICIEL (LSOLV).
  !       PRUISS, PRUISP (IDEM) : RUISSELLEMENT DES RESERVOIRS D EAU.
  !       PSNS1  : RESERVOIR DE NEIGE.
  !       PVEG   : PROPORTION DE SOL COUVERT PAR LA VEGETATION.
  
  !     ARGUMENTS IMPLICITES
  !     --------------------
  !       CONSTANTES UNIVERSELLES =  COMMON /YOMCST/ : RDAY,RLMLT
  !       PROPRIETES DU SOL       =  COMMON /YOMPHY1/: RTINER
  !       LOGIQUES DE LA PHYSIQUE =  COMMON /YOMPHY/ : LSOLV,LNEIGE,LFGEL,
  !                                                    LFGELS
  
  !     SORTIES
  !     -------
  !          PTDTS(KLON) : TENDANCE PHYSIQUE SUR LA TEMPERATURE DE SURFACE
  !          PTDTP(IDEM,KCSS) :    "     "   SUR LA TEMPERATURE PROFONDE
  !          PTDWS(IDEM) :    "     "   SUR LE RESERVOIR D'EAU SUPERFICIEL
  !          PTDWP(IDEM) :    "     "   SUR LE RESERVOIR D'EAU PROFOND
  !          PTDWPI(IDEM):    "     "   SUR LE RESERVOIR D'EAU PROFOND GELE
  !          PTDWSI(IDEM):    "     "   SUR LE RESERVOIR D'EAU GELEE DE SURFACE
  !          PTDWL(IDEM) :    "     "   SUR LE RESERVOIR D'EAU D'INTERCEPTION
  !                                         SI LSOLV
  !          PTDSNS(IDEM):    "     "   SUR LE RESERVOIR DE NEIGE (LNEIGE)
  !          PTDALBNS(IDEM):  "     "   SUR L'ALBEDO DE LA NEIGE
  !          PTDRHONS(IDEM):  "     "   SUR LA DENSITE DE LA NEIGE
  
  !     AUTEUR:
  !     ------- ERIC BAZILE  92/10 INSPIRE PAR J.NOILHAN ET A.JOLY
  !             MODIFIE 94/10 PAR M. DEQUE (MODELE 4 TEMPERATURES SOL)
  
  !     MODIFICATIONS:
  !     --------------
  !      02-09 Schema de neige LVGSN. - E. Bazile.
  !      M.Hamrud      01-Oct-2003 CY28 Cleaning
  !     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
  !     ------------------------------------------------------------------
  
!$acc routine( CPTENDS_OPENACC ) seq
  
  USE MODEL_PHYSICS_MF_MOD, ONLY: MODEL_PHYSICS_MF_TYPE
  USE PARKIND1, ONLY: JPIM, JPRB
  USE YOMHOOK, ONLY: DR_HOOK, JPHOOK, LHOOK
  
  USE YOMCST, ONLY: TCST
  
  !     ------------------------------------------------------------------
  
  USE STACK_MOD
#include "stack.h"
  
  IMPLICIT NONE
  
  TYPE(TCST), INTENT(IN) :: YDCST
  TYPE(MODEL_PHYSICS_MF_TYPE), INTENT(IN) :: YDML_PHY_MF
  INTEGER(KIND=JPIM), INTENT(IN) :: KLON
  INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KFLEV
  INTEGER(KIND=JPIM), INTENT(IN) :: KCSS
  REAL(KIND=JPRB), INTENT(IN) :: PDT
  REAL(KIND=JPRB), INTENT(IN) :: PFPLCL(KLON, 0:KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFPLSL(KLON, 0:KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFPLCN(KLON, 0:KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFPLSN(KLON, 0:KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFRSO(KLON, 0:KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFRTH(KLON, 0:KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PALBNS1(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PCT(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PC1(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PC2(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFCHSP(KLON, KCSS)
  REAL(KIND=JPRB), INTENT(IN) :: PFCLL(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFCLN(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFCS(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFEVI(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFEVL(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFEVN(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFEVV(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFGEL(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFGELS(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFLWSP(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFONTE(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFTR(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PLSM(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PRHONS1(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PRUISL(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PRUISP(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PRUISS(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PSNS1(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PVEG(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDTS(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDTP(KLON, KCSS)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDWS(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDWSI(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDWP(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDWPI(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDWL(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDSNS(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDALBNS(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PTDRHONS(KLON)
  
  !     ------------------------------------------------------------------
  
  INTEGER(KIND=JPIM) :: JCSS
  INTEGER(KIND=JPIM) :: JROF
  
  REAL(KIND=JPRB) :: ZBFLO
  REAL(KIND=JPRB) :: ZCI
  REAL(KIND=JPRB) :: ZEPSN
  REAL(KIND=JPRB) :: ZFGEL
  REAL(KIND=JPRB) :: ZITS
  REAL(KIND=JPRB) :: ZPRECN
  REAL(KIND=JPRB) :: ZTO3
  REAL(KIND=JPRB) :: ZVEG
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  INTEGER(KIND=    JPIM) :: JLON
  TYPE(STACK), INTENT(IN) :: YDSTACK
  TYPE(STACK) :: YLSTACK
  YLSTACK = YDSTACK
  JLON = KIDIA
  
  !     ------------------------------------------------------------------
  !     ------------------------------------------------------------------
  
  !     ------------------------------------------------------------------
  !     1.- CALCUL DES TENDANCES PHYSIQUES
  
  ZEPSN = 1.E-3_JPRB
  IF (YDML_PHY_MF%YRPHY%LSOLV) THEN
    
    !        CAS LSOLV ROUTINE ACDROV ACTIVE
    
    !     ... DE LA TEMPERATURE PROFONDE :
    !     -------------------------------
    
    DO JCSS=1,KCSS - 1
      PTDTP(JLON, JCSS) = PLSM(JLON)*(YDML_PHY_MF%YRPHY1%SODELX(0) / YDML_PHY_MF%YRPHY1%SODELX(JCSS))*PCT(JLON)*(PFCHSP(JLON,  &
      & JCSS) - PFCHSP(JLON, JCSS + 1))
    END DO
    PTDTP(JLON, KCSS) = PLSM(JLON)*(YDML_PHY_MF%YRPHY1%SODELX(0) / YDML_PHY_MF%YRPHY1%SODELX(KCSS))*PCT(JLON)*PFCHSP(JLON, KCSS)
    IF (YDML_PHY_MF%YRPHY%LFGEL) THEN
      PTDTP(JLON, 1) = PTDTP(JLON, 1) + PLSM(JLON)*PCT(JLON)*YDCST%RLMLT*PFGEL(JLON)
    END IF
    
    ZITS = PLSM(JLON) / YDCST%RDAY
    
    !     ... DE LA TEMPERATURE DE SURFACE :
    !     ---------------------------------
    IF (YDML_PHY_MF%YRPHY%LFGEL) THEN
      ZFGEL = PFGELS(JLON)
    ELSE
      ZFGEL = PFGEL(JLON)
    END IF
    PTDTS(JLON) = PLSM(JLON)*PCT(JLON)*(PFRSO(JLON, KFLEV) + PFRTH(JLON, KFLEV) + PFCS(JLON) + PFCLL(JLON) + PFCLN(JLON) -  &
    & PFCHSP(JLON, 1) - YDCST%RLMLT*(PFONTE(JLON) - ZFGEL))
    
    !     ... DU RESERVOIR D'INTERCEPTION:
    !     --------------------------------
    
    ZVEG = PVEG(JLON) / (1.0_JPRB + YDML_PHY_MF%YRPHY0%USUPRC*PFPLCL(JLON, KFLEV))
    PTDWL(JLON) = PLSM(JLON)*(ZVEG*(PFPLSL(JLON, KFLEV) + PFPLCL(JLON, KFLEV)) + PFEVV(JLON) - PFTR(JLON) - PRUISL(JLON))
    
    !     ... DU RESERVOIR SUPERFICIEL LIQUIDE ET GLACE :
    !     -----------------------------------------------
    
    ZBFLO =  &
    & (1.0_JPRB - ZVEG)*(PFPLSL(JLON, KFLEV) + PFPLCL(JLON, KFLEV)) + PRUISL(JLON) + PFONTE(JLON) + PFEVL(JLON) - PFEVV(JLON)
    ZCI = PC2(JLON)*PDT*ZITS
    PTDWS(JLON) = PLSM(JLON)*((PC1(JLON)*ZBFLO - PFLWSP(JLON)) / (1.0_JPRB + ZCI) - PRUISS(JLON) - PFGELS(JLON))
    
    IF (YDML_PHY_MF%YRPHY%LFGEL) THEN
      PTDWSI(JLON) = PLSM(JLON)*(PFGELS(JLON) + PFEVI(JLON))
    END IF
    
    !     ... DU RESERVOIR PROFOND LIQUIDE ET GLACE:
    !     -----------------------------------------
    
    PTDWP(JLON) = PLSM(JLON)*(ZBFLO + PFTR(JLON) - PRUISP(JLON) - PFGEL(JLON) - PFGELS(JLON))
    
    IF (YDML_PHY_MF%YRPHY%LFGEL) THEN
      PTDWPI(JLON) = PLSM(JLON)*(PFGEL(JLON) + PFGELS(JLON) + PFEVI(JLON))
    END IF
    
  ELSE
    
    !        ROUTINE ACDRO ACTIVE
    
    DO JCSS=2,KCSS
      PTDTP(JLON, JCSS) = 0
    END DO
    
    
    !     ... DE LA TEMPERATURE DE SURFACE :
    !     ---------------------------------
    
    PTDTS(JLON) = PLSM(JLON)*PCT(JLON)*(PFRSO(JLON, KFLEV) + PFRTH(JLON, KFLEV) + PFCS(JLON) + PFCLL(JLON) + PFCLN(JLON) -  &
    & PFCHSP(JLON, 1) - YDCST%RLMLT*PFONTE(JLON))
    
    !     ... DE LA TEMPERATURE PROFONDE :
    !     -------------------------------
    
    PTDTP(JLON, 1) = PLSM(JLON)*(PCT(JLON) / YDML_PHY_MF%YRPHY1%RTINER)*PFCHSP(JLON, 1)
    
    !     ... DU RESERVOIR SUPERFICIEL :
    !     -----------------------------
    
    PTDWS(JLON) =  &
    & PLSM(JLON)*(PFPLSL(JLON, KFLEV) + PFPLCL(JLON, KFLEV) + PFONTE(JLON) - PRUISS(JLON) + PFEVL(JLON) - PFLWSP(JLON))
    
    !     ... DU RESERVOIR PROFOND :
    !     -------------------------
    
    PTDWP(JLON) = PLSM(JLON)*(PFLWSP(JLON) - PRUISP(JLON))
    
    
  END IF
  
  !     ... ET DES VARIABLES NEIGE EVENTUELLES:
  !     --------------------------------------
  
  IF (YDML_PHY_MF%YRPHY%LNEIGE) THEN
    PTDSNS(JLON) = PLSM(JLON)*(PFPLCN(JLON, KFLEV) + PFPLSN(JLON, KFLEV) - PFONTE(JLON) + PFEVN(JLON) - PFEVI(JLON))
    IF (YDML_PHY_MF%YRPHY%LSNV .or. YDML_PHY_MF%YRPHY%LVGSN) THEN
      ZPRECN = PLSM(JLON)*(PFPLCN(JLON, KFLEV) + PFPLSN(JLON, KFLEV))
      IF (PFONTE(JLON) > 0.0_JPRB) THEN
        PTDALBNS(JLON) = -YDML_PHY_MF%YRPHY1%TOEXP*(PALBNS1(JLON) - YDML_PHY_MF%YRPHY1%ALBMIN) + ZPRECN / YDML_PHY_MF%YRPHY1%WNEW
      ELSE
        PTDALBNS(JLON) = -YDML_PHY_MF%YRPHY1%TOLIN + ZPRECN / YDML_PHY_MF%YRPHY1%WNEW
      END IF
      ZTO3 = MIN(0.5_JPRB / PDT, ZPRECN / MAX(PSNS1(JLON), ZEPSN))
      PTDRHONS(JLON) =  &
      & -YDML_PHY_MF%YRPHY1%TOEXP*(PRHONS1(JLON) - YDML_PHY_MF%YRPHY1%RHOMAX) - ZTO3*(PRHONS1(JLON) - YDML_PHY_MF%YRPHY1%RHOMIN)
    END IF
  END IF
  
  !     ------------------------------------------------------------------
  !     REMARQUE IMPORTANTE
  !     -------------------
  !      RESTE A TRAITER LA QUESTION DES CONDITIONS AUX LIMITES INFERIEURES
  !      DE L ADVECTION VERTICALE
  !      CE CALCUL DEPEND DES VARIABLES PHYSIQUES AU SOL
  !     ------------------------------------------------------------------
  
  !     ------------------------------------------------------------------
END SUBROUTINE CPTENDS_OPENACC
