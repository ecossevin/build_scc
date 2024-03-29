SUBROUTINE ACDROV_OPENACC (YDCST, YDML_PHY_MF, KIDIA, KFDIA, KLON, KLEV, KCSS, PFPLCL, PFPLCN, PFPLSL, PFPLSN, PFRSO, PFRTH,  &
& PC1, PC2, PC3, PCN, PCT, PD2, PFEVV, PFTR, PLAI, PNEIJ, PVEG, PWFC, PWPMX, PWL, PWLMX, PWSEQ, PWSMX, PFCHSP, PFCLL, PFCLN,  &
& PFCS, PFEVI, PFEVL, PFEVN, PLSM, PSNS, PTP, PTS, PWP, PWPI, PWS, PWSI, PFGEL, PFGELS, PFLWSP, PFONTE, PRUISP, PRUISL, PRUISS,  &
& YDSTACK)
  !-----------------------------------------------------------------------
  ! - INPUT  2D .
  ! - INPUT  1D .
  ! - OUTPUT 1D .
  
  !**** *ACDROV  * - CALCUL DES FLUX D'EAU DANS LE SOL.
  
  !     Sujet.
  !     ------
  !     - ROUTINE DE CALCUL ACTIF .
  !       CALCUL DES FLUX D'EAU DANS LE SOL : FONTE DE LA NEIGE, TRANSFERT
  !       VERS LE SOL PROFOND, RUISSELLEMENTS EN SURFACE ET EN PROFONDEUR .
  !     - COMPUTATION OF WATER FLUXES IN SOIL : SNOW MELTING, SURFACE AND
  !       SOIL RUN-OFF.
  
  !**   Interface.
  !     ----------
  !        *CALL* *ACDROV*
  
  !-----------------------------------------------------------------------
  ! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
  !          "APLPAR" CODE.
  !-----------------------------------------------------------------------
  
  ! -   ARGUMENTS D'ENTREE.
  !     -------------------
  
  ! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.
  
  ! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
  ! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
  ! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
  ! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".
  ! KCSS       : NBRE DE NIVEAUX DANS LE SOL PROFOND.
  
  ! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
  !   CATEGORIE).
  
  ! - 2D (0:KLEV) .
  
  ! PFPLCL     : PRECIPITATIONS CONVECTIVES SOUS FORME LIQUIDE.
  ! PFPLCN     : PRECIPITATIONS CONVECTIVES SOUS FORME NEIGE.
  ! PFPLSL     : PRECIPITATIONS STRATIFORMES SOUS FORME LIQUIDE.
  ! PFPLSN     : PRECIPITATIONS STRATIFORMES SOUS FORME NEIGE.
  ! PFRSO      : FLUX DE RAYONNEMENT SOLAIRE.
  ! PFRTH      : FLUX DE RAYONNEMENT THERMIQUE.
  
  ! - 1D (PROGNOSTIQUE) .
  
  ! PSNS       : MASSE PAR UNITE DE SURFACE DE LA COUCHE NEIGEUSE.
  ! PTP        : TEMPERATURE DE SUBSURFACE.
  ! PTS        : TEMPERATURE DE SURFACE.
  ! PWP        : CONTENU EN EAU DU RESERVOIR EN PROFONDEUR.
  ! PWPI       : CONTENU EN EAU GELEE DU RESERVOIR EN PROFONDEUR.
  ! PWL        : CONTENU EN EAU DU RESERVOIR D'INTERCEPTION.
  ! PWS        : CONTENU EN EAU DU RESERVOIR DE SURFACE.
  ! PWSI       : CONTENU EN EAU GELEE DU RESERVOIR DE SURFACE.
  
  ! - 1D (GEOGRAPHIQUE) .
  
  ! PD2        : PROFONDEUR DU RESERVOIR TOTAL (M)
  ! PLSM       : INDICE TERRE/MER.
  ! PLAI       : INDICE FOLIAIRE.
  ! PNEIJ      : FRACTION DE NEIGE.
  ! PVEG       : FRACTION DE SOL RECOUVERT DE VEGETATION.
  
  ! - 1D (DIAGNOSTIQUE) .
  
  ! PFCHSP     : FLUX DE CHALEUR DE LA SURFACE VERS LE SOL PROFOND.
  ! PFCLL      : FLUX DE CHALEUR LATENTE SUR EAU LIQUIDE (OU SOL HUMIDE).
  ! PFCLN      : FLUX DE CHALEUR LATENTE SUR NEIGE (OU GLACE).
  ! PFCS       : FLUX DE CHALEUR SENSIBLE EN SURFACE.
  ! PFEVI      : FLUX DE VAPEUR D'EAU SUR SOL GELE.
  ! PFEVL      : FLUX DE VAPEUR D'EAU SUR EAU LIQUIDE (OU SOL HUMIDE).
  ! PFEVN      : FLUX DE VAPEUR D'EAU SUR NEIGE (OU GLACE) ET SOL GELE.
  ! PC1        : COEF. HYDRIQUE REPRESENTANT L'INTENSITE AVEC LAQUELLE
  !              LES FLUX DE SURFACE PARTICIPENT A L'EVOLUTION DE WS.
  ! PC2        : COEF. HYDRIQUE TRADUISANT LA RAPIDITE DES TRANSFERTS D'EAU
  !              ENTRE LES DEUX RESERVOIRS.
  ! PC3        : COEFFICIENT UTILE POUR LE CALCUL DU DRAINAGE.
  ! PCN        : COEFFICIENT THERMIQUE DE LA NEIGE.
  ! PCT        : COEFFICIENT THERMIQUE DU MILIEU SOL-VEGETATION.
  ! PFEVV      : FLUX D'EVAPOTRANSPIRATION DE LA VEGETATION.
  ! PFTR       : FLUX DE TRANSPIRATION DE LA VEGETATION.
  ! PWFC       : CAPACITE AU CHAMP.
  ! PWPMX      : CONTENU EN EAU MAXIMAL DU RESERVOIR PROFOND.
  ! PWLMX      : CONTENU EN EAU MAXIMAL DU RESERVOIR D'INTERCEPTION.
  ! PWSEQ      : TENEUR EN EAU DE SURFACE A L'EQUILIBRE (GRAVITE-CAPILLARITE).
  ! PWSMX      : CONTENU EN EAU MAXIMAL DU RESERVOIR SUPERFICIEL.
  
  !-----------------------------------------------------------------------
  
  ! -   ARGUMENTS DE SORTIE.
  !     --------------------
  
  ! - 1D (DIAGNOSTIQUE) .
  
  ! PFGEL      : FLUX DE GEL POUR L'EAU DU SOL.
  ! PFGELS     : FLUX DE GEL POUR L'EAU DE SURFACE.
  ! PFLWSP     : FLUX D'EAU LIQUIDE DE LA SURFACE VERS LE SOL PROFOND.
  ! PFONTE     : FLUX D'EAU CORRESPONDANT A LA FONTE DE NEIGE EN SURFACE.
  ! PRUISP     : FLUX DE RUISSELLEMENT EN PROFONDEUR.
  ! PRUISL     : FLUX DE RUISSELLEMENT DU RESERVOIR D'INTERCEPTION.
  ! PRUISS     : FLUX DE RUISSELLEMENT EN SURFACE.
  
  !-----------------------------------------------------------------------
  
  ! -   ARGUMENTS IMPLICITES.
  !     ---------------------
  
  !-----------------------------------------------------------------------
  
  !     Externes.
  !     ---------
  
  !     Methode.
  !     --------
  
  !     Auteur.
  !     -------
  !      91-12, J. Noilhan.
  
  !     Modifications.
  !     --------------
  !      2001-02, Cor. d'une petite erreur dans PFONTE - E.Bazile.
  !      R. El Khatib : 01-08-07 Pruning options
  !      M.Hamrud      01-Oct-2003 CY28 Cleaning
  !      K. Yessad (Jul 2009): remove CDLOCK + some cleanings
  !     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
  !-----------------------------------------------------------------------
  
!$acc routine( ACDROV_OPENACC ) seq
  
  USE MODEL_PHYSICS_MF_MOD, ONLY: MODEL_PHYSICS_MF_TYPE
  USE PARKIND1, ONLY: JPIM, JPRB
  USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
  
  USE YOMCST, ONLY: TCST
  
  !-----------------------------------------------------------------------
  
  USE STACK_MOD
#include "stack.h"
  
  IMPLICIT NONE
  
  TYPE(TCST), INTENT(IN) :: YDCST
  TYPE(MODEL_PHYSICS_MF_TYPE), INTENT(IN) :: YDML_PHY_MF
  INTEGER(KIND=JPIM), INTENT(IN) :: KLON
  INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
  INTEGER(KIND=JPIM), INTENT(IN) :: KCSS
  INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
  REAL(KIND=JPRB), INTENT(IN) :: PFPLCL(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFPLCN(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFPLSL(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFPLSN(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFRSO(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFRTH(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PC1
  REAL(KIND=JPRB), INTENT(IN) :: PC2
  REAL(KIND=JPRB), INTENT(IN) :: PC3
  REAL(KIND=JPRB), INTENT(IN) :: PCN
  REAL(KIND=JPRB), INTENT(IN) :: PCT
  REAL(KIND=JPRB), INTENT(IN) :: PD2
  REAL(KIND=JPRB), INTENT(IN) :: PFEVV
  REAL(KIND=JPRB), INTENT(IN) :: PFTR
  REAL(KIND=JPRB), INTENT(IN) :: PLAI
  REAL(KIND=JPRB), INTENT(IN) :: PNEIJ
  REAL(KIND=JPRB), INTENT(IN) :: PVEG
  REAL(KIND=JPRB), INTENT(IN) :: PWFC
  REAL(KIND=JPRB), INTENT(IN) :: PWPMX
  REAL(KIND=JPRB), INTENT(IN) :: PWL
  REAL(KIND=JPRB), INTENT(IN) :: PWLMX
  REAL(KIND=JPRB), INTENT(IN) :: PWSEQ
  REAL(KIND=JPRB), INTENT(IN) :: PWSMX
  REAL(KIND=JPRB), INTENT(IN) :: PFCHSP
  REAL(KIND=JPRB), INTENT(IN) :: PFCLL
  REAL(KIND=JPRB), INTENT(IN) :: PFCLN
  REAL(KIND=JPRB), INTENT(IN) :: PFCS
  REAL(KIND=JPRB), INTENT(IN) :: PFEVI
  REAL(KIND=JPRB), INTENT(IN) :: PFEVL
  REAL(KIND=JPRB), INTENT(IN) :: PFEVN
  REAL(KIND=JPRB), INTENT(IN) :: PLSM
  REAL(KIND=JPRB), INTENT(IN) :: PSNS
  REAL(KIND=JPRB), INTENT(IN) :: PTP(KLON, KCSS)
  REAL(KIND=JPRB), INTENT(IN) :: PTS
  REAL(KIND=JPRB), INTENT(IN) :: PWP
  REAL(KIND=JPRB), INTENT(IN) :: PWPI
  REAL(KIND=JPRB), INTENT(IN) :: PWS
  REAL(KIND=JPRB), INTENT(IN) :: PWSI
  REAL(KIND=JPRB), INTENT(INOUT) :: PFGEL
  REAL(KIND=JPRB), INTENT(INOUT) :: PFGELS
  REAL(KIND=JPRB), INTENT(OUT) :: PFLWSP
  REAL(KIND=JPRB), INTENT(INOUT) :: PFONTE
  REAL(KIND=JPRB), INTENT(OUT) :: PRUISP
  REAL(KIND=JPRB), INTENT(OUT) :: PRUISL
  REAL(KIND=JPRB), INTENT(OUT) :: PRUISS
  
  !-----------------------------------------------------------------------
  
  REAL(KIND=JPRB) :: ZITS
  
  INTEGER(KIND=JPIM) :: JLON
  
  REAL(KIND=JPRB) :: ZBFLO
  REAL(KIND=JPRB) :: ZCI
  REAL(KIND=JPRB) :: ZCONV
  REAL(KIND=JPRB) :: ZEPS1
  REAL(KIND=JPRB) :: ZFGEL
  REAL(KIND=JPRB) :: ZFGELS
  REAL(KIND=JPRB) :: ZFLDT
  REAL(KIND=JPRB) :: ZFONTE
  REAL(KIND=JPRB) :: ZFONTES
  REAL(KIND=JPRB) :: ZIDAY
  REAL(KIND=JPRB) :: ZITSP
  REAL(KIND=JPRB) :: ZNEIJC
  REAL(KIND=JPRB) :: ZPCNDT
  REAL(KIND=JPRB) :: ZPCTDT
  REAL(KIND=JPRB) :: ZSNSP
  REAL(KIND=JPRB) :: ZTF
  REAL(KIND=JPRB) :: ZTPP
  REAL(KIND=JPRB) :: ZTSP
  REAL(KIND=JPRB) :: ZVEG
  REAL(KIND=JPRB) :: ZWFC
  REAL(KIND=JPRB) :: ZWPID
  REAL(KIND=JPRB) :: ZWPIMX
  REAL(KIND=JPRB) :: ZWPMX
  REAL(KIND=JPRB) :: ZWPP
  REAL(KIND=JPRB) :: ZWSMX
  REAL(KIND=JPRB) :: ZWSP
  REAL(KIND=JPRB) :: ZZ
  REAL(KIND=JPRB) :: ZZK
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  TYPE(STACK), INTENT(IN) :: YDSTACK
  TYPE(STACK) :: YLSTACK
  YLSTACK = YDSTACK
  JLON = KIDIA
  
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  
  !     ------------------------------------------------------------------
  !     I - CALCUL DES PARAMETRES DERIVES ET CONSTANTE DE SECURITE (POUR
  !     CSDT).
  
  !         COMPUTATION OF DERIVED PARAMETERS AND SECURITY CONSTANT (FOR
  !     CSDT).
  
  ZITSP = 1.0_JPRB / YDML_PHY_MF%YRPHY2%TSPHY
  ZIDAY = 1.0_JPRB / YDCST%RDAY
  
  ZEPS1 = 1.E-08_JPRB
  
  !     CONVERSION M --> KG/M**2
  ZCONV = 1.E+3_JPRB
  !*
  !     ------------------------------------------------------------------
  !     II - MISE A DES VALEURS CONSTANTES DES VALEURS DERIVEES DE
  !     L'INERTIE THERMIQUE ET DES TEMPS CARACTERISTIQUES (DIURNE PAR
  !     DEFINITION POUR LA COUCHE SUPERFICIELLE) DU SOL AINSI QUE DES
  !     CONTENUS EN EAU MAXIMA DONT LE RAPPORT FIXE CELUI DES TEMPS
  !     CARACTERISTIQUES (PHYSIQUE DU SOL HYPER-SIMPLIFIEE).
  
  !          SETTING TO CONSTANT VALUES THE SOIL THERMAL INERTIA AND
  !     CHARACTERISTIC TIME CONSTANTS (DIURNAL BY DEFINITION FOR THE
  !     SURFACE LAYER) AS WELL AS THE MAXIMUM WATER CONTENTS WHOSE RATIO
  !     IMPOSES THE OTHER RATIO OF THE CHARACTERISTIC TIME CONSTANTS
  !     (HYPER-SIMPLIFIED SOIL PHYSICS).
  
  ! - TEMPORAIRE(S) 1D .
  
  ! ZPCNDT     : PRODUIT DU PAS DE TEMPS PAR LA CONSTANTE DE LA NEIGE.
  !            : PRODUCT OF TIME-STEP BY THE SNOW INERTIAL CONSTANT.
  ! ZPCTDT     : PRODUIT DU PAS DE TEMPS PAR LA CONSTANTE DU SOL.
  !            : PRODUCT OF TIME-STEP BY THE SOIL INERTIAL CONSTANT.
  ! ZITS      : INVERSE DU TEMPS CARACTERISTIQUE EN SURFACE.
  !            : INVERSE OF THE CHARACTERISTIC SURFACE TIME CONSTANT.
  
  ZITS = PLSM*ZIDAY
  
  !     ------------------------------------------------------------------
  !     III - CALCULS PROPREMENTS DITS.
  
  !           EFFECTIVE COMPUTATIONS.
  
  !*******************************************************************************
  !     CALCUL DU RUISSELLEMENT DU RESERVOIR D'INTERCEPTION.
  !     COMPUTATION OF INTERCEPTION RUN-OFFS.
  !*******************************************************************************
  
  !     ZVEG EST UNE FRACTION DE VEGETATION APPARENTE:
  !        QUAND IL PLEUT BEAUCOUP (CONVECTIF) UNE FRACTION DU RESERVOIR
  !        FOLIAIRE SEULEMENT SE REMPLIT
  !     ZVEG IS A PSEUDO VEGETATION FRACTION:
  !        IT DECREASES WITH RAINFALL. THE INTERCEPTION BY VEGETATION
  !        IS LIMITED BY PVEG/USUPRC
  ZVEG = PVEG / (1.0_JPRB + YDML_PHY_MF%YRPHY0%USUPRC*PFPLCL(JLON, KLEV))
  ZFLDT = YDML_PHY_MF%YRPHY2%TSPHY*(ZVEG*(PFPLSL(JLON, KLEV) + PFPLCL(JLON, KLEV)) + PFEVV - PFTR)
  PRUISL = ZITSP*PLSM*(ZFLDT - MIN(PWLMX - PWL, MAX(-PWL, ZFLDT)))
  ZBFLO = (1.0_JPRB - ZVEG)*(PFPLSL(JLON, KLEV) + PFPLCL(JLON, KLEV)) + PRUISL + PFEVL - PFEVV
  
  !*******************************************************************************
  !     CALCULS DE FONTE EN CAS D'OPTION NEIGE.
  !     MELTING CALCULATIONS IN CASE OF ACTIVATED SNOW OPTION.
  !*******************************************************************************
  
  IF (YDML_PHY_MF%YRPHY%LNEIGE) THEN
    ZPCTDT = PCT*YDML_PHY_MF%YRPHY2%TSPHY*PLSM
    ZTSP = PTS + ZPCTDT*(PFRSO(JLON, KLEV) + PFRTH(JLON, KLEV) + PFCS + PFCLL + PFCLN - PFCHSP)
    ZSNSP = PSNS*ZITSP + (PFPLSN(JLON, KLEV) + PFPLCN(JLON, KLEV) + PFEVN - PFEVI)
    IF (YDML_PHY_MF%YRPHY%LSNV) THEN
      ZTF = (1.0_JPRB - PVEG)*ZTSP + PVEG*PTP(JLON, 1)
      PFONTE =  &
      & MAX(0.0_JPRB, (ZTF - (PLSM*YDCST%RTT + (1.0_JPRB - PLSM)*YDML_PHY_MF%YRPHY1%TMERGL)) / (MAX(ZEPS1, ZPCTDT)*YDCST%RLMLT))
      ZNEIJC = MAX(0.0_JPRB, MIN(1.0_JPRB, PSNS / YDML_PHY_MF%YRPHY1%WCRINC))
      ZPCNDT = PCN*YDML_PHY_MF%YRPHY2%TSPHY*PLSM
      PFONTE = ZNEIJC / MAX(ZEPS1, ZPCNDT)*MAX(ZEPS1, ZPCTDT)*PFONTE
      PFONTE = PLSM*MIN(ZSNSP, PFONTE)
    ELSE
      PFONTE = PLSM*MIN(ZSNSP, MAX(0.0_JPRB, (ZTSP - (PLSM*YDCST%RTT + (1.0_JPRB - PLSM)*YDML_PHY_MF%YRPHY1%TMERGL)) /  &
      & (MAX(ZEPS1, ZPCTDT)*YDCST%RLMLT)))
    END IF
    
    ZTSP = ZTSP - ZPCTDT*YDCST%RLMLT*PFONTE
    
    IF (YDML_PHY_MF%YRPHY%LFGEL) THEN
      
      !  GEL DE SURFACE.
      ZZK = PLSM*YDML_PHY_MF%YRPHY1%GCGELS*MAX(0.0_JPRB, 1.0_JPRB - PVEG / YDML_PHY_MF%YRPHY1%GVEGMXS)*MAX(0.0_JPRB, 1.0_JPRB -  &
      & PLAI / YDML_PHY_MF%YRPHY1%GLAIMXS)*MAX(0.0_JPRB, 1.0_JPRB - PNEIJ / YDML_PHY_MF%YRPHY1%GNEIMXS)
      ZZ = ZZK*(YDCST%RTT - ZTSP) / (YDML_PHY_MF%YRPHY1%RCTGLA*YDCST%RLMLT)
      ZWSP = MAX(0.0_JPRB, PWS)
      ZFGELS = ZWSP / PWSMX*MAX(0.0_JPRB, ZZ)
      ZFGELS = MAX(0.0_JPRB, MIN(ZFGELS, ZWSP*ZITSP))
      ZFONTES = MIN(0.0_JPRB, ZZ)
      ZFONTES = -MAX(0.0_JPRB, MIN(-ZFONTES, PWSI*ZITSP))
      PFGELS = ZFGELS + ZFONTES
      
      !   GEL PROFOND DANS LE CAS DU GEL DE SURFACE
      ZZK = PLSM*YDML_PHY_MF%YRPHY1%GCGEL*MAX(0.0_JPRB, 1.0_JPRB - PVEG / YDML_PHY_MF%YRPHY1%GVEGMX)*MAX(0.0_JPRB, 1.0_JPRB -  &
      & PLAI / YDML_PHY_MF%YRPHY1%GLAIMX)*MAX(0.0_JPRB, 1.0_JPRB - PNEIJ / YDML_PHY_MF%YRPHY1%GNEIMX)
      ZTPP = PTP(JLON, 1) + ZPCTDT*(YDML_PHY_MF%YRPHY1%SODELX(0) / YDML_PHY_MF%YRPHY1%SODELX(1))*PFCHSP
      ZZ = ZZK*(YDCST%RTT - ZTPP) / (YDML_PHY_MF%YRPHY1%RCTGLA*YDCST%RLMLT)
      ZWPP = MAX(0.0_JPRB, PWP - PWS - PWSI)
      ZFGEL = ZWPP / PWPMX*MAX(0.0_JPRB, ZZ)
      ZFGEL = MAX(0.0_JPRB, MIN(ZFGEL, ZWPP*ZITSP))
      ZWPIMX = MIN(YDML_PHY_MF%YRPHY1%GWPIMX, PWPMX)
      ZWPID = MAX(0.0_JPRB, PWPI - PWSI)
      ZFGEL = MAX(0.0_JPRB, MIN((ZWPIMX - ZWPID)*ZITSP, ZFGEL))
      ZFONTE = MIN(0.0_JPRB, ZZ)
      ZFONTE = -MAX(0.0_JPRB, MIN(-ZFONTE, ZWPID*ZITSP))
      PFGEL = ZFGEL + ZFONTE
    ELSE
      PFGEL = 0.0_JPRB
      PFGELS = 0.0_JPRB
    END IF
  ELSE
    PFONTE = 0.0_JPRB
    PFGEL = 0.0_JPRB
    PFGELS = 0.0_JPRB
  END IF
  
  !*******************************************************************************
  !     CALCULS DU TRANSFERT VERTICAL ET DU RUISSELLEMENT SUPERFICIEL.
  !     COMPUTATIONS OF VERTICAL TRANSFER AND OF SUPERFICIAL RUN-OFFS.
  !*******************************************************************************
  PFLWSP = PC2*ZITS*(PWS - PWSEQ)
  ZBFLO = ZBFLO + PFONTE
  ZCI = PC2*YDML_PHY_MF%YRPHY2%TSPHY*ZITS
  IF (YDML_PHY_MF%YRPHY%LFGEL) THEN
    PFLWSP = PC2*ZITS*(PWS - MAX(0.0_JPRB, PWSEQ - PWSI))
    ZWSMX = PWSMX - PWSI
    PFLWSP = MAX(-ZITSP*PWP, PFLWSP)
  ELSE
    ZWSMX = PWSMX
  END IF
  ZFLDT = YDML_PHY_MF%YRPHY2%TSPHY*((PC1*ZBFLO - PFLWSP) / (1.0_JPRB + ZCI) - PFGELS)
  PRUISS = ZITSP*PLSM*(ZFLDT - MIN(ZWSMX - PWS, MAX(-PWS, ZFLDT)))
  
  !*******************************************************************************
  !     CALCUL DU RUISSELLEMENT DU RESERVOIR PROFOND.
  !     COMPUTATION OF DEPP RUN-OFFS.
  !*******************************************************************************
  
  ZFLDT = YDML_PHY_MF%YRPHY2%TSPHY*(ZBFLO + PFTR - PFGEL)
  IF (YDML_PHY_MF%YRPHY%LFGEL) THEN
    ZWPMX = MAX(0.0_JPRB, PWPMX - PWPI)
    ZWFC = MAX(0.0_JPRB, PWFC*PD2*ZCONV - PWPI)
  ELSE
    ZWPMX = PWPMX
    ZWFC = PWFC*PD2*ZCONV
  END IF
  
  PRUISP = ZITSP*PLSM*(ZFLDT - MIN(ZWPMX - PWP, MAX(-PWP, ZFLDT))) + PC3*ZITS*MAX(0.0_JPRB, PWP - ZWFC)
  
  !-----------------------------------------------------------------------
END SUBROUTINE ACDROV_OPENACC
