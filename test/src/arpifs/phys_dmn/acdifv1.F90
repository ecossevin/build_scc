!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACDIFV1 ( YDCST, YGFL,YDML_PHY_MF,KIDIA,KFDIA,KLON,KTDIA,KLEV,KNBTRA,&
 !-----------------------------------------------------------------------
 ! - INPUT  2D .
 & PAPHIF,PAPRSF,PCP,PDELP,PKTROV,PKQROV,PKUROV,&
 & PQ,PQL,PQI,&
 & PRDELP,PT,PU,PV,PSV,PXTROV,PXUROV,&
 & PXDROV,PXHROV,&
 & PFS_UP,PFQT_UP,PFU_UP,PFV_UP,PFTRA_UP,PMF_UP, &
 ! - OUTPUT 2D .
 & PXURO,PXQRO,PXTRO,&
 !
 & PCFAQ,PCFAS,PCFATH,PCFAU,PCFASV,&
 & PCFBQ,PCFBS,PCFBTH,PCFBU,PCFBV,PCFBSV,&
 & PDSE,PQT)

!**** *ACDIFV1 * - CALCULS PRELIMINAIRES POUR LA DIFFUSION VERTICALE.

!     Sujet.
!     ------
!     - ROUTINE DE CALCUL ACTIF .
!       CALCULS DES COEFFICIENTS POUR LA DIFFUSION VERTICALE TURBULENTE
!       DU SOMMET VERS L'AVANT DERNIER NIVEAU POUR LA QUANTITE DE 
!       MOUVEMENT, L'ENERGIE STATIQUE SECHE, LA TEMPERATURE POTENTIELLE,
!       L'HUMIDITE SPECIFIQUE (EN INCLUANT LA CORRECTION DES HUMIDITES 
!       NEGATIVES) ET L'EAU LIQUIDE ET SOLIDE.
!       CAS DU SCHEMA DE SOL VEGETATION EXTERNE (CLE LSVEXT)
!     - COMPUTATION OF COEFFICIENTS FOR VERTICAL TURBULENT DIFFUSION 
!       FROM THE TOP TO THE LAST BUT ONE LAYER FOR MOMENTUM,
!       DRY STATIC ENERGY, POTENTIAL TEMPERATURE, SPECIFIC HUMIDITY 
!       (INCLUDING A NEGATIVE HUMIDITY CORRECTION) AND LIQUID AND SOLID
!       WATER.
!       CASE OF EXTERNAL SOIL VEGETATION SCHEME (LSVEXT KEY)

!**   Interface.
!     ----------
!        *CALL* *ACDIFV1*

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
! KTDIA      : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL).
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".
! KNBTRA     : NOMBRE DE TRACEURS (SCALAIRES PASSIFS, AEROSOL, ESPECES CHIMIQUES).

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .

! PKTROV     : COEFFICIENT D'ECHANGE VERTICAL DE T EN KG/(M*M*S).
! PKQROV     : COEFFICIENT D'ECHANGE VERTICAL DE Q EN KG/(M*M*S).
! PKUROV     : COEFFICIENT D'ECHANGE VERTICAL DE U ET V EN KG/(M*M*S).
! PXTROV     : MULTIPLICATEUR "ANTI-FIBRILLATION" DE PKTROV.
! PXUROV     : MULTIPLICATEUR "ANTI-FIBRILLATION" DE PKUROV.

! - 2D (1:KLEV) .

! PAPHIF     : GEOPOTENTIEL AUX NIVEAUX DES COUCHES.
! PAPRSF     : PRESSION AUX NIVEAUX DES COUCHES.
! PCP        : CHALEUR MASSIQUE A PRESSION CONSTANTE DE L'AIR.
! PDELP      : EPAISSEUR EN PRESSION DE LA COUCHE.
! PQ         : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
! PRDELP     : INVERSE DE L'EPAISSEUR EN PRESSION DE LA COUCHE.
! PT         : TEMPERATURE.
! PU         : COMPOSANTE EN X DU VENT.
! PV         : COMPOSANTE EN Y DU VENT.
! PQL        : HUMIDITE SPECIFIQUE DE L'EAU LIQUIDE.
! PQI        : HUMIDITE SPECIFIQUE DE L'EAU GLACE.
! PSV        : TRACEURS (SCALAIRES PASSIFS, AEROSOLS, ESPECES CHIMIQUES)

! PXDROV     : MULTIPLICATEUR "ANTI-FIBRILLATION" DE PCDROV.
! PXHROV     : MULTIPLICATEUR "ANTI-FIBRILLATION" DE PCHROV.

!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - 2D (1:KLEV) .

! PCFAQ      : COEFFICIENT D'ELIMINATION POUR Q
! PCFAS      : COEFFICIENT D'ELIMINATION POUR S
! PCFATH     : COEFFICIENT D'ELIMINATION POUR THETA
! PCFAU      : COEFFICIENT D'ELIMINATION POUR U ET V


! PCFBQ      : SECOND MEMBRE POUR Q
! PCFBS      : SECOND MEMBRE POUR S
! PCFBTH     : SECOND MEMBRE POUR THETA
! PCFBU      : SECOND MEMBRE POUR U
! PCFBV      : SECOND MEMBRE POUR V
! PCFBSV     : SECOND MEMBRE POUR SCALAIRES PASSIFS


! - 2D (0:KLEV) .

! PDSE       : ENERGIE STATIQUE SECHE

!-----------------------------------------------------------------------

! -   ARGUMENTS IMPLICITES.
!     ---------------------

! COMMON/YOMCST /
! COMMON/YOMPHY2/
! COMMON/YOMPHY4/
! COMMON/FCTTRM/

!-----------------------------------------------------------------------

!     Externes.
!     ---------

!     Methode.
!     --------

!     Auteur.
!     -------
!        2002-01, A.L. Gibelin d'apres ACDIFUS et Polcher et al (1998)

!     Modifications.
!     --------------
!        2004-05, Adaptation a la nouvelle version d'Arpege-Climat (4.2): 
!                 Calcul des flux de ql/qi (PDIFTQL/N) et utilisation en
!                 entree de PKQROV/PKQLROV (J.F. Gueremy) - I. Zuurendonk
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        2007-05, Bug correction on antifibrillation - F. Bouyssel
!        2008-02, Introduction of pTKE - R. Brozkova
!        F. Vana  01-Sep-2008 moving all pTKE related stuff to ACPTKE
!        2008-10, Suppresion correction humidite negative - F. Bouyssel
!        K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!        Y. Bouteloup (Nov 2013) : New formulation for PMMC09
!        R. Roehrig (Sept 2018) : add PKQROV (from JF Guérémy)
!        2022-03, M. Michou : introduce diffusive transport of chemical species (as tracers)
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!        M. Cussac 2022-09 : fixing diffusive transport for tracers when LMDUST=F and NGFLEXT>0
!-----------------------------------------------------------------------

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY :  TCST 

USE YOM_YGFL , ONLY : TYPE_GFLD

!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(MODEL_PHYSICS_MF_TYPE),INTENT(IN):: YDML_PHY_MF
TYPE (TCST), INTENT (IN) :: YDCST
TYPE(TYPE_GFLD)   ,INTENT(IN)    :: YGFL
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KNBTRA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKTROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKQROV(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSV(KLON,KLEV,KNBTRA)

REAL(KIND=JPRB)   ,INTENT(IN)    :: PFS_UP(KLON,0:KLEV)  ! <== explicit flux from mass flux scheme
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFQT_UP(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFU_UP(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFV_UP(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFTRA_UP(KLON,0:KLEV,1:KNBTRA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMF_UP(KLON,0:KLEV)   !<== mass flux (more precisely -mass flux)

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXTROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXDROV(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXHROV(KLON)

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXURO(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXQRO(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PXTRO(KLON,0:KLEV)

REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFAQ(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFAS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFATH(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFAU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFASV(KLON,KLEV,KNBTRA) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFBQ(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFBS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFBTH(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFBU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFBV(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFBSV(KLON,KLEV,KNBTRA)

REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDSE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQT(KLON,KLEV) 
!-----------------------------------------------------------------------

REAL(KIND=JPRB) :: ZIPOI(KLON,KLEV),ZPOID(KLON,KLEV)&
 & ,ZTHETA(KLON,KLEV),ZTL(KLON,KLEV),ZRT(KLON,KLEV)

INTEGER(KIND=JPIM) :: JLEV, JLON, JGFL, JTRA

REAL(KIND=JPRB) :: ZGDT, ZGDTI
REAL(KIND=JPRB) :: ZAE(KLON,KLEV), ZBE(KLON,KLEV), ZCE(KLON,KLEV)
REAL(KIND=JPRB) :: ZAAER(KLON,KLEV), ZBAER(KLON,KLEV), ZCAER(KLON,KLEV)
REAL(KIND=JPRB) :: ZAT(KLON,KLEV), ZBT(KLON,KLEV), ZCT(KLON,KLEV)

REAL(KIND=JPRB) :: ZAU(KLON,KLEV), ZBU(KLON,KLEV), ZCU(KLON,KLEV)

!  Mass flux (0 !) and updraft value (0 also !)
!  For variables which are not taken into account by PMMC09 mass flux scheme
REAL(KIND=JPRB) :: ZMF_UP(KLON,0:KLEV), ZF_UP(KLON,0:KLEV)

REAL(KIND=JPRB) :: ZLV, ZLS
REAL(KIND=JPRB) :: RAERDIFF
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------

#include "tridifv1.intfb.h"

#include "fcttrm.func.h"

!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ACDIFV1',0,ZHOOK_HANDLE)
ASSOCIATE(TSPHY=>YDML_PHY_MF%YRPHY2%TSPHY, LMULAF=>YDML_PHY_MF%YRPHY2%LMULAF, &
 & LMDUST=>YDML_PHY_MF%YRARPHY%LMDUST, &
 & NGFL_EXT=>YGFL%NGFL_EXT, &
 & RATM=>YDCST%RATM, RG=>YDCST%RG, RKAPPA=>YDCST%RKAPPA, &
 & LEDMFI=>YDML_PHY_MF%YRPHY%LEDMFI, LDIFCONS=>YDML_PHY_MF%YRPHY%LDIFCONS, GCVIMPT=>YDML_PHY_MF%YRPHY0%GCVIMPT,&
 & LDIFCEXP=>YDML_PHY_MF%YRPHY%LDIFCEXP)
!-----------------------------------------------------------------------

!  Some initialisation

ZMF_UP(:,:) = 0.0_JPRB
ZF_UP (:,:) = 0.0_JPRB

!     ------------------------------------------------------------------
!     I - VERT DIFF DES VARIABLES CONSERV.
!         VERT DIFF OF CONSERV VARIABLES.

IF ( LDIFCONS ) THEN
!cdir unroll=8
  DO JLEV = KTDIA, KLEV
    DO JLON = KIDIA, KFDIA
      ZLV  = FOLH(PT(JLON,JLEV),0.0_JPRB)
      ZLS  = FOLH(PT(JLON,JLEV),1.0_JPRB)
      PQT(JLON,JLEV) = PQ(JLON,JLEV)+PQL(JLON,JLEV)+PQI(JLON,JLEV)
      ZRT(JLON,JLEV) = PQT(JLON,JLEV)/(1._JPRB-PQT(JLON,JLEV))
      ZTL(JLON,JLEV) = PT(JLON,JLEV)&
       & - (ZLV*PQL(JLON,JLEV)+ZLS*PQI(JLON,JLEV))/PCP(JLON,JLEV)
    ENDDO
  ENDDO
ELSE
!cdir unroll=8
  DO JLEV = KTDIA, KLEV
    DO JLON = KIDIA, KFDIA
      PQT(JLON,JLEV)  = PQ (JLON,JLEV)
      ZTL(JLON,JLEV)  = PT (JLON,JLEV)
    ENDDO
  ENDDO
ENDIF !(LDIFCONS)

!*
!     ------------------------------------------------------------------
!     II - CALCUL DES PARAMETRES DERIVES 

!          COMPUTATION OF DERIVED PARAMETERS 

ZGDT=RG*TSPHY
ZGDTI=1.0_JPRB/ZGDT

IF (LMULAF) THEN
  DO JLEV=KTDIA+1,KLEV-1
    DO JLON=KIDIA,KFDIA
      PXTROV(JLON,JLEV)=MAX(PXTROV(JLON,JLEV),PXTROV(JLON,JLEV-1))
      PXUROV(JLON,JLEV)=MAX(PXUROV(JLON,JLEV),PXUROV(JLON,JLEV-1))
    ENDDO
  ENDDO
  DO JLON=KIDIA,KFDIA
    PXDROV(JLON)=MAX(PXDROV(JLON),PXUROV(JLON,KLEV-1))
    PXHROV(JLON)=MAX(PXHROV(JLON),PXTROV(JLON,KLEV-1))
  ENDDO
ENDIF

!*
!     ------------------------------------------------------------------
!     I - CALCULS PRELIMINAIRES D'ENERGIE STATIQUE SECHE, D'HUMIDITE
!     SPECIFIQUE CORRIGEE DES VALEURS NEGATIVES PAR POMPAGE DEPUIS LE
!     BAS (SAUF DANS LE CAS DU PLUS BAS NIVEAU CONSIDERE COMME POSSEDANT
!     UNE SOURCE FICTIVE A LA SURFACE) ET DE CARACTERISTIQUE DES NIVEAUX
!     DANS L'UNITE DES COEFFICIENTS D'ECHANGE NORMALISES (KG/M**2/S)
!     POUR "POID" ET SON INVERSE POUR "IPOI".

!           PRELIMINARY COMPUTATIONS OF DRY STATIC ENERGY, SPECIFIC
!     HUMIDITY CORRECTED OF ITS NEGATIVE VALUES THROUGH UPWARDS PUMPING
!     (EXCEPT IN THE CASE OF THE LOWEST LEVEL ASSUMED TO POSSES A
!     FICTITIOUS SOURCE AT THE SURFACE) AND OF LAYERS' CHARACTERISTICS
!     IN THE UNITS OF THE NORMALIZED EXCHANGE COEFFICIENTS (KG/M**2/S)
!     FOR "POID" AND ITS INVERSE FOR "IPOI".

! - TEMPORAIRE(S) 2D (1:KLEV) .

! ZPOID     : DP/(RG*DT) POUR UNE COUCHE ET UN PAS DE TEMPS DONNES.
!           : DP/(RG*DT) FOR A GIVEN LAYER AND A GIVEN TIME STEP.
! ZIPOI     : INVERSE DE ZPOID.
!           : INVERSE OF ZPOID.
! ZTHETA    : TEMPERATURE POTENTIELLE
!           : POTENTIAL TEMPERATURE

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZPOID(JLON,JLEV)=PDELP(JLON,JLEV)*ZGDTI
    ZIPOI(JLON,JLEV)=PRDELP(JLON,JLEV)*ZGDT
    PDSE(JLON,JLEV)=PCP(JLON,JLEV)*ZTL(JLON,JLEV)+PAPHIF(JLON,JLEV)
    ZTHETA(JLON,JLEV)=ZTL(JLON,JLEV)&
     & *(RATM/PAPRSF(JLON,JLEV))**RKAPPA  
  ENDDO
ENDDO


!     AMPLIFICATION EVENTUELLE "ANTI-FIBRILLATION" DES COEFFICIENTS
!     D'ECHANGE.
!     POTENTIAL "ANTI-FIBRILLATION" AMPLIFICATION OF THE EXCHANGE
!     COEFFICIENTS.

!cdir unroll=8
DO JLEV=KTDIA,KLEV-1
  DO JLON=KIDIA,KFDIA
    PXURO(JLON,JLEV)=PKUROV(JLON,JLEV)*PXUROV(JLON,JLEV)
  ENDDO
ENDDO


!*

!cdir unroll=8
DO JLEV=KTDIA,KLEV-1
  DO JLON=KIDIA,KFDIA
    PXTRO(JLON,JLEV)=PKTROV(JLON,JLEV)*PXTROV(JLON,JLEV)
  ENDDO
ENDDO
IF (.NOT.LDIFCEXP) THEN
  DO JLEV=KTDIA,KLEV-1
    DO JLON=KIDIA,KFDIA
      PXQRO(JLON,JLEV)=PKTROV(JLON,JLEV)*PXTROV(JLON,JLEV)
    ENDDO
  ENDDO
ELSEIF (LDIFCEXP) THEN
  DO JLEV=KTDIA,KLEV-1
    DO JLON=KIDIA,KFDIA
      PXQRO(JLON,JLEV)=PKQROV(JLON,JLEV)*PXTROV(JLON,JLEV)
    ENDDO
  ENDDO
ENDIF

!  Construction of the triband matrix  (1) Mass Flux part


IF (LEDMFI) THEN
  DO JLON=KIDIA,KFDIA
    ZAE(JLON,KTDIA) =  0._JPRB
    ZBE(JLON,KTDIA) =  - PMF_UP(JLON,KTDIA) * 0.5_JPRB*GCVIMPT*ZIPOI(JLON,KTDIA)
    ZCE(JLON,KTDIA) =  - PMF_UP(JLON,KTDIA) * 0.5_JPRB*GCVIMPT*ZIPOI(JLON,KTDIA)
  ENDDO  

  DO JLEV=KTDIA+1,KLEV-1
    DO JLON=KIDIA,KFDIA
      ZAE(JLON,JLEV) =   PMF_UP(JLON,JLEV-1) * 0.5_JPRB*GCVIMPT *ZIPOI(JLON,JLEV) 
      ZBE(JLON,JLEV) = + PMF_UP(JLON,JLEV-1) * 0.5_JPRB*GCVIMPT*ZIPOI(JLON,JLEV)&
                &      - PMF_UP(JLON,JLEV  ) * 0.5_JPRB*GCVIMPT*ZIPOI(JLON,JLEV)
      ZCE(JLON,JLEV) = - PMF_UP(JLON,JLEV  ) * 0.5_JPRB*GCVIMPT *ZIPOI(JLON,JLEV)
    ENDDO  
  ENDDO

  DO JLON=KIDIA,KFDIA
    ZAE(JLON,KLEV) =  + PMF_UP(JLON,KLEV-1)*0.5_JPRB*GCVIMPT *ZIPOI(JLON,KLEV)  
    ZBE(JLON,KLEV) =  + PMF_UP(JLON,KLEV-1)*0.5_JPRB*GCVIMPT*ZIPOI(JLON,KLEV) 
    ZCE(JLON,KLEV) =  0.0_JPRB         
  ENDDO 
ELSE
  ZAE(:,:) = 0._JPRB
  ZBE(:,:) = 0._JPRB
  ZCE(:,:) = 0._JPRB                  
ENDIF

!  Construction of the triband matrix  (2) Eddy difusivity part T and Q

ZAT(:,:) = 0._JPRB
ZBT(:,:) = 1._JPRB
ZCT(:,:) = 0._JPRB

DO JLON=KIDIA,KFDIA
    ZAT(JLON,KTDIA)           = -PXTRO(JLON,KTDIA)*ZIPOI(JLON,KTDIA)                    ! useless but this initialyse the array !
    ZBT(JLON,KTDIA)           = 1._JPRB + ZBE(JLON,KTDIA) + PXTRO(JLON,KTDIA)*ZIPOI(JLON,KTDIA)  ! Diagonal (ligne 1)
    ZCT(JLON,KTDIA)           = ZCE(JLON,KTDIA) - PXTRO(JLON,KTDIA)*ZIPOI(JLON,KTDIA)            ! Subdiagonal right  (ligne 1)
ENDDO    

DO JLEV=KTDIA+1,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZAT(JLON,JLEV)          = ZAE(JLON,JLEV) - ZIPOI(JLON,JLEV)*PXTRO(JLON,JLEV-1)
    ZBT(JLON,JLEV)          = 1._JPRB + ZBE(JLON,JLEV) + ZIPOI(JLON,JLEV)*(PXTRO(JLON,JLEV)+PXTRO(JLON,JLEV-1))
    ZCT(JLON,JLEV)          = ZCE(JLON,JLEV) - ZIPOI(JLON,JLEV)*PXTRO(JLON,JLEV)
  ENDDO
ENDDO

DO JLON=KIDIA,KFDIA
  ZAT(JLON,KLEV)          = ZAE(JLON,KLEV) - ZIPOI(JLON,KLEV)*PXTRO(JLON,KLEV-1)
  ZBT(JLON,KLEV)          = 1._JPRB + ZBE(JLON,KLEV) + ZIPOI(JLON,KLEV)*(PXTRO(JLON,KLEV)+PXTRO(JLON,KLEV-1))
  ZCT(JLON,KLEV)          = ZIPOI(JLON,KLEV)
ENDDO

!  Construction of the triband matrix  (2bis) Eddy difusivity part aerosols/pass scalars

ZAAER(:,:) = 0._JPRB
ZBAER(:,:) = 1._JPRB
ZCAER(:,:) = 0._JPRB
RAERDIFF   = 10._JPRB  ! coefficient to be put in namelist naeaer

DO JLON=KIDIA,KFDIA
    ZAAER(JLON,KTDIA) = -RAERDIFF*PXTRO(JLON,KTDIA)*ZIPOI(JLON,KTDIA)                     ! useless but this initialyse the array !
    ZBAER(JLON,KTDIA) = 1._JPRB + ZBE(JLON,KTDIA) + RAERDIFF*PXTRO(JLON,KTDIA)*ZIPOI(JLON,KTDIA)  ! Diagonal (ligne 1)
    ZCAER(JLON,KTDIA) = ZCE(JLON,KTDIA) - RAERDIFF*PXTRO(JLON,KTDIA)*ZIPOI(JLON,KTDIA)            ! Subdiagonal right  (ligne 1)
ENDDO

DO JLEV=KTDIA+1,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZAAER(JLON,JLEV)  = ZAE(JLON,JLEV) - ZIPOI(JLON,JLEV)*RAERDIFF*PXTRO(JLON,JLEV-1)
    ZBAER(JLON,JLEV)  = 1._JPRB + ZBE(JLON,JLEV) + ZIPOI(JLON,JLEV)*RAERDIFF*(PXTRO(JLON,JLEV)+PXTRO(JLON,JLEV-1))
    ZCAER(JLON,JLEV)  = ZCE(JLON,JLEV) - ZIPOI(JLON,JLEV)*RAERDIFF*PXTRO(JLON,JLEV)
  ENDDO
ENDDO

DO JLON=KIDIA,KFDIA
  ZAAER(JLON,KLEV)    = ZAE(JLON,KLEV) - ZIPOI(JLON,KLEV)*RAERDIFF*PXTRO(JLON,KLEV-1)
  ZBAER(JLON,KLEV)    = 1._JPRB + ZBE(JLON,KLEV) + ZIPOI(JLON,KLEV)*RAERDIFF*(PXTRO(JLON,KLEV)+PXTRO(JLON,KLEV-1))
  ZCAER(JLON,KLEV)    = ZIPOI(JLON,KLEV)
ENDDO



!  Construction of the triband matrix  (3) Eddy difusivity part U and V

ZAU(:,:) = 0._JPRB
ZBU(:,:) = 1._JPRB
ZCU(:,:) = 0._JPRB

DO JLON=KIDIA,KFDIA
    ZAU(JLON,KTDIA)           = -PXURO(JLON,KTDIA)*ZIPOI(JLON,KTDIA)                    ! useless but this initialyse the arrey !
    ZBU(JLON,KTDIA)           = 1._JPRB + ZBE(JLON,KTDIA) + PXURO(JLON,KTDIA)*ZIPOI(JLON,KTDIA)    ! Diagonal (ligne 1)
    ZCU(JLON,KTDIA)           = ZCE(JLON,KTDIA) - PXURO(JLON,KTDIA)*ZIPOI(JLON,KTDIA)              ! Subdiagonal right  (ligne 1)
ENDDO    

DO JLEV=KTDIA+1,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZAU(JLON,JLEV)          = ZAE(JLON,JLEV) - ZIPOI(JLON,JLEV)*PXURO(JLON,JLEV-1)
    ZBU(JLON,JLEV)          = 1._JPRB + ZBE(JLON,JLEV) + ZIPOI(JLON,JLEV)*(PXURO(JLON,JLEV)+PXURO(JLON,JLEV-1))
    ZCU(JLON,JLEV)          = ZCE(JLON,JLEV) - ZIPOI(JLON,JLEV)*PXURO(JLON,JLEV)
  ENDDO
ENDDO

DO JLON=KIDIA,KFDIA
  ZAU(JLON,KLEV)          = ZAE(JLON,KLEV) - ZIPOI(JLON,KLEV)*PXURO(JLON,KLEV-1)
  ZBU(JLON,KLEV)          = 1._JPRB + ZBE(JLON,KLEV) + ZIPOI(JLON,KLEV)*(PXURO(JLON,KLEV)+PXURO(JLON,KLEV-1))
  ZCU(JLON,KLEV)          = ZIPOI(JLON,KLEV)
ENDDO

! Resolution of the linear system (right member is built in tridifv1)

!---------------- Mixing of S, Qt, theta and scalar --------------

CALL TRIDIFV1(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0,KIDIA,KFDIA,KLON,KTDIA,KLEV,ZAT,ZBT,ZCT,PFS_UP,PMF_UP,ZIPOI,PDSE,PCFAS,PCFBS)
CALL TRIDIFV1(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0,KIDIA,KFDIA,KLON,KTDIA,KLEV,ZAT,ZBT,ZCT,PFQT_UP,PMF_UP,ZIPOI,PQT,PCFAQ,PCFBQ)
! WARNING : For theta and scalar mass flux scheme is not taken into account
CALL TRIDIFV1(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0,KIDIA,KFDIA,KLON,KTDIA,KLEV,ZAT,ZBT,ZCT,ZF_UP,ZMF_UP,ZIPOI,ZTHETA,PCFATH,PCFBTH)

! dans ARPEGE-Climat v6 pas de tableau ZMF_UP=0, mais utilisation de PMF_UP pour les traceurs.
IF (KNBTRA > 0) THEN  
  IF (LMDUST.AND.(NGFL_EXT/=0)) THEN  ! Specific case for NGFL_EXT tracers (unchanged from cy48t1 main) MM no diffusive transport?
     DO JTRA=1, YGFL%NGFL_EXT
        CALL TRIDIFV1(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0,KIDIA,KFDIA,KLON,KTDIA,KLEV,ZAT,ZBT,ZCT,ZF_UP,ZMF_UP,ZIPOI,PSV(:,:,JTRA), &
 &     PCFASV(:,:,JTRA),PCFBSV(:,:,JTRA))
     ENDDO
     DO JTRA=YGFL%NGFL_EXT+1, KNBTRA
       CALL TRIDIFV1(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0,KIDIA,KFDIA,KLON,KTDIA,KLEV,ZAT,ZBT,ZCT,PFTRA_UP(:,:,JTRA),PMF_UP,ZIPOI,PSV(:,:,JTRA), &
 &     PCFASV(:,:,JTRA),PCFBSV(:,:,JTRA))
     ENDDO
  ELSE
     DO JTRA=1,KNBTRA
       CALL TRIDIFV1(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0,KIDIA,KFDIA,KLON,KTDIA,KLEV,ZAAER,ZBAER,ZCAER,PFTRA_UP(:,:,JTRA),PMF_UP,ZIPOI,PSV(:,:,JTRA), &
 &     PCFASV(:,:,JTRA),PCFBSV(:,:,JTRA))
     ENDDO
  ENDIF
ENDIF  

!---------------- Mixing of wind --------------

CALL TRIDIFV1(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0,KIDIA,KFDIA,KLON,KTDIA,KLEV,ZAU,ZBU,ZCU,PFU_UP,PMF_UP,ZIPOI,PU,PCFAU,PCFBU)
CALL TRIDIFV1(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY0,KIDIA,KFDIA,KLON,KTDIA,KLEV,ZAU,ZBU,ZCU,PFV_UP,PMF_UP,ZIPOI,PV,PCFAU,PCFBV)

!     ------------------------------------------------------------------
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACDIFV1',1,ZHOOK_HANDLE)
END SUBROUTINE ACDIFV1
