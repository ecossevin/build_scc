!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACDIFV2 (LDSFORCS, YDCST, YGFL,YDML_PHY_MF,KIDIA,KFDIA,KLON,KTDIA,KLEV,KNBTRA,&
 !-----------------------------------------------------------------------
 ! - INPUT  2D .
 & PAPRS,&
 & PCFAQ,PCFAS,PCFAU,PCFASV,&
 & PCFBQ,PCFBS,PCFBU,PCFBV,PCFBSV,&
 & PKTROV,PKQROV,PKQLROV,PKUROV,&
 & PDSE,PQT,PU,PV,POID,&
 & PT,PQL,PQI,PRDELP,&
 ! - INPUT  1D .
 & PCOEFA,PALPHA1,PLVT,PQICE,&
 & PSFSV,PFCS,PFEV,PFMDU,PFMDV,&
 & PTSN,PXHROV,&
 ! - OUTPUT 2D .
 & PDIFSV,PDIFTQ,PDIFTS,PSTRTU,PSTRTV,&
 & PDIFTQL,PDIFTQI,PDIFCQ,PDIFCS,PSTRCU,PSTRCV,PSHEAR)

!**** *ACDIFV2 * - DIFFUSION VERTICALE TURBULENTE.

!     Sujet.
!     ------
!     - ROUTINE DE CALCUL ACTIF .
!       CALCULS DE DIFFUSION VERTICALE TURBULENTE PAR SUBSTITUTION
!       ASCENDANTE, FLUX TURBULENTS VERTICAUX, 
!       CORRECTION DES FLUX DE RAYONNEMENT THERMIQUE.
!       CAS DU SCHEMA DE SOL VEGETATION EXTERNE (CLE LSVEXT).
!     - COMPUTATION OF VERTICAL TURBULENT DIFFUSION BY BACK
!       SUBSTITUTION, VERTICAL TURBULENT FLUXES,
!       THERMAL RADIATION FLUXES' CORRECTION.
!       CASE OF EXTERNAL SOIL VEGETATION SCHEME (LSVEXT KEY).

!**   Interface.
!     ----------
!        *CALL* *ACDIFV2*

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

! PAPRS      : PRESSION AUX DEMI-NIVEAUX.
! PKTROV     : COEFFICIENT D'ECHANGE VERTICAL DE T EN KG/(M*M*S).
! PKQROV     : COEFFICIENT D'ECHANGE VERTICAL DE Q EN KG/(M*M*S).
! PKQLROV    : COEFFICIENT D'ECHANGE VERTICAL DE QL EN KG/(M*M*S).
! PKUROV     : COEFFICIENT D'ECHANGE VERTICAL DE U ET V EN KG/(M*M*S).
! PT         : TEMPERATURE.
! PQL        : EAU LIQUIDE.
! PQL        : GLACE.
! PRDELP     : INVERSE DE L'EPAISSEUR EN PRESSION DE LA COUCHE.

! - 2D (1:KLEV) .

! PCFAQ      : COEFFICIENT D'ELIMINATION POUR Q.
! PCFAS      : COEFFICIENT D'ELIMINATION POUR S.
! PCFAU      : COEFFICIENT D'ELIMINATION POUR U ET V.
! PCFASV     : COEFFICIENT D'ELIMINATION POUR LES TRACEURS (SCALAIRES PASSIFS, AEROSOL, ESPECES CHIMIQUES).
! PCFBQ      : SECOND MEMBRE POUR Q.
! PCFBS      : SECOND MEMBRE POUR S.
! PCFBU      : SECOND MEMBRE POUR U.
! PCFBV      : SECOND MEMBRE POUR V.
! PCFBSV     : SECOND MEMBRE POUR LES SCALAIRES PASSIFS

! - 1D (PROGNOSTIQUE) .

! PTSN       : TEMPERATURE DE SURFACE AU PAS DE TEMPS +.

! - 1D (DIAGNOSTIQUE) .

! PFCS       : FLUX DE CHALEUR SENSIBLE EN SURFACE.
! PFEV       : FLUX D'EVAPORATION EN SURFACE.
! PSFSV      : FLUX DES TRACEURS EN SURFACE
! PFMDU      : FLUX DE QUANTITE DE MOUVEMENT (U) EN SURFACE.
! PFMDV      : FLUX DE QUANTITE DE MOUVEMENT (V) EN SURFACE.
! PXHROV     : MULTIPLICATEUR "ANTI-FIBRILLATION" EN SURFACE POUR S ET Q.

!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - 2D (0:KLEV) .

! PDIFTQ     : FLUX TURBULENT (ET Q NEGATIF) D'HUMIDITE SPECIFIQUE.
! PDIFTS     : FLUX TURBULENT D'ENTHALPIE (OU D'ENERGIE STATIQUE SECHE).
! PSTRTU     : FLUX TURBULENT DE QUANTITE DE MOUVEMENT "U".
! PSTRTV     : FLUX TURBULENT DE QUANTITE DE MOUVEMENT "V".
! PDIFSV     : FLUX TURBULENT DES TRACEURS (SCALAIRES PASSIFS, AEROSOL, ESPECES CHIMIQUES).
! PDIFCQ     : FLUX CONVECTIF (ET Q NEGATIF) D'HUMIDITE SPECIFIQUE.
! PDIFCS     : FLUX CONVECTIF D'ENTHALPIE (OU D'ENERGIE STATIQUE SECHE).
! PSTRCU     : FLUX CONVECTIF DE QUANTITE DE MOUVEMENT "U".
! PSTRCV     : FLUX CONVECTIF DE QUANTITE DE MOUVEMENT "V".
! PSHEAR     : TERM SOURCE POUR LA TURBULENCE.
!-----------------------------------------------------------------------

! -   ARGUMENTS IMPLICITES.
!     ---------------------

! COMMON/YOMCST /
! COMMON/YOMPHY2/
! COMMON/YOMPHY4/
! COMMON/FCTTRM /

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
!        2002-06, Flux de quantite de mouvement en surface externalisation
!                 du calcul de PGZ0F - A.L. Gibelin
!        2004-05, Adaptation a la nouvelle version d'Arpege-Climat (4.2): 
!                 Calcul des flux de ql/qi (PDIFTQL/N) et utilisation en
!                 entree de PKQROV/PKQLROV (J.F. Gueremy) - I. Zuurendonk
!        2004-09: Supprimer calcul de PFRTH pour externalisation boucle
!                 temporelle du schema de surface - I. Zuurendonk
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        2007-05, Bug correction on antifibrillation - F. Bouyssel
!        2008-02, Projection sur Ql & Qi si L3MT - R. Brozkova
!        2008-10, Suppression correction humidite negative - F. Bouyssel
!        K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!        Y. Bouteloup (Nov 2013) LEDKFI case
!        2014-10, TOUCANS - shear term, - R. Brozkova
!        Y. Bouteloup (July 2016) rename LEDKFI in LEDMFI
!        Y. Bouteloup (August 2016) Split between turbulent and convectif fluxes
!                           in LEDMFI case
!        2017-09, removed incorrect protection of shear term - R. Brozkova
!        2018-09, R. Roehrig: add PKQROV,PKQLROV + LNODIFQC + LDIFQL (from JF Guérémy)
!        2022-03, M. Michou : introduce diffusive transport of chemical species (as tracers)
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!-----------------------------------------------------------------------

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOMCST   , ONLY : TCST 
USE YOM_YGFL , ONLY : TYPE_GFLD

!-----------------------------------------------------------------------

IMPLICIT NONE

LOGICAL           ,INTENT(IN)    :: LDSFORCS
TYPE(MODEL_PHYSICS_MF_TYPE),INTENT(IN):: YDML_PHY_MF
TYPE(TCST)        , INTENT(IN)   :: YDCST
TYPE(TYPE_GFLD)   ,INTENT(IN)    :: YGFL
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KNBTRA
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCFAQ(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCFAS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCFAU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCFASV(KLON,KLEV,KNBTRA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCFBQ(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCFBS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCFBU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCFBV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCFBSV(KLON,KLEV,KNBTRA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKTROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKQROV(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKQLROV(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFCS(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFEV(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSFSV(KLON,KNBTRA) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFMDU(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFMDV(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSN(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXHROV(KLON) 

REAL(KIND=JPRB)   ,INTENT(IN)    :: PALPHA1(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCOEFA(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLVT(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQICE(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRDELP(KLON,KLEV)

REAL(KIND=JPRB)   ,INTENT(IN)    :: PDSE(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PU(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PV(KLON,KLEV)

REAL(KIND=JPRB)   ,INTENT(IN)    :: POID(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFSV(KLON,0:KLEV,KNBTRA)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PDIFTQ(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PDIFTS(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSTRTU(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSTRTV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFCQ(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFCS(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSTRCU(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSTRCV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFTQL(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFTQI(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSHEAR(KLON,KLEV)

!-----------------------------------------------------------------------

REAL(KIND=JPRB) :: ZKQRV(KLON,0:KLEV)&
 & ,ZKSRV(KLON,0:KLEV),ZN1(KLON,KLEV),ZN2(KLON,KLEV) &
 & ,ZNSV1(KLON,KLEV,KNBTRA),ZKSVRV(KLON,0:KLEV) &
 & ,ZEDMFQ(KLON,0:KLEV),ZEDMFS(KLON,0:KLEV) &
 & ,ZEDMFU(KLON,0:KLEV),ZEDMFV(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZIPOI(KLON,KLEV),ZSUB1(KLON,KLEV)

INTEGER(KIND=JPIM) :: JLEV, JLON, JGFL

REAL(KIND=JPRB) :: ZCPVMD,ZEXNER
REAL(KIND=JPRB) :: ZGDT,ZMUL,ZELIM,ZQC,ZTH,ZQICE
REAL(KIND=JPRB) :: ZDIFTS,ZDIFTQ,ZDIFTQC
REAL(KIND=JPRB) :: RAERDIFF

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------

#include "fcttrm.func.h"
#include "fctdoi.func.h"

!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ACDIFV2',0,ZHOOK_HANDLE)
ASSOCIATE(TSPHY=>YDML_PHY_MF%YRPHY2%TSPHY,RDTFAC=>YDML_PHY_MF%YRPHY0%RDTFAC, &
 & LMSE=>YDML_PHY_MF%YRARPHY%LMSE, LMDUST=>YDML_PHY_MF%YRARPHY%LMDUST, &
 & NGFL_EXT=>YGFL%NGFL_EXT, &
 & RATM=>YDCST%RATM, RCPD=>YDCST%RCPD, RCPV=>YDCST%RCPV, RG=>YDCST%RG, RKAPPA=>YDCST%RKAPPA, &
 & L3MT=>YDML_PHY_MF%YRPHY%L3MT, LCOEFKTKE=>YDML_PHY_MF%YRPHY%LCOEFKTKE, LSTRAPRO=>YDML_PHY_MF%YRPHY%LSTRAPRO, &
 & LEDMFI=>YDML_PHY_MF%YRPHY%LEDMFI, LEDMFS=>YDML_PHY_MF%YRPHY%LEDMFS, LDIFCONS=>YDML_PHY_MF%YRPHY%LDIFCONS,&
 & LDIFCEXP=>YDML_PHY_MF%YRPHY%LDIFCEXP,LNODIFQC=>YDML_PHY_MF%YRPHY%LNODIFQC,LDIFQL=>YDML_PHY_MF%YRPHY%LDIFQL)
!-----------------------------------------------------------------------

!*
!     ------------------------------------------------------------------
!     I - CONSTANTES AUXILIAIRES.

!         AUXILIARY CONSTANTS.

ZCPVMD=RCPV-RCPD
ZGDT=RG*TSPHY

!*
!     ------------------------------------------------------------------
!     IV - CALCULS POUR LA QUANTITE DE MOUVEMENT : CONDITION DE SURFACE,
!     CALCUL DE "CHARNOCK" PUIS SUBSTITUTION AVEC CALCUL DES FLUX A TOUS
!     LES NIVEAUX.

!          COMPUTATION FOR MOMENTUM : SURFACE CONDITION, "CHARNOCK-TYPE"
!     CALCULATION THEN SUBSTITUTION WITH FLUXES' COMPUTATION AT ALL
!     LEVELS.

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZN1(JLON,JLEV)=PCFBU(JLON,JLEV)
    ZN2(JLON,JLEV)=PCFBV(JLON,JLEV)
  ENDDO
ENDDO


!     CALCULS EN SURFACE.
!     SURFACE CALCULATIONS.

DO JLON=KIDIA,KFDIA
  PSTRTU(JLON,KLEV) = PFMDU(JLON)
  PSTRTV(JLON,KLEV) = PFMDV(JLON)
ENDDO

IF (LCOEFKTKE) THEN
  DO JLON=KIDIA,KFDIA
    PSHEAR(JLON,KLEV)= PSTRTU(JLON,KLEV)*ZN1(JLON,KLEV)+ &
     &                 PSTRTV(JLON,KLEV)*ZN2(JLON,KLEV)
  ENDDO
ENDIF

IF  (LEDMFI .AND. LEDMFS) THEN 
  DO JLON=KIDIA,KFDIA
     ZEDMFU(JLON,KLEV) = PSTRTU(JLON,KLEV)
     ZEDMFV(JLON,KLEV) = PSTRTV(JLON,KLEV)
  ENDDO
ENDIF

IF (LMSE .OR. LDSFORCS .OR. LEDMFI) THEN
  DO JLON=KIDIA,KFDIA
    ZN1(JLON,KLEV) = ZN1(JLON,KLEV) + PCFAU(JLON,KLEV)*PSTRTU(JLON,KLEV)
    ZN2(JLON,KLEV) = ZN2(JLON,KLEV) + PCFAU(JLON,KLEV)*PSTRTV(JLON,KLEV)
  ENDDO
ENDIF


!     SUBSTITUTION POUR UNE COUCHE STANDARD ET AU SOMMET.
!     BACK-SUBSTITUTION FOR A STANDARD LAYER AND AT THE TOP.

IF (LEDMFI) THEN
   DO JLEV=KLEV-1,KTDIA,-1
     DO JLON=KIDIA,KFDIA
       ZN1(JLON,JLEV)=ZN1(JLON,JLEV)+PCFAU(JLON,JLEV)*ZN1(JLON,JLEV+1)
       ZN2(JLON,JLEV)=ZN2(JLON,JLEV)+PCFAU(JLON,JLEV)*ZN2(JLON,JLEV+1)
       IF (LEDMFS) THEN
         ZEDMFU(JLON,JLEV)=ZEDMFU(JLON,JLEV+1)+POID(JLON,JLEV+1)*(ZN1(JLON,JLEV+1)-PU(JLON,JLEV+1))
         ZEDMFV(JLON,JLEV)=ZEDMFV(JLON,JLEV+1)+POID(JLON,JLEV+1)*(ZN2(JLON,JLEV+1)-PV(JLON,JLEV+1))
         PSTRTU(JLON,JLEV)=PKUROV(JLON,JLEV)*(ZN1(JLON,JLEV)-ZN1(JLON,JLEV+1))
         PSTRTV(JLON,JLEV)=PKUROV(JLON,JLEV)*(ZN2(JLON,JLEV)-ZN2(JLON,JLEV+1))
         PSTRCU(JLON,JLEV)=ZEDMFU(JLON,JLEV) - PSTRTU(JLON,JLEV)
         PSTRCV(JLON,JLEV)=ZEDMFV(JLON,JLEV) - PSTRTV(JLON,JLEV)
       ELSE
         PSTRTU(JLON,JLEV)=PSTRTU(JLON,JLEV+1)+POID(JLON,JLEV+1)*(ZN1(JLON,JLEV+1)-PU(JLON,JLEV+1))
         PSTRTV(JLON,JLEV)=PSTRTV(JLON,JLEV+1)+POID(JLON,JLEV+1)*(ZN2(JLON,JLEV+1)-PV(JLON,JLEV+1))
       ENDIF   
     ENDDO
   ENDDO

ELSE

   DO JLEV=KLEV-1,KTDIA,-1
     DO JLON=KIDIA,KFDIA
       ZN1(JLON,JLEV)=ZN1(JLON,JLEV)+PCFAU(JLON,JLEV)*ZN1(JLON,JLEV+1)
       ZN2(JLON,JLEV)=ZN2(JLON,JLEV)+PCFAU(JLON,JLEV)*ZN2(JLON,JLEV+1)
       PSTRTU(JLON,JLEV)=PKUROV(JLON,JLEV)*(ZN1(JLON,JLEV)-ZN1(JLON,JLEV+1))
       PSTRTV(JLON,JLEV)=PKUROV(JLON,JLEV)*(ZN2(JLON,JLEV)-ZN2(JLON,JLEV+1))
       IF (LCOEFKTKE) THEN
         PSHEAR(JLON,JLEV)=                                      &
          & PSTRTU(JLON,JLEV)*(ZN1(JLON,JLEV)-ZN1(JLON,JLEV+1))+ &
          & PSTRTV(JLON,JLEV)*(ZN2(JLON,JLEV)-ZN2(JLON,JLEV+1))
       ENDIF
     ENDDO
   ENDDO

ENDIF


!     CONDITION A LA LIMITE SUPERIEURE.
!     UPPER BOUNDARY CONDITION.

  DO JLON=KIDIA,KFDIA
    PSTRTU(JLON,KTDIA-1)=0.0_JPRB
    PSTRTV(JLON,KTDIA-1)=0.0_JPRB
       IF (LEDMFS) THEN
         PSTRCU(JLON,KTDIA-1)=0.0_JPRB
         PSTRCV(JLON,KTDIA-1)=0.0_JPRB
       ENDIF  
  ENDDO


!*
!     ------------------------------------------------------------------
!     V - CALCULS POUR L'ENERGIE STATIQUE SECHE ET L'HUMIDITE SPECIFIQUE :
!     CALCULS D'ELIMINATION EN SURFACE POUR S ET  Q.

!          COMPUTATIONS FOR TEMPERATURE AND SPECIFIC HUMIDITY :
!     SURFACE ELIMINATION CALCULATION  FOR  S AND Q.

! - TEMPORAIRE(S) 2D (0:KLEV) .

! ZKQRV     : VALEUR (DUPLIQUEE DE PKTROV) POUR LA DIFFUSION DE Q.
!            : VALUE (DUPLICATED FROM PKTROV) FOR THE Q DIFFUSION.
! ZKSRV     : VALEUR (DUPLIQUEE DE PKTROV) POUR LA DIFFUSION DE S.
!            : VALUE (DUPLICATED FROM PKTROV) FOR THE S DIFFUSION.

!     DUPLICATION DES COEFFICIENTS D'ECHANGE VERTICAL DE S POUR Q.
!     DUPLICATION OF THE VERTICAL EXCHANGE COEFFICIENTS FOR S AND Q.

DO JLEV=KTDIA,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZKQRV(JLON,JLEV)=PKTROV(JLON,JLEV)
    ZKSRV(JLON,JLEV)=PKTROV(JLON,JLEV)
  ENDDO
ENDDO

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZN1(JLON,JLEV)=PCFBS(JLON,JLEV)
    ZN2(JLON,JLEV)=PCFBQ(JLON,JLEV)
  ENDDO
ENDDO

IF (LMSE .OR. LDSFORCS) THEN

  DO JLON=KIDIA,KFDIA
    ZEXNER       =  (PAPRS(JLON,KLEV)/RATM)**RKAPPA
    PDIFTQ(JLON,KLEV) = PFEV(JLON)
    PDIFTS(JLON,KLEV) = PFCS(JLON)*ZEXNER + PFEV(JLON)*ZCPVMD*PTSN(JLON)
  ENDDO
ENDIF

IF  (LEDMFI .AND. LEDMFS) THEN 
  DO JLON=KIDIA,KFDIA
     ZEDMFQ(JLON,KLEV) = PDIFTQ(JLON,KLEV)
     ZEDMFS(JLON,KLEV) = PDIFTS(JLON,KLEV)
  ENDDO
ENDIF

IF (LMSE .OR. LDSFORCS .OR. LEDMFI) THEN
  DO JLON=KIDIA,KFDIA
    ZN1(JLON,KLEV)  =ZN1(JLON,KLEV)+&
      &PXHROV(JLON)*PCFAS(JLON,KLEV)*PDIFTS(JLON,KLEV)
    ZN2(JLON,KLEV)  =ZN2(JLON,KLEV)+&
      &PXHROV(JLON)*PCFAQ(JLON,KLEV)*PDIFTQ(JLON,KLEV)
  ENDDO


!*
!     ------------------------------------------------------------------
!     VI - CALCULS POUR LA TEMPERATURE ET L'HUMIDITE SPECIFIQUE : PARTIE
!     SUBSTITUTION ET CALCUL DES FLUX A TOUS LES NIVEAUX

!          COMPUTATIONS FOR TEMPERATURE AND SPECIFIC HUMIDITY :
!     BACK-SUBSTITUTION PART AND CALCULATION OF FLUXES AT ALL LEVELS


!     SUBSTITUTION POUR UNE COUCHE STANDARD ET AU SOMMET POUR S ET Q.
!     BACK-SUBSTITUTION FOR A STANDARD LEVEL AND AT THE TOP FOR S AND Q.

  IF (LEDMFI) THEN
     DO JLEV=KLEV-1,KTDIA,-1
       DO JLON=KIDIA,KFDIA
         ZN1(JLON,JLEV)=ZN1(JLON,JLEV)+PCFAS(JLON,JLEV)*ZN1(JLON,JLEV+1)
         ZN2(JLON,JLEV)=ZN2(JLON,JLEV)+PCFAQ(JLON,JLEV)*ZN2(JLON,JLEV+1)
         IF (LEDMFS) THEN
           ZEDMFQ(JLON,JLEV)=ZEDMFQ(JLON,JLEV+1)+POID(JLON,JLEV+1)*(ZN2(JLON,JLEV+1)-PQT(JLON,JLEV+1))
           ZEDMFS(JLON,JLEV)=ZEDMFS(JLON,JLEV+1)+POID(JLON,JLEV+1)*(ZN1(JLON,JLEV+1)-PDSE(JLON,JLEV+1))
           PDIFTQ(JLON,JLEV)=ZKQRV(JLON,JLEV)*(ZN2(JLON,JLEV)-ZN2(JLON,JLEV+1))
           PDIFTS(JLON,JLEV)=ZKSRV(JLON,JLEV)*(ZN1(JLON,JLEV)-ZN1(JLON,JLEV+1))
           PDIFCQ(JLON,JLEV)=ZEDMFQ(JLON,JLEV) - PDIFTQ(JLON,JLEV)
           PDIFCS(JLON,JLEV)=ZEDMFS(JLON,JLEV) - PDIFTS(JLON,JLEV)
         ELSE
           PDIFTQ(JLON,JLEV)=PDIFTQ(JLON,JLEV+1)+POID(JLON,JLEV+1)*(ZN2(JLON,JLEV+1)-PQT(JLON,JLEV+1))
           PDIFTS(JLON,JLEV)=PDIFTS(JLON,JLEV+1)+POID(JLON,JLEV+1)*(ZN1(JLON,JLEV+1)-PDSE(JLON,JLEV+1)) 
         ENDIF            
       ENDDO
     ENDDO
  ELSE
     DO JLEV=KLEV-1,KTDIA,-1
       DO JLON=KIDIA,KFDIA
         ZN1(JLON,JLEV)=ZN1(JLON,JLEV)+PCFAS(JLON,JLEV)*ZN1(JLON,JLEV+1)
         ZN2(JLON,JLEV)=ZN2(JLON,JLEV)+PCFAQ(JLON,JLEV)*ZN2(JLON,JLEV+1)
         PDIFTQ(JLON,JLEV)=ZKQRV(JLON,JLEV)*(ZN2(JLON,JLEV)-ZN2(JLON,JLEV+1))
         PDIFTS(JLON,JLEV)=ZKSRV(JLON,JLEV)*(ZN1(JLON,JLEV)-ZN1(JLON,JLEV+1))
       ENDDO
     ENDDO
  ENDIF

!     CONDITIONS A LA LIMITE SUPERIEURE.
!     UPPER BOUNDARY CONDITIONS.

  DO JLON=KIDIA,KFDIA
    PDIFTQ(JLON,KTDIA-1)=0.0_JPRB
    PDIFTS(JLON,KTDIA-1)=0.0_JPRB
    IF (LEDMFS) THEN
      PDIFCQ(JLON,KTDIA-1)=0.0_JPRB
      PDIFCS(JLON,KTDIA-1)=0.0_JPRB
    ENDIF  
  ENDDO

  IF ( (L3MT.OR.LSTRAPRO.OR..NOT.LNODIFQC) .AND. LDIFCONS .AND. (.NOT. LCOEFKTKE) ) THEN
  !     ON PASSE DES VARIABLES "CONSERVATIVES" AUX VARIABLES "SECHES".
    DO JLEV = KLEV-1,KTDIA,-1
      DO JLON = KIDIA, KFDIA
        ZDIFTS=ZKSRV(JLON,JLEV)*(ZN1(JLON,JLEV)-ZN1(JLON,JLEV+1))
        ZDIFTQ=ZKQRV(JLON,JLEV)*(ZN2(JLON,JLEV)-ZN2(JLON,JLEV+1))
        ZDIFTQC=PCOEFA(JLON,JLEV)*(ZDIFTQ-PALPHA1(JLON,JLEV)*ZDIFTS)
        PDIFTS (JLON,JLEV)=ZDIFTS+ZDIFTQC*PLVT(JLON,JLEV)
        PDIFTQ (JLON,JLEV)=ZDIFTQ-ZDIFTQC
        PDIFTQL(JLON,JLEV)=ZDIFTQC*(1.0_JPRB-PQICE(JLON,JLEV))
        PDIFTQI(JLON,JLEV)=ZDIFTQC*PQICE(JLON,JLEV)
      ENDDO
    ENDDO
  ENDIF

!     ------------------------------------------------------------------
!     VII - CALCULS POUR LES SCALAIRES PASSIFS : PARTIE
!     SUBSTITUTION ET CALCUL DES FLUX A TOUS LES NIVEAUX
!     SUBSTITUTION POUR UNE COUCHE STANDARD ET AU SOMMET POUR SVS.
!     BACK-SUBSTITUTION FOR A STANDARD LEVEL AND AT THE TOP FOR SVS.

  IF (KNBTRA > 0) THEN
    RAERDIFF   = 10._JPRB  ! coefficient to be put in namelist naeaer
    DO JGFL=1,KNBTRA 
      DO JLEV=KTDIA,KLEV
      DO JLON=KIDIA,KFDIA
        ZNSV1(JLON,JLEV,JGFL)=PCFBSV(JLON,JLEV,JGFL)
      ENDDO
      ENDDO

! ZKSVRV     : VALEUR (DUPLIQUEE DE PKTROV) POUR LA DIFFUSION DE SVS.
!            : VALUE (DUPLICATED FROM PKTROV) FOR THE SVS DIFFUSION.

!     DUPLICATION DES COEFFICIENTS D'ECHANGE VERTICAL DE SVS.


      DO JLEV=KTDIA,KLEV-1
      DO JLON=KIDIA,KFDIA
        ZKSVRV(JLON,JLEV)=RAERDIFF*PKTROV(JLON,JLEV)
      ENDDO
      ENDDO

!     CALCULS EN SURFACE.
!     SURFACE CALCULATIONS.

      DO JLON=KIDIA,KFDIA
        PDIFSV(JLON,KLEV,JGFL) = PSFSV(JLON,JGFL)
        ZNSV1(JLON,KLEV,JGFL)  =ZNSV1(JLON,KLEV,JGFL)+&
          &PXHROV(JLON)*PCFASV(JLON,KLEV,JGFL)*PDIFSV(JLON,KLEV,JGFL)
      ENDDO

!     SUBSTITUTION POUR UNE COUCHE STANDARD ET AU SOMMET.
!     BACK-SUBSTITUTION FOR A STANDARD LAYER AND AT THE TOP.

      DO JLEV=KLEV-1,KTDIA,-1
      DO JLON=KIDIA,KFDIA
        ZNSV1(JLON,JLEV,JGFL)=ZNSV1(JLON,JLEV,JGFL)+PCFASV(JLON,JLEV,JGFL)*ZNSV1(JLON,JLEV+1,JGFL)
        PDIFSV(JLON,JLEV,JGFL)=ZKSVRV(JLON,JLEV)*(ZNSV1(JLON,JLEV,JGFL)-ZNSV1(JLON,JLEV+1,JGFL))
      ENDDO
      ENDDO

!     CONDITION A LA LIMITE SUPERIEURE.
!     UPPER BOUNDARY CONDITION.

      DO JLON=KIDIA,KFDIA
        PDIFSV(JLON,KTDIA-1,JGFL)=0.0_JPRB
      ENDDO

    ENDDO  !  
  ENDIF    !  ENDIF (KNBTRA)

ELSE

  IF ( (L3MT.OR.LSTRAPRO.OR..NOT.LNODIFQC) .AND. LDIFCONS .AND. (.NOT. LCOEFKTKE) ) THEN

  !     ON PASSE DES VARIABLES "CONSERVATIVES" AUX VARIABLES "SECHES".
  !     FROM "MOIST AND  CONSERVATIVE" VARIABLES TO THE "DRY" ONES.

    DO JLEV = KLEV-1,KTDIA,-1
      DO JLON = KIDIA, KFDIA
        ZDIFTS=ZKSRV(JLON,JLEV)*(ZN1(JLON,JLEV)-ZN1(JLON,JLEV+1))
        ZDIFTQ=ZKQRV(JLON,JLEV)*(ZN2(JLON,JLEV)-ZN2(JLON,JLEV+1))
        ZDIFTQC=PCOEFA(JLON,JLEV)*(ZDIFTQ-PALPHA1(JLON,JLEV)*ZDIFTS)
        PDIFTS (JLON,JLEV)=ZDIFTS+ZDIFTQC*PLVT(JLON,JLEV)
        PDIFTQ (JLON,JLEV)=ZDIFTQ-ZDIFTQC
        PDIFTQL(JLON,JLEV)=ZDIFTQC*(1.0_JPRB-PQICE(JLON,JLEV))
        PDIFTQI(JLON,JLEV)=ZDIFTQC*PQICE(JLON,JLEV)
      ENDDO
    ENDDO

  ELSE

    DO JLEV=KLEV-1,KTDIA,-1
      DO JLON=KIDIA,KFDIA
        PDIFTS (JLON,JLEV)=ZKSRV(JLON,JLEV)*(ZN1(JLON,JLEV)-ZN1(JLON,JLEV+1))
        PDIFTQ (JLON,JLEV)=ZKQRV(JLON,JLEV)*(ZN2(JLON,JLEV)-ZN2(JLON,JLEV+1))
        PDIFTQL(JLON,JLEV)=0.0_JPRB
        PDIFTQI(JLON,JLEV)=0.0_JPRB
      ENDDO
    ENDDO

  ENDIF

!     CONDITION A LA LIMITE SUPERIEURE.
!     UPPER BOUNDARY CONDITION.

  DO JLON=KIDIA,KFDIA
    PDIFTQ (JLON,KTDIA-1)=0.0_JPRB
    PDIFTS (JLON,KTDIA-1)=0.0_JPRB
    PDIFTQL(JLON,KTDIA-1)=0.0_JPRB
    PDIFTQI(JLON,KTDIA-1)=0.0_JPRB
  ENDDO

!     CONDITIONS A LA LIMITE INFERIEURE.
!     LOWER BOUNDARY CONDITIONS.

  DO JLON=KIDIA,KFDIA
    PDIFTQL(JLON,KLEV)=0.0_JPRB
    PDIFTQI(JLON,KLEV)=0.0_JPRB
  ENDDO

ENDIF

IF (LDIFQL) THEN
  IF (LDIFCEXP) THEN
!*
!     ------------------------------------------------------------------
!     VII - CALCULS POUR L'EAU LIQUIDE ET SOLIDE

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZIPOI(JLON,JLEV)=PRDELP(JLON,JLEV)*ZGDT
  ENDDO
ENDDO

!     ELIMINATION AU SOMMET.
!     ELIMINATION AT THE TOP.

DO JLON=KIDIA,KFDIA
  ZKQRV(JLON,KTDIA)=PKQLROV(JLON,KTDIA)

  ZMUL=1._JPRB/(1._JPRB+ZKQRV(JLON,KTDIA)*ZIPOI(JLON,KTDIA))
  ZSUB1(JLON,KTDIA)=ZMUL*ZKQRV(JLON,KTDIA)*ZIPOI(JLON,KTDIA)
  ZQC=PQL(JLON,KTDIA)+PQI(JLON,KTDIA)
  ZN1(JLON,KTDIA)=ZMUL*ZQC
ENDDO

!     ELIMINATION POUR UNE COUCHE STANDARD.
!     ELIMINATION FOR A STANDART LEVEL.

DO JLEV=KTDIA+1,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZKQRV(JLON,JLEV)=PKQLROV(JLON,JLEV)

    ZELIM=ZKQRV(JLON,JLEV-1)*ZIPOI(JLON,JLEV)
    ZMUL=1._JPRB/(1._JPRB+ZELIM*(1._JPRB-ZSUB1(JLON,JLEV-1))+ZKQRV(JLON,JLEV)&
     &*ZIPOI(JLON,JLEV))
    ZSUB1(JLON,JLEV)=ZMUL*ZKQRV(JLON,JLEV)*ZIPOI(JLON,JLEV)
    ZQC=PQL(JLON,JLEV)+PQI(JLON,JLEV)
    ZN1(JLON,JLEV)=ZMUL*(ZQC+ZELIM*ZN1(JLON,JLEV-1))
  ENDDO
ENDDO

!     CALCULS EN SURFACE.
!     SURFACE CALCULATIONS.

DO JLON=KIDIA,KFDIA
  ZELIM=ZKQRV(JLON,KLEV-1)*ZIPOI(JLON,KLEV)
  ZMUL=1._JPRB/(1._JPRB+ZELIM*(1._JPRB-ZSUB1(JLON,KLEV-1)))
  ZQC=PQL(JLON,KLEV)+PQI(JLON,KLEV)
  ZN1(JLON,KLEV)=ZMUL*(ZQC+ZELIM*ZN1(JLON,KLEV-1))
ENDDO

!     SUBSTITUTION POUR UNE COUCHE STANDARD ET AU SOMMET.
!     BACK-SUBSTITUTION FOR A STANDART LAYER AND AT THE TOP.

DO JLEV=KLEV-1,KTDIA,-1
  DO JLON=KIDIA,KFDIA
    ZN1(JLON,JLEV)=ZN1(JLON,JLEV)+ZSUB1(JLON,JLEV)*ZN1(JLON,JLEV+1)
    PDIFTQL(JLON,JLEV)=PKQLROV(JLON,JLEV)*(ZN1(JLON,JLEV)-ZN1(JLON,JLEV+1))
  ENDDO
ENDDO

DO JLEV=KTDIA,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZTH=(PT(JLON,JLEV)+PT(JLON,JLEV+1))*0.5_JPRB
    ZQICE=FONICE(ZTH,RDTFAC)
    PDIFTQI(JLON,JLEV)=ZQICE*PDIFTQL(JLON,JLEV)
    PDIFTQL(JLON,JLEV)=(1._JPRB-ZQICE)*PDIFTQL(JLON,JLEV)
  ENDDO
ENDDO
  ENDIF !LDIFCEXP
ENDIF !LDIFQL

!-----------------------------------------------------------------------
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('ACDIFV2',1,ZHOOK_HANDLE)
END SUBROUTINE ACDIFV2
