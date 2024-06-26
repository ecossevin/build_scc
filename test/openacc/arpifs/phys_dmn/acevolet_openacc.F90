SUBROUTINE ACEVOLET_OPENACC (YDCST, YDML_PHY_MF, KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV, PAPHI, PAPHIF, PAPRS, PAPRSF, PDELP,  &
& PECT, PKUROV, PR, PT, PU, PV, PUSLE, PPRODTH, PKCLS, PECTCLS, PECT1, PPRDY, PDIFF, PDISS, YDSTACK)
  !-----------------------------------------------------------------------
  ! - INPUT  2D .
  ! - INPUT  1D .
  ! - OUTPUT 2D .
  
  !**** *ACEVOLET* - EVOLUTION DE L'ENERGIE CINETIQUE TURBULENTE.
  
  !     Sujet.
  !     ------
  !     - ROUTINE DE CALCUL ACTIF .
  !       CALCUL DES DIFFERENTS FACTEURS INTERVENANT DANS L'EQUATION
  !       D'EVOLUTION DE l'ENERGIE CINETIQUE TURBULENTE (DIMENSION M2/S3)
  
  !**   Interface.
  !     ----------
  !        *CALL* *ACEVOLET*
  
  !-----------------------------------------------------------------------
  ! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
  !          "APLPAR" CODE, EXCEPT FOR KTDIAT AND KTDIAN.
  !-----------------------------------------------------------------------
  
  ! -   ARGUMENTS D'ENTREE.
  !     -------------------
  
  ! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.
  
  ! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
  ! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
  ! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
  ! KTDIAT     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
  !              POUR LES CALCULS DE TURBULENCE.
  ! KTDIAN     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
  !              POUR LES CALCULS DE TURBULENCE + NEBULOSITE.
  ! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".
  
  ! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
  !   CATEGORIE).
  
  ! - 2D (0:KLEV) .
  
  ! PAPHI      : GEOPOTENTIEL AUX DEMI-NIVEAUX.
  ! PAPRS      : PRESSION AUX DEMI-NIVEAUX.
  ! PKUROV     : COEFFICIENT D'ECHANGE VERTICAL DE U ET V EN KG/(M*M*S).
  
  ! - 2D (1:KLEV) .
  
  ! PAPHIF     : GEOPOTENTIEL AUX NIVEAUX DES COUCHES.
  ! PLSCPE     : RAPPORT EFECTIF DES L ET CP EN CONDENSATION/EVAPORATION.
  ! PAPRSF     : PRESSION AU NIVEAU DES COUCHES.
  ! PDELP      : EPAISSEUR EN PRESSION DE LA COUCHE.
  ! PR         : CONSTANTE DES GAZ POUR L'AIR.
  ! PT         : TEMPERATURE.
  ! PU         : COMPOSANTE EN X DU VENT.
  ! PUSLE      : INVERSE DE G FOIS * LONGUEUR DE MELANGE.
  ! PV         : COMPOSANTE EN Y DU VENT.
  ! PPRODTH    : LA PRODUCTION THERMIQUE : +(g/T)*(w'X') avec X=(THETA)vl
  
  ! - 1D (DIAGNOSTIQUE) .
  
  ! PKCLS      : G FOIS LE COEFFICIENT D'ECHANGE DANS LA C.L.S.
  ! PECTCLS    : ENERGIE CINETIQUE TURBULENTE DANS LA C.L.S.
  
  !-----------------------------------------------------------------------
  
  ! -   ARGUMENTS DE SORTIE.
  !     --------------------
  
  ! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
  !   CATEGORIE).
  
  ! PECT1      : NOUVELLE ECT.
  ! PPRDY      : Dynamical production (m2/s3)
  ! PDIFF      : TKE diffusion (m2/s3)
  ! PDISS      : Dissipation (m2/s3)
  !-----------------------------------------------------------------------
  
  ! -   ARGUMENTS IMPLICITES.
  !     ---------------------
  
  ! COMMON/YOMCST /
  ! COMMON/YOMPHY0/
  ! COMMON/YOMPHY2/
  
  !-----------------------------------------------------------------------
  
  !     Externes.
  !     ---------
  
  !     Methode.
  !     --------
  
  !     Auteur.
  !     -------
  !      1993-02, P. Lacarrere (from PERIDOT-RECHERCHE)
  
  !     Modifications.
  !     --------------
  !      2001-01  P. Marquet - Debug : ZDPHI in "low level resolution"
  !      2004-05  P. Marquet - Change KTDIAT into KTDIAN (at the end)
  !        M.Hamrud      01-Oct-2003 CY28 Cleaning
  !      2007-04  Y. Bouteloup - a debug for ZD2 computations
  !      2009-11  F. Bouyssel - Security value on TKE (ECTMAX)
  !      2009-11  E. Bazile - Use of PDELP for the flux computations
  !                            (Corr. problem in case of  VFE)
  !      K. Yessad (Jul 2009): remove CDLOCK + some cleanings
  !      2012-02  E. Bazile - Output of the term used in the TKE equation
  !               for DDH and MUSC
  !     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
  !-----------------------------------------------------------------------
  
!$acc routine( ACEVOLET_OPENACC ) seq
  
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
  INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KTDIAT
  INTEGER(KIND=JPIM), INTENT(IN) :: KTDIAN
  REAL(KIND=JPRB), INTENT(IN) :: PAPHI(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PAPHIF(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PAPRS(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PDELP(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PKUROV(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PR(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PT(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PU(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PV(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PUSLE(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(INOUT) :: PPRODTH(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PKCLS(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PECTCLS(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PECT(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(OUT) :: PECT1(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(OUT) :: PPRDY(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(OUT) :: PDIFF(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(OUT) :: PDISS(KLON, KLEV)
  
  !-----------------------------------------------------------------------
  
  REAL(KIND=JPRB) :: ZANKP1
  
  temp (REAL (KIND=JPRB), ZA, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZE, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZPA, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZGKU, (KLON, 0:KLEV))
  temp (REAL (KIND=JPRB), ZDET, (KLON, KLEV))
  temp (REAL (KIND=JPRB), ZGKEF, (KLON, KLEV))
  REAL(KIND=JPRB) :: ZCOR
  
  
  INTEGER(KIND=JPIM) :: JLEV
  INTEGER(KIND=JPIM) :: JLON
  
  REAL(KIND=JPRB) :: ZB
  REAL(KIND=JPRB) :: ZCIS
  REAL(KIND=JPRB) :: ZD
  REAL(KIND=JPRB) :: ZD1
  REAL(KIND=JPRB) :: ZD2
  REAL(KIND=JPRB) :: ZD3
  REAL(KIND=JPRB) :: ZDPHI
  REAL(KIND=JPRB) :: ZDPHISRO
  REAL(KIND=JPRB) :: ZGSDP
  REAL(KIND=JPRB) :: ZMDP
  REAL(KIND=JPRB) :: ZRHOF
  REAL(KIND=JPRB) :: ZRTI
  REAL(KIND=JPRB) :: ZUSGDT
  REAL(KIND=JPRB) :: ZX
  REAL(KIND=JPRB) :: ZY
  REAL(KIND=JPRB) :: ZZDIS
  REAL(KIND=JPRB) :: ZZGKEF
  REAL(KIND=JPRB) :: ZDPHI2
  
  REAL(KIND=JPRB) :: ZEPS
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  TYPE(STACK), INTENT(IN) :: YDSTACK
  TYPE(STACK) :: YLSTACK
  YLSTACK = YDSTACK
  IF (KIND (ZA) == 8) THEN
    alloc8 (ZA)
  ELSE
    IF (KIND (ZA) == 4) THEN
      alloc4 (ZA)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZE) == 8) THEN
    alloc8 (ZE)
  ELSE
    IF (KIND (ZE) == 4) THEN
      alloc4 (ZE)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZPA) == 8) THEN
    alloc8 (ZPA)
  ELSE
    IF (KIND (ZPA) == 4) THEN
      alloc4 (ZPA)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZGKU) == 8) THEN
    alloc8 (ZGKU)
  ELSE
    IF (KIND (ZGKU) == 4) THEN
      alloc4 (ZGKU)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZDET) == 8) THEN
    alloc8 (ZDET)
  ELSE
    IF (KIND (ZDET) == 4) THEN
      alloc4 (ZDET)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZGKEF) == 8) THEN
    alloc8 (ZGKEF)
  ELSE
    IF (KIND (ZGKEF) == 4) THEN
      alloc4 (ZGKEF)
    ELSE
      STOP 1
    END IF
  END IF
  JLON = KIDIA
  
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  !*
  !-----------------------------------------------------------------------
  !     I - CALCULS UTILES
  !-----------------------------------------------------------------------
  
  !          - - - - - - - - - - - - - - - - - - - -
  !     I-1  ON MULTIPLIE "PKUROV" PAR D(PHI)/ RHO
  !          IL RESTE : ZGKU = RG * KU
  !          - - - - - - - - - - - - - - - - - - - -
  ZEPS = 1.E-14
  ZUSGDT = 1.0_JPRB / (YDCST%RG*YDML_PHY_MF%YRPHY2%TSPHY)
  
  DO JLEV=KTDIAN,KLEV - 1
    ZDPHI = PAPHIF(JLON, JLEV) - PAPHIF(JLON, JLEV + 1)
    ZRTI = 0.5_JPRB*(PR(JLON, JLEV)*PT(JLON, JLEV) + PR(JLON, JLEV + 1)*PT(JLON, JLEV + 1))
    ZDPHISRO = ZDPHI*ZRTI / PAPRS(JLON, JLEV)
    ZGKU(JLON, JLEV) = PKUROV(JLON, JLEV)*ZDPHISRO
    ! JLON
  END DO
  ! JLEV
  
  !          - - - - - - - - - - -
  !     I.2  MISE A ZERO DES FLUX
  !          - - - - - - - - - - -
  
  PPRDY(JLON, :) = 0.0_JPRB
  PDIFF(JLON, :) = 0.0_JPRB
  PDISS(JLON, :) = 0.0_JPRB
  
  !-----------------------------------------------------------------------
  !     II - CALCUL DES COEFFICIENTS ET INVERSION DE LA MATRICE
  !-----------------------------------------------------------------------
  
  !          ON ECRIT L'EQUATION D(ET)/DT*(DELTA PHI)/G AU NIVEAU K SOUS
  !          LA FORME SUIVANTE
  
  !          A(K)*ECT(K-1) + B(K)*ECT(K) + C(K)*ECT(K+1) = D(K)
  
  !          CE SYSTEME EST RESOLU PAR LES RELATIONS DE RECURRENCES
  !          AVEC UNE DESCENTE ET UNE MONTE
  
  !        --------------
  !        POUR   K = 0 :
  !        --------------
  !        E(0) = F(0) = A(1) = 0
  !        --------------------
  !        POUR   K = 1 , N-2 :
  !        --------------------
  !        E(K)     = -C(K) / ( B(K) + A(K) * E(K-1) )
  !        F(K)     = ( D(K) - A(K) * F(K-1) ) / ( B(K) + A(K) * E(K-1) )
  !        -----------------------------------------
  !        POUR   K = N-1 AVEC ECT(N)=ECTCLS CONNU :
  !        -----------------------------------------
  !        ECT(N-1) = D(N-1) - A(N-1) * F(N-2) - C(N-1)*ECT(N)
  !        --------------------
  !        POUR   K = N-2 , 1 :
  !        --------------------
  !        ECT(K)   = E(K) * ECT(K+1) + F(K)
  
  !-----------------------------------------------------------------------
  
  !          CALCUL DES DEUX PREMIERES RECURRENCES
  
  ZA(JLON, KTDIAN) = 0.0_JPRB
  ZA(JLON, KTDIAN + 1) = 0.0_JPRB
  ZE(JLON, KTDIAN) = 0.0_JPRB
  ZPA(JLON, KTDIAN) = 0.0_JPRB
  ! JLON
  
  !     ICI : ZA(JLEV+1) = A(K) = (Rho*G*Ke)/d(Phi)
  
  DO JLEV=KTDIAN + 1,KLEV - 1
    ZGKEF(JLON, JLEV) = YDML_PHY_MF%YRPHY0%ALPHAE*(ZGKU(JLON, JLEV) + ZGKU(JLON, JLEV - 1)) / 2.0_JPRB
    ! JLON
  END DO
  ! JLEV
  
  DO JLEV=KTDIAN + 1,KLEV - 1
    ZDPHI = PAPHI(JLON, JLEV - 1) - PAPHI(JLON, JLEV)
    ZRHOF = PAPRSF(JLON, JLEV) / (PR(JLON, JLEV)*PT(JLON, JLEV))
    ZA(JLON, JLEV + 1) = ZGKEF(JLON, JLEV)*ZRHOF / ZDPHI
    ZPA(JLON, JLEV) = ZA(JLON, JLEV + 1)
    ! JLON
  END DO
  ! JLEV
  
  !     - - - - - - - - - - - - - - - - - -
  !       ZD2 : LA PRODUCTION DYNAMIQUE :
  
  !       ZD3 : LA PRODUCTION THERMIQUE
  !     - - - - - - - - - - - - - - - - - -
  
  DO JLEV=KTDIAN,KLEV - 2
    
    ZMDP = PAPRSF(JLON, JLEV) - PAPRSF(JLON, JLEV + 1)
    ZDPHI2 = (PAPHIF(JLON, JLEV) - PAPHIF(JLON, JLEV + 1))**2
    ZGSDP = YDCST%RG / ZMDP
    ZX = ZMDP*ZUSGDT
    ZZDIS = PUSLE(JLON, JLEV)*SQRT(PECT(JLON, JLEV))*ZMDP
    ZB = ZX - ZA(JLON, JLEV + 1) - ZA(JLON, JLEV + 2) + YDML_PHY_MF%YRPHY0%ADISI*ZZDIS
    ZY = 1.0_JPRB / (ZB + ZA(JLON, JLEV + 1)*ZE(JLON, JLEV))
    ZE(JLON, JLEV + 1) = -ZA(JLON, JLEV + 2)*ZY
    ZCIS = (PU(JLON, JLEV + 1) - PU(JLON, JLEV))**2 + (PV(JLON, JLEV + 1) - PV(JLON, JLEV))**2
    ZD1 = PECT(JLON, JLEV)*(ZX - YDML_PHY_MF%YRPHY0%ADISE*ZZDIS)
    ZD2 = ZGKU(JLON, JLEV)*ZCIS*ZMDP / ZDPHI2
    ZD3 = PPRODTH(JLON, JLEV) / ZGSDP
    
    ZD = ZD1 + ZD2 + ZD3
    
    ZA(JLON, JLEV + 1) = (ZD - ZA(JLON, JLEV + 1)*ZA(JLON, JLEV))*ZY
    
    !         - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !         ON RANGE 4 DES TERMES QUI COMPOSENT L'EVOLUTION DE L'ECT
    !         -> PROD.dyn,  PROD.ther,  ADVEC.vert,  DISSIP(1)
    !         - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    PPRDY(JLON, JLEV) = ZD2*ZGSDP
    PDISS(JLON, JLEV) = -ZZDIS*ZGSDP
    
    ! JLON
  END DO
  ! JLEV
  
  !*
  !-----------------------------------------------------------------------
  !     III RESOLUTION AU NIVEAU INFERIEUR
  !-----------------------------------------------------------------------
  
  !         SI ON VEUT IMPOSER UNE CONDITION A LA LIMITE INFERIEURE A
  !         FLUX NUL IL SUFFIT DE METTRE A ZERO PKCLS
  
  !       ZD2 : LA PRODUCTION DYNAMIQUE :
  
  !       ZD3 : LA PRODUCTION THERMIQUE
  
  !-----------------------------------------------------------------------
  
  
  !       CALCUL DE LA DIFFUSION VERTICALE DE L'ECT
  
  ZDPHI = PAPHI(JLON, KLEV - 1) - PAPHI(JLON, KLEV)
  ZDPHI2 = (PAPHIF(JLON, KLEV - 1) - PAPHIF(JLON, KLEV))**2
  ZRHOF = PAPRSF(JLON, KLEV) / (PR(JLON, KLEV)*PT(JLON, KLEV))
  ZZGKEF = YDML_PHY_MF%YRPHY0%ALPHAE*PKCLS(JLON)
  ZANKP1 = ZZGKEF*ZRHOF / ZDPHI
  
  ZMDP = PAPRSF(JLON, KLEV - 1) - PAPRSF(JLON, KLEV)
  ZGSDP = YDCST%RG / ZMDP
  ZX = ZMDP*ZUSGDT
  ZZDIS = PUSLE(JLON, KLEV - 1)*SQRT(PECT(JLON, KLEV - 1))*ZMDP
  ZB = ZX - ZA(JLON, KLEV) - ZANKP1 + YDML_PHY_MF%YRPHY0%ADISI*ZZDIS
  ZY = 1.0_JPRB / (ZB + ZA(JLON, KLEV)*ZE(JLON, KLEV - 1))
  ZCIS = (PU(JLON, KLEV) - PU(JLON, KLEV - 1))**2 + (PV(JLON, KLEV) - PV(JLON, KLEV - 1))**2
  ZD1 = PECT(JLON, KLEV - 1)*(ZX - YDML_PHY_MF%YRPHY0%ADISE*ZZDIS)
  ZD2 = ZGKU(JLON, KLEV - 1)*ZCIS*ZMDP / ZDPHI2
  ZD3 = PPRODTH(JLON, KLEV - 1) / ZGSDP
  
  ZD = ZD1 + ZD2 + ZD3 - ZANKP1*PECTCLS(JLON)
  
  ZA(JLON, KLEV) = (ZD - ZA(JLON, KLEV)*ZA(JLON, KLEV - 1))*ZY
  
  !       - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !       ON RANGE 3 DES TERMES QUI COMPOSENT L'EVOLUTION DE L'ECT
  !         -> PROD.dyn,  PROD.ther,  DISSIP(1)
  !       - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  PPRDY(JLON, KLEV - 1) = ZD2*ZGSDP
  PDISS(JLON, KLEV - 1) = -ZZDIS*ZGSDP*(YDML_PHY_MF%YRPHY0%ADISI*ZA(JLON, KLEV) + YDML_PHY_MF%YRPHY0%ADISE*PECT(JLON, KLEV - 1))
  
  ! JLON
  
  !*
  !-----------------------------------------------------------------------
  !     IV  RESOLUTION DE LA TROISIEME RECURENCE
  !         -> les termes ZA et PDISS(2)
  !-----------------------------------------------------------------------
  
  DO JLEV=KLEV - 2,KTDIAN,-1
    ZA(JLON, JLEV + 1) = ZE(JLON, JLEV + 1)*ZA(JLON, JLEV + 2) + ZA(JLON, JLEV + 1)
    PDISS(JLON, JLEV) =  &
    & PDISS(JLON, JLEV)*(YDML_PHY_MF%YRPHY0%ADISI*ZA(JLON, JLEV + 1) + YDML_PHY_MF%YRPHY0%ADISE*PECT(JLON, JLEV))
    ! JLON
  END DO
  ! JLEV
  !*
  !-----------------------------------------------------------------------
  !     V   CALCUL DE LA DIFFUSION VERTICALE -D/DP ( K D/DZ (ECT) )
  !          -> le terme PDIFF.
  !-----------------------------------------------------------------------
  
  ZGSDP = YDCST%RG / (PAPRSF(JLON, KLEV - 1) - PAPRSF(JLON, KLEV))
  PDIFF(JLON, KLEV - 1) =  &
  & (ZPA(JLON, KLEV - 1)*(ZA(JLON, KLEV) - ZA(JLON, KLEV - 1)) - ZANKP1*(PECTCLS(JLON) - ZA(JLON, KLEV)))*ZGSDP
  ! JLON
  
  DO JLEV=KLEV - 2,KTDIAN,-1
    ZGSDP = YDCST%RG / (PAPRSF(JLON, JLEV) - PAPRSF(JLON, JLEV + 1))
    PDIFF(JLON, JLEV) = (ZPA(JLON, JLEV)*(ZA(JLON, JLEV + 1) - ZA(JLON, JLEV)) - ZPA(JLON, JLEV + 1)*(ZA(JLON, JLEV + 2) -  &
    & ZA(JLON, JLEV + 1)))*ZGSDP
    ! JLON
  END DO
  ! JLEV
  !
  !  Calcul de la nouvelle TKE et du terme correctif du aux ECTMIN/MAX
  !
  DO JLEV=KTDIAT,KTDIAN - 1
    ZDET(JLON, JLEV) = 0.0_JPRB
    PECT1(JLON, JLEV) = PECT(JLON, JLEV)
    ! JLON
  END DO
  ! JLEV
  DO JLEV=KTDIAN,KLEV - 1
    ZDET(JLON, JLEV) = MIN((YDML_PHY_MF%YRPHY0%ECTMAX - PECT(JLON, JLEV)) / YDML_PHY_MF%YRPHY2%TSPHY,  &
    & MAX((YDML_PHY_MF%YRPHY0%ECTMIN - PECT(JLON, JLEV)) / YDML_PHY_MF%YRPHY2%TSPHY, PDIFF(JLON, JLEV) + PDISS(JLON, JLEV) +  &
    & PPRDY(JLON, JLEV) + PPRODTH(JLON, JLEV)))
    PECT1(JLON, JLEV) = PECT(JLON, JLEV) + YDML_PHY_MF%YRPHY2%TSPHY*ZDET(JLON, JLEV)
    ! JLON
  END DO
  ! JLEV
  !
  ! Calcul pour la surface
  !
  IF (YDML_PHY_MF%YRPHY%LECTREP) THEN
    ZDET(JLON, KLEV) = (MIN(YDML_PHY_MF%YRPHY0%ECTMAX, MAX(YDML_PHY_MF%YRPHY0%ECTMIN, PECT1(JLON, KLEV - 1))) - PECT(JLON, KLEV)) &
    &  / YDML_PHY_MF%YRPHY2%TSPHY
    ! JLON
  ELSE
    ZDET(JLON, KLEV) = (MIN(YDML_PHY_MF%YRPHY0%ECTMAX, MAX(YDML_PHY_MF%YRPHY0%ECTMIN, PECTCLS(JLON))) - PECT(JLON, KLEV)) /  &
    & YDML_PHY_MF%YRPHY2%TSPHY
    ! JLON
  END IF
  ! LIMITE INFERIEURE A PRIORI POUR AVOIR DES TERMES SYMPAS !!
  PECT1(JLON, KLEV) = PECT(JLON, KLEV) + YDML_PHY_MF%YRPHY2%TSPHY*ZDET(JLON, KLEV)
  ZCOR = ZDET(JLON, KLEV) / (SIGN(1._JPRB, ZDET(JLON, KLEV - 1))*MAX(ZEPS, ABS(ZDET(JLON, KLEV - 1))))
  ZCOR = MAX(1.0_JPRB, ZCOR)
  PDIFF(JLON, KLEV) = PDIFF(JLON, KLEV - 1)*ZCOR
  PDISS(JLON, KLEV) = PDISS(JLON, KLEV - 1)*ZCOR
  PPRDY(JLON, KLEV) = PPRDY(JLON, KLEV - 1)*ZCOR
  PPRODTH(JLON, KLEV) = PPRODTH(JLON, KLEV - 1)*ZCOR
  ! JLON
  !-----------------------------------------------------------------------
END SUBROUTINE ACEVOLET_OPENACC
