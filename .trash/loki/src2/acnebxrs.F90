SUBROUTINE ACNEBXRS_OPENACC (YDPHY, YDPHY0, KIDIA, KFDIA, KLON, KTDIA, KLEV, PQ, PQC, PQSAT, PNEB, YDSTACK)
  !-----------------------------------------------------------------------
  ! - INPUT  2D .
  ! - OUTPUT 2D .
  
  !**** *ACNEBXR* - CALCUL DE LA NEBULOSITE XU-RANDALL.
  !                 COMPUTATION OF XU-RANDALL CLOUDINESS.
  
  !     Sujet.
  !     ------
  !     CALCUL DE LA NEBULOSITE CONFORMEMENT A XU ET RANDALL, JAS 96.
  !     CLOUDINESS COMPUTATION, AS IN XU AND RANDALL, JAS 96.
  
  !**   Interface.
  !     ----------
  !        *CALL* *ACNEBXRS*
  
  ! -   ARGUMENTS D'ENTREE.
  !     -------------------
  
  ! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.
  
  ! KIDIA, KFDIA : BORNES BOUCLES HORIZONTALES   (IST,IEND DANS CPG).
  ! KIDIA, KFDIA : START/END OF HORIZONTAL LOOP  (IST,IEND IN *CPG*).
  ! KLON : DIMENSION HORIZONTALE                 (NPROMA DANS CPG).
  ! KLON : HORIZONTAL DIMENSION                  (NPROMA IN *CPG*).
  ! KTDIA : DEBUT BOUCLE VERTICALE DANS LA PHYSIQUE.
  ! KTDIA : START OF THE VERTICAL LOOP IN THE PHYSICS (IF SOME LEVELS ARE
  !                     SKIPPED AT THE TOP OF THE MODEL).
  ! KLEV : FIN BOUCLE VERTICE ET DIMENSION VERTICALE (NFLEV DANS CPG).
  ! KLEV : END OF VERTICAL LOOP AND VERTICAL DIMENSION(NFLEV IN *CPG*).
  
  ! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
  !   CATEGORIE).
  
  ! - 2D (1:KLEV) .
  
  ! PQ         : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
  ! PQ         : SPECIFIC HUMIDITY OF WATER VAPOUR.
  ! PQC        : HUMIDITE SPECIFIQUE DE L'EAU CONDENSEE (LIQUIDE + GLACE).
  ! PQC        : SPECIFIC HUMIDITY OF CONDENSATED WATER.
  ! PQSAT      : HUMIDITE SPECIFIQUE DE SATURATION.
  ! PQSAT      : SPECIFIC HUMIDITY AT SATURATION.
  
  ! -   ARGUMENTS DE SORTIE.
  !     --------------------
  
  ! - 2D (1:KLEV) .
  
  ! PNEB       : NEBULOSITE PARTIELLE "RADIATIVE".
  ! PNEB       : FRACTIONAL CLOUDINESS FOR RADIATION.
  
  !-----------------------------------------------------------------------
  
  ! -   ARGUMENTS IMPLICITES.
  !     ---------------------
  
  ! COMMON/YOMPHY0/
  
  !-----------------------------------------------------------------------
  
  !     Externes.
  !     ---------
  
  !     Methode.
  !     --------
  
  !     Auteur.
  !     -------
  !        2002-01, J.M. Piriou.
  
  !     Modifications.
  !     --------------
  !        M.Hamrud      01-Oct-2003 CY28 Cleaning
  !        R.Brozkova    05-Nov-2004 new RH function
  
  !-----------------------------------------------------------------------
  
!$acc routine( ACNEBXRS_OPENACC ) seq
  
  USE PARKIND1, ONLY: JPIM, JPRB
  USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
  
  USE YOMPHY, ONLY: TPHY
  USE YOMPHY0, ONLY: TPHY0
  USE STACK_MOD
#include "stack.h"
  
  IMPLICIT NONE
  
  TYPE(TPHY), INTENT(IN) :: YDPHY
  TYPE(TPHY0), INTENT(IN) :: YDPHY0
  INTEGER(KIND=JPIM), INTENT(IN) :: KLON
  INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
  INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA
  REAL(KIND=JPRB), INTENT(IN) :: PQ(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PQC(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PQSAT(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(INOUT) :: PNEB(KLON, KLEV)
  INTEGER(KIND=JPIM) :: JLEV
  INTEGER(KIND=JPIM) :: JLON
  
  REAL(KIND=JPRB) :: ZEPS1
  REAL(KIND=JPRB) :: ZRH
  REAL(KIND=JPRB) :: ZRHLIM
  REAL(KIND=JPRB) :: ZNEB
  REAL(KIND=JPRB) :: ZBIN
  REAL(KIND=JPRB) :: ZARGLI
  REAL(KIND=JPRB) :: ZRHEXP
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  TYPE(STACK), INTENT(IN) :: YDSTACK
  TYPE(STACK) :: YLSTACK
  YLSTACK = YDSTACK
  JLON = KIDIA
  
  !*
  !     ------------------------------------------------------------------
  !     I - CALCUL DE LA NEBULOSITE.
  !         CLOUDINESS COMPUTATION.
  
  ZEPS1 = 1.E-6_JPRB
  ZARGLI = 125._JPRB**(1.0_JPRB / YDPHY0%QXRTGH)
  
  IF (YDPHY%LQXRTGH) THEN
    DO JLEV=KTDIA,KLEV
      !DEC$ IVDEP
      ZRH = MIN(ZARGLI, MAX(ZEPS1, PQ(JLON, JLEV) / PQSAT(JLON, JLEV)))
      ZRHEXP = EXP(-2.0_JPRB*ZRH**YDPHY0%QXRTGH)
      ZRH = ((1.0_JPRB - ZRHEXP) / (1.0_JPRB + ZRHEXP))**(1.0_JPRB / YDPHY0%QXRTGH)
      ZRHLIM = MAX(ZEPS1, MIN(1.0_JPRB - ZEPS1, ZRH))
      ZNEB = ZRHLIM**YDPHY0%QXRR*(1.0_JPRB - EXP(-YDPHY0%QXRAL*PQC(JLON, JLEV) / ((1.0_JPRB - ZRHLIM)*PQSAT(JLON, JLEV)) &
      & **YDPHY0%QXRDEL))
      ZBIN = MAX(0.0_JPRB, SIGN(1.0_JPRB, ZRH - 1.0_JPRB))
      PNEB(JLON, JLEV) = MAX(ZEPS1, MIN(1.0_JPRB - ZEPS1, ZBIN + (1.0_JPRB - ZBIN)*ZNEB))
    END DO
  ELSE
    DO JLEV=KTDIA,KLEV
      !DEC$ IVDEP
      ZRH = MIN(YDPHY0%QXRHX, PQ(JLON, JLEV) / PQSAT(JLON, JLEV))
      ZRHLIM = MAX(ZEPS1, MIN(1.0_JPRB - ZEPS1, ZRH))
      ZNEB = ZRHLIM**YDPHY0%QXRR*(1.0_JPRB - EXP(-YDPHY0%QXRAL*PQC(JLON, JLEV) / ((1.0_JPRB - ZRHLIM)*PQSAT(JLON, JLEV)) &
      & **YDPHY0%QXRDEL))
      ZBIN = MAX(0.0_JPRB, SIGN(1.0_JPRB, ZRH - 1.0_JPRB))
      PNEB(JLON, JLEV) = MAX(ZEPS1, MIN(1.0_JPRB - ZEPS1, ZBIN + (1.0_JPRB - ZBIN)*ZNEB))
    END DO
  END IF
  
END SUBROUTINE ACNEBXRS_OPENACC
