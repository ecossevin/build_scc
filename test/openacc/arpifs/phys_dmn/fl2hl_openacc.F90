SUBROUTINE FL2HL_OPENACC (KIDIA, KFDIA, KLON, KTDIAT, KLEV, PAPRS, PAPRSF, PXFL, PXHL, KINI, YDSTACK)
  !
  !**** * FL2HL* - Passage des Full-level aux Half level
  !-----------------------------------------------------------------------
  !     Auteur.
  !     -------
  !       30/06/2011, Eric BAZILE
  
  !   Modifications.
  !   --------------
  !
  !   ARGUMENTS D'ENTREE.
  !   -------------------
  ! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT.
  ! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
  ! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
  ! KTDIAT     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
  ! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".
  ! KINI       : INDICE DE DEPART DE LA DIMENSION VERTICALE DU TABLEAU HALF LEVEL
  !              DE SORTIE en general 0 (KINI:KLEV) pour acbl89, acturb,acevolet
  
  ! - 2D (0:KLEV) .
  
  ! PAPRS      : PRESSION AUX DEMI-NIVEAUX.
  
  ! - 2D (1:KLEV) .
  
  ! PAPRSF     : PRESSION AUX NIVEAUX DES COUCHES.
  ! PXFL       : CHAMP A PASSER SUR LES HALF-LEVEL
  
  ! -   ARGUMENTS EN ENTREE/SORTIE.
  !     ---------------------------
  ! - 2D (0:KLEV) .
  ! PXHL       : PXFL INTERPOLE SUR LES HL.
  
!$acc routine( FL2HL_OPENACC ) seq
  
  USE PARKIND1, ONLY: JPIM, JPRB
  USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
  
  USE STACK_MOD
#include "stack.h"
  
  IMPLICIT NONE
  
  INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KLON
  INTEGER(KIND=JPIM), INTENT(IN) :: KTDIAT
  INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
  REAL(KIND=JPRB), INTENT(IN) :: PAPRS(KLON, 0:KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PXFL(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(OUT) :: PXHL(KLON, KINI:KLEV)
  INTEGER(KIND=JPIM), INTENT(IN) :: KINI
  INTEGER(KIND=JPIM) :: JLEV
  INTEGER(KIND=JPIM) :: JLON
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  TYPE(STACK), INTENT(IN) :: YDSTACK
  TYPE(STACK) :: YLSTACK
  YLSTACK = YDSTACK
  JLON = KIDIA
  
  !   CHECK RELIABILITY OF INPUT ARGUMENTS.
  
  
  DO JLEV=KTDIAT,KLEV - 1
    PXHL(JLON, JLEV) = ((PXFL(JLON, JLEV + 1) - PXFL(JLON, JLEV))*PAPRS(JLON, JLEV) + PXFL(JLON, JLEV)*PAPRSF(JLON, JLEV + 1) -  &
    & PXFL(JLON, JLEV + 1)*PAPRSF(JLON, JLEV)) / (PAPRSF(JLON, JLEV + 1) - PAPRSF(JLON, JLEV))
  END DO
  IF (KINI == 0) THEN
    PXHL(JLON, KTDIAT - 1) = PXFL(JLON, KTDIAT)
  END IF
  PXHL(JLON, KLEV) = PXFL(JLON, KLEV)
  
END SUBROUTINE FL2HL_OPENACC
