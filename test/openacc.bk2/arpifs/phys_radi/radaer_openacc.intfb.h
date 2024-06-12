INTERFACE
  SUBROUTINE RADAER_OPENACC (YDEAERD, YDERAD, YDPHY, KIDIA, KFDIA, KLON, KLEV, PAPRS, PAPRSF, PT, PTS, PAESEA, PAELAN, PAESOO,  &
  & PAEDES, PAESUL, PAEVOL, PAER, PAERINDS, YDSTACK)
!$acc routine( RADAER_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: DR_HOOK, JPHOOK, LHOOK
    USE YOEAERD, ONLY: TEAERD
    USE YOERAD, ONLY: TERAD
    USE YOMPHY, ONLY: TPHY
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    TYPE(TEAERD), INTENT(IN) :: YDEAERD
    TYPE(TERAD), INTENT(IN) :: YDERAD
    TYPE(TPHY), INTENT(IN) :: YDPHY
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    REAL(KIND=JPRB), INTENT(IN) :: PAPRS(KLON, KLEV + 1)
    REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PTS(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PAESEA(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PAELAN(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PAESOO(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PAEDES(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PAESUL(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PAEVOL(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PAER(KLON, KLEV, 6)
    REAL(KIND=JPRB), INTENT(OUT) :: PAERINDS(KLON, KLEV)
    !     -----------------------------------------------------------------
    
    !*       0.1   ARGUMENTS.
    !              ----------
    
    !     -----------------------------------------------------------------
    
    !*       0.2   LOCAL ARRAYS.
    !              -------------
    
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE RADAER_OPENACC
END INTERFACE