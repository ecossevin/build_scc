INTERFACE
  SUBROUTINE ACNPART_CLOUD_COVER_OPENACC (YDCST, YDML_PHY_MF, KIDIA, KFDIA, KLON, KTDIA, KLEV, PDECRDRED, PWMXOV, KL1, KL2,  &
  & PDECRD, PNEB, PAPRSF, PCLC, YDSTACK)
!$acc routine( ACNPART_CLOUD_COVER_OPENACC )
    
    USE MODEL_PHYSICS_MF_MOD, ONLY: MODEL_PHYSICS_MF_TYPE
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    USE STACK_MOD
    
    USE YOMCST, ONLY: TCST
    
    ! Interface:
    ! ----------
    ! INPUT:
    !   KL1    - initial level
    !   KL2    - final level
    !   PNEB   - cloud cover on levels
    !   PAPRSF - full level pressure
    
    ! OUTPUT:
    !   PCLC   - cloud cover between model levels KL1 and KL2
    
    TYPE(TCST), INTENT(IN) :: YDCST
    TYPE(MODEL_PHYSICS_MF_TYPE), INTENT(IN) :: YDML_PHY_MF
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    REAL(KIND=JPRB), INTENT(IN) :: PDECRDRED
    REAL(KIND=JPRB), INTENT(IN) :: PWMXOV
    
    INTEGER(KIND=JPIM), INTENT(IN) :: KL1
    INTEGER(KIND=JPIM), INTENT(IN) :: KL2
    
    REAL(KIND=JPRB), INTENT(IN) :: PDECRD(KLON)
    REAL(KIND=JPRB), INTENT(IN) :: PNEB(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PCLC(KLON)
    
    
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE ACNPART_CLOUD_COVER_OPENACC
END INTERFACE
