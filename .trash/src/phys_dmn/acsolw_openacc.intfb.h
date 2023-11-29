INTERFACE

SUBROUTINE ACSOLW_OPENACC (YDPHY1, KIDIA, KFDIA, KLON, PARG, PD2, PLSM&
&, PIVEG, PSAB, LDHMT, PWFC, PWPMX, PWSAT, PWSMX, PWWILT, YDSTACK)
!$acc routine (ACSOLW_OPENACC) seq
USE PARKIND1,ONLY:JPIM, JPRB
USE YOMPHY1,ONLY:TPHY1
USE STACK_MOD
IMPLICIT NONE
TYPE (TPHY1), INTENT (IN)::YDPHY1
INTEGER (KIND=JPIM), INTENT (IN)::KLON
INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
REAL (KIND=JPRB), INTENT (IN)::PARG (KLON)
REAL (KIND=JPRB), INTENT (IN)::PD2 (KLON)
REAL (KIND=JPRB), INTENT (IN)::PLSM (KLON)
REAL (KIND=JPRB), INTENT (IN)::PIVEG (KLON)
REAL (KIND=JPRB), INTENT (IN)::PSAB (KLON)
LOGICAL, INTENT (IN)::LDHMT
REAL (KIND=JPRB), INTENT (INOUT)::PWFC (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PWPMX (KLON)
REAL (KIND=JPRB), INTENT (INOUT)::PWSAT (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PWSMX (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PWWILT (KLON)
TYPE(STACK) :: YDSTACK
ENDSUBROUTINE ACSOLW_OPENACC

END INTERFACE