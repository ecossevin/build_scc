SUBROUTINE FL2HL_OPENACC (KIDIA, KFDIA, KLON, KTDIAT&
&, KLEV, PAPRS, PAPRSF, PXFL, PXHL, KINI, YDSTACK)
!$acc routine (FL2HL_OPENACC) seq
USE PARKIND1,ONLY:JPIM, JPRB

USE STACK_MOD
#include "stack.h"

IMPLICIT NONE

INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
INTEGER (KIND=JPIM), INTENT (IN)::KLON
INTEGER (KIND=JPIM), INTENT (IN)::KTDIAT
INTEGER (KIND=JPIM), INTENT (IN)::KLEV
REAL (KIND=JPRB), INTENT (IN)::PAPRS (KLON, 0:KLEV)
REAL (KIND=JPRB), INTENT (IN)::PAPRSF (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PXFL (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PXHL (KLON, KINI:KLEV)
INTEGER (KIND=JPIM), INTENT (IN)::KINI
TYPE(STACK) :: YDSTACK
TYPE(STACK) :: YLSTACK

INTEGER (KIND=JPIM)::JLON
INTEGER (KIND=JPIM)::JLEV


YLSTACK = YDSTACK




JLON = KIDIA


DO JLEV=KTDIAT, KLEV-1
  
  PXHL (JLON, JLEV)=((PXFL (JLON, JLEV+1)-PXFL (JLON, JLEV))*PAPRS (JLON&
  &, JLEV)+PXFL (JLON, JLEV)*PAPRSF (JLON, JLEV+1)-PXFL (JLON, JLEV+1)*PAPRSF&
  & (JLON, JLEV))/(PAPRSF (JLON, JLEV+1)-PAPRSF (JLON, JLEV))
  
ENDDO



IF (KINI==0) THEN
  PXHL (JLON, KTDIAT-1)=PXFL (JLON, KTDIAT)
ENDIF

PXHL (JLON, KLEV)=PXFL (JLON, KLEV)


ENDSUBROUTINE FL2HL_OPENACC

! 56ad6923076b622f9a4a36289517d0f4b37156a2
