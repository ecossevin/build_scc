SUBROUTINE DPRECIPS_OPENACC (YDCST, YDPRECIPS, KIDIA, KFDIA, KLON, KLEV, POROG, PCLSTPW, PDIAGH, PAPHIFM, PDZZ, PTPW, PQCM,  &
& PFPLSL, PFPLSN, PFPLSG, PDPRECIPS, YDSTACK)
  
  !**** *DPRECIPS*   -  Compute precipitation type diagnostic
  
  !     Purpose.
  !     --------
  !           Compute precipitation type diagnostic (PDPRECIPS), each time step
  
  !**   Interface.
  !     ----------
  !        *CALL* *DPRECIPS(...)
  
  !        Explicit arguments :
  !        --------------------
  !----
  ! 0D :
  !----
  ! KIDIA, KFDIA : START/END OF HORIZONTAL LOOP  (IST,IEND IN *CPG*).
  ! KLON : HORIZONTAL DIMENSION                  (KLON IN *APL_AROME*)
  ! KLEV : END OF VERTICAL LOOP AND VERTICAL DIMENSION(NFLEVG IN *CPG*)
  !----
  ! 1D :
  !----
  ! POROG      :  SURFACE GEOPOTENTIAL (mgp)
  ! PCLSTPW : 2m T'w
  ! PGIAGH : Hail diagnostic
  !----
  ! 2D :
  !----
  ! PAPHIFM : RG*Full levels height (in m)
  ! PDZZ : Full levels depth (in m)
  ! PTPW : T'w 3D field
  ! PQCM        : SPECIFIC HUMIDITY OF CLOUD WATER
  ! PFPLSL      : SURFACE PRECIPITATION FLUX LIQUID
  ! PFPLSN      : SURFACE PRECIPITATION FLUX SNOW
  ! PFPLSG      : SURFACE PRECIPITATION FLUX GRAUPEL
  ! ------
  ! INOUT :
  ! ------
  ! PDPRECIPS   : precipitation type diagnostic :
  !    0: no precipitation
  !    1: rain   / pluie
  !    3: freezing rain / pluie verglacante
  !    5: dry snow / neige seche
  !    6: wet snow / neige humide
  !    7: rain now mixture / pluie et neige melees
  !    8: ice pellets/ granules de glace
  !    9: graupel   / gresil
  !   10: hail      / grele
  !   11: drizzle/ bruine
  !   12: freezing drizzle / bruine verglacante
  !  193: moist snow / neige mouillee
  !
  !        Implicit arguments :
  !        --------------------
  !        COMMON YOMDPRECIPS
  
  !     Method.
  !     -------
  !        See documentation
  
  !     Externals.
  !     ----------
  
  !     Reference.
  !     ----------
  !        Documentation ARPEGE/AROME
  
  !     Authors.
  !     -------
  !      I.Etchevers Y. Seity.
  !      Original : 2018-07-17
  
  !     Modifications.
  !     --------------
  !        2019-10, I. Etchevers : optimization and cleaning
  !     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
  !        2023-08, R. Brozkova  : further optimization
  !
  !     ------------------------------------------------------------------
  
!$acc routine( DPRECIPS_OPENACC ) seq
  
  USE PARKIND1, ONLY: JPIM, JPRB
  USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
  USE YOMDPRECIPS, ONLY: TDPRECIPS
  USE YOMCST, ONLY: TCST
  
  !     ------------------------------------------------------------------
  
  USE STACK_MOD
#include "stack.h"
  
  IMPLICIT NONE
  
  TYPE(TCST), INTENT(IN) :: YDCST
  TYPE(TDPRECIPS), TARGET, INTENT(IN) :: YDPRECIPS
  
  !     ------------------------------------------------------------------
  
  INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
  INTEGER(KIND=JPIM), INTENT(IN) :: KLON
  INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
  REAL(KIND=JPRB), INTENT(IN) :: POROG(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PCLSTPW(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PDIAGH(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PAPHIFM(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PDZZ(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PTPW(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PQCM(KLON, KLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PFPLSL(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFPLSN(KLON)
  REAL(KIND=JPRB), INTENT(IN) :: PFPLSG(KLON)
  REAL(KIND=JPRB), INTENT(OUT) :: PDPRECIPS(KLON)
  
  
  INTEGER(KIND=JPIM) :: JLON
  INTEGER(KIND=JPIM) :: JLEV
  INTEGER(KIND=JPIM) :: JL
  INTEGER(KIND=JPIM) :: JLEVH
  INTEGER(KIND=JPIM) :: JLEVB
  INTEGER(KIND=JPIM) :: INDIH
  INTEGER(KIND=JPIM) :: INDI1
  INTEGER(KIND=JPIM) :: INDI2
  INTEGER(KIND=JPIM) :: ITOP
  INTEGER(KIND=JPIM) :: JLEVM1
  INTEGER(KIND=JPIM) :: JJLEVM1
  INTEGER(KIND=JPIM) :: I1
  INTEGER(KIND=JPIM) :: I2
  REAL(KIND=JPRB) :: ZZ
  REAL(KIND=JPRB) :: ZALPHA
  REAL(KIND=JPRB) :: ZINVG
  REAL(KIND=JPRB) :: ZRATIO
  REAL(KIND=JPRB) :: ZLIQ
  REAL(KIND=JPRB) :: ZPROFNEG
  REAL(KIND=JPRB) :: ZICE
  REAL(KIND=JPRB) :: ZTOT
  REAL(KIND=JPRB) :: ZHEIGHT
  temp (REAL (KIND=JPRB), ZHF, (KLON, KLEV))
  REAL(KIND=JPRB) :: ZAPOS
  REAL(KIND=JPRB) :: ZANEG
  REAL(KIND=JPRB) :: ZDCLWC
  
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  TYPE(STACK), INTENT(IN) :: YDSTACK
  TYPE(STACK) :: YLSTACK
  YLSTACK = YDSTACK
  IF (KIND (ZHF) == 8) THEN
    alloc8 (ZHF)
  ELSE
    IF (KIND (ZHF) == 4) THEN
      alloc4 (ZHF)
    ELSE
      STOP 1
    END IF
  END IF
  JLON = KIDIA
  !     ------------------------------------------------------------------
  
  
  !     ------------------------------------------------------------------
  
  
  !     -------------------
  !*       0.    Initialisations
  !     -------------------
  
  
  ZINVG = 1._JPRB / YDCST%RG
  
  ZLIQ = MAX(PFPLSL(JLON), 0._JPRB)
  ZICE = MAX(PFPLSN(JLON), 0._JPRB) + MAX(PFPLSG(JLON), 0._JPRB)
  ZTOT = ZLIQ + ZICE
  ZTOT = MAX(ZTOT, 1.E-8_JPRB)
  ZRATIO = ZLIQ / ZTOT
  ZHEIGHT = POROG(JLON)*ZINVG
  
  ! Preparations
  ITOP = 1._JPIM
  I1 = 1._JPIM
  I2 = 1._JPIM
  ZPROFNEG = 0._JPRB
  ZANEG = 0._JPRB
  ZAPOS = 0._JPRB
  
  DO JLEV=KLEV,1,-1
    ZHF(JLON, JLEV) = PAPHIFM(JLON, JLEV)*ZINVG
    ! first level index > 4000m above ground
    INDIH = INT(MAX(0._JPRB, SIGN(1._JPRB, ZHF(JLON, JLEV) - YDPRECIPS%RHTOP - ZHEIGHT)))
    ITOP = MAX(ITOP, JLEV*INDIH)
    
    ! first level index above ground where T'w > RTT in the layer limited by 4000 m
    I1 = MAX(I1, ITOP, JLEV*INT(MAX(0._JPRB, SIGN(1._JPRB, PTPW(JLON, JLEV) - YDCST%RTT))))
    
    ! first level index above I1 where T'w < RTT in the layer from I1 to 4000 m
    I2 = MIN(I1, MAX(I2, ITOP, JLEV*INT(MAX(0._JPRB, SIGN(1._JPRB, YDCST%RTT - PTPW(JLON, JLEV))))))
    
    ! integral from the ground up to the level 4000 m above ground
    ZPROFNEG = ZPROFNEG + (1 - INDIH)*MAX(0._JPRB, SIGN(1._JPRB, PTPW(JLON, JLEV) - YDCST%RTT))
    
    ! integral from ground up to the level I1+1 securized by MIN with KLEV
    INDI1 = MAX(0, SIGN(1, JLEV - MIN(I1 + 1, KLEV)))
    ZANEG = ZANEG + INDI1*PDZZ(JLON, JLEV)*(YDCST%RTT - PTPW(JLON, JLEV))
    
    ! integral from the level I1 up to the level I2+1, securized by MIN with I1
    INDI2 = MAX(0, SIGN(1, JLEV - MIN(I2 + 1, I1)))*MAX(0, SIGN(1, I1 - JLEV))
    ZAPOS = ZAPOS + INDI2*PDZZ(JLON, JLEV)*(PTPW(JLON, JLEV) - YDCST%RTT)
  END DO
  
  ! computation of qc(HDCLWC)
  JLEVM1 = 0
  ! level JLEV just above HDCLWC
  DO JLEV=KLEV,1,-1
    ZZ = ZHF(JLON, JLEV) - YDPRECIPS%HDCLWC - ZHEIGHT
    JJLEVM1 = MAX(0._JPRB, SIGN(1._JPRB, ZZ))*JLEV
    JLEVM1 = MAX(JJLEVM1, JLEVM1)
  END DO
  
  JLEVH = JLEVM1
  JLEVB = MIN(JLEVM1 + 1, KLEV)
  IF (JLEVH == JLEVB) THEN
    ZDCLWC = PQCM(JLON, JLEV)
  ELSE
    ZALPHA = (ZHF(JLON, JLEVH) - YDPRECIPS%HDCLWC - ZHEIGHT) / (ZHF(JLON, JLEVH) - ZHF(JLON, JLEVB))
    ZDCLWC = PQCM(JLON, JLEVB)*ZALPHA + PQCM(JLON, JLEVH)*(1 - ZALPHA)
  END IF
  
  
  ! Determining the type of precipitation
  
  
  IF (ZTOT >= YDPRECIPS%RPRECSEUIL) THEN
    
    !     -------------------
    !*       1.    Hail
    !     -------------------
    IF (PDIAGH(JLON) >= YDPRECIPS%RDHAIL1) THEN
      IF (PDIAGH(JLON) >= YDPRECIPS%RDHAIL2) THEN
        PDPRECIPS(JLON) = 10._JPRB          ! Hail
      ELSE
        PDPRECIPS(JLON) = 9._JPRB          ! Small Hail
      END IF
      
      !     -------------------
      !*       2.    T'w2m < 0
      !     -------------------
      ! diag freezing drizzle
      
    ELSE IF (PCLSTPW(JLON) < YDPRECIPS%RTPW) THEN
      
      IF (ZPROFNEG == 0._JPRB) THEN
        
        IF (ZDCLWC > YDPRECIPS%RDCLWC) THEN
          PDPRECIPS(JLON) = 12._JPRB            ! Freezing drizzle
        ELSE IF (PCLSTPW(JLON) <= YDPRECIPS%RTPW - 2) THEN
          PDPRECIPS(JLON) = 5._JPRB            ! Dry snow
        ELSE
          PDPRECIPS(JLON) = 6._JPRB            ! Wet snow
        END IF
        
        ! diag freezing rain
        
      ELSE IF (ZAPOS >= YDPRECIPS%RAWARM) THEN
        IF (ZANEG < YDPRECIPS%RACOLD) THEN
          PDPRECIPS(JLON) = 3._JPRB            ! Freezing rain
        ELSE
          PDPRECIPS(JLON) = 8._JPRB            ! Ice pellets
        END IF
      ELSE IF (ZAPOS >= YDPRECIPS%RAWARM2) THEN
        IF (ZANEG < YDPRECIPS%RACOLD2) THEN
          PDPRECIPS(JLON) = 7._JPRB            ! Rain snow mixture
        ELSE
          PDPRECIPS(JLON) = 8._JPRB            ! Ice pellets
        END IF
      ELSE
        PDPRECIPS(JLON) = 6._JPRB          ! Wet snow
      END IF
      
      !     -------------------
      !*       3.    T'w2m >= 0
      !     -------------------
      
    ELSE IF (ZRATIO > YDPRECIPS%RDSEUIL4) THEN
      IF (ZLIQ > YDPRECIPS%RDSEUIL5) THEN
        PDPRECIPS(JLON) = 1._JPRB          ! Rain
      ELSE
        PDPRECIPS(JLON) = 11._JPRB          ! Drizzle
      END IF
    ELSE IF (ZRATIO > YDPRECIPS%RDSEUIL3) THEN
      PDPRECIPS(JLON) = 7._JPRB        ! Rain snow mixture
    ELSE IF (ZRATIO > YDPRECIPS%RDSEUIL2) THEN
      PDPRECIPS(JLON) = 193._JPRB        ! Moist snow
    ELSE IF (ZRATIO > YDPRECIPS%RDSEUIL1) THEN
      PDPRECIPS(JLON) = 6._JPRB        ! Wet snow
    ELSE
      PDPRECIPS(JLON) = 5._JPRB        ! Dry snow
      
    END IF
    
  ELSE
    PDPRECIPS(JLON) = 0._JPRB      ! No significant precipitation
    
  END IF
  
  
  
END SUBROUTINE DPRECIPS_OPENACC
