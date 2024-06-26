SUBROUTINE GPGEO_EXPL_OPENACC (KPROMA, KST, KEND, KFLEV, PHI, PHIF, PT, PR, PLNPR, PALPH, YDVGEOM, YDSTACK)
  
  !**** *GPGEO_EXPL* - Computes half and full level geopotential height "gz".
  
  !     Purpose.
  !     --------
  
  !      Computes half and full level geopotential height "gz".
  
  !      Laplace relation writes:
  
  !       d (gz)/d prehyd = - RT/pre = - (RT/prehyd) * (prehyd/pre)
  
  !      where:
  !       - "gz" is the geopotential height.
  !       - "prehyd" is the hydrostatic pressure.
  !       - "pre" is the total pressure including non-hydrostatic effects.
  !       - "R" is the air constant (including moisture effects).
  !       - "T" is the temperature.
  
  !      It is important to note that when relaxing the thin layer hypothesis
  !      the geopotential height "gz" is different from the total geopotential
  !      "Phi" (used in the RHS of the horizontal wind equation), except
  !      at the surface where Phi_s can still be defined by Phi_s = g z[surf].
  
  !      Integrating the Laplace equations yields the following discretisation
  !      for "gz".
  
  !      * "gz" at interlayer "lbar":
  
  !        g z[lbar] = g z[surf]
  !        + sum[k=L to l+1] (prehyd/pre)[k] R[k] T[k] delta[k]
  
  !      * "gz" at layer "l":
  
  !        g z[l] = g z[lbar] + (prehyd/pre)[l] R[l] T[l] alpha[l]
  
  !**   Interface.
  !     ----------
  !        *CALL* *GPGEO_EXPL(...)
  
  !        Explicit arguments :
  !        --------------------
  !          KPROMA : horizontal dimensioning                          (input)
  !          KSTART : start of work                                    (input)
  !          KPROF  : depth of work                                    (input)
  !          KFLEV  : number of levels                                 (input)
  !          PHI    : geopotential height "gz" at interlayers          (output)
  !          PHIF   : geopotential height "gz" at layers               (output)
  !          PT     : temperature at layers                            (input)
  !          PR     : "R" at layers for hydrostatic model              (input)
  !                    "(prehyd/pre) R" at layers for NH model
  !          PLNPR  : term "delta" on layers                           (input)
  !                   (= logarithm of ratio of pressure if "ndlnpr=0")
  !          PALPH  : term "alpha" on layers                           (input)
  !          YDVGEOM : vertical geometry from the model                (input)
  
  !        Implicit arguments :    None.
  !        --------------------
  
  !     Method.
  !     -------
  !        See documentation
  
  !     Externals.   None.
  !     ----------
  
  !     Reference.
  !     ----------
  !        ECMWF Research Department documentation of the IFS
  
  !     Author.
  !     -------
  !      Mats Hamrud and Philippe Courtier  *ECMWF*
  
  !     Modifications.
  !     --------------
  !      J. Vivoda and P. Smolikova (Sep 2017): new options for VFE-NH
  !      H Petithomme (Dec 2020): merge VFD loops
  !     ------------------------------------------------------------------
  
!$acc routine( GPGEO_EXPL_OPENACC ) seq
  
  USE PARKIND1, ONLY: JPIM, JPRB
  USE YOMHOOK, ONLY: LHOOK, JPHOOK, DR_HOOK
  
  USE YOMVERT, ONLY: TVERTICAL_GEOM
  
  !     ------------------------------------------------------------------
  
  USE STACK_MOD
#include "stack.h"
  
  IMPLICIT NONE
  
  INTEGER(KIND=JPIM), INTENT(IN) :: KPROMA
  INTEGER(KIND=JPIM), INTENT(IN) :: KST
  INTEGER(KIND=JPIM), INTENT(IN) :: KEND
  INTEGER(KIND=JPIM), INTENT(IN) :: KFLEV
  REAL(KIND=JPRB), INTENT(INOUT) :: PHI(KPROMA, 0:KFLEV)
  REAL(KIND=JPRB), INTENT(OUT) :: PHIF(KPROMA, KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PT(KPROMA, KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PR(KPROMA, KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PLNPR(KPROMA, KFLEV)
  REAL(KIND=JPRB), INTENT(IN) :: PALPH(KPROMA, KFLEV)
  TYPE(TVERTICAL_GEOM), INTENT(IN) :: YDVGEOM
  
  !     ------------------------------------------------------------------
  
  INTEGER(KIND=JPIM) :: JLEV
  INTEGER(KIND=JPIM) :: JLON
  temp (REAL (KIND=JPRB), ZPHI, (KPROMA, 0:KFLEV + 1))
  temp (REAL (KIND=JPRB), ZOUT, (KPROMA, KFLEV + 1))
  REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
  
  !     ------------------------------------------------------------------
  
#include "verdisint_openacc.intfb.h"
  TYPE(STACK), INTENT(IN) :: YDSTACK
  TYPE(STACK) :: YLSTACK
  YLSTACK = YDSTACK
  IF (KIND (ZPHI) == 8) THEN
    alloc8 (ZPHI)
  ELSE
    IF (KIND (ZPHI) == 4) THEN
      alloc4 (ZPHI)
    ELSE
      STOP 1
    END IF
  END IF
  IF (KIND (ZOUT) == 8) THEN
    alloc8 (ZOUT)
  ELSE
    IF (KIND (ZOUT) == 4) THEN
      alloc4 (ZOUT)
    ELSE
      STOP 1
    END IF
  END IF
  JLON = KST
  
  !     ------------------------------------------------------------------
  
  
  !     ------------------------------------------------------------------
  
  !*       1.    COMPUTES HALF AND FULL LEVEL GEOPOTENTIAL HEIGHT.
  !              -------------------------------------------------
  
  IF (YDVGEOM%YRCVER%LVERTFE) THEN
    DO JLEV=1,KFLEV
      ZPHI(JLON, JLEV) = -PR(JLON, JLEV)*PT(JLON, JLEV)*PLNPR(JLON, JLEV)*YDVGEOM%YRVETA%VFE_RDETAH(JLEV)
    END DO
    
    ZPHI(JLON, 0) = 0.0_JPRB
    ZPHI(JLON, KFLEV + 1) = 0.0_JPRB
    CALL VERDISINT_OPENACC(YDVGEOM%YRVFE, YDVGEOM%YRCVER, 'IBOT', '11', KPROMA, KST, KEND, KFLEV, ZPHI, ZOUT, YDSTACK=YLSTACK)
    
    DO JLEV=KFLEV,1,-1
      PHIF(JLON, JLEV) = ZOUT(JLON, JLEV) + PHI(JLON, KFLEV)
      PHI(JLON, JLEV - 1) = PHI(JLON, JLEV) + PR(JLON, JLEV)*PT(JLON, JLEV)*PLNPR(JLON, JLEV)
    END DO
  ELSE
    DO JLEV=KFLEV,1,-1
      PHI(JLON, JLEV - 1) = PHI(JLON, JLEV) + PR(JLON, JLEV)*PT(JLON, JLEV)*PLNPR(JLON, JLEV)
      PHIF(JLON, JLEV) = PHI(JLON, JLEV) + PALPH(JLON, JLEV)*PR(JLON, JLEV)*PT(JLON, JLEV)
    END DO
  END IF
  
  !     ------------------------------------------------------------------
  
END SUBROUTINE GPGEO_EXPL_OPENACC
