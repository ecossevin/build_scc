SUBROUTINE CUDUDV &
 & (YDCST,   YDECUMF,  YDSPP_CONFIG,       YDPERTPAR,KIDIA,    KFDIA,    KLON,     KLEV, &
 & KTOPM2,   KTYPE,    KCBOT,    KCTOP,    LDCUM,    PTSPHY,&
 & PAPH,     PAP,      PUEN,     PVEN,     PMFU,     PMFD,&
 & PMFUO,    PMFDO,    PUU,      PUD,      PVU,      PVD,  PGP2DSPP,&
 & PTENU,    PTENV  )  

!**** *CUDUDV* - UPDATES U AND V TENDENCIES,
!                DOES GLOBAL DIAGNOSTIC OF DISSIPATION

!**   INTERFACE.
!     ----------

!          *CUDUDV* IS CALLED FROM *CUMASTR*

!     PARAMETER     DESCRIPTION                                   UNITS
!     ---------     -----------                                   -----
!     INPUT PARAMETERS (INTEGER):

!    *KIDIA*        START POINT
!    *KFDIA*        END POINT
!    *KLON*         NUMBER OF GRID POINTS PER PACKET
!    *KLEV*         NUMBER OF LEVELS
!    *KTYPE*        TYPE OF CONVECTION
!                       1 = PENETRATIVE CONVECTION
!                       2 = SHALLOW CONVECTION
!                       3 = MIDLEVEL CONVECTION
!    *KCBOT*        CLOUD BASE LEVEL
!    *KCTOP*        CLOUD TOP LEVEL

!    INPUT PARAMETERS (LOGICAL):

!    *LDCUM*        FLAG: .TRUE. FOR CONVECTIVE POINTS 

!    INPUT PARAMETERS (REAL):

!    *PTSPHY*       TIME STEP FOR THE PHYSICS                      S
!    *PAPH*         PROVISIONAL PRESSURE ON HALF LEVELS            PA
!    *PAP *         PROVISIONAL PRESSURE ON FULL LEVELS            PA
!    *PUEN*         PROVISIONAL ENVIRONMENT U-VELOCITY (T+1)       M/S
!    *PVEN*         PROVISIONAL ENVIRONMENT V-VELOCITY (T+1)       M/S
!    *PMFU*         MASSFLUX UPDRAFTS                             KG/(M2*S)
!    *PMFD*         MASSFLUX DOWNDRAFTS                           KG/(M2*S)
!    *PUU*          U-VELOCITY IN UPDRAFTS                         M/S
!    *PUD*          U-VELOCITY IN DOWNDRAFTS                       M/S
!    *PVU*          V-VELOCITY IN UPDRAFTS                         M/S
!    *PVD*          V-VELOCITY IN DOWNDRAFTS                       M/S
!    *PGP2DSPP*     Standard stochastic variable (mean=0, SD=1)

!    UPDATED PARAMETERS (REAL):

!    *PTENU*        TENDENCY OF U-COMP. OF WIND                    M/S2
!    *PTENV*        TENDENCY OF V-COMP. OF WIND                    M/S2

!            METHOD
!            -------
!       EXPLICIT UPSTREAM AND IMPLICIT SOLUTION OF VERTICAL ADVECTION
!       DEPENDING ON VALUE OF RMFSOLUV: 
!       0=EXPLICIT 0-1 SEMI-IMPLICIT >=1 IMPLICIT 

!       FOR EXPLICIT SOLUTION: ONLY ONE SINGLE ITERATION
!       FOR IMPLICIT SOLUTION: FIRST IMPLICIT SOLVER, THEN EXPLICIT SOLVER
!                              TO CORRECT TENDENCIES BELOW CLOUD BASE

!            EXTERNALS
!            ---------
!            CUBIDIAG

!     AUTHOR.
!     -------
!      M.TIEDTKE         E.C.M.W.F.     7/86 MODIF. 12/89

!     MODIFICATIONS.
!     --------------
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      P.BECHTOLD        E.C.M.W.F.    11/02/05 IMPLICIT SOLVER
!      M. Leutbecher&S.-J. Lock (Jan 2016) Introduced SPP scheme (LSPP)
!      S.-J. Lock    Jul 2017    Extended SPP perturbations to include shallow convection
!      L. Descamps 2020-02-25 Introduced perturbed parameter option for PEARP (LPERTPAR)
!      M. Leutbecher & S. Lang Oct 2020    SPP abstraction and revision
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!     R. El Khatib 20-Jul-2024 Remove PM's Q&D patch after LD's merge fix.
!----------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY : TCST
USE YOECUMF  , ONLY : TECUMF
USE SPP_MOD     , ONLY : TSPP_CONFIG
USE YOMPERTPAR , ONLY : TPERTPAR
USE SPP_GEN_MOD , ONLY : SPP_PERT

IMPLICIT NONE

TYPE(TCST)         ,INTENT(IN)     :: YDCST
TYPE(TECUMF)       ,INTENT(IN)     :: YDECUMF
TYPE(TSPP_CONFIG)  ,INTENT(IN)     :: YDSPP_CONFIG
TYPE(TPERTPAR)     ,INTENT(IN)     :: YDPERTPAR
INTEGER(KIND=JPIM) ,INTENT(IN)     :: KIDIA 
INTEGER(KIND=JPIM) ,INTENT(IN)     :: KFDIA 
INTEGER(KIND=JPIM) ,INTENT(IN)     :: KLON 
INTEGER(KIND=JPIM) ,INTENT(IN)     :: KLEV 
INTEGER(KIND=JPIM) ,INTENT(IN)     :: KTOPM2 
INTEGER(KIND=JPIM) ,INTENT(IN)     :: KTYPE(KLON) 
INTEGER(KIND=JPIM) ,INTENT(IN)     :: KCBOT(KLON) 
INTEGER(KIND=JPIM) ,INTENT(IN)     :: KCTOP(KLON) 
LOGICAL            ,INTENT(IN)     :: LDCUM(KLON) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PTSPHY
REAL(KIND=JPRB)    ,INTENT(IN)     :: PAPH(KLON,KLEV+1) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PAP(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PUEN(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PVEN(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PMFU(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PMFD(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PMFUO(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PMFDO(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PUU(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PUD(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PVU(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PVD(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(IN)     :: PGP2DSPP(KLON,YDSPP_CONFIG%SM%NRFTOTAL)
REAL(KIND=JPRB)    ,INTENT(INOUT)  :: PTENU(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(INOUT)  :: PTENV(KLON,KLEV) 
REAL(KIND=JPRB) ::  ZMFUU(KLON,KLEV),    ZMFDU(KLON,KLEV),&
 & ZMFUV(KLON,KLEV),    ZMFDV(KLON,KLEV), ZADVW(KLON)

INTEGER(KIND=JPIM) :: IK, IM, IKB, JK, JL
INTEGER(KIND=JPIM) :: IPCUDU, IPCUDV, IPCUDUS, IPCUDVS
INTEGER(KIND=JPIM) :: IPN1, IPN2
TYPE(SPP_PERT)     :: PN1, PN2  ! SPP pertn. config. 

REAL(KIND=JPRB) :: ZZP, ZIMP, ZTSPHY, ZU, ZV
REAL(KIND=JPRB) :: ZMDU, ZMDV, ZLIMN2
REAL(KIND=JPRB) :: ZXU, ZXV, ZXU_MAG, ZXV_MAG, ZN2, ZFAC
REAL(KIND=JPRB), DIMENSION(KLON)      :: ZRDU, ZRDV
REAL(KIND=JPRB), DIMENSION(KLON,KLEV) :: ZDUDT, ZDVDT, ZDP
REAL(KIND=JPRB), DIMENSION(KLON,KLEV) :: ZB,  ZR1,  ZR2
LOGICAL,DIMENSION(KLON,KLEV) :: LLCUMBAS
LOGICAL     :: LLPPAR_CUDUV
LOGICAL   :: LLPERT_CUDUDV, LLPERT_CUDUDVS ! SPP perturbation on?
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "cubidiag.intfb.h"
!----------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CUDUDV',0,ZHOOK_HANDLE)
ASSOCIATE(RMFSOLUV=>YDECUMF%RMFSOLUV,RMFSOLRHS=>YDECUMF%RMFSOLRHS,RMFADVW=>YDECUMF%RMFADVW,&
 & RG=>YDCST%RG, &
 &RMFADVWDD=>YDECUMF%RMFADVWDD)
ZIMP=1.0_JPRB-RMFSOLUV
ZTSPHY=1.0_JPRB/PTSPHY

LLPPAR_CUDUV   = .FALSE.
LLPERT_CUDUDV  = .FALSE.
LLPERT_CUDUDVS = .FALSE.

DO JL=KIDIA,KFDIA
  ZADVW(JL)=0.0_JPRB
  IF(KTYPE(JL)==1) ZADVW(JL)=RMFADVW
ENDDO


DO JK=1,KLEV
  DO JL=KIDIA,KFDIA
    IF(LDCUM(JL)) THEN
      ZDP(JL,JK)=RG/(PAPH(JL,JK+1)-PAPH(JL,JK))
    ENDIF
  ENDDO
ENDDO

!----------------------------------------------------------------------


!*    1.0          CALCULATE FLUXES AND UPDATE U AND V TENDENCIES
!                  ----------------------------------------------

  DO JK=KTOPM2,KLEV
    IK=JK-1
    DO JL=KIDIA,KFDIA
      IF(LDCUM(JL)) THEN
        ZMFUU(JL,JK)=PMFU(JL,JK)*(PUU(JL,JK)-ZIMP*PUEN(JL,IK))
        ZMFUV(JL,JK)=PMFU(JL,JK)*(PVU(JL,JK)-ZIMP*PVEN(JL,IK))
        ZMFDU(JL,JK)=PMFD(JL,JK)*(PUD(JL,JK)-ZIMP*PUEN(JL,IK))
        ZMFDV(JL,JK)=PMFD(JL,JK)*(PVD(JL,JK)-ZIMP*PVEN(JL,IK))
      ENDIF
    ENDDO
  ENDDO
  
! linear fluxes below cloud
  IF(RMFSOLUV==0.0_JPRB) THEN
    DO JK=KTOPM2,KLEV
    !DIR$ IVDEP
    !OCL NOVREC
      DO JL=KIDIA,KFDIA
        IF(LDCUM(JL).AND.JK > KCBOT(JL)) THEN
          IKB=KCBOT(JL)
          ZZP=((PAPH(JL,KLEV+1)-PAPH(JL,JK))/(PAPH(JL,KLEV+1)-PAPH(JL,IKB)))
          IF(KTYPE(JL) == 3) THEN
            ZZP=ZZP*ZZP
          ENDIF
          ZMFUU(JL,JK)=ZMFUU(JL,IKB)*ZZP
          ZMFUV(JL,JK)=ZMFUV(JL,IKB)*ZZP
          ZMFDU(JL,JK)=ZMFDU(JL,IKB)*ZZP
          ZMFDV(JL,JK)=ZMFDV(JL,IKB)*ZZP
        ENDIF
      ENDDO
    ENDDO
  ENDIF

!*    1.2          COMPUTE TENDENCIES
!                  ------------------

  DO JK=KTOPM2,KLEV
  
    IF(JK < KLEV) THEN
      IK=JK+1
      DO JL=KIDIA,KFDIA
        IF(LDCUM(JL)) THEN
          ZDUDT(JL,JK)=ZDP(JL,JK)*&
           & (ZMFUU(JL,IK)-ZMFUU(JL,JK)+ZMFDU(JL,IK)-ZMFDU(JL,JK))  
          ZDVDT(JL,JK)=ZDP(JL,JK)*&
           & (ZMFUV(JL,IK)-ZMFUV(JL,JK)+ZMFDV(JL,IK)-ZMFDV(JL,JK))  
        ENDIF
      ENDDO
  
    ELSE
      DO JL=KIDIA,KFDIA
        IF(LDCUM(JL)) THEN
          ZDUDT(JL,JK)=-ZDP(JL,JK)*(ZMFUU(JL,JK)+ZMFDU(JL,JK))
          ZDVDT(JL,JK)=-ZDP(JL,JK)*(ZMFUV(JL,JK)+ZMFDV(JL,JK))
        ENDIF
      ENDDO
    ENDIF
  
  ENDDO

  IF ( RMFSOLUV==0.0_JPRB ) THEN

!*    1.3          UPDATE TENDENCIES
!                  -----------------

    DO JK=KTOPM2,KLEV
      DO JL=KIDIA,KFDIA
        IF(LDCUM(JL)) THEN
          PTENU(JL,JK)=PTENU(JL,JK)+ZDUDT(JL,JK)
          PTENV(JL,JK)=PTENV(JL,JK)+ZDVDT(JL,JK)
        ENDIF
      ENDDO
    ENDDO

  ELSE
!----------------------------------------------------------------------
      
!*      1.6          IMPLICIT SOLUTION
!                    -----------------
   
     ! Fill bi-diagonal Matrix vectors A=k-1, B=k;
     ! reuse ZMFUU=A and ZB=B; 
     ! ZDUDT and ZDVDT correspond to the RHS ("constants") of the equation
     ! The solution is in ZR1 and ZR2
     
      LLCUMBAS(:,:)=.FALSE.
      ZB(:,:)=1.0_JPRB
      ZMFUU(:,:)=0.0_JPRB
     
     ! Fill vectors A, B and RHS 
     
      DO JK=KTOPM2,KLEV
         IK=JK+1
         IM=JK-1
         DO JL=KIDIA,KFDIA
           LLCUMBAS(JL,JK)=LDCUM(JL).AND.JK>=KCTOP(JL)-1 
           IF(LLCUMBAS(JL,JK)) THEN
             ZZP=RMFSOLUV*ZDP(JL,JK)*PTSPHY
             ZMFUU(JL,JK)=-ZZP*(PMFU(JL,JK)+PMFD(JL,JK))
             IF(JK<KLEV) THEN
               ZB(JL,JK)=1.0_JPRB+ZZP*(PMFU(JL,IK)+PMFD(JL,IK))
             ELSE
               ZB(JL,JK)=1.0_JPRB
             ENDIF
             ZZP=RG*(PMFUO(JL,JK)+RMFADVWDD*PMFDO(JL,JK))/(PAP(JL,JK)-PAP(JL,IM))*PTSPHY*ZADVW(JL)
             ZU=ZZP*(PUEN(JL,IM)-PUEN(JL,JK))
             ZV=ZZP*(PVEN(JL,IM)-PVEN(JL,JK))
             ZDUDT(JL,JK) = (ZDUDT(JL,JK)+PTENU(JL,JK)*RMFSOLRHS)*PTSPHY+PUEN(JL,JK)-ZU
             ZDVDT(JL,JK) = (ZDVDT(JL,JK)+PTENV(JL,JK)*RMFSOLRHS)*PTSPHY+PVEN(JL,JK)-ZV
           ENDIF
         ENDDO
      ENDDO
     
      CALL CUBIDIAG&
         &( KIDIA, KFDIA, KLON, KLEV, &
         &  KCTOP, LLCUMBAS, &
         &  ZMFUU,    ZB,    ZDUDT,   ZR1 )
     
      CALL CUBIDIAG&
         &( KIDIA, KFDIA, KLON, KLEV, &
         &  KCTOP, LLCUMBAS, &
         &  ZMFUU,    ZB,    ZDVDT,   ZR2 )
     ! prepare RP perturbations (MF PEARP)
      LLPPAR_CUDUV=YDPERTPAR%LPERTPAR.AND.YDPERTPAR%LPERT_CUDUV
     ! prepare SPP perturbations
      IF (YDSPP_CONFIG%LSPP) THEN
        IPN1 = YDSPP_CONFIG%PPTR%CUDUDV
        LLPERT_CUDUDV= IPN1 > 0
        IF (LLPERT_CUDUDV) THEN
          PN1=YDSPP_CONFIG%SM%PN(IPN1)
          IPCUDU = PN1%MP
          IF (PN1%NRF == 1) THEN
            IPCUDV = IPCUDU
          ELSE
            IPCUDV = IPCUDU+1
          ENDIF
        ENDIF
        IPN2 = YDSPP_CONFIG%PPTR%CUDUDVS
        LLPERT_CUDUDVS= IPN2 > 0
        IF (LLPERT_CUDUDVS) THEN
          PN2=YDSPP_CONFIG%SM%PN(IPN2)
          IPCUDUS = PN2%MP
          IF (PN2%NRF == 1) THEN
            IPCUDVS = IPCUDUS
          ELSE
            IPCUDVS = IPCUDUS+1
          ENDIF
        ENDIF
      ELSE
        LLPERT_CUDUDV=.FALSE.
        LLPERT_CUDUDVS=.FALSE.
      ENDIF

      IF (LLPERT_CUDUDV .OR. LLPERT_CUDUDVS .OR. LLPPAR_CUDUV) THEN
          ZMDU   = 1.0_JPRB    ! mean      zonal momentum transport pdf
          ZMDV   = 1.0_JPRB    ! mean meridional momentum transport pdf
          ZLIMN2 = 9.0_JPRB*PN1%SDEV**2    ! limit for norm squared (3 stdev)

          DO JL=KIDIA,KFDIA
          IF ((KTYPE(JL)==1 .AND. (LLPERT_CUDUDV .OR. LLPPAR_CUDUV)) .OR. &
            & (KTYPE(JL)==2 .AND. (LLPERT_CUDUDVS .OR. LLPPAR_CUDUV))) THEN  !perturb deep/shallow convection

              IF (KTYPE(JL)==1) THEN
                IF (.NOT.LLPPAR_CUDUV) THEN
                  ZXU=PGP2DSPP(JL, IPCUDU)
                  ZXV=PGP2DSPP(JL, IPCUDV)
                  ZXU_MAG = PN1%XMAG(1)
                  ZXV_MAG = PN1%XMAG(2)
                ELSE
                  ZXU=YDPERTPAR%CUDU_MOD
                  ZXV=YDPERTPAR%CUDV_MOD
                ENDIF
              ENDIF

              IF (KTYPE(JL)==2) THEN
             IF (.NOT.LLPPAR_CUDUV) THEN
                ZXU=PGP2DSPP(JL, IPCUDUS)
                ZXV=PGP2DSPP(JL, IPCUDVS)
                ZXU_MAG = PN2%XMAG(1)
                ZXV_MAG = PN2%XMAG(2)
             ELSE
              ZXU=YDPERTPAR%CUDU_MOD
              ZXV=YDPERTPAR%CUDV_MOD
             ENDIF
              ENDIF

              ZN2=ZXU**2+ZXV**2
              IF (ZN2>ZLIMN2) THEN
                ZFAC=SQRT(ZLIMN2/ZN2)
                ZXU = ZFAC*ZXU
                ZXV = ZFAC*ZXV
              ENDIF
              IF(.NOT.LLPPAR_CUDUV) THEN
                ZRDU(JL)=ZMDU + ZXU_MAG*ZXU
                ZRDV(JL)=ZMDV + ZXV_MAG*ZXV
              ELSE
                ZRDU(JL)=ZMDU + ZXU
                ZRDV(JL)=ZMDV + ZXV
              ENDIF
            ELSE  !other convection - unperturbed
              ZRDU(JL)=ZMDU
              ZRDV(JL)=ZMDV
            ENDIF
          ENDDO
      ENDIF

     
     ! Compute tendencies
     
      DO JK=KTOPM2,KLEV
         DO JL=KIDIA,KFDIA
           IF(LLCUMBAS(JL,JK)) THEN
             IF ((LLPERT_CUDUDV).OR.(LLPPAR_CUDUV)) THEN  !Apply SPP perturbations or RP perturbation
               PTENU(JL,JK)=PTENU(JL,JK)*(1.0_JPRB-RMFSOLRHS)+ZRDU(JL)*(ZR1(JL,JK)-PUEN(JL,JK))*ZTSPHY
               PTENV(JL,JK)=PTENV(JL,JK)*(1.0_JPRB-RMFSOLRHS)+ZRDV(JL)*(ZR2(JL,JK)-PVEN(JL,JK))*ZTSPHY
             ELSE
               PTENU(JL,JK)=PTENU(JL,JK)*(1.0_JPRB-RMFSOLRHS)+(ZR1(JL,JK)-PUEN(JL,JK))*ZTSPHY
               PTENV(JL,JK)=PTENV(JL,JK)*(1.0_JPRB-RMFSOLRHS)+(ZR2(JL,JK)-PVEN(JL,JK))*ZTSPHY
             ENDIF
           ENDIF
         ENDDO
      ENDDO


   ENDIF
!----------------------------------------------------------------------

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('CUDUDV',1,ZHOOK_HANDLE)
END SUBROUTINE CUDUDV

