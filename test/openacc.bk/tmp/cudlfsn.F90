SUBROUTINE CUDLFSN &
 & (YDTHF, YDCST, YDEPHLI, YDECUMF,&
 & KIDIA,    KFDIA,    KLON,    KLEV,&
 & KCBOT,    KCTOP,    LDCUM,&
 & PTENH,    PQENH,    PTEN,     PQSEN,    PGEO,&
 & PGEOH,    PAPH,     PTU,      PQU,&
 & PMFUB,    PRFL,&
 & PTD,      PQD,&
 & PMFD,     PMFDS,    PMFDQ,    PDMFDP,&
 & KDTOP,    LDDRAF)  

!          THIS ROUTINE CALCULATES LEVEL OF FREE SINKING FOR
!          CUMULUS DOWNDRAFTS AND SPECIFIES T,Q,U AND V VALUES

!          PURPOSE.
!          --------
!          TO PRODUCE LFS-VALUES FOR CUMULUS DOWNDRAFTS
!          FOR MASSFLUX CUMULUS PARAMETERIZATION

!          INTERFACE
!          ---------
!          THIS ROUTINE IS CALLED FROM *CUMASTR*.
!          INPUT ARE ENVIRONMENTAL VALUES OF T,Q,U,V,P,PHI
!          AND UPDRAFT VALUES T,Q,U AND V AND ALSO
!          CLOUD BASE MASSFLUX AND CU-PRECIPITATION RATE.
!          IT RETURNS T,Q,U AND V VALUES AND MASSFLUX AT LFS.

!          METHOD.

!          CHECK FOR NEGATIVE BUOYANCY OF AIR OF EQUAL PARTS OF
!          MOIST ENVIRONMENTAL AIR AND CLOUD AIR.

!     PARAMETER     DESCRIPTION                                   UNITS
!     ---------     -----------                                   -----
!     INPUT PARAMETERS (INTEGER):

!    *KIDIA*        START POINT
!    *KFDIA*        END POINT
!    *KLON*         NUMBER OF GRID POINTS PER PACKET
!    *KLEV*         NUMBER OF LEVELS
!    *KCBOT*        CLOUD BASE LEVEL
!    *KCTOP*        CLOUD TOP LEVEL

!    INPUT PARAMETERS (LOGICAL):

!    *LDCUM*        FLAG: .TRUE. FOR CONVECTIVE POINTS

!    INPUT PARAMETERS (REAL):

!    *PTENH*        ENV. TEMPERATURE (T+1) ON HALF LEVELS          K
!    *PQENH*        ENV. SPEC. HUMIDITY (T+1) ON HALF LEVELS     KG/KG
!    *PTEN*         PROVISIONAL ENVIRONMENT TEMPERATURE (T+1)       K
!    *PQSEN*        ENVIRONMENT SPEC. SATURATION HUMIDITY (T+1)   KG/KG
!    *PGEO*         GEOPOTENTIAL                                  M2/S2
!    *PGEOH*        GEOPOTENTIAL ON HALF LEVELS                  M2/S2
!    *PAPH*         PROVISIONAL PRESSURE ON HALF LEVELS           PA
!    *PTU*          TEMPERATURE IN UPDRAFTS                        K
!    *PQU*          SPEC. HUMIDITY IN UPDRAFTS                   KG/KG
!    *PMFUB*        MASSFLUX IN UPDRAFTS AT CLOUD BASE           KG/(M2*S)

!    UPDATED PARAMETERS (REAL):

!    *PRFL*         PRECIPITATION RATE                           KG/(M2*S)

!    OUTPUT PARAMETERS (REAL):

!    *PTD*          TEMPERATURE IN DOWNDRAFTS                      K
!    *PQD*          SPEC. HUMIDITY IN DOWNDRAFTS                 KG/KG
!    *PMFD*         MASSFLUX IN DOWNDRAFTS                       KG/(M2*S)
!    *PMFDS*        FLUX OF DRY STATIC ENERGY IN DOWNDRAFTS       J/(M2*S)
!    *PMFDQ*        FLUX OF SPEC. HUMIDITY IN DOWNDRAFTS         KG/(M2*S)
!    *PDMFDP*       FLUX DIFFERENCE OF PRECIP. IN DOWNDRAFTS     KG/(M2*S)

!    OUTPUT PARAMETERS (INTEGER):

!    *KDTOP*        TOP LEVEL OF DOWNDRAFTS

!    OUTPUT PARAMETERS (LOGICAL):

!    *LDDRAF*       .TRUE. IF DOWNDRAFTS EXIST

!          EXTERNALS
!          ---------
!          *CUADJTQ* FOR CALCULATING WET BULB T AND Q AT LFS

!     AUTHOR.
!     -------
!      M.TIEDTKE         E.C.M.W.F.    12/86 MODIF. 12/89

!     MODIFICATIONS.
!     --------------
!      M.Hamrud      01-Oct-2003 CY28 Cleaning
!      P. Lopez      20-Jun-2007 CY32R2 Bug correction in latent heat when LPHYLIN=T.
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY : TCST
USE YOETHF   , ONLY : TTHF  
USE YOECUMF  , ONLY : TECUMF
USE YOEPHLI  , ONLY : TEPHLI

IMPLICIT NONE

TYPE(TTHF)        ,INTENT(IN)    :: YDTHF
TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(TECUMF)      ,INTENT(IN)    :: YDECUMF
TYPE(TEPHLI)      ,INTENT(IN)    :: YDEPHLI
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KCBOT(KLON)
INTEGER(KIND=JPIM),INTENT(IN)    :: KCTOP(KLON)
LOGICAL           ,INTENT(IN)    :: LDCUM(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTENH(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQENH(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTEN(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQSEN(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEO(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEOH(KLON,KLEV+1) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPH(KLON,KLEV+1) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMFUB(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PRFL(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTD(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQD(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PMFD(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMFDS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PMFDQ(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDMFDP(KLON,KLEV) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: KDTOP(KLON) 
LOGICAL           ,INTENT(OUT)   :: LDDRAF(KLON) 
INTEGER(KIND=JPIM) ::            IKHSMIN(KLON)
REAL(KIND=JPRB) ::     ZTENWB(KLON,KLEV),      ZQENWB(KLON,KLEV),&
 & ZCOND(KLON),            ZPH(KLON),&
 & ZHSMIN(KLON)  
LOGICAL ::  LLO2(KLON)

INTEGER(KIND=JPIM) :: IK, IKE, IS, JK, JL

REAL(KIND=JPRB) :: ZBUO, ZHSK, ZMFTOP, ZOEALFA,&
 & ZOELHM, ZQTEST, ZTARG, ZTTEST  
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "cuadjtq.intfb.h"

#include "fcttre.func.h"
!----------------------------------------------------------------------

!     1.           SET DEFAULT VALUES FOR DOWNDRAFTS
!                  ---------------------------------

IF (LHOOK) CALL DR_HOOK('CUDLFSN',0,ZHOOK_HANDLE)
ASSOCIATE(LMFDD=>YDECUMF%LMFDD, RMFDEPS=>YDECUMF%RMFDEPS, &
 & RCPD=>YDCST%RCPD, RETV=>YDCST%RETV, RLSTT=>YDCST%RLSTT, RLVTT=>YDCST%RLVTT, &
 & LPHYLIN=>YDEPHLI%LPHYLIN, RLPTRC=>YDEPHLI%RLPTRC)
DO JL=KIDIA,KFDIA
  LDDRAF(JL)=.FALSE.
  KDTOP(JL)=KLEV+1
  IKHSMIN(JL)=KLEV+1
  ZHSMIN(JL)=1.E8_JPRB
ENDDO

!orig IF(.NOT.LMFDD) GO TO 300
IF(LMFDD) THEN

!----------------------------------------------------------------------

!     2.           DETERMINE LEVEL OF FREE SINKING:
!                  DOWNDRAFTS SHALL START AT MODEL LEVEL OF MINIMUM
!                  OF SATURATION MOIST STATIC ENERGY OR BELOW
!                  RESPECTIVELY

!                  FOR EVERY POINT AND PROCEED AS FOLLOWS:

!                    (1) DETERMINE LEVEL OF MINIMUM OF HS
!                    (2) DETERMINE WET BULB ENVIRONMENTAL T AND Q
!                    (3) DO MIXING WITH CUMULUS CLOUD AIR
!                    (4) CHECK FOR NEGATIVE BUOYANCY
!                    (5) IF BUOYANCY>0 REPEAT (2) TO (4) FOR NEXT
!                        LEVEL BELOW

!                  THE ASSUMPTION IS THAT AIR OF DOWNDRAFTS IS MIXTURE
!                  OF 50% CLOUD AIR + 50% ENVIRONMENTAL AIR AT WET BULB
!                  TEMPERATURE (I.E. WHICH BECAME SATURATED DUE TO
!                  EVAPORATION OF RAIN AND CLOUD WATER)
!                  ----------------------------------------------------

  DO JK=3,KLEV-2

    IF (LPHYLIN) THEN

      DO JL=KIDIA,KFDIA
        ZTARG=PTEN(JL,JK)
        ZOEALFA=MIN(1.0_JPRB,0.545_JPRB*(TANH(0.17_JPRB*(ZTARG-RLPTRC))+1.0_JPRB))
        ZOELHM =ZOEALFA*RLVTT+(1.0_JPRB-ZOEALFA)*RLSTT
        ZHSK=RCPD*PTEN(JL,JK)+PGEO(JL,JK)+ZOELHM*PQSEN(JL,JK)
        IF(ZHSK < ZHSMIN(JL)) THEN
          ZHSMIN(JL)=ZHSK
          IKHSMIN(JL)=JK
        ENDIF
      ENDDO

    ELSE

      DO JL=KIDIA,KFDIA
        ZHSK=RCPD*PTEN(JL,JK)+PGEO(JL,JK)+FOELHMCU(PTEN(JL,JK))*PQSEN(JL,JK)
        IF(ZHSK < ZHSMIN(JL)) THEN
          ZHSMIN(JL)=ZHSK
          IKHSMIN(JL)=JK
        ENDIF
      ENDDO

    ENDIF

  ENDDO
  IKE=KLEV-3
  DO JK=3,IKE

!     2.1          CALCULATE WET-BULB TEMPERATURE AND MOISTURE
!                  FOR ENVIRONMENTAL AIR IN *CUADJTQ*
!                  -------------------------------------------

    IS=0
    DO JL=KIDIA,KFDIA
      ZTENWB(JL,JK)=PTENH(JL,JK)
      ZQENWB(JL,JK)=PQENH(JL,JK)
      ZPH(JL)=PAPH(JL,JK)
      LLO2(JL)=LDCUM(JL).AND.PRFL(JL) > 0.0_JPRB.AND..NOT.LDDRAF(JL).AND.&
       & (JK < KCBOT(JL).AND.JK > KCTOP(JL)).AND.&
       & JK >= IKHSMIN(JL)  
      IF(LLO2(JL))THEN
        IS=IS+1
      ENDIF
    ENDDO
!orig   IF(IS.EQ.0) GO TO 290
    IF(IS == 0) CYCLE

    IK=JK
    CALL CUADJTQ &
     & ( YDTHF, YDCST, YDEPHLI,  KIDIA,    KFDIA,    KLON,     KLEV,     IK,&
     &   ZPH,      ZTENWB,   ZQENWB,   LLO2,     KCALL=2)  

!     2.2          DO MIXING OF CUMULUS AND ENVIRONMENTAL AIR
!                  AND CHECK FOR NEGATIVE BUOYANCY.
!                  THEN SET VALUES FOR DOWNDRAFT AT LFS.
!                  ----------------------------------------

!DIR$ IVDEP
!OCL NOVREC
    DO JL=KIDIA,KFDIA
      IF(LLO2(JL)) THEN
        ZTTEST=0.5_JPRB*(PTU(JL,JK)+ZTENWB(JL,JK))
        ZQTEST=0.5_JPRB*(PQU(JL,JK)+ZQENWB(JL,JK))
        ZBUO=ZTTEST*(1.0_JPRB+RETV  *ZQTEST)-&
         & PTENH(JL,JK)*(1.0_JPRB+RETV  *PQENH(JL,JK))  
        ZCOND(JL)=PQENH(JL,JK)-ZQENWB(JL,JK)
        ZMFTOP=-RMFDEPS*PMFUB(JL)
        IF(ZBUO < 0.0_JPRB.AND.PRFL(JL) > 10._JPRB*ZMFTOP*ZCOND(JL)) THEN
          KDTOP(JL)=JK
          LDDRAF(JL)=.TRUE.
          PTD(JL,JK)=ZTTEST
          PQD(JL,JK)=ZQTEST
          PMFD(JL,JK)=ZMFTOP
          PMFDS(JL,JK)=PMFD(JL,JK)*(RCPD*PTD(JL,JK)+PGEOH(JL,JK))
          PMFDQ(JL,JK)=PMFD(JL,JK)*PQD(JL,JK)
          PDMFDP(JL,JK-1)=-0.5_JPRB*PMFD(JL,JK)*ZCOND(JL)
          PRFL(JL)=PRFL(JL)+PDMFDP(JL,JK-1)
        ENDIF
      ENDIF
    ENDDO

! 290   continue
  ENDDO

!300  CONTINUE
ENDIF

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('CUDLFSN',1,ZHOOK_HANDLE)
CONTAINS

! (C) Copyright 1988- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

SUBROUTINE CUADJTQ &
 & (YDTHF, YDCST, YDEPHLI,KIDIA,    KFDIA,    KLON,    KLEV,     KK,&
 &  PSP,      PT,       PQ,      LDFLAG,   KCALL,   LDOFLAG)  

!          PURPOSE.
!          --------
!          TO PRODUCE T,Q AND L VALUES FOR CLOUD ASCENT

!          INTERFACE
!          ---------
!          THIS ROUTINE IS CALLED FROM SUBROUTINES:
!              *COND*     (T AND Q AT CONDENSATION LEVEL)
!              *CUBASE*   (T AND Q AT CONDENSATION LEVEL)
!              *CUASC*    (T AND Q AT CLOUD LEVELS)
!              *CUINI*    (ENVIRONMENTAL T AND QS VALUES AT HALF LEVELS)
!              *CUSTRAT*  (T AND Q AT CONDENSATION LEVEL)
!          INPUT ARE UNADJUSTED T AND Q VALUES,
!          IT RETURNS ADJUSTED VALUES OF T AND Q

!     PARAMETER     DESCRIPTION                                   UNITS
!     ---------     -----------                                   -----
!     INPUT PARAMETERS (INTEGER):

!    *KIDIA*        START POINT
!    *KFDIA*        END POINT
!    *KLON*         NUMBER OF GRID POINTS PER PACKET
!    *KLEV*         NUMBER OF LEVELS
!    *KK*           LEVEL
!    *KCALL*        DEFINES CALCULATION AS
!                      KCALL=0  ENV. T AND QS IN*CUINI*
!                      KCALL=1  CONDENSATION IN UPDRAFTS  (E.G. CUBASE, CUASC)
!                      KCALL=2  EVAPORATION IN DOWNDRAFTS (E.G. CUDLFS,CUDDRAF)

!     INPUT PARAMETERS (LOGICAL):

!    *LDLAND*       LAND-SEA MASK (.TRUE. FOR LAND POINTS)

!     INPUT PARAMETERS (REAL):

!    *PSP*          PRESSURE                                        PA

!     UPDATED PARAMETERS (REAL):

!    *PT*           TEMPERATURE                                     K
!    *PQ*           SPECIFIC HUMIDITY                             KG/KG

!          EXTERNALS   
!          ---------
!          3 LOOKUP TABLES ( TLUCUA, TLUCUB, TLUCUC )
!          FOR CONDENSATION CALCULATIONS.
!          THE TABLES ARE INITIALISED IN *SUPHEC*.

!     AUTHOR.
!     -------
!      M.TIEDTKE         E.C.M.W.F.     12/89

!     MODIFICATIONS.
!     --------------
!      J.HAGUE               03-01-13   MASS Vector Functions
!      J.HAGUE               03-07-07   More MASS V.F.
!      M.Hamrud              01-Oct-2003 CY28 Cleaning
!      J.Hague & D.Salmond   22-Nov-2005 Optimisations 
!     R. El Khatib 22-Jun-2022 A contribution to simplify phasing after the refactoring of YOMCLI/YOMCST/YOETHF.
!----------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM     ,JPRB
USE YOMHOOK  , ONLY : LHOOK,   DR_HOOK, JPHOOK

USE YOMCST   , ONLY : TCST
USE YOETHF   , ONLY : TTHF  
USE YOEPHLI  , ONLY : TEPHLI

IMPLICIT NONE

TYPE(TTHF)         ,INTENT(IN)               :: YDTHF
TYPE(TCST)         ,INTENT(IN)               :: YDCST
TYPE(TEPHLI)       ,INTENT(IN)               :: YDEPHLI
INTEGER(KIND=JPIM) ,INTENT(IN)               :: KIDIA 
INTEGER(KIND=JPIM) ,INTENT(IN)               :: KFDIA 
INTEGER(KIND=JPIM) ,INTENT(IN)               :: KLON 
INTEGER(KIND=JPIM) ,INTENT(IN)               :: KLEV 
INTEGER(KIND=JPIM) ,INTENT(IN)               :: KK 
REAL(KIND=JPRB)    ,INTENT(IN)               :: PSP(KLON) 
REAL(KIND=JPRB)    ,INTENT(INOUT)            :: PT(KLON,KLEV) 
REAL(KIND=JPRB)    ,INTENT(INOUT)            :: PQ(KLON,KLEV) 
LOGICAL            ,INTENT(IN)               :: LDFLAG(KLON) 
INTEGER(KIND=JPIM) ,INTENT(IN)               :: KCALL 
LOGICAL            ,INTENT(IN)    ,OPTIONAL  :: LDOFLAG(KLON) 

INTEGER(KIND=JPIM) :: JL

REAL(KIND=JPRB) :: Z1S, Z2S, ZCOND,ZCOND1, ZCOR, ZFOEEWI, ZFOEEWL,&
 & ZOEALFA, ZQMAX, ZQSAT, ZTARG, ZQP
REAL(KIND=JPRB) :: ZL, ZI, ZF

LOGICAL :: LLFLAG(KLON)

#include "abor1.intfb.h"

!DIR$ VFUNCTION EXPHF
#include "fcttre.func.h"
#include "cuadjtq.func.h"

!     STATEMENT FUNCTIONS

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!----------------------------------------------------------------------

!     1.           DEFINE CONSTANTS
!                  ----------------


!IF (LHOOK) CALL DR_HOOK('CUADJTQ',0,ZHOOK_HANDLE)

ASSOCIATE(LPHYLIN=>YDEPHLI%LPHYLIN, RLPAL1=>YDEPHLI%RLPAL1, &
 & R2ES=>YDTHF%R2ES, R3IES=>YDTHF%R3IES, R3LES=>YDTHF%R3LES, R4IES=>YDTHF%R4IES, &
 & R4LES=>YDTHF%R4LES, R5ALSCP=>YDTHF%R5ALSCP, R5ALVCP=>YDTHF%R5ALVCP, RALSDCP=>YDTHF%RALSDCP, &
 & RALVDCP=>YDTHF%RALVDCP, &
 & RLPAL2=>YDEPHLI%RLPAL2, RLPTRC=>YDEPHLI%RLPTRC, RETV=>YDCST%RETV, RTT=>YDCST%RTT)

ZQMAX=0.5_JPRB

!*********************************************
IF (.NOT.LPHYLIN) THEN
!*********************************************                 

!     2.           CALCULATE CONDENSATION AND ADJUST T AND Q ACCORDINGLY
!                  -----------------------------------------------------

  IF (KCALL == 1 .OR. KCALL == 6 ) THEN

!   mixed phase saturation

      LLFLAG(KIDIA:KFDIA) = LDFLAG(KIDIA:KFDIA)
      IF(KCALL == 6) THEN
        IF(PRESENT(LDOFLAG)) THEN
          LLFLAG(KIDIA:KFDIA) = LLFLAG(KIDIA:KFDIA) .AND. .NOT.LDOFLAG(KIDIA:KFDIA)
        ELSE
          CALL ABOR1("CUADJTQ: LDOFLAG has to be present when KCALL==6")
        ENDIF
      ENDIF

      DO JL=KIDIA,KFDIA
      IF (LLFLAG(JL)) THEN
        ZQP    =1.0_JPRB/PSP(JL)
        ZL=1.0_JPRB/(PT(JL,KK)-R4LES)
        ZI=1.0_JPRB/(PT(JL,KK)-R4IES)
!       ZQSAT=FOEEWMCU(PT(JL,KK))*ZQP
        ZQSAT=R2ES *(FOEALFCU(PT(JL,KK))*EXP(R3LES*(PT(JL,KK)-RTT)*ZL)+&
          &(1.0_JPRB-FOEALFCU(PT(JL,KK)))*EXP(R3IES*(PT(JL,KK)-RTT)*ZI))
        ZQSAT=ZQSAT*ZQP
        ZQSAT=MIN(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB-RETV*ZQSAT
        ZF=FOEALFCU(PT(JL,KK))*R5ALVCP*ZL**2 +&
          &(1.0_JPRB-FOEALFCU(PT(JL,KK)))*R5ALSCP*ZI**2
        ZCOND=(PQ(JL,KK)*ZCOR**2-ZQSAT*ZCOR)/(ZCOR**2+ZQSAT*ZF)
        ZCOND=MAX(ZCOND,0.0_JPRB)
        PT(JL,KK)=PT(JL,KK)+FOELDCPMCU(PT(JL,KK))*ZCOND
        PQ(JL,KK)=PQ(JL,KK)-ZCOND
        ZL=1.0_JPRB/(PT(JL,KK)-R4LES)
        ZI=1.0_JPRB/(PT(JL,KK)-R4IES)
!       ZQSAT=FOEEWMCU(PT(JL,KK))*ZQP
        ZQSAT=R2ES *(FOEALFCU(PT(JL,KK))*EXP(R3LES*(PT(JL,KK)-RTT)*ZL)+&
          &(1.0_JPRB-FOEALFCU(PT(JL,KK)))*EXP(R3IES*(PT(JL,KK)-RTT)*ZI))
        ZQSAT=ZQSAT*ZQP
        ZQSAT=FMINJ(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB-RETV*ZQSAT
        ZF=FOEALFCU(PT(JL,KK))*R5ALVCP*ZL**2 +&
          &(1.0_JPRB-FOEALFCU(PT(JL,KK)))*R5ALSCP*ZI**2
        ZCOND1=(PQ(JL,KK)*ZCOR**2-ZQSAT*ZCOR)/(ZCOR**2+ZQSAT*ZF)
        IF(ZCOND ==  0.0_JPRB)ZCOND1=0.0_JPRB
        PT(JL,KK)=PT(JL,KK)+FOELDCPMCU(PT(JL,KK))*ZCOND1
        PQ(JL,KK)=PQ(JL,KK)-ZCOND1
      ENDIF
      ENDDO

      IF(KCALL==6) THEN
  
        DO JL=KIDIA,KFDIA
        IF(LDFLAG(JL).AND.LDOFLAG(JL)) THEN
          ZQP    =1.0_JPRB/PSP(JL)
          ZL=1.0_JPRB/(PT(JL,KK)-R4LES)
          ZQSAT=R2ES *EXP(R3LES*(PT(JL,KK)-RTT)*ZL)
          ZQSAT=ZQSAT*ZQP
          ZQSAT=MIN(0.5_JPRB,ZQSAT)
          ZCOR=1.0_JPRB-RETV*ZQSAT
          ZF=R5ALVCP*ZL**2
          ZCOND=(PQ(JL,KK)*ZCOR**2-ZQSAT*ZCOR)/(ZCOR**2+ZQSAT*ZF)
          ZCOND=MAX(ZCOND,0.0_JPRB)
          PT(JL,KK)=PT(JL,KK)+RALVDCP*ZCOND
          PQ(JL,KK)=PQ(JL,KK)-ZCOND
          ZL=1.0_JPRB/(PT(JL,KK)-R4LES)
          ZQSAT=R2ES *EXP(R3LES*(PT(JL,KK)-RTT)*ZL)
          ZQSAT=ZQSAT*ZQP
          ZQSAT=FMINJ(0.5_JPRB,ZQSAT)
          ZCOR=1.0_JPRB-RETV*ZQSAT
          ZF=R5ALVCP*ZL**2
          ZCOND1=(PQ(JL,KK)*ZCOR**2-ZQSAT*ZCOR)/(ZCOR**2+ZQSAT*ZF)
          IF(ZCOND ==  0.0_JPRB)ZCOND1=0.0_JPRB
          PT(JL,KK)=PT(JL,KK)+RALVDCP*ZCOND1
          PQ(JL,KK)=PQ(JL,KK)-ZCOND1
        ENDIF
        ENDDO
  
      ENDIF

  ENDIF

  IF(KCALL == 2) THEN

!DIR$    IVDEP
!OCL NOVREC
    DO JL=KIDIA,KFDIA
      IF(LDFLAG(JL)) THEN
        ZQP    =1.0_JPRB/PSP(JL)
        ZQSAT=FOEEWMCU(PT(JL,KK))*ZQP    
        ZQSAT=MIN(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        ZCOND=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEMCU(PT(JL,KK)))
        ZCOND=MIN(ZCOND,0.0_JPRB)
        PT(JL,KK)=PT(JL,KK)+FOELDCPMCU(PT(JL,KK))*ZCOND
        PQ(JL,KK)=PQ(JL,KK)-ZCOND
        ZQSAT=FOEEWMCU(PT(JL,KK))*ZQP    
        ZQSAT=MIN(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEMCU(PT(JL,KK)))
        IF(ZCOND == 0.0_JPRB)ZCOND1=MIN(ZCOND1,0.0_JPRB)
        PT(JL,KK)=PT(JL,KK)+FOELDCPMCU(PT(JL,KK))*ZCOND1
        PQ(JL,KK)=PQ(JL,KK)-ZCOND1
      ENDIF
    ENDDO

  ENDIF

  IF(KCALL == 0) THEN

!DIR$    IVDEP
!OCL NOVREC

!DIR$ LOOP_INFO EST_TRIPS(16)
    DO JL=KIDIA,KFDIA
      ZQP    =1.0_JPRB/PSP(JL)
      ZQSAT=FOEEWM(PT(JL,KK))*ZQP    
      ZQSAT=MIN(0.5_JPRB,ZQSAT)
      ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
      ZQSAT=ZQSAT*ZCOR
      ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEM(PT(JL,KK)))
      PT(JL,KK)=PT(JL,KK)+FOELDCPM(PT(JL,KK))*ZCOND1
      PQ(JL,KK)=PQ(JL,KK)-ZCOND1
      ZQSAT=FOEEWM(PT(JL,KK))*ZQP    
      ZQSAT=MIN(0.5_JPRB,ZQSAT)
      ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
      ZQSAT=ZQSAT*ZCOR
      ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEM(PT(JL,KK)))
      PT(JL,KK)=PT(JL,KK)+FOELDCPM(PT(JL,KK))*ZCOND1
      PQ(JL,KK)=PQ(JL,KK)-ZCOND1
    ENDDO

  ENDIF

  IF(KCALL == 4 )THEN

    DO JL=KIDIA,KFDIA
      IF(LDFLAG(JL)) THEN
        ZQP    =1.0_JPRB/PSP(JL)
        ZQSAT=FOEEWM(PT(JL,KK))*ZQP
        ZQSAT=MIN(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        ZCOND=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEM(PT(JL,KK)))
        PT(JL,KK)=PT(JL,KK)+FOELDCPM(PT(JL,KK))*ZCOND
        PQ(JL,KK)=PQ(JL,KK)-ZCOND
        ZQSAT=FOEEWM(PT(JL,KK))*ZQP
        ZQSAT=MIN(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEM(PT(JL,KK)))
        PT(JL,KK)=PT(JL,KK)+FOELDCPM(PT(JL,KK))*ZCOND1
        PQ(JL,KK)=PQ(JL,KK)-ZCOND1
      ENDIF
    ENDDO
      
  ENDIF 

  IF(KCALL == 5) THEN  ! Same as 4 but with LDFLAG all true

!OCL NOVREC
!DIR$    IVDEP
!DIR$ LOOP_INFO EST_TRIPS(16)
      DO JL=KIDIA,KFDIA
        ZQP    =1.0_JPRB/PSP(JL)
        ZQSAT=FOEEWM(PT(JL,KK))*ZQP    
        ZQSAT=MIN(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        ZCOND=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEM(PT(JL,KK)))
        PT(JL,KK)=PT(JL,KK)+FOELDCPM(PT(JL,KK))*ZCOND
        PQ(JL,KK)=PQ(JL,KK)-ZCOND
        ZQSAT=FOEEWM(PT(JL,KK))*ZQP    
        ZQSAT=MIN(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEM(PT(JL,KK)))
        PT(JL,KK)=PT(JL,KK)+FOELDCPM(PT(JL,KK))*ZCOND1
        PQ(JL,KK)=PQ(JL,KK)-ZCOND1
      ENDDO
  ENDIF 

  IF(KCALL == 3) THEN 
!DIR$ LOOP_INFO EST_TRIPS(16)
      DO JL=KIDIA,KFDIA
        ZQP    =1.0_JPRB/PSP(JL)
        ZQSAT=FOEEWMCU(PT(JL,KK))*ZQP
        ZQSAT=MIN(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEMCU(PT(JL,KK)))
        PT(JL,KK)=PT(JL,KK)+FOELDCPMCU(PT(JL,KK))*ZCOND1
        PQ(JL,KK)=PQ(JL,KK)-ZCOND1
        ZQSAT=FOEEWMCU(PT(JL,KK))*ZQP
        ZQSAT=MIN(0.5_JPRB,ZQSAT)
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*FOEDEMCU(PT(JL,KK)))
        PT(JL,KK)=PT(JL,KK)+FOELDCPMCU(PT(JL,KK))*ZCOND1
        PQ(JL,KK)=PQ(JL,KK)-ZCOND1
      ENDDO

  ENDIF
!*********************************************
ELSE
!*********************************************                 

!     2.           CALCULATE CONDENSATION AND ADJUST T AND Q ACCORDINGLY
!                  -----------------------------------------------------

  IF (KCALL == 1 ) THEN

!DIR$    IVDEP
!OCL NOVREC
!DIR$ LOOP_INFO EST_TRIPS(16)
    DO JL=KIDIA,KFDIA
      IF(LDFLAG(JL)) THEN
        ZQP    =1.0_JPRB/PSP(JL)
        ZTARG=PT(JL,KK)
        ZOEALFA=0.5_JPRB*(TANH(RLPAL1*(ZTARG-RLPTRC))+1.0_JPRB)
        ZFOEEWL=R2ES*EXP(R3LES*(ZTARG-RTT)/(ZTARG-R4LES))
        ZFOEEWI=R2ES*EXP(R3IES*(ZTARG-RTT)/(ZTARG-R4IES))
        ZQSAT=ZQP    *(ZOEALFA*ZFOEEWL+(1.0_JPRB-ZOEALFA)*ZFOEEWI)
        Z1S=TANH(RLPAL2*(ZQSAT-ZQMAX))
        ZQSAT=0.5_JPRB*((1.0_JPRB-Z1S)*ZQSAT+(1.0_JPRB+Z1S)*ZQMAX)

        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR

        Z2S=    ZOEALFA *R5ALVCP*(1.0_JPRB/(ZTARG-R4LES)**2)+&
         & (1.0_JPRB-ZOEALFA)*R5ALSCP*(1.0_JPRB/(ZTARG-R4IES)**2)  
        ZCOND=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*Z2S)

        ZCOND=MAX(ZCOND,0.0_JPRB)

        IF(ZCOND /= 0.0_JPRB) THEN

          PT(JL,KK)=PT(JL,KK)+&
           & (ZOEALFA*RALVDCP+(1.0_JPRB-ZOEALFA)*RALSDCP)*ZCOND  
          PQ(JL,KK)=PQ(JL,KK)-ZCOND
          ZTARG=PT(JL,KK)
          ZOEALFA=0.5_JPRB*(TANH(RLPAL1*(ZTARG-RLPTRC))+1.0_JPRB)
          ZFOEEWL=R2ES*EXP(R3LES*(ZTARG-RTT)/(ZTARG-R4LES))
          ZFOEEWI=R2ES*EXP(R3IES*(ZTARG-RTT)/(ZTARG-R4IES))
          ZQSAT=ZQP    *(ZOEALFA*ZFOEEWL+(1.0_JPRB-ZOEALFA)*ZFOEEWI)
          Z1S=TANH(RLPAL2*(ZQSAT-ZQMAX))
          ZQSAT=0.5_JPRB*((1.0_JPRB-Z1S)*ZQSAT+(1.0_JPRB+Z1S)*ZQMAX)
  
          ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
          ZQSAT=ZQSAT*ZCOR
  
          Z2S=    ZOEALFA *R5ALVCP*(1.0_JPRB/(ZTARG-R4LES)**2)+&
           & (1.0_JPRB-ZOEALFA)*R5ALSCP*(1.0_JPRB/(ZTARG-R4IES)**2)  
          ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*Z2S)
  
          PT(JL,KK)=PT(JL,KK)+(ZOEALFA*RALVDCP+(1.0_JPRB-ZOEALFA)*RALSDCP)*ZCOND1
  
          PQ(JL,KK)=PQ(JL,KK)-ZCOND1
        ENDIF
      ENDIF
    ENDDO

  ENDIF

  IF(KCALL == 2) THEN

!DIR$    IVDEP
!OCL NOVREC
    DO JL=KIDIA,KFDIA
      IF(LDFLAG(JL)) THEN
        ZQP    =1.0_JPRB/PSP(JL)

        ZTARG=PT(JL,KK)
        ZOEALFA=0.5_JPRB*(TANH(RLPAL1*(ZTARG-RLPTRC))+1.0_JPRB)
        ZFOEEWL=R2ES*EXP(R3LES*(ZTARG-RTT)/(ZTARG-R4LES))
        ZFOEEWI=R2ES*EXP(R3IES*(ZTARG-RTT)/(ZTARG-R4IES))
        ZQSAT=ZQP    *(ZOEALFA*ZFOEEWL+(1.0_JPRB-ZOEALFA)*ZFOEEWI)
        Z1S=TANH(RLPAL2*(ZQSAT-ZQMAX))
        ZQSAT=0.5_JPRB*((1.0_JPRB-Z1S)*ZQSAT+(1.0_JPRB+Z1S)*ZQMAX)

        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR

        Z2S=    ZOEALFA *R5ALVCP*(1.0_JPRB/(ZTARG-R4LES)**2)+&
         & (1.0_JPRB-ZOEALFA)*R5ALSCP*(1.0_JPRB/(ZTARG-R4IES)**2)  
        ZCOND=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*Z2S)

        ZCOND=MIN(ZCOND,0.0_JPRB)

        IF(ZCOND /= 0.0_JPRB) THEN

          PT(JL,KK)=PT(JL,KK)+&
           & (ZOEALFA*RALVDCP+(1.0_JPRB-ZOEALFA)*RALSDCP)*ZCOND  
          PQ(JL,KK)=PQ(JL,KK)-ZCOND
          ZTARG=PT(JL,KK)
          ZOEALFA=0.5_JPRB*(TANH(RLPAL1*(ZTARG-RLPTRC))+1.0_JPRB)
          ZFOEEWL=R2ES*EXP(R3LES*(ZTARG-RTT)/(ZTARG-R4LES))
          ZFOEEWI=R2ES*EXP(R3IES*(ZTARG-RTT)/(ZTARG-R4IES))
          ZQSAT=ZQP    *(ZOEALFA*ZFOEEWL+(1.0_JPRB-ZOEALFA)*ZFOEEWI)
          Z1S=TANH(RLPAL2*(ZQSAT-ZQMAX))
          ZQSAT=0.5_JPRB*((1.0_JPRB-Z1S)*ZQSAT+(1.0_JPRB+Z1S)*ZQMAX)
  
          ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
          ZQSAT=ZQSAT*ZCOR
  
          Z2S=    ZOEALFA *R5ALVCP*(1.0_JPRB/(ZTARG-R4LES)**2)+&
           & (1.0_JPRB-ZOEALFA)*R5ALSCP*(1.0_JPRB/(ZTARG-R4IES)**2)  
          ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*Z2S)
  
          PT(JL,KK)=PT(JL,KK)+(ZOEALFA*RALVDCP+(1.0_JPRB-ZOEALFA)*RALSDCP)*ZCOND1
  
          PQ(JL,KK)=PQ(JL,KK)-ZCOND1
        ENDIF
      ENDIF
    ENDDO

  ENDIF

  IF(KCALL == 0) THEN

!DIR$    IVDEP
!OCL NOVREC
    DO JL=KIDIA,KFDIA
      ZQP    =1.0_JPRB/PSP(JL)

      ZTARG=PT(JL,KK)
      ZOEALFA=0.5_JPRB*(TANH(RLPAL1*(ZTARG-RLPTRC))+1.0_JPRB)
      ZFOEEWL=R2ES*EXP(R3LES*(ZTARG-RTT)/(ZTARG-R4LES))
      ZFOEEWI=R2ES*EXP(R3IES*(ZTARG-RTT)/(ZTARG-R4IES))
      ZQSAT=ZQP    *(ZOEALFA*ZFOEEWL+(1.0_JPRB-ZOEALFA)*ZFOEEWI)
      Z1S=TANH(RLPAL2*(ZQSAT-ZQMAX))
      ZQSAT=0.5_JPRB*((1.0_JPRB-Z1S)*ZQSAT+(1.0_JPRB+Z1S)*ZQMAX)

      ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
      ZQSAT=ZQSAT*ZCOR

      Z2S=    ZOEALFA *R5ALVCP*(1.0_JPRB/(ZTARG-R4LES)**2)+&
       & (1.0_JPRB-ZOEALFA)*R5ALSCP*(1.0_JPRB/(ZTARG-R4IES)**2)  
      ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*Z2S)

      PT(JL,KK)=PT(JL,KK)+(ZOEALFA*RALVDCP+(1.0_JPRB-ZOEALFA)*RALSDCP)*ZCOND1

      PQ(JL,KK)=PQ(JL,KK)-ZCOND1

      ZTARG=PT(JL,KK)
      ZOEALFA=0.5_JPRB*(TANH(RLPAL1*(ZTARG-RLPTRC))+1.0_JPRB)
      ZFOEEWL=R2ES*EXP(R3LES*(ZTARG-RTT)/(ZTARG-R4LES))
      ZFOEEWI=R2ES*EXP(R3IES*(ZTARG-RTT)/(ZTARG-R4IES))
      ZQSAT=ZQP    *(ZOEALFA*ZFOEEWL+(1.0_JPRB-ZOEALFA)*ZFOEEWI)
      Z1S=TANH(RLPAL2*(ZQSAT-ZQMAX))
      ZQSAT=0.5_JPRB*((1.0_JPRB-Z1S)*ZQSAT+(1.0_JPRB+Z1S)*ZQMAX)

      ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
      ZQSAT=ZQSAT*ZCOR

      Z2S=    ZOEALFA *R5ALVCP*(1.0_JPRB/(ZTARG-R4LES)**2)+&
       & (1.0_JPRB-ZOEALFA)*R5ALSCP*(1.0_JPRB/(ZTARG-R4IES)**2)  
      ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*Z2S)

      PT(JL,KK)=PT(JL,KK)+(ZOEALFA*RALVDCP+(1.0_JPRB-ZOEALFA)*RALSDCP)*ZCOND1

      PQ(JL,KK)=PQ(JL,KK)-ZCOND1
    ENDDO

  ENDIF

  IF(KCALL == 4) THEN

!DIR$    IVDEP
!OCL NOVREC
    DO JL=KIDIA,KFDIA
      IF(LDFLAG(JL)) THEN
        ZQP    =1.0_JPRB/PSP(JL)

        ZTARG=PT(JL,KK)
        ZOEALFA=0.5_JPRB*(TANH(RLPAL1*(ZTARG-RLPTRC))+1.0_JPRB)
        ZFOEEWL=R2ES*EXP(R3LES*(ZTARG-RTT)/(ZTARG-R4LES))
        ZFOEEWI=R2ES*EXP(R3IES*(ZTARG-RTT)/(ZTARG-R4IES))
        ZQSAT=ZQP    *(ZOEALFA*ZFOEEWL+(1.0_JPRB-ZOEALFA)*ZFOEEWI)
        Z1S=TANH(RLPAL2*(ZQSAT-ZQMAX))
        ZQSAT=0.5_JPRB*((1.0_JPRB-Z1S)*ZQSAT+(1.0_JPRB+Z1S)*ZQMAX)
        
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        
        Z2S=    ZOEALFA *R5ALVCP*(1.0_JPRB/(ZTARG-R4LES)**2)+&
         & (1.0_JPRB-ZOEALFA)*R5ALSCP*(1.0_JPRB/(ZTARG-R4IES)**2)  
        ZCOND=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*Z2S)
        
        PT(JL,KK)=PT(JL,KK)+(ZOEALFA*RALVDCP+(1.0_JPRB-ZOEALFA)*RALSDCP)*ZCOND
        
        PQ(JL,KK)=PQ(JL,KK)-ZCOND
        
        ZTARG=PT(JL,KK)
        ZOEALFA=0.5_JPRB*(TANH(RLPAL1*(ZTARG-RLPTRC))+1.0_JPRB)
        ZFOEEWL=R2ES*EXP(R3LES*(ZTARG-RTT)/(ZTARG-R4LES))
        ZFOEEWI=R2ES*EXP(R3IES*(ZTARG-RTT)/(ZTARG-R4IES))
        ZQSAT=ZQP    *(ZOEALFA*ZFOEEWL+(1.0_JPRB-ZOEALFA)*ZFOEEWI)
        Z1S=TANH(RLPAL2*(ZQSAT-ZQMAX))
        ZQSAT=0.5_JPRB*((1.0_JPRB-Z1S)*ZQSAT+(1.0_JPRB+Z1S)*ZQMAX)
        
        ZQSAT=MIN(ZQMAX,ZQSAT)
        ZCOR=1.0_JPRB/(1.0_JPRB-RETV  *ZQSAT)
        ZQSAT=ZQSAT*ZCOR
        
        Z2S=    ZOEALFA *R5ALVCP*(1.0_JPRB/(ZTARG-R4LES)**2)+&
         & (1.0_JPRB-ZOEALFA)*R5ALSCP*(1.0_JPRB/(ZTARG-R4IES)**2)  
        ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.0_JPRB+ZQSAT*ZCOR*Z2S)
        
        PT(JL,KK)=PT(JL,KK)+(ZOEALFA*RALVDCP+(1.0_JPRB-ZOEALFA)*RALSDCP)*ZCOND1

        PQ(JL,KK)=PQ(JL,KK)-ZCOND1
      ENDIF
    ENDDO

  ENDIF

!*********************************************
ENDIF
!*********************************************                 

END ASSOCIATE

!IF (LHOOK) CALL DR_HOOK('CUADJTQ',1,ZHOOK_HANDLE)

END SUBROUTINE CUADJTQ

END SUBROUTINE CUDLFSN
