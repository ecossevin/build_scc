SUBROUTINE CUCALLN_MF_OPENACC (PPLDARE, PPLRG, KSTEP, YDTHF, YDCST, YDERAD, YDML_PHY_SLIN, YDML_PHY_EC&
&, YGFL, YDCHEM, YDSPP_CONFIG, KIDIA, KFDIA, KLON, KSMAX, KLEV, PDX, KSPPN2D, LDMCAPEA, LDLAND&
&, LDSLPHY, PTSPHY, PVDIFTS, PTM1, PQM1, PUM1, PVM1, PLITOT, PVERVEL, PQHFL, PAHFS, PAPHM1, PAP&
&, PAPH, PGEO, PGEOH, PGAW, PCUCONVCA, PGP2DSPP, PTENT, PTENQ, PTENU, PTENV, PTENTA, PTENQA, PARPRC&
&, KTOPC, KBASEC, KTYPE, KCBOT, KCTOP, KBOTSC, LDCUM, LDSC, KCBOT_LIG, KCTOP_LIG, LDCUM_LIG, LDSHCV&
&, PLCRIT_AER, PLU, PLUDE, PLUDELI, PSNDE, PMFU, PMFD, PDIFCQ, PDIFCS, PFHPCL, PFHPCN, PFPLCL&
&, PFPLCN, PLRAIN, PRSUD, PSTRCU, PSTRCV, PFCQLF, PFCQIF, PMFUDE_RATE, PMFDDE_RATE, PCAPE, PWMEAN&
&, PVDISCU, PDISS, KTRAC, PCM1, PTENC, PSCAV, PSCAV0, YDSTACK)
!$acc routine (CUCALLN_MF_OPENACC) seq
USE MODEL_PHYSICS_ECMWF_MOD,ONLY:MODEL_PHYSICS_ECMWF_TYPE
USE MODEL_PHYSICS_SIMPLINEAR_MOD,ONLY:MODEL_PHYSICS_SIMPLINEAR_TYPE
USE YOERAD,ONLY:TERAD
USE YOM_YGFL,ONLY:TYPE_GFLD
USE YOMCHEM,ONLY:TCHEM
USE SPP_MOD,ONLY:TSPP_CONFIG
USE PARKIND1,ONLY:JPIM, JPRB

USE YOMCST,ONLY:TCST
USE YOETHF,ONLY:TTHF
USE STACK_MOD
#include "stack.h"

IMPLICIT NONE

REAL (KIND=JPRB), INTENT (IN)::PPLDARE
REAL (KIND=JPRB), INTENT (IN)::PPLRG
INTEGER (KIND=JPIM), INTENT (IN)::KSTEP
TYPE (TTHF), INTENT (IN)::YDTHF
TYPE (TCST), INTENT (IN)::YDCST
TYPE (TERAD), INTENT (IN)::YDERAD
TYPE (MODEL_PHYSICS_SIMPLINEAR_TYPE), INTENT (IN)::YDML_PHY_SLIN
TYPE (MODEL_PHYSICS_ECMWF_TYPE), INTENT (IN)::YDML_PHY_EC
TYPE (TYPE_GFLD), INTENT (IN)::YGFL
TYPE (TCHEM), INTENT (IN)::YDCHEM
TYPE (TSPP_CONFIG), INTENT (IN)::YDSPP_CONFIG
INTEGER (KIND=JPIM), INTENT (IN)::KLON
INTEGER (KIND=JPIM), INTENT (IN)::KSMAX
INTEGER (KIND=JPIM), INTENT (IN)::KLEV
INTEGER (KIND=JPIM), INTENT (IN)::KSPPN2D
INTEGER (KIND=JPIM), INTENT (IN)::KTRAC
INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
LOGICAL, INTENT (IN)::LDMCAPEA
LOGICAL, INTENT (IN)::LDLAND (KLON)
LOGICAL, INTENT (IN)::LDSLPHY
REAL (KIND=JPRB), INTENT (IN)::PTSPHY
REAL (KIND=JPRB), INTENT (IN)::PVDIFTS
REAL (KIND=JPRB), INTENT (IN)::PLCRIT_AER (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PTM1 (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PQM1 (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PUM1 (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PVM1 (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PCM1 (KLON, KLEV, KTRAC)
REAL (KIND=JPRB), INTENT (IN)::PLITOT (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PVERVEL (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PQHFL (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (IN)::PAHFS (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (IN)::PAPHM1 (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (IN)::PAP (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PAPH (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (IN)::PGEO (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PGEOH (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (INOUT)::PTENT (KLON, KLEV)
REAL (KIND=JPRB), INTENT (INOUT)::PTENQ (KLON, KLEV)
REAL (KIND=JPRB), INTENT (INOUT)::PTENU (KLON, KLEV)
REAL (KIND=JPRB), INTENT (INOUT)::PTENV (KLON, KLEV)
REAL (KIND=JPRB), INTENT (INOUT)::PTENTA (KLON, KLEV)
REAL (KIND=JPRB), INTENT (INOUT)::PTENQA (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PGAW (KLON)
REAL (KIND=JPRB), INTENT (IN)::PCUCONVCA (KLON)
REAL (KIND=JPRB), INTENT (IN)::PGP2DSPP (KLON, KSPPN2D)
REAL (KIND=JPRB), INTENT (IN)::PSCAV (KTRAC)
REAL (KIND=JPRB), INTENT (IN)::PSCAV0 (KTRAC)
TYPE(STACK) :: YDSTACK
TYPE(STACK) :: YLSTACK
REAL (KIND=JPRB), INTENT (IN)::PDX (KLON)
REAL (KIND=JPRB), INTENT (INOUT)::PTENC (KLON, KLEV, KTRAC)
REAL (KIND=JPRB), INTENT (OUT)::PARPRC (KLON)
INTEGER (KIND=JPIM), INTENT (OUT)::KTOPC (KLON)
INTEGER (KIND=JPIM), INTENT (OUT)::KBASEC (KLON)
INTEGER (KIND=JPIM), INTENT (OUT)::KTYPE (KLON)
INTEGER (KIND=JPIM), INTENT (OUT)::KCBOT (KLON)
INTEGER (KIND=JPIM), INTENT (OUT)::KCTOP (KLON)
INTEGER (KIND=JPIM), INTENT (OUT)::KCBOT_LIG (KLON)
INTEGER (KIND=JPIM), INTENT (OUT)::KCTOP_LIG (KLON)
INTEGER (KIND=JPIM), INTENT (OUT)::KBOTSC (KLON)
LOGICAL, INTENT (OUT)::LDCUM (KLON)
LOGICAL, INTENT (OUT)::LDCUM_LIG (KLON)
LOGICAL, INTENT (OUT)::LDSC (KLON)
LOGICAL, INTENT (IN)::LDSHCV (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PLU (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PLUDE (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PLUDELI (KLON, KLEV, 4)
REAL (KIND=JPRB), INTENT (OUT)::PSNDE (KLON, KLEV, 2)
REAL (KIND=JPRB), INTENT (OUT)::PMFU (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PMFD (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PDIFCQ (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PDIFCS (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PFHPCL (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PFHPCN (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PFPLCL (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PFPLCN (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PLRAIN (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PRSUD (KLON, KLEV, 2)
REAL (KIND=JPRB), INTENT (OUT)::PSTRCU (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PSTRCV (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PFCQLF (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PFCQIF (KLON, KLEV+1)
REAL (KIND=JPRB), INTENT (OUT)::PMFUDE_RATE (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PMFDDE_RATE (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PCAPE (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PWMEAN (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PVDISCU (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PDISS (KLON, KLEV)

temp (REAL (KIND=JPRB), ZRAIN, (KLON))
temp (REAL (KIND=JPRB), ZQSAT, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZQU, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZTU, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZVP1, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZUP1, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZQP1, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZTP1, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZCP1, (KLON, KLEV, KTRAC))

temp (REAL (KIND=JPRB), ZENTHS, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZENTHD, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZUEA, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZVEA, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZTEA, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZQEA, (KLON, KLEV))

REAL (KIND=JPRB)::ZCONDFLN 
REAL (KIND=JPRB)::ZCONDFLL 

INTEGER (KIND=JPIM)::JN
INTEGER (KIND=JPIM)::JLON
INTEGER (KIND=JPIM)::JLEV
INTEGER (KIND=JPIM)::IFLAG

REAL (KIND=JPRB)::ZEPS
REAL (KIND=JPRB)::ZGDPH
REAL (KIND=JPRB)::ZCP
LOGICAL::LLTDKMF

#include "cuccdia_openacc.intfb.h"
#include "cumastrn_openacc.intfb.h"
#include "satur_openacc.intfb.h"
#include "fcttre.func.h"
#include "fcttrm.func.h"

YLSTACK = YDSTACK

alloc (ZRAIN)
alloc (ZQSAT)
alloc (ZQU)
alloc (ZTU)
alloc (ZVP1)
alloc (ZUP1)
alloc (ZQP1)
alloc (ZTP1)
alloc (ZCP1)
alloc (ZENTHS)
alloc (ZENTHD)
alloc (ZUEA)
alloc (ZVEA)
alloc (ZTEA)
alloc (ZQEA)



JLON = KIDIA



DO JLEV=1, KLEV
  
  ZQEA (JLON, JLEV)=PTENQ (JLON, JLEV)
  ZTEA (JLON, JLEV)=PTENT (JLON, JLEV)
  ZVEA (JLON, JLEV)=PTENV (JLON, JLEV)
  ZUEA (JLON, JLEV)=PTENU (JLON, JLEV)
  ZENTHD (JLON, JLEV)=0.0_JPRB
  ZENTHS (JLON, JLEV)=0.0_JPRB
  
ENDDO


DO JLEV=1, KLEV+1
  
  PFPLCL (JLON, JLEV)=0.0_JPRB
  PFPLCN (JLON, JLEV)=0.0_JPRB
  
ENDDO

ZEPS=1.0E-10_JPRB

DO JLEV=1, KLEV
  
  ZUP1 (JLON, JLEV)=PUM1 (JLON, JLEV)+PTENU (JLON, JLEV)*PTSPHY
  ZVP1 (JLON, JLEV)=PVM1 (JLON, JLEV)+PTENV (JLON, JLEV)*PTSPHY
  ZTP1 (JLON, JLEV)=PTM1 (JLON, JLEV)+PTENT (JLON, JLEV)*PTSPHY
  ZQP1 (JLON, JLEV)=PQM1 (JLON, JLEV)+PTENQ (JLON, JLEV)*PTSPHY
  ZQSAT (JLON, JLEV)=ZQP1 (JLON, JLEV)

  IF (ZQSAT (JLON, JLEV)<=0.0_JPRB)  THEN
      ZQSAT (JLON, JLEV)=ZEPS
  ENDIF

  
ENDDO


IF (LDSLPHY) THEN

  IF (KTRAC>0.AND.YDML_PHY_EC%YREPHY%LMFTRAC) THEN

    DO JN=1, KTRAC

      DO JLEV=1, KLEV
        
        ZCP1 (JLON, JLEV, JN)=PCM1 (JLON, JLEV, JN)
      
      ENDDO

    ENDDO

  ENDIF

ELSE

  IF (KTRAC>0.AND.YDML_PHY_EC%YREPHY%LMFTRAC) THEN

    DO JN=1, KTRAC

      DO JLEV=1, KLEV
        
        ZCP1 (JLON, JLEV, JN)=PCM1 (JLON, JLEV, JN)+PTENC (JLON, JLEV, JN)*PTSPHY
      
      ENDDO

    ENDDO

  ENDIF

ENDIF

IFLAG=1
CALL SATUR_OPENACC (YDTHF, YDCST, KIDIA, KFDIA, KLON, YDML_PHY_EC%YRECUMF%NJKT2, KLEV&
&, YDML_PHY_SLIN%YREPHLI%LPHYLIN, PAP, ZTP1, ZQSAT, IFLAG, YDSTACK=YLSTACK)

ZRAIN (JLON)=0.0_JPRB

LLTDKMF=.TRUE.
CALL CUMASTRN_OPENACC (PPLDARE, PPLRG, YDTHF, YDCST, YDML_PHY_SLIN, YDML_PHY_EC, YGFL, YDCHEM&
&, YDSPP_CONFIG, KIDIA, KFDIA, KLON, KLEV, PDX, LLTDKMF, LDMCAPEA, LDLAND, PTSPHY, ZTP1, ZQP1&
&, ZUP1, ZVP1, PLITOT, PVERVEL, ZQSAT, PQHFL, PAHFS, PAP, PAPH, PGEO, PGEOH, PGAW, PCUCONVCA&
&, PGP2DSPP, PTENT, PTENQ, PTENU, PTENV, PTENTA, PTENQA, LDCUM, KTYPE, KCBOT, KCTOP, LDCUM_LIG&
&, KCBOT_LIG, KCTOP_LIG, KBOTSC, LDSC, LDSHCV, PLCRIT_AER, ZTU, ZQU, PLU, PLUDE, PLUDELI, PSNDE&
&, ZENTHD, PFPLCL, PFPLCN, ZRAIN, PLRAIN, PRSUD, PMFU, PMFD, PMFUDE_RATE, PMFDDE_RATE, PCAPE&
&, PWMEAN, PVDISCU, PDISS, KTRAC, ZCP1, PTENC, PSCAV, PSCAV0, YDSTACK=YLSTACK)
CALL CUCCDIA_OPENACC (YDERAD, YDML_PHY_SLIN%YREPHLI, YDML_PHY_EC%YREPHY, KIDIA, KFDIA, KLON, KLEV&
&, KSTEP, KCBOT, KCTOP, LDCUM, ZQU, PLU, PMFU, ZRAIN, PARPRC, KTOPC, KBASEC, YDSTACK=YLSTACK)

PDIFCQ (JLON, 1)=0.0_JPRB
PDIFCS (JLON, 1)=0.0_JPRB
PSTRCU (JLON, 1)=0.0_JPRB
PSTRCV (JLON, 1)=0.0_JPRB
PFCQLF (JLON, 1)=0.0_JPRB
PFCQIF (JLON, 1)=0.0_JPRB
PFHPCL (JLON, 1)=0.0_JPRB
PFHPCN (JLON, 1)=0.0_JPRB
PDIFCS (JLON, 1)=0.0_JPRB
PDIFCQ (JLON, 1)=0.0_JPRB
ZCONDFLL =0.0_JPRB
ZCONDFLN =0.0_JPRB


DO JLEV=1, KLEV
  
  ZGDPH=-YDCST%RG/(PAPHM1 (JLON, JLEV+1)-PAPHM1 (JLON, JLEV))
  ZCP=YDCST%RCPD*(1+YDTHF%RVTMP2*ZQP1 (JLON, JLEV))
  PDIFCS (JLON, JLEV+1)=(PTENT (JLON, JLEV)-ZTEA (JLON, JLEV))/ZGDPH*ZCP+PDIFCS (JLON, JLEV)
  PSTRCU (JLON, JLEV+1)=(PTENU (JLON, JLEV)-ZUEA (JLON, JLEV))/ZGDPH+PSTRCU (JLON, JLEV)
  PSTRCV (JLON, JLEV+1)=(PTENV (JLON, JLEV)-ZVEA (JLON, JLEV))/ZGDPH+PSTRCV (JLON, JLEV)
  PDIFCQ (JLON, JLEV+1)=(PTENQ (JLON, JLEV)-ZQEA (JLON, JLEV))/ZGDPH+PDIFCQ (JLON, JLEV)
  
ENDDO


DO JLEV=1, KLEV
  
  PFHPCL (JLON, JLEV+1)=-PFPLCL (JLON, JLEV+1)*YDCST%RLVTT
  PFHPCN (JLON, JLEV+1)=-PFPLCN (JLON, JLEV+1)*YDCST%RLSTT
  ZCONDFLL =ZCONDFLL +PLUDELI (JLON, JLEV, 1)
  ZCONDFLN =ZCONDFLN +PLUDELI (JLON, JLEV, 2)
  PDIFCS (JLON, JLEV+1)=PDIFCS (JLON, JLEV+1)-PFHPCL (JLON, JLEV+1)-PFHPCN&
  & (JLON, JLEV+1)+YDCST%RLVTT*ZCONDFLL +YDCST%RLSTT*ZCONDFLN 
  PDIFCQ (JLON, JLEV+1)=PDIFCQ (JLON, JLEV+1)-PFPLCL (JLON&
  &, JLEV+1)-PFPLCN (JLON, JLEV+1)-ZCONDFLL -ZCONDFLN 
  PFCQLF (JLON, JLEV+1)=ZCONDFLL 
  PFCQIF (JLON, JLEV+1)=ZCONDFLN 
  
ENDDO



ENDSUBROUTINE CUCALLN_MF_OPENACC

! 56ad6923076b622f9a4a36289517d0f4b37156a2
