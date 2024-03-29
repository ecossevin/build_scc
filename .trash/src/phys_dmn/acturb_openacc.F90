SUBROUTINE ACTURB_OPENACC (YDCST, YDPHY, YDPHY0, KIDIA, KFDIA, KLON, KTDIAT, KTDIAN&
&, KLEV, PAPHI, PAPHIF, PAPRS, PAPRSF, PR, PT, PU, PV, PECT, PQV, LDCONV, PLSCPE&
&, PLMECT, PPHI3, PCD, PCH, PGZ0, PTS, PQS, PQICE, PQLI, PKTROV, PKQROV, PKQLROV&
&, PKUROV, PNBVNO, PPRODTH, PNEBS, PQCS, PL3F2, PGKCLS, PECTCLS, YDSTACK)
!$acc routine (ACTURB_OPENACC) seq
USE PARKIND1,ONLY:JPIM, JPRB, JPRD

USE YOMCST,ONLY:TCST
USE YOMPHY0,ONLY:TPHY0
USE YOMPHY,ONLY:TPHY
USE YOMLSFORC,ONLY:LMUSCLFA, NMUSCLFA
USE STACK_MOD
#include "stack.h"

IMPLICIT NONE

TYPE (TCST), INTENT (IN)::YDCST
TYPE (TPHY), INTENT (IN)::YDPHY
TYPE (TPHY0), INTENT (IN)::YDPHY0
INTEGER (KIND=JPIM), INTENT (IN)::KLON
INTEGER (KIND=JPIM), INTENT (IN)::KLEV
INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
INTEGER (KIND=JPIM), INTENT (IN)::KTDIAT
INTEGER (KIND=JPIM), INTENT (IN)::KTDIAN
REAL (KIND=JPRB), INTENT (IN)::PAPHI (KLON, 0:KLEV)
REAL (KIND=JPRB), INTENT (IN)::PAPHIF (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PAPRS (KLON, 0:KLEV)
REAL (KIND=JPRB), INTENT (IN)::PAPRSF (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PR (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PT (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PU (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PV (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PECT (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PQV (KLON, KLEV)
LOGICAL, INTENT (IN)::LDCONV (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PLSCPE (KLON, KLEV)
REAL (KIND=JPRB), INTENT (INOUT)::PLMECT (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PPHI3 (KLON, KLEV)
REAL (KIND=JPRB), INTENT (IN)::PCD (KLON)
REAL (KIND=JPRB), INTENT (IN)::PCH (KLON)
REAL (KIND=JPRB), INTENT (IN)::PGZ0 (KLON)
REAL (KIND=JPRB), INTENT (IN)::PTS (KLON)
REAL (KIND=JPRB), INTENT (IN)::PQS (KLON)
REAL (KIND=JPRB), INTENT (INOUT)::PQICE (KLON, KLEV)
REAL (KIND=JPRB), INTENT (INOUT)::PQLI (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PKTROV (KLON, 0:KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PKQROV (KLON, 0:KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PKQLROV (KLON, 0:KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PKUROV (KLON, 0:KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PNBVNO (KLON, 0:KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PPRODTH (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PNEBS (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PQCS (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PL3F2 (KLON, KLEV)
REAL (KIND=JPRB), INTENT (OUT)::PGKCLS (KLON)
REAL (KIND=JPRB), INTENT (OUT)::PECTCLS (KLON)
TYPE(STACK) :: YDSTACK
TYPE(STACK) :: YLSTACK

REAL (KIND=JPRB)::ZBLH 
REAL (KIND=JPRB)::ZRS 
REAL (KIND=JPRB)::ZSTAB 
temp (REAL (KIND=JPRB), ZRTV, (KLON, KLEV))

temp (REAL (KIND=JPRB), ZZ, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZGDZF, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZTHETA, (KLON, KLEV))

temp (REAL (KIND=JPRB), ZLOCPEXF, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZTHETALF, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZTHETAVL, (KLON, KLEV))

temp (REAL (KIND=JPRB), ZECTF, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZLMECTF, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZGKTF, (KLON, KLEV))

temp (REAL (KIND=JPRB), ZGKUH, (KLON, 0:KLEV))
temp (REAL (KIND=JPRB), ZGKTH, (KLON, 0:KLEV))
temp (REAL (KIND=JPRB), ZLM, (KLON, 0:KLEV))

REAL (KIND=JPRB)::ZWSTAR 
REAL (KIND=JPRB)::ZUSTAR 
REAL (KIND=JPRB)::ZECTINT 
REAL (KIND=JPRB)::ZQCINT 
temp (INTEGER (KIND=JPIM), ICM, (KLON, KLEV))
INTEGER (KIND=JPIM)::ILEVBI 

REAL (KIND=JPRB)::ZQCBLK
REAL (KIND=JPRB)::ZLINV
REAL (KIND=JPRB)::ZNUM
REAL (KIND=JPRB)::ZECTBLK
REAL (KIND=JPRB)::ZDEN
REAL (KIND=JPRB)::ZBI
REAL (KIND=JPRB)::ZGKENT

INTEGER (KIND=JPIM)::ILEVT
INTEGER (KIND=JPIM)::ILEVM1
INTEGER (KIND=JPIM)::ICLA

INTEGER (KIND=JPIM)::JLON
INTEGER (KIND=JPIM)::JLEV
INTEGER (KIND=JPIM)::INQ1
INTEGER (KIND=JPIM)::INIV
INTEGER (KIND=JPIM)::IJLEVP1
INTEGER (KIND=JPIM)::IJLEVM1
INTEGER (KIND=JPIM)::IHCLPMIN
INTEGER (KIND=JPIM)::IHCLPMAX
LOGICAL::LLIMQ1

REAL (KIND=JPRB)::ZPRETURB
REAL (KIND=JPRB)::ZSIGCR
REAL (KIND=JPRB)::ZHBOT
REAL (KIND=JPRB)::ZHTOP
REAL (KIND=JPRB)::ZEPSIG
REAL (KIND=JPRB)::ZILIMQ1
REAL (KIND=JPRB)::ZL3F2
REAL (KIND=JPRB)::ZZT
REAL (KIND=JPRB)::ZZRT
REAL (KIND=JPRB)::ZZQC
REAL (KIND=JPRB)::ZZN1D
REAL (KIND=JPRB)::ZZLMF
REAL (KIND=JPRB)::ZZKTH
REAL (KIND=JPRB)::ZZF1
REAL (KIND=JPRB)::ZZF0
REAL (KIND=JPRB)::ZZETF
REAL (KIND=JPRB)::ZWTL
REAL (KIND=JPRB)::ZWQW
REAL (KIND=JPRB)::ZWDIFF
REAL (KIND=JPRB)::ZUSTAR2
REAL (KIND=JPRB)::ZU
REAL (KIND=JPRB)::ZTLF2
REAL (KIND=JPRB)::ZTLF1
REAL (KIND=JPRB)::ZTLF
REAL (KIND=JPRB)::ZTHETAOT
REAL (KIND=JPRB)::ZTH
REAL (KIND=JPRB)::ZTF2
REAL (KIND=JPRB)::ZTF1
REAL (KIND=JPRB)::ZTF
REAL (KIND=JPRB)::ZTETA
REAL (KIND=JPRB)::ZSURSAT
REAL (KIND=JPRB)::ZSTA
REAL (KIND=JPRB)::ZSRC
REAL (KIND=JPRB)::ZSIGMAS2
REAL (KIND=JPRB)::ZSIGMAS
REAL (KIND=JPRB)::ZRTI
REAL (KIND=JPRB)::ZRQZERO
REAL (KIND=JPRB)::ZROSDPHI
REAL (KIND=JPRB)::ZRLTLU
REAL (KIND=JPRB)::ZRIH
REAL (KIND=JPRB)::ZRICHF
REAL (KIND=JPRB)::ZRIC
REAL (KIND=JPRB)::ZRHOH
REAL (KIND=JPRB)::ZRH
REAL (KIND=JPRB)::ZRESUL
REAL (KIND=JPRB)::ZQWH
REAL (KIND=JPRB)::ZQWF2
REAL (KIND=JPRB)::ZQWF1
REAL (KIND=JPRB)::ZQWF
REAL (KIND=JPRB)::ZQSLTLF2
REAL (KIND=JPRB)::ZQSLTLF1
REAL (KIND=JPRB)::ZQSLTLF
REAL (KIND=JPRB)::ZQSATF2
REAL (KIND=JPRB)::ZQSATF1
REAL (KIND=JPRB)::ZQSATF
REAL (KIND=JPRB)::ZQLP1
REAL (KIND=JPRB)::ZQLM1
REAL (KIND=JPRB)::ZQLH
REAL (KIND=JPRB)::ZQCF2
REAL (KIND=JPRB)::ZQCF1
REAL (KIND=JPRB)::ZQLF
REAL (KIND=JPRB)::ZQC
REAL (KIND=JPRB)::ZQ1MIN
REAL (KIND=JPRB)::ZQ1MAX
REAL (KIND=JPRB)::ZQ11
REAL (KIND=JPRB)::ZPRODH
REAL (KIND=JPRB)::ZPRODC
REAL (KIND=JPRB)::ZPREF
REAL (KIND=JPRB)::ZPHMIN
REAL (KIND=JPRB)::ZPHMAX
REAL (KIND=JPRB)::ZPHI3MIN
REAL (KIND=JPRB)::ZNEBLOW
REAL (KIND=JPRB)::ZMODU
REAL (KIND=JPRB)::ZMAXQ1
REAL (KIND=JPRB)::ZLSCPEH
REAL (KIND=JPRB)::ZLSCPEF2
REAL (KIND=JPRB)::ZLSCPEF1
REAL (KIND=JPRB)::ZLSCPEF
REAL (KIND=JPRB)::ZLOS
REAL (KIND=JPRB)::ZLOI
REAL (KIND=JPRB)::ZLMECT
REAL (KIND=JPRB)::ZLCPP1
REAL (KIND=JPRB)::ZLCPM1
REAL (KIND=JPRB)::ZIS
REAL (KIND=JPRB)::ZINC
REAL (KIND=JPRB)::ZIGMAS2
REAL (KIND=JPRB)::ZIGMAS
REAL (KIND=JPRB)::ZH2
REAL (KIND=JPRB)::ZH1
REAL (KIND=JPRB)::ZH
REAL (KIND=JPRB)::ZGZ
REAL (KIND=JPRB)::ZGLUZ
REAL (KIND=JPRB)::ZGLU
REAL (KIND=JPRB)::ZGLTZ
REAL (KIND=JPRB)::ZGLT
REAL (KIND=JPRB)::ZGLMU2
REAL (KIND=JPRB)::ZGLMT2
REAL (KIND=JPRB)::ZGALP2
REAL (KIND=JPRB)::ZFACT
REAL (KIND=JPRB)::ZEW2
REAL (KIND=JPRB)::ZEW1
REAL (KIND=JPRB)::ZEW
REAL (KIND=JPRB)::ZEPSV
REAL (KIND=JPRB)::ZEPSQ1
REAL (KIND=JPRB)::ZEPSQ
REAL (KIND=JPRB)::ZEPS1
REAL (KIND=JPRB)::ZEPS
REAL (KIND=JPRB)::ZECTBLH
REAL (KIND=JPRB)::ZEPNEBS
REAL (KIND=JPRB)::ZEPDELT
REAL (KIND=JPRB)::ZECTH
REAL (KIND=JPRB)::ZDU2
REAL (KIND=JPRB)::ZDTL
REAL (KIND=JPRB)::ZDTETA
REAL (KIND=JPRB)::ZDT
REAL (KIND=JPRB)::ZDSTA
REAL (KIND=JPRB)::ZDS
REAL (KIND=JPRB)::ZDQW
REAL (KIND=JPRB)::ZDQLST
REAL (KIND=JPRB)::ZDPHI0
REAL (KIND=JPRB)::ZDPHI
REAL (KIND=JPRB)::ZDLEWF2
REAL (KIND=JPRB)::ZDLEWF1
REAL (KIND=JPRB)::ZDLEWF
REAL (KIND=JPRB)::ZDIFFH
REAL (KIND=JPRB)::ZDIFFC
REAL (KIND=JPRB)::ZDI
REAL (KIND=JPRB)::ZDELTQH
REAL (KIND=JPRB)::ZDELTQF2
REAL (KIND=JPRB)::ZDELTQF1
REAL (KIND=JPRB)::ZDELTQF
REAL (KIND=JPRB)::ZDD
REAL (KIND=JPRB)::ZCTO
REAL (KIND=JPRB)::ZCK
REAL (KIND=JPRB)::ZCIS
REAL (KIND=JPRB)::ZCE1
REAL (KIND=JPRB)::ZAA
REAL (KIND=JPRB)::ZA
REAL (KIND=JPRB)::Z3BCF
REAL (KIND=JPRB)::Z3B
REAL (KIND=JPRB)::Z2B

REAL (KIND=JPRB)::ZQV
REAL (KIND=JPRB)::ZGAUSS
REAL (KIND=JPRB)::ZDELTA
REAL (KIND=JPRB)::ZPLS

temp (REAL (KIND=JPRB), ZAH, (KLON, KLEV))
temp (REAL (KIND=JPRB), ZQSLTLH, (KLON, KLEV))

REAL (KIND=JPRB)::ZEPS2
REAL (KIND=JPRB)::ZEPS3
REAL (KIND=JPRB)::ZDQLI
REAL (KIND=JPRB)::ZDIFTQL
REAL (KIND=JPRB)::ZDTETL
REAL (KIND=JPRB)::ZZRTV
REAL (KIND=JPRB)::ZZDPHI

REAL (KIND=JPRB)::ZGKTAH
REAL (KIND=JPRB)::ZGKQLH
REAL (KIND=JPRB)::ZGKQH
REAL (KIND=JPRB)::ZDIFTQ
REAL (KIND=JPRB)::ZDQI
REAL (KIND=JPRB)::ZDTETI
REAL (KIND=JPRB)::ZDIFTTET

REAL (KIND=JPRB)::ZKROVN
REAL (KIND=JPRB)::ZBICX
REAL (KIND=JPRB)::ZBIC

REAL (KIND=JPRB)::ZDLEWH
REAL (KIND=JPRB)::ZQSATH
REAL (KIND=JPRB)::ZEWH
REAL (KIND=JPRB)::ZHH
REAL (KIND=JPRB)::ZTLH

#include "fcttrm.func.h"
#include "wrscmr.intfb.h"

YLSTACK = YDSTACK

alloc (ZRTV)
alloc (ZZ)
alloc (ZGDZF)
alloc (ZTHETA)
alloc (ZLOCPEXF)
alloc (ZTHETALF)
alloc (ZTHETAVL)
alloc (ZECTF)
alloc (ZLMECTF)
alloc (ZGKTF)
alloc (ZGKUH)
alloc (ZGKTH)
alloc (ZLM)
alloc (ICM)
alloc (ZAH)
alloc (ZQSLTLH)



JLON = KIDIA


ZCE1=YDPHY0%UDECT
ZPHMAX=YDPHY0%UPRETMIN
ZPHMIN=YDPHY0%UPRETMAX
IHCLPMAX=KTDIAN
IHCLPMIN=KLEV-2
ZEPS=YDPHY0%ECTMIN
ZEPS1=YDPHY0%USHEARM
ZEPS2=1.E+04_JPRB
ZEPS3=1.E-12_JPRB
PKTROV (JLON,:)=0.0_JPRB
PKUROV (JLON,:)=0.0_JPRB
PPRODTH (JLON,:)=0.0_JPRB

ZRS =YDCST%RD+(YDCST%RV-YDCST%RD)*PQS (JLON)
ZDPHI0=PAPHIF (JLON, KLEV)-PAPHI (JLON, KLEV)
ZRTI=2.0_JPRB/(PR (JLON, KLEV)*PT (JLON, KLEV)+YDCST%RKAPPA*ZDPHI0+ZRS *PTS (JLON))
ZSTA=ZDPHI0*(PR (JLON, KLEV)*PT (JLON, KLEV)+YDCST%RKAPPA*ZDPHI0-ZRS *PTS (JLON))*ZRTI
ZSTAB =MAX (0.0_JPRB, SIGN (1.0_JPRB, ZSTA))

ZEPSQ=1.E-10_JPRB

IF (JPRB==JPRD) THEN
  ZEPNEBS=1.E-12_JPRB
ELSE
  ZEPNEBS=1.E-06_JPRB
ENDIF

ZEPDELT=1.E-12_JPRB
ZEPSV=1.E-10_JPRB
ZEPSIG=1.E-10_JPRB
ZMAXQ1=20._JPRB
ZEPSQ1=1.E-6_JPRB
LLIMQ1=.TRUE.

IF (LLIMQ1) THEN
  ZILIMQ1=1.0_JPRB
ELSE
  ZILIMQ1=0.0_JPRB
ENDIF

ZSIGCR=YDPHY0%GCVTURB
ZPRETURB=65000._JPRB
ZGAUSS=1.0_JPRB/(2.0_JPRB*YDCST%RDT**2)

DO JLEV=KTDIAN, KLEV
  
  ZZ (JLON, JLEV)=PAPHIF (JLON, JLEV)-PAPHI (JLON, KLEV)
  ZRTV (JLON, JLEV)=PR (JLON, JLEV)*PT (JLON, JLEV)
  
ENDDO


DO JLEV=KTDIAN+1, KLEV
  
  ZGDZF (JLON, JLEV)=PAPHIF (JLON, JLEV-1)-PAPHIF (JLON, JLEV)
  
ENDDO

Z2B=2.0_JPRB*YDPHY0%EDB
Z3B=3._JPRB*YDPHY0%EDB
Z3BCF=YDPHY0%EDB*YDPHY0%EDC*YDPHY0%VKARMN**2/SQRT (3._JPRB)
ZRLTLU=SQRT (1.5_JPRB*YDPHY0%EDD)
ZGLU=YDCST%RG*YDPHY0%ALMAV
ZGLT=ZGLU*ZRLTLU

DO JLEV=KTDIAT, KTDIAN-1
  ZGLUZ=ZGLU
  ZGLMU2=ZGLUZ**2
  ZGLTZ=ZGLT
  ZGLMT2=ZGLTZ**2
  
  ZGZ=PAPHI (JLON, JLEV)-PAPHI (JLON, KLEV)+PGZ0 (JLON)
  ZCK=Z3BCF*(ZGLTZ/(YDPHY0%VKARMN*ZGZ))**2
  ZDPHI0=PAPHIF (JLON, JLEV)-PAPHIF (JLON, JLEV+1)
  ZCIS=MAX (ZEPS1,(PU (JLON, JLEV)-PU (JLON, JLEV+1))**2+(PV (JLON, JLEV)-PV (JLON, JLEV+1))**2)
  ZU=SQRT (ZCIS)
  ZDTETA=PR (JLON, JLEV)*PT (JLON, JLEV)-PR (JLON, JLEV+1)*PT (JLON, JLEV+1)+YDCST%RKAPPA*ZDPHI0
  ZRTI=2.0_JPRB/(PR (JLON, JLEV)*PT (JLON, JLEV)+PR (JLON, JLEV+1)*PT (JLON, JLEV+1))
  ZSTA=ZDPHI0*ZDTETA*ZRTI
  ZSTA=ZSTA/(1.0_JPRB+MAX (0.0_JPRB, ZSTA)*YDPHY0%USURIC/ZCIS)
  ZIS=MAX (0.0_JPRB, SIGN (1.0_JPRB, ZSTA))
  ZDS=SQRT (ZCIS+YDPHY0%EDD*ABS (ZSTA))
  ZDI=1.0_JPRB/(ZU+ZCK*SQRT (ABS (ZSTA)))
  ZLOS=ZCIS*ZDS/(ZU*ZDS+Z2B*ABS (ZSTA))
  ZLOI=ZU-Z2B*ZSTA*ZDI
  PKUROV (JLON, JLEV)=(ZLOI+ZIS*(ZLOS-ZLOI))*ZGLMU2*PAPRS (JLON, JLEV)*ZRTI/ZDPHI0**2
  ZLOS=ZCIS**2/(ZU*ZCIS+Z3B*ABS (ZSTA)*ZDS)
  ZLOI=ZU-Z3B*ZSTA*ZDI
  PKTROV (JLON, JLEV)=(ZLOI+ZIS*(ZLOS-ZLOI))*ZGLMT2*PAPRS (JLON, JLEV)*ZRTI/ZDPHI0**2
  PKQROV (JLON, JLEV)=PKTROV (JLON, JLEV)
  PKQLROV (JLON, JLEV)=0.0_JPRB
  PNBVNO (JLON, JLEV)=ZSTA/(PAPRS (JLON, JLEV)*ZRTI*ZDPHI0)**2
  
ENDDO

ZBLH =0._JPRB

IF (YDPHY%LPBLE.OR.(.NOT.YDPHY%LECTREP)) THEN
  
  ILEVBI =0
  
  ICM (JLON,:)=0
  ZECTBLH=0.01_JPRB

  DO JLEV=KTDIAN, KLEV
    
    ICM (JLON, JLEV)=INT (MAX (0.0_JPRB, SIGN (1.0_JPRB, PECT (JLON, JLEV)-ZECTBLH)))
  
  ENDDO


  DO JLEV=KLEV, KTDIAN,-1
    
    ILEVM1=MAX (KTDIAN, JLEV-1)

    IF ((ICM (JLON, JLEV)==1).AND.(ICM (JLON, ILEVM1)==0)) THEN
      ILEVBI =MAX (JLEV, ILEVBI )
    ENDIF

  
  ENDDO

  
  ILEVBI =ILEVBI *MAX (ICM (JLON, KLEV), ICM (JLON, KLEV-1))

  IF ((ICM (JLON, KLEV)==0).AND.(ILEVBI ==0))  THEN
      ILEVBI =KLEV
  ENDIF


  

  

  IF ((ILEVBI >1).AND.(ILEVBI <KLEV)) THEN
    ZHBOT=(PAPHI (JLON, ILEVBI )-PAPHI (JLON, KLEV))/YDCST%RG
    ZHTOP=(PAPHI (JLON, ILEVBI -1)-PAPHI (JLON, KLEV))/YDCST%RG
    ZBLH =ZHBOT+(ZHTOP-ZHBOT)/(PECT (JLON, ILEVBI -1)-PECT&
    & (JLON, ILEVBI ))*(ZECTBLH-PECT (JLON, ILEVBI ))
  ELSEIF (ILEVBI ==KLEV) THEN
    ZBLH =(PAPHIF (JLON, KLEV)-PAPHI (JLON, KLEV))/YDCST%RG
  ELSEIF (ILEVBI ==0) THEN
    ZBLH =(PAPHI (JLON, KTDIAN)-PAPHI (JLON, KLEV))/YDCST%RG
  ELSE
    ZBLH =(PAPHI (JLON, ILEVBI )-PAPHI (JLON, KLEV))/YDCST%RG
  ENDIF

  
ENDIF


IF (.NOT.YDPHY%LECTREP) THEN
  
  ZMODU=SQRT (PU (JLON, KLEV)**2+PV (JLON, KLEV)**2)
  ZUSTAR2=PCD (JLON)*ZMODU*ZMODU
  ZUSTAR =SQRT (ZUSTAR2)
  ZTETA=ZRTV (JLON, KLEV)+YDCST%RKAPPA*ZZ (JLON, KLEV)-ZRS *PTS (JLON)
  ZRQZERO=MAX (ZEPS,-PCH (JLON)*ZTETA*ZMODU)
  ZWSTAR =(YDCST%RG*ZBLH *ZRQZERO/ZRTV (JLON, KLEV))**YDPHY0%UCWSTAR
  PECTCLS (JLON)=MAX (YDPHY0%ECTMIN, YDPHY0%AECLS3*ZUSTAR&
  & **2+YDPHY0%AECLS4*ZWSTAR **2*(1.0_JPRB-ZSTAB ))
  
ELSE
  
  PECTCLS (JLON)=PECT (JLON, KLEV-1)
  
ENDIF


DO JLEV=KTDIAN, KLEV
  
  ZPREF=PAPRSF (JLON, JLEV)
  ZTHETA (JLON, JLEV)=PT (JLON, JLEV)*(YDCST%RATM/ZPREF)**(YDCST%RKAPPA)
  
ENDDO


DO JLEV=KTDIAN, KLEV
  
  ZZT=PT (JLON, JLEV)
  ZQC=PQLI (JLON, JLEV)+PQICE (JLON, JLEV)
  ZTHETAOT=ZTHETA (JLON, JLEV)/ZZT
  ZLOCPEXF (JLON, JLEV)=PLSCPE (JLON, JLEV)*ZTHETAOT
  ZTHETALF (JLON, JLEV)=ZTHETA (JLON, JLEV)-ZQC*ZLOCPEXF (JLON, JLEV)
  
ENDDO


DO JLEV=KTDIAN, KLEV
  
  ZQV=PQV (JLON, JLEV)
  ZQC=PQLI (JLON, JLEV)+PQICE (JLON, JLEV)
  ZTHETAVL (JLON, JLEV)=ZTHETA (JLON, JLEV)*(1.0_JPRB+YDCST%RETV*ZQV-ZQC)
  
ENDDO

ZGKUH (JLON,:)=1.E-14_JPRB
ZGKTH (JLON,:)=1.E-14_JPRB

DO JLEV=KTDIAN, KLEV-1
  
  ZDPHI=ZGDZF (JLON, JLEV+1)
  ZZRT=0.5_JPRB*(ZRTV (JLON, JLEV)+ZRTV (JLON, JLEV+1))
  ZROSDPHI=PAPRS (JLON, JLEV)/(ZDPHI*ZZRT)
  ZGKUH (JLON, JLEV)=YDPHY0%AKN*PLMECT (JLON, JLEV)*SQRT (PECT (JLON, JLEV))
  ZGKTH (JLON, JLEV)=ZGKUH (JLON, JLEV)*PPHI3 (JLON, JLEV)*YDPHY0%ALPHAT
  PKTROV (JLON, JLEV)=ZGKTH (JLON, JLEV)*ZROSDPHI
  PKUROV (JLON, JLEV)=ZGKUH (JLON, JLEV)*ZROSDPHI
  PKQROV (JLON, JLEV)=PKTROV (JLON, JLEV)
  PKQLROV (JLON, JLEV)=0.0_JPRB
  
ENDDO



IF (YDPHY%LECTFL0) THEN
  PGKCLS (JLON)=0._JPRB
  ZGKTH (JLON, KLEV)=0._JPRB
  ZGKUH (JLON, KLEV)=0._JPRB
ELSE
  PGKCLS (JLON)=YDPHY0%AKN*PLMECT (JLON, KLEV)*SQRT (PECTCLS (JLON))
  ZGKTH (JLON, KLEV)=PGKCLS (JLON)*PPHI3 (JLON, KLEV-1)*YDPHY0%ALPHAT
  ZGKUH (JLON, KLEV)=PGKCLS (JLON)
ENDIF




DO JLEV=KTDIAN, KLEV-1
  
  ZDPHI=PAPHIF (JLON, JLEV)-PAPHIF (JLON, JLEV+1)
  ZCIS=MAX (ZEPS1,(PU (JLON, JLEV)-PU (JLON, JLEV+1))**2+(PV (JLON, JLEV)-PV (JLON, JLEV+1))**2)
  ZDTETA=ZRTV (JLON, JLEV)-ZRTV (JLON, JLEV+1)+YDCST%RKAPPA*ZDPHI
  ZZRT=2.0_JPRB/(ZRTV (JLON, JLEV)+ZRTV (JLON, JLEV+1))
  ZSTA=ZDPHI*ZDTETA*ZZRT
  ZSTA=ZSTA/(1.0_JPRB+MAX (0.0_JPRB, ZSTA)*YDPHY0%USURIC/ZCIS)
  PNBVNO (JLON, JLEV)=ZSTA/(PAPRS (JLON, JLEV)*ZZRT*ZDPHI)**2
  
ENDDO


DO JLEV=KTDIAN, KLEV-1
  
  ZTF1=PT (JLON, JLEV)
  ZTF2=PT (JLON, JLEV+1)
  ZQWF1=PQV (JLON, JLEV)+PQLI (JLON, JLEV)+PQICE (JLON, JLEV)
  ZQWF2=PQV (JLON, JLEV+1)+PQLI (JLON, JLEV+1)+PQICE (JLON, JLEV+1)
  ZQWF1=MAX (ABS (ZQWF1), ZEPSQ)
  ZQWF2=MAX (ABS (ZQWF2), ZEPSQ)
  ZLSCPEF1=PLSCPE (JLON, JLEV)
  ZLSCPEF2=PLSCPE (JLON, JLEV+1)
  ZQCF1=PQLI (JLON, JLEV)+PQICE (JLON, JLEV)
  ZQCF2=PQLI (JLON, JLEV+1)+PQICE (JLON, JLEV+1)
  ZTLF1=ZTF1-ZLSCPEF1*ZQCF1
  ZTLF2=ZTF2-ZLSCPEF2*ZQCF2
  ZRH=(PR (JLON, JLEV)+PR (JLON, JLEV+1))/2.0_JPRB
  ZQWH=(ZQWF1+ZQWF2)/2.0_JPRB
  ZTH=(ZTF1+ZTF2)/2.0_JPRB
  ZQLH=(ZQCF1+ZQCF2)/2.0_JPRB
  ZLSCPEH=(ZLSCPEF1+ZLSCPEF2)/2.0_JPRB

  IF (YDPHY%LDISTUR) THEN
    ZTLH=(ZTLF1+ZTLF2)/2.0_JPRB
    ZHH=MAX (0.0_JPRB, SIGN (1.0_JPRB, YDCST%RTT-ZTLH))
    ZEWH=FOEW (ZTLH, ZHH)/PAPRSF (JLON, JLEV)
    ZQSATH=FOQS (ZEWH)
    ZDLEWH=FODLEW (ZTLH, ZHH)
    ZQSLTLH (JLON, JLEV)=FDQW (ZEWH, ZDLEWH)
    ZDELTQH=ZQWH-ZQSATH
    ZDELTQH=SIGN (MAX (ABS (ZDELTQH), ZEPDELT), ZDELTQH)
  ELSE
    ZH1=MAX (0.0_JPRB, SIGN (1.0_JPRB, YDCST%RTT-ZTLF1))
    ZEW1=FOEW (ZTLF1, ZH1)/PAPRSF (JLON, JLEV)
    ZQSATF1=FOQS (ZEW1)
    ZDLEWF1=FODLEW (ZTLF1, ZH1)
    ZQSLTLF1=FDQW (ZEW1, ZDLEWF1)
    ZDELTQF1=ZQWF1-ZQSATF1
    ZDELTQF1=SIGN (MAX (ABS (ZDELTQF1), ZEPDELT), ZDELTQF1)
    ZH2=MAX (0.0_JPRB, SIGN (1.0_JPRB, YDCST%RTT-ZTLF2))
    ZEW2=FOEW (ZTLF2, ZH2)/PAPRSF (JLON, JLEV+1)
    ZQSATF2=FOQS (ZEW2)
    ZDLEWF2=FODLEW (ZTLF2, ZH2)
    ZQSLTLF2=FDQW (ZEW2, ZDLEWF2)
    ZDELTQF2=ZQWF2-ZQSATF2
    ZDELTQF2=SIGN (MAX (ABS (ZDELTQF2), ZEPDELT), ZDELTQF2)
    ZQSLTLH (JLON, JLEV)=(ZQSLTLF1+ZQSLTLF2)/2.0_JPRB
    ZDELTQH=(ZDELTQF1+ZDELTQF2)/2.0_JPRB
  ENDIF

  ZAA=1.0_JPRB/(1.0_JPRB+ZLSCPEH*ZQSLTLH (JLON, JLEV))
  ZAH (JLON, JLEV)=ZAA
  ZDD=ZLSCPEH-(1.0_JPRB+YDCST%RETV)*ZTH
  ZCTO=YDCST%RETV*ZTH
  ZDPHI=PAPHIF (JLON, JLEV)-PAPHIF (JLON, JLEV+1)
  ZDQW=PQV (JLON, JLEV)-PQV (JLON, JLEV+1)+PQLI (JLON, JLEV)-PQLI&
  & (JLON, JLEV+1)+PQICE (JLON, JLEV)-PQICE (JLON, JLEV+1)
  ZDT=PT (JLON, JLEV)-PT (JLON, JLEV+1)

  IF (YDPHY%LDISTUR) THEN
    ZDQLST=ZQCF1*ZLSCPEF1/ZTF1-ZQCF2*ZLSCPEF2/ZTF2
    ZDSTA=ZDT+ZDPHI*YDCST%RKAPPA/ZRH
    ZDTL=ZDSTA*(1.0_JPRB-ZLSCPEH*ZQLH/ZTH)-ZTH*ZDQLST
  ELSE
    ZDTL=(ZTHETALF (JLON, JLEV)-ZTHETALF (JLON, JLEV+1))*ZTH/(ZTHETALF&
    & (JLON, JLEV)+ZTHETALF (JLON, JLEV+1))*2._JPRB
  ENDIF

  ZDIFFH=ZDTL+ZCTO*ZDQW
  ZDIFFC=ZDQW-ZQSLTLH (JLON, JLEV)*ZDTL
  ZRHOH=PAPRS (JLON, JLEV)/ZRH/ZTH
  ZZKTH=PKTROV (JLON, JLEV)/ZRHOH
  ZECTH=MAX (YDPHY0%ECTMIN, PECT (JLON, JLEV))
  ZLMECT=PLMECT (JLON, JLEV)
  ZWQW=-ZZKTH*ZDQW
  ZWTL=-ZZKTH*ZDTL
  ZWDIFF=ZWQW-ZQSLTLH (JLON, JLEV)*ZWTL
  ZIGMAS2=-ZAA*ZAA*YDPHY0%ARSB2*ZLMECT/4._JPRB/SQRT (ZECTH)*ZWDIFF*ZDIFFC/ZDPHI
  ZIGMAS=MAX (ZEPSIG, SQRT (ABS (ZIGMAS2)))
  ZQ11=ZAA*ZDELTQH/(2*ZIGMAS)

  IF (YDPHY%LCVTURB.AND.ZDELTQH>0._JPRB.AND.PAPRSF (JLON, JLEV)<ZPRETURB.AND.LDCONV (JLON, JLEV)) THEN
    ZIGMAS=MAX (ZSIGCR, ZIGMAS)
    ZQ11=ZAA*ZDELTQH/(2.0_JPRB*ZIGMAS)
  ENDIF


  IF (YDPHY%LECTQ1) THEN
    ZGALP2=YDPHY0%GALP*YDPHY0%GALP/YDPHY0%TURB
    ZPHI3MIN=1.0_JPRB/(1.0_JPRB+YDPHY0%ARSC1*ZGALP2)
    ZQ1MAX=ZDELTQH*ZDPHI/ZLMECT/SQRT (YDPHY0%ARSB2*YDPHY0%AKN&
    &*YDPHY0%ALPHAT*ZPHI3MIN)/MAX (ABS (ZDIFFC), ZEPSQ1)
    ZQ1MAX=SIGN (MAX (ABS (ZQ1MAX), ZEPSQ1), ZQ1MAX)
    ZQ1MAX=SIGN (MIN (ABS (ZQ1MAX), ZMAXQ1), ZQ1MAX)
    ZQ1MAX=ZILIMQ1*ZQ1MAX-(1.0_JPRB-ZILIMQ1)*ZMAXQ1
    ZQ1MIN=YDPHY0%STTBMIN*ZDELTQH*ABS (ZDQW)/(ZQWH*MAX (ABS (ZDIFFC), ZEPSQ1))
    ZQ1MIN=SIGN (MAX (ABS (ZQ1MIN), ZEPSQ1), ZQ1MIN)
    ZQ1MIN=ZILIMQ1*ZQ1MIN+(1.0_JPRB-ZILIMQ1)*ZEPSQ1
    ZQ11=SIGN (MAX (ABS (ZQ11), ABS (ZQ1MIN)), ZQ11)
    ZQ11=SIGN (MIN (ABS (ZQ11), ABS (ZQ1MAX)), ZQ11)
    ZIGMAS=ZAA*ZDELTQH/(2.0_JPRB*ZQ11)
    ZIGMAS=MAX (ZEPSIG, ZIGMAS)
  ENDIF

  ZPRODH=-YDCST%RG*ZZKTH*ZDIFFH/ZTH
  ZPRODC=-YDCST%RG*ZZKTH*ZDIFFC*ZAA*ZDD/ZTH
  INQ1=MIN (MAX (-22, FLOOR (2*ZQ11)), 10)
  ZINC=2.0_JPRB*ZQ11-INQ1
  ZSRC=(1.0_JPRB-ZINC)*YDPHY0%RSRC1D (INQ1)+ZINC*YDPHY0%RSRC1D (INQ1+1)
  ZL3F2=MIN (1.0_JPRB, ZSRC)*MIN (MAX (1.0_JPRB, 1.0_JPRB-ZQ11), 3._JPRB)
  PL3F2 (JLON, JLEV)=ZL3F2
  PPRODTH (JLON, JLEV)=ZPRODH+ZL3F2*ZPRODC
  
ENDDO


PPRODTH (JLON, KLEV)=PPRODTH (JLON, KLEV-1)


IF (YDPHY%LPBLE) THEN
  
  ICM (JLON, KLEV)=INT (1.0_JPRB-ZSTAB )
  ILEVBI =ILEVBI *ICM (JLON, KLEV)
  
  
  ZECTINT =0.0_JPRB
  ZQCINT =0.0_JPRB

  

  DO JLEV=KLEV-1, KTDIAN,-1
    
    ICLA=MAX (0, ISIGN (1, JLEV-ILEVBI ))
    ZECTINT =ZECTINT +ICLA*PECT (JLON, JLEV)*(PAPHIF (JLON, JLEV)-PAPHIF (JLON, JLEV+1))
    ZQCINT =ZQCINT +ICLA*(PQLI (JLON, JLEV+1)+PQICE (JLON,&
    & JLEV+1))*(PAPHI (JLON, JLEV)-PAPHI (JLON, JLEV+1))
  
  ENDDO


  

  IF (ILEVBI >0) THEN
    ILEVT=MIN (ILEVBI , KLEV-1)
    ZNUM=ZECTINT +PECTCLS (JLON)*(PAPHIF (JLON, KLEV)-PAPHI (JLON, KLEV))
    ZDEN=PAPHIF (JLON, ILEVT)-PAPHI (JLON, KLEV)

    IF (YDPHY0%AGREF<0._JPRB) THEN
      ZECTBLK=ZNUM/ZDEN
    ELSE
      ZECTBLK=MIN (PECTCLS (JLON), ZNUM/ZDEN)
    ENDIF

    ZDEN=PAPHI (JLON, ILEVT)-PAPHI (JLON, KLEV)
    ZQCBLK=ZQCINT /ZDEN
    ZDPHI=PAPHIF (JLON, ILEVT)-PAPHIF (JLON, ILEVT+1)
    ZBI=-YDCST%RG*PPRODTH (JLON, ILEVT)/ZGKTH (JLON, ILEVT)
    ZBIC=YDCST%RG*YDCST%RG*YDPHY0%AJBUMIN/ZDPHI
    ZGZ=PAPHI (JLON, ILEVT)-PAPHI (JLON, KLEV)+PGZ0 (JLON)
    ZLINV=YDPHY0%RCOFLM*ZGZ/YDCST%RG

    IF (YDPHY0%AGREF<0._JPRB) THEN
      ZBICX=ZBIC*1.05_JPRB
      ZA=-YDPHY0%AGREF*YDPHY0%AGRE1*(1._JPRB-(1._JPRB/YDPHY0%AGREF+1._JPRB)*SIN (YDCST%RPI&
      &/2._JPRB*MAX (0._JPRB, MIN (1._JPRB,((ZBICX-ZBI)/(ZBICX-ZBIC)))))**2._JPRB)
      ZQCBLK=PQLI (JLON, ILEVT+1)+PQICE (JLON, ILEVT+1)
      ZA=ZA*(1._JPRB+YDPHY0%AGRE2*PLSCPE (JLON, ILEVT)*ZQCBLK/MAX (ZTHETAVL (JLON&
      &, ILEVT)-ZTHETAVL (JLON, ILEVT+1), YDPHY0%AJBUMIN*ZTHETA (JLON, ILEVT)))
    ELSE
      ZA=YDPHY0%AGRE1*(1._JPRB+YDPHY0%AGRE2*YDPHY0%AGREF*PLSCPE (JLON, ILEVT)*ZQCBLK/MAX (ZTHETAVL&
      & (JLON, ILEVT)-ZTHETAVL (JLON, ILEVT+1), YDPHY0%AJBUMIN*ZTHETA (JLON, ILEVT)))
    ENDIF

    ZBI=MAX (ZBI, ZBIC)
    ZGKENT=MAX (ZA*ZECTBLK*SQRT (ZECTBLK)*YDCST%RG/ZLINV/ZBI, ZGKTH (JLON, ILEVT))
    ZGKTH (JLON, ILEVT)=ZGKENT
    ZGKUH (JLON, ILEVT)=ZGKENT
    ZDPHI=PAPHIF (JLON, ILEVT)-PAPHIF (JLON, ILEVT+1)
    ZZRT=0.5_JPRB*(ZRTV (JLON, ILEVT)+ZRTV (JLON, ILEVT+1))
    ZROSDPHI=PAPRS (JLON, ILEVT)/(ZDPHI*ZZRT)
    PKTROV (JLON, ILEVT)=ZGKTH (JLON, ILEVT)*ZROSDPHI
    PKUROV (JLON, ILEVT)=ZGKUH (JLON, ILEVT)*ZROSDPHI
  ENDIF

  
ENDIF


IF (YDPHY%LDIFCEXP) THEN

  DO JLEV=KTDIAN, KLEV-1
    
    ZZDPHI=PAPHIF (JLON, JLEV)-PAPHIF (JLON, JLEV+1)
    ZZRTV=0.5_JPRB*(PR (JLON, JLEV)*PT (JLON, JLEV)+PR (JLON, JLEV+1)*PT (JLON, JLEV+1))
    ZROSDPHI=PAPRS (JLON, JLEV)/(ZZDPHI*ZZRTV)
    ZDQW=PQV (JLON, JLEV)-PQV (JLON, JLEV+1)+PQLI (JLON, JLEV)-PQLI&
    & (JLON, JLEV+1)+PQICE (JLON, JLEV)-PQICE (JLON, JLEV+1)
    ZDTL=PT (JLON, JLEV)-PT (JLON, JLEV+1)-PLSCPE (JLON, JLEV)*(PQLI (JLON, JLEV)+PQICE&
    & (JLON, JLEV))+PLSCPE (JLON, JLEV+1)*(PQLI (JLON, JLEV+1)+PQICE (JLON, JLEV+1))
    ZDTETL=ZDTL+ZZDPHI/YDCST%RCPD
    ZDIFTQL=ZAH (JLON, JLEV)*PL3F2 (JLON, JLEV)*ZGKTH (JLON&
    &, JLEV)/ZZDPHI*(ZDQW-ZQSLTLH (JLON, JLEV)*ZDTETL)
    ZDQLI=PQLI (JLON, JLEV)-PQLI (JLON, JLEV+1)+PQICE (JLON, JLEV)-PQICE (JLON, JLEV+1)
    ZDQLI=MAX (ZEPS3, ABS (ZDQLI))*SIGN (1._JPRB, ZDQLI)
    ZGKQLH=MIN (ZEPS2, MAX (ZEPS3, ZDIFTQL*ZZDPHI/(YDCST%RG*ZDQLI)))*YDCST%RG
    ZDIFTQL=ZGKQLH*ZDQLI/ZZDPHI
    ZLSCPEH=0.5_JPRB*(PLSCPE (JLON, JLEV)+PLSCPE (JLON, JLEV+1))
    ZDIFTTET=ZGKTH (JLON, JLEV)*ZDTETL/ZZDPHI+ZLSCPEH*ZDIFTQL
    ZDTETI=PT (JLON, JLEV)-PT (JLON, JLEV+1)+ZZDPHI/YDCST%RCPD
    ZDTETI=MAX (ZEPS3, ABS (ZDTETI))*SIGN (1._JPRB, ZDTETI)
    ZDQI=PQV (JLON, JLEV)-PQV (JLON, JLEV+1)
    ZDQI=MAX (ZEPS3, ABS (ZDQI))*SIGN (1._JPRB, ZDQI)
    ZDIFTQ=ZGKTH (JLON, JLEV)*ZDQW/ZZDPHI-ZDIFTQL
    ZGKTAH=MIN (ZEPS2, MAX (ZEPS3, ZDIFTTET*ZZDPHI/(YDCST%RG*ZDTETI)))*YDCST%RG
    ZGKQH=MIN (ZEPS2, MAX (ZEPS3, ZDIFTQ*ZZDPHI/(YDCST%RG*ZDQI)))*YDCST%RG
    PKTROV (JLON, JLEV)=ZGKTAH*ZROSDPHI
    PKQLROV (JLON, JLEV)=ZGKQLH*ZROSDPHI
    PKQROV (JLON, JLEV)=ZGKQH*ZROSDPHI
  
  ENDDO


  DO JLEV=KTDIAN, KLEV-1
    
    ZKROVN=5.E-3_JPRB
    ZKROVN=MAX (0.0_JPRB,(-ZKROVN/200._JPRB)*(PAPHI (JLON, JLEV)-PAPHI (JLON, KLEV))/YDCST%RG+ZKROVN)
    PKTROV (JLON, JLEV)=MAX (ZKROVN, PKTROV (JLON, JLEV))
    PKQROV (JLON, JLEV)=MAX (ZKROVN, PKQROV (JLON, JLEV))
    PKQLROV (JLON, JLEV)=MAX (ZKROVN, PKQLROV (JLON, JLEV))
    PKUROV (JLON, JLEV)=MAX (ZKROVN, PKUROV (JLON, JLEV))
  
  ENDDO

ENDIF


IF (YDPHY%LNEBECT) THEN
  
  ZGKTF (JLON, KTDIAN)=ZGKTH (JLON, KTDIAN)
  ZECTF (JLON, KTDIAN)=PECT (JLON, KTDIAN)
  ZLMECTF (JLON, KTDIAN)=PLMECT (JLON, KTDIAN)

  

  DO JLEV=KTDIAN+1, KLEV
    
    ZGKTF (JLON, JLEV)=(ZGKTH (JLON, JLEV)+ZGKTH (JLON, JLEV-1))/2.0_JPRB
    ZECTF (JLON, JLEV)=(PECT (JLON, JLEV)+PECT (JLON, JLEV-1))/2.0_JPRB
    ZLMECTF (JLON, JLEV)=(PLMECT (JLON, JLEV)+PLMECT (JLON, JLEV-1))/2.0_JPRB
  
  ENDDO


  DO JLEV=KLEV, KTDIAN,-1
    
    ZTF=PT (JLON, JLEV)
    ZQWF=PQV (JLON, JLEV)+PQLI (JLON, JLEV)+PQICE (JLON, JLEV)
    ZQWF=MAX (ABS (ZQWF), ZEPSQ)
    ZLSCPEF=PLSCPE (JLON, JLEV)
    ZQLF=PQLI (JLON, JLEV)+PQICE (JLON, JLEV)
    ZTLF=ZTF-ZLSCPEF*ZQLF
    ZH=MAX (0.0_JPRB, SIGN (1.0_JPRB, YDCST%RTT-ZTLF))
    ZEW=FOEW (ZTLF, ZH)/PAPRSF (JLON, JLEV)
    ZQSATF=FOQS (ZEW)
    ZDLEWF=FODLEW (ZTLF, ZH)
    ZQSLTLF=FDQW (ZEW, ZDLEWF)
    ZDELTQF=ZQWF-ZQSATF
    ZDELTQF=SIGN (MAX (ABS (ZDELTQF), ZEPDELT), ZDELTQF)
    ZAA=1.0_JPRB/(1.0_JPRB+ZLSCPEF*ZQSLTLF)
    ZDD=ZLSCPEF-(1.0_JPRB+YDCST%RETV)*ZTF
    ZCTO=YDCST%RETV*ZTF
    INIV=MAX (SIGN (1, KLEV-JLEV-1), 0)
    IJLEVM1=MAX (KTDIAN, JLEV-1)
    IJLEVP1=MIN (KLEV, JLEV+1)
    ZDPHI=PAPHIF (JLON, IJLEVM1)-PAPHIF (JLON, IJLEVP1)*REAL&
    & (INIV, JPRB)-PAPHI (JLON, KLEV)*REAL (1-INIV, JPRB)
    ZDQW=PQV (JLON, IJLEVM1)-PQV (JLON, IJLEVP1)*REAL (INIV, JPRB)-PQS (JLON)*REAL (1-INIV, JPRB&
    &)+PQLI (JLON, IJLEVM1)+PQICE (JLON, IJLEVM1)-PQLI (JLON, IJLEVP1)-PQICE (JLON, IJLEVP1)
    ZDT=PT (JLON, IJLEVM1)-PT (JLON, IJLEVP1)*REAL (INIV, JPRB)-PTS (JLON)*REAL (1-INIV, JPRB)
    ZQLM1=PQLI (JLON, IJLEVM1)+PQICE (JLON, IJLEVM1)
    ZQLP1=PQLI (JLON, IJLEVP1)+PQICE (JLON, IJLEVP1)
    ZLCPM1=PLSCPE (JLON, IJLEVM1)
    ZLCPP1=PLSCPE (JLON, IJLEVP1)
    ZDQLST=ZQLM1*ZLCPM1/PT (JLON, IJLEVM1)-ZQLP1*ZLCPP1/PT (JLON, IJLEVP1)*REAL (INIV, JPRB)
    ZDSTA=ZDT+ZDPHI*YDCST%RKAPPA/PR (JLON, JLEV)
    ZDTL=ZDSTA*(1.0_JPRB-ZLSCPEF*ZQLF/ZTF)-ZTF*ZDQLST
    ZDIFFH=ZDTL+ZCTO*ZDQW
    ZDIFFC=ZDQW-ZQSLTLF*ZDTL
    ZDU2=(PU (JLON, IJLEVM1)-PU (JLON, IJLEVP1)*REAL (INIV, JPRB))**2&
    &+(PV (JLON, IJLEVM1)-PV (JLON, IJLEVP1)*REAL (INIV, JPRB))**2
    ZDU2=MAX (ABS (ZDU2), ZEPSV)
    ZZETF=MAX (ZEPNEBS, ZECTF (JLON, JLEV))
    ZZLMF=ZLMECTF (JLON, JLEV)
    ZWQW=-ZGKTF (JLON, JLEV)*ZDQW/ZDPHI
    ZWTL=-ZGKTF (JLON, JLEV)*ZDTL/ZDPHI
    ZWDIFF=ZWQW-ZQSLTLF*ZWTL
    ZSIGMAS2=-ZAA*ZAA*YDPHY0%ARSB2*ZZLMF/4._JPRB/SQRT (ZZETF)*ZWDIFF*ZDIFFC/ZDPHI
    ZSIGMAS=MAX (ZEPSIG, SQRT (ABS (ZSIGMAS2)))
    ZQ11=ZAA*ZDELTQF/(2*ZSIGMAS)

    IF (YDPHY%LCVTURB.AND.ZDELTQF>0._JPRB.AND.PAPRSF (JLON, JLEV)<ZPRETURB.AND.LDCONV (JLON, JLEV)) THEN
      ZSIGMAS=MAX (ZSIGCR, ZSIGMAS)
      ZQ11=ZAA*ZDELTQF/(2.0_JPRB*ZSIGMAS)
    ENDIF


    IF (YDPHY%LECTQ1) THEN
      ZGALP2=YDPHY0%GALP*YDPHY0%GALP/YDPHY0%TURB
      ZPHI3MIN=1.0_JPRB/(1.0_JPRB+YDPHY0%ARSC1*ZGALP2)
      ZQ1MAX=ZDELTQF*ZDPHI/ZZLMF/SQRT (YDPHY0%ARSB2*YDPHY0%AKN&
      &*YDPHY0%ALPHAT*ZPHI3MIN)/MAX (ABS (ZDIFFC), ZEPSQ1)
      ZQ1MAX=SIGN (MAX (ABS (ZQ1MAX), ZEPSQ1), ZQ1MAX)
      ZQ1MAX=SIGN (MIN (ABS (ZQ1MAX), ZMAXQ1), ZQ1MAX)
      ZQ1MAX=ZILIMQ1*ZQ1MAX-(1.0_JPRB-ZILIMQ1)*ZMAXQ1
      ZQ1MIN=YDPHY0%STTBMIN*ZDELTQF*ABS (ZDQW)/(ZQWF*MAX (ABS (ZDIFFC), ZEPSQ1))
      ZQ1MIN=SIGN (MAX (ABS (ZQ1MIN), ZEPSQ1), ZQ1MIN)
      ZQ1MIN=ZILIMQ1*ZQ1MIN+(1.0_JPRB-ZILIMQ1)*ZEPSQ1
      ZQ11=SIGN (MAX (ABS (ZQ11), ABS (ZQ1MIN)), ZQ11)
      ZQ11=SIGN (MIN (ABS (ZQ11), ABS (ZQ1MAX)), ZQ11)
      ZSIGMAS=MAX (ZEPNEBS, ZAA*ZDELTQF/(2.0_JPRB*ZQ11))
    ENDIF

    ZFACT=ZDPHI/ZTF/ZDU2
    ZRIH=ZDIFFH*ZFACT
    ZRIC=ZDIFFC*ZFACT*ZAA*ZDD
    INQ1=MIN (MAX (-22, FLOOR (2*ZQ11)), 10)
    ZINC=2.0_JPRB*ZQ11-INQ1
    ZSRC=(1.0_JPRB-ZINC)*YDPHY0%RSRC1D (INQ1)+ZINC*YDPHY0%RSRC1D (INQ1+1)
    ZL3F2=MIN (1.0_JPRB, ZSRC)*MIN (MAX (1.0_JPRB, 1.0_JPRB-ZQ11), 3._JPRB)
    ZRICHF=ZRIH+ZL3F2*ZRIC

    IF (YDPHY%LNEBRIC) THEN
      ZRESUL=MAX (0.0_JPRB, SIGN (1.0_JPRB, YDPHY0%RICRET-ZRICHF))
    ELSE
      ZRESUL=1.0_JPRB
    ENDIF

    ZSURSAT=MAX (0.0_JPRB, SIGN (1.0_JPRB, ZDELTQF))
    INQ1=MIN (MAX (-22, FLOOR (2*ZQ11)), 10)
    ZINC=2.0_JPRB*ZQ11-INQ1
    ZZN1D=(1.0_JPRB-ZINC)*YDPHY0%RN1D (INQ1)+ZINC*YDPHY0%RN1D (INQ1+1)
    ZZF0=MIN (1.0_JPRB, ZZN1D)
    PNEBS (JLON, JLEV)=ZZF0*ZRESUL+ZSURSAT*(1.0_JPRB-ZRESUL)

    IF (YDPHY%LCVTURB.AND.ZDELTQF>0._JPRB.AND.PAPRSF (JLON, JLEV)<ZPRETURB.AND.LDCONV (JLON, JLEV)) THEN
      PNEBS (JLON, JLEV)=ZZF0
    ENDIF

    PNEBS (JLON, JLEV)=MAX (ZEPNEBS, MIN (PNEBS (JLON, JLEV), 1.0_JPRB-ZEPNEBS))
    INQ1=MIN (MAX (-22, FLOOR (2*ZQ11)), 10)
    ZINC=2.0_JPRB*ZQ11-INQ1
    ZZF1=(1.0_JPRB-ZINC)*YDPHY0%RRC1D (INQ1)+ZINC*YDPHY0%RRC1D (INQ1+1)
    ZZQC=2.0_JPRB*ZSIGMAS*ZZF1*ZRESUL+ABS (ZAA*ZDELTQF)*ZSURSAT*(1.0_JPRB-ZRESUL)

    IF (YDPHY%LCVTURB.AND.ZDELTQF>0._JPRB.AND.PAPRSF (JLON, JLEV)<ZPRETURB.AND.LDCONV (JLON, JLEV)) THEN
      ZZQC=2.0_JPRB*ZSIGMAS*ZZF1
    ENDIF

    ZZQC=ZZQC/(1.0_JPRB+ZZQC)
    PQCS (JLON, JLEV)=MIN (ZZQC, PQV (JLON, JLEV)+PQICE (JLON, JLEV)+PQLI (JLON, JLEV))
    ZNEBLOW=MAX (0.0_JPRB, SIGN (1.0_JPRB, ZEPNEBS-PNEBS (JLON, JLEV)))
    PQCS (JLON, JLEV)=PQCS (JLON, JLEV)*(1.0_JPRB-ZNEBLOW)

    IF (YDPHY%LNEIGE) THEN
      ZDELTA=MAX (0.0_JPRB, SIGN (1.0_JPRB, YDCST%RTT-PT (JLON, JLEV)))
      ZPLS=ZDELTA*(1.0_JPRB-EXP (-(YDCST%RTT-PT (JLON, JLEV))**2*ZGAUSS))
      PQLI (JLON, JLEV)=PQCS (JLON, JLEV)*(1.0_JPRB-ZPLS)
      PQICE (JLON, JLEV)=PQCS (JLON, JLEV)*ZPLS
    ELSE
      PQLI (JLON, JLEV)=PQCS (JLON, JLEV)
      PQICE (JLON, JLEV)=0.0_JPRB
    ENDIF

  
  ENDDO

  
  PQCS (JLON, KLEV)=PQCS (JLON, KLEV)*ZSTAB 
  PNEBS (JLON, KLEV)=PNEBS (JLON, KLEV)*ZSTAB 
  PNEBS (JLON, KLEV)=MAX (ZEPNEBS, MIN (PNEBS (JLON, KLEV), 1.0_JPRB-ZEPNEBS))
  
ELSE
  PNEBS (JLON,:)=ZEPNEBS
  PQCS (JLON,:)=0.0_JPRB
ENDIF



ENDSUBROUTINE ACTURB_OPENACC

! 56ad6923076b622f9a4a36289517d0f4b37156a2
