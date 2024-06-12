INTERFACE
  SUBROUTINE APLPAR_INIT_OPENACC (LDAROME, KIDIA, KFDIA, KLON, KLEV, KSGST, KCSS, PVEG0, PFRMQ, PCPS, PLHS, PRS, PLH, PLSCPE,  &
  & PQSAT, PQW, PTW, PCD, PCDN, PCH, PC1, PC2, PEMIS, PFEVI, PFTKE, PFTKEI, PFEFB1, PFEFB2, PFEFB3, PNEIJ, PVEG, PQSATS, KCLPH,  &
  & YDSTACK)
!$acc routine( APLPAR_INIT_OPENACC )
    
    USE PARKIND1, ONLY: JPIM, JPRB
    USE YOMHOOK, ONLY: LHOOK, DR_HOOK, JPHOOK
    USE YOMARPHY, ONLY: TARPHY
    
    !**** *APLPAR_INIT*
    
    !     Purpose.
    !     --------
    !         Initialise outputs of "aplpar" routine.
    
    !**   Interface.
    !     ----------
    !        *CALL* *APLPAR_INIT(...)*
    
    !        Explicit arguments :
    !        --------------------
    
    ! -   ARGUMENTS D'ENTREE.
    ! -   INPUT ARGUMENTS.
    !     -------------------
    
    ! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.
    ! - DIMENSIONS.
    
    ! KIDIA, KFDIA : BORNES BOUCLES HORIZONTALES   (IST,IEND DANS CPG).
    ! KIDIA, KFDIA : START/END OF HORIZONTAL LOOP  (IST,IEND IN *CPG*).
    ! KLON : DIMENSION HORIZONTALE                 (NPROMA DANS CPG).
    ! KLON : HORIZONTAL DIMENSION                  (NPROMA IN *CPG*).
    ! KLON: IDEM BUT FOR ARRAYS USED SOLELY BY DMN PHYSICS
    ! KLEV : FIN BOUCLE VERTICE ET DIMENSION VERTICALE (NFLEVG DANS CPG).
    ! KLEV : END OF VERTICAL LOOP AND VERTICAL DIMENSION(NFLEVG IN *CPG*).
    ! KSGST      : NOMBRE DE TEMPERATURES ET DE FLUX DE SURFACE SOUS-MAILLE
    !                     (NTSSG DANS CPG)
    ! KSGST      : NUMBER OF SUBGRID SURFACE TEMPERATURES AND FLUXES
    !                     (NTSSG IN *CPG*)
    ! KCSS       : NBRE DE NIVEAUX DANS LE SOL PROFOND
    ! KCSS       : NBR OF VERTICAL LAYERS IN THE DEEP SOIL
    
    ! - 1D (GEOGRAPHIQUE) .
    ! - 1D (GEOGRAPHICAL DISTRIBUTION) .
    
    ! PVEG0      : PROPORTION DE SOL COUVERTE DE VEGETATION.
    ! PVEG0      : FRACTIONAL COVER BY VEGETATION.
    
    !-----------------------------------------------------------------------
    
    ! -   ARGUMENTS DE SORTIE.
    ! -   OUTPUT ARGUMENTS.
    !     --------------------
    
    ! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
    !   CATEGORIE).
    
    ! - 2D (0:KLEV) .
    
    ! PDIFCQ     : FLUX CONVECTIF D'HUMIDITE SPECIFIQUE (HORS PLUIE/NEIGE).
    ! PDIFCQ     : CONVECTIVE FLUX OF SPECIFIC HUMIDITY (NOT RAIN/SNOW).
    ! PDIFCQN    : FLUX CONVECTIF D'EAU SOLIDE (HORS PLUIE/NEIGE).
    ! PDIFCQN    : CONVECTIVE FLUX OF SOLID WATER (NOT RAIN/SNOW).
    ! PDIFCQL    : FLUX CONVECTIF D'EAU LIQUIDE (HORS PLUIE/NEIGE).
    ! PDIFCQL    : CONVECTIVE FLUX OF LIQUID WATER (NOT RAIN/SNOW).
    ! PDIFCS     : FLUX CONVECTIF D'ENTHALPIE (HORS PLUIE/NEIGE).
    ! PDIFCS     : CONVECTIVE FLUX OF ENTHALPY (NOT RAIN/SNOW).
    ! PDIFTQ     : FLUX TURBULENT (ET Q NEGATIF) D'HUMIDITE SPECIFIQUE.
    ! PDIFSV     : FLUX DES SCLAIRES PASSIFS.
    ! PDIFSV     : PASSIFS SCALAR FLUX
    ! PDIFTQ     : TURBULENT FLUX (INC. Q NEGATIVE) OF SPECIFIC HUMIDITY.
    ! PDIFTQN    : FLUX TURBULENT (ET Q NEGATIF) D'EAU GLACE.
    ! PDIFTQN    : TURBULENT FLUX (INC. Q NEGATIVE) OF SOLID WATER.
    ! PDIFTQL    : FLUX TURBULENT (ET Q NEGATIF) D'EAU LIQUIDE.
    ! PDIFTQL    : TURBULENT FLUX (INC. Q NEGATIVE) OF LIQUID WATER.
    ! PDIFTS     : FLUX TURBULENT D'ENTHALPIE (OU D'ENERGIE STATIQUE SECHE).
    ! PDIFTS     : TURBULENT FLUX OF ENTHALPY (OR DRY STATIC ENERGY).
    ! PFCHOZ     : FLUX PHOTO-CHIMIQUE D'OZONE.
    ! PFCHOZ     : OZONE PHOTO-CHEMICAL FLUX.
    ! PFCCQN     : FLUX DE CONDENSATION LIE AUX RR CONVECTIVES NEIGEUSES.
    ! PFCCQN     : CONVECTIVE CONDENSATION FLUX FOR ICE.
    ! PFCCQL     : FLUX DE CONDENSATION LIE AUX RR CONVECTIVES LIQUIDES.
    ! PFCCQL     : CONVECTIVE CONDENSATION FLUX FOR LIQUID WATER.
    ! PFCQNG     : FLUX FICTIF DE VAPEUR POUR CORRIGER LES Q<0.
    ! PFCQNG     : PSEUDO-FLUX OF WATER TO CORRECT FOR Q<0.
    ! PFCQING    : FLUX FICTIF DE GLACE POUR CORRIGER LES QI<0.
    ! PFCQING    : PSEUDO-FLUX OF ICE TO CORRECT FOR QI<0.
    ! PFCQLNG    : FLUX FICTIF D'EAU LIQUIDE POUR CORRIGER LES QL<0.
    ! PFCQLNG    : PSEUDO-FLUX OF LIQUID WATER TO CORRECT FOR QL<0.
    ! PFCQRNG    : PSEUDO-FLUX OF RAIN TO CORRECT FOR QL<0.
    ! PFCQSNG    : PSEUDO-FLUX OF SNOW TO CORRECT FOR QL<0.
    ! PFPEVPSL   : FLUX DE PRECIPITATIONS DU A L'EVAPORATION RESOLUE.
    ! PFPEVPSL   : PRECIPITATION FLUX DUE TO RESOLVED EVAPORATION.
    ! PFPEVPSN   : FLUX DE PRECIPITATIONS DU A LA SUBLIMATION RESOLUE.
    ! PFPEVPSN   : PRECIPITATION FLUX DUE TO RESOLVED SUBLIMATION.
    ! PFPEVPCL   : FLUX DE PRECIPITATIONS DU A L'EVAPORATION CONVECTIVE.
    ! PFPEVPCL   : PRECIPITATION FLUX DUE TO CONVECTIVE EVAPORATION.
    ! PFPEVPCN   : FLUX DE PRECIPITATIONS DU A LA CONVECTIVE SUBLIMATION (NEIGE).
    ! PFPEVPCN   : PRECIPITATION FLUX DUE TO SUBLIMATION CONVECTIVE (SNOW).
    ! PFPEVPCG   : FLUX DE PRECIPITATIONS DU A LA CONVECTIVE SUBLIMATION (GRAUPEL).
    ! PFPEVPCG   : PRECIPATIONS FLUX DUE TO SUBLIMATION CONVECTIVE (GRAUPEL).
    ! PFTKE      : FLUX DE TKE.
    ! PFTKE      : TKE FLUX.
    ! PFPFPSL    : FLUX DE PRECIPITATION RESOLUE: TERME DE GENERATION/FORMATION.
    ! PFPFPSL    : FLUX OF RESOLVED PRECIPITATION : THE GENERATION TERM.
    ! PFPFPSN    : FLUX DE PRECIPITATION RESOLUE: TERME DE GENERATION/FORMATION.
    ! PFPFPSN    : FLUX OF RESOLVED PRECIPITATION : THE GENERATION TERM.
    ! PFPFPCL    : FLUX DE PRECIPITATION CONVECTIVE: TERME DE GENERATION/FORMATION.
    ! PFPFPCL    : FLUX OF CONVECTIVE PRECIPITATION : THE GENERATION TERM.
    ! PFPFPCN    : FLUX DE PRECIPITATION CONVECTIVE: TERME DE GENERATION/FORMATION.
    ! PFPFPCN    : FLUX OF CONVECTIVE PRECIPITATION : THE GENERATION TERM.
    ! PFPLCL     : PRECIPITATIONS CONVECTIVES SOUS FORME LIQUIDE.
    ! PFPLCL     : CONVECTIVE PRECIPITATION AS RAIN.
    ! PFPLCN     : PRECIPITATIONS CONVECTIVES SOUS FORME NEIGE.
    ! PFPLCN     : CONVECTIVE PRECIPITATION AS SNOW.
    ! PFPLCG     : PRECIPITATIONS CONVECTIVES SOUS FORME GRAUPEL.
    ! PFPLCG     : CONVECTIVE PRECIPITATION AS GRAUPEL.
    ! PFPLCH     : PRECIPITATIONS CONVECTIVES SOUS FORME GRELE.
    ! PFPLCH     : CONVECTIVE PRECIPITATION AS HAIL.
    ! PFPLSL     : PRECIPITATIONS STRATIFORMES SOUS FORME LIQUIDE.
    ! PFPLSL     : STRATIFORM PRECIPITATION AS RAIN.
    ! PFPLSN     : PRECIPITATIONS STRATIFORMES SOUS FORME NEIGE.
    ! PFPLSN     : STRATIFORM PRECIPITATION AS SNOW.
    ! PFPLSG     : PRECIPITATIONS STRATIFORMES SOUS FORME GRAUPEL.
    ! PFPLSG     : STRATIFORM PRECIPITATION AS GRAUPEL.
    ! PFPLSH     : PRECIPITATIONS STRATIFORMES SOUS FORME GRELE.
    ! PFPLSH     : STRATIFORM PRECIPITATION AS HAIL.
    ! PFRMH      : FLUX MESOSPHERIQUE D'ENTHALPIE.
    ! PFRMH      : MESOSPHERIC ENTHALPY FLUX.
    ! PFRMQ      : FLUX MESOSPHERIQUE D'HUMIDITE.
    ! PFRMQ      : MESOSPHERIC HUMIDITY FLUX.
    ! PFRSO      : FLUX DE RAYONNEMENT SOLAIRE.
    ! PFRSO      : SHORTWAVE RADIATIVE FLUX.
    ! PFRTH      : FLUX DE RAYONNEMENT THERMIQUE.
    ! PFRTH      : LONGWAVE RADIATIVE FLUX.
    ! PFCSQN     : FLUX DE CONDENSATION LIE AUX RR STRATIFORMES NEIGEUSES.
    ! PFCSQN     : STRATIFORM CONDENSATION FLUX FOR ICE.
    ! PFCSQL     : FLUX DE CONDENSATION LIE AUX RR STRATIFORMES LIQUIDES.
    ! PFCSQL     : STRATIFORM CONDENSATION FLUX FOR LIQUID WATER.
    ! PSTRCU     : FLUX CONVECTIF DE QUANTITE DE MOUVEMENT "U".
    ! PSTRCU     : CONVECTIVE FLUX OF MOMENTUM "U".
    ! PSTRCV     : FLUX CONVECTIF DE QUANTITE DE MOUVEMENT "V".
    ! PSTRCV     : CONVECTIVE FLUX OF MOMENTUM "V".
    ! PSTRDU     : FLUX "GRAVITY WAVE DRAG" "U".
    ! PSTRDU     : GRAVITY WAVE DRAG FLUX "U".
    ! PSTRDV     : FLUX "GRAVITY WAVE DRAG" "V".
    ! PSTRDV     : GRAVITY WAVE DRAG FLUX "V".
    ! PSTRTU     : FLUX TURBULENT DE QUANTITE DE MOUVEMENT "U".
    ! PSTRTU     : TURBULENT FLUX OF MOMENTUM "U".
    ! PSTRTV     : FLUX TURBULENT DE QUANTITE DE MOUVEMENT "V".
    ! PSTRTV     : TURBULENT FLUX OF MOMENTUM "V".
    ! PSTRMU     : FLUX MESOSPHERIQUE DE QUANTITE DE MOUVEMENT "U".
    ! PSTRMU     : MESOSPHERIC FLUX FOR "U"-MOMENTUM.
    ! PSTRMV     : FLUX MESOSPHERIQUE DE QUANTITE DE MOUVEMENT "V".
    ! PSTRMV     : MESOSPHERIC FLUX FOR "V"-MOMENTUM.
    
    ! - 2D (1:KLEV) .
    
    ! PLH        : CHALEUR LATENTE A LA TEMPERATURE DE L'AIR.
    ! PLH        : LATENT HEAT AT AIR TEMPERATURE.
    ! PLSCPE     : RAPPORT EFECTIF DES L ET CP EN CONDENSATION/EVAPORATION.
    ! PLSCPE     : EFFECTIVE RATIO OF L AND CP FOR CONDENSATION/EVAPORATION.
    ! PNEB       : NEBULOSITE PARTIELLE "RADIATIVE".
    ! PNEB       : FRACTIONAL CLOUDINESS FOR RADIATION.
    ! PQICE      : HUMIDITE SPECIFIQUE SOLIDE "RADIATIVE".
    ! PQICE      : SPECIFIC HUMIDITY OF SOLID WATER FOR RADIATION.
    ! PQLI       : HUMIDITE SPECIFIQUE LIQUIDE "RADIATIVE".
    ! PQLI       : SPECIFIC HUMIDITY OF LIQUID WATER FOR RADIATION.
    ! PQSAT      : HUMIDITE SPECIFIQUE DE SATURATION.
    ! PQSAT      : SPECIFIC HUMIDITY AT SATURATION.
    ! PQW        : HUMIDITE SPECIFIQUE DU THERMOMETRE MOUILLE.
    ! PQW        : SPECIFIC HUMIDITY OF THE WET THERMOMETER.
    ! PRH        : HUMIDITE RELATIVE.
    ! PRH        : RELATIVE HUMIDITY.
    ! PTW        : TEMPERATURE DU THERMOMETRE MOUILLE.
    ! PTW        : TEMPERATURE OF THE WET THERMOMETER.
    
    ! - 2D (CLOUD AND RADIATION I/O FOR ECMWF PHYSICS)
    
    ! PEMTD      : DOWNWARD LONGWAVE EMISSIVITY
    ! PEMTU      : UPWARD   LONGWAVE EMISSIVITY
    ! PTRSO      : SHORTWAVE TRANSMISSIVITY
    ! ZTENT      : TENDENCY OF TEMPERATURE.
    
    ! - 2D (0:1)
    ! PFRSOC     : SHORTWAVE CLEAR SKY RADIATIVE FLUX
    ! PFRTHC     : LONGWAVE CLEAR SKY RADIATIVE FLUX
    
    ! - 1D (DIAGNOSTIQUE) .
    
    ! PALB       : ALBEDO DE SURFACE COURANT.
    ! PALB       : MODEL SURFACE SHORTWAVE ALBEDO.
    ! PCD        : COEFFICIENT D'ECHANGE EN SURFACE POUR U ET V.
    ! PCD        : EXCHANGE COEFFICIENT AT SURFACE LEVEL FOR U AND V.
    ! PCDN       : COEFFICIENT NEUTRE D'ECHANGE EN SURFACE.
    ! PCDN       : EXCHANGE COEFF. AT SURFACE LEVEL IN NEUTRAL CONDITIONS.
    ! PCH        : COEFFICIENT D'ECHANGE EN SURFACE POUR T ET Q.
    ! PCH        : EXCHANGE COEFFICIENT AT SURFACE LEVEL FOR T AND Q.
    ! PCPS       : CHALEUR MASSIQUE DE L'AIR EN SURFACE.
    ! PC1        : COEFF. HYDRIQUE REPRESENTANT L'INTENSITE AVEC LAQUELLE
    !              LES FLUX DE SURFACE PARTICIPENT A L'EVOLUTION DE WS.
    ! PC1        : HYDROLOGICAL COEFF. SHOWING THE CONTRIBUTION OF SURFACE
    !              FLUXES IN THE WS EVOLUTION.
    ! PC2        : COEFF. HYDRIQUE TRADUISANT LA RAPIDITE DES TRANSFERTS
    !              D'EAU ENTRE LES DEUX RESERVOIRS.
    ! PC2        : HYDROLOGICAL COEFFICIENT SHOWING THE QUICKNESS OF WATER
    !              TRANSFERS BETWEEN BOTH TANKS.
    ! PCT        : COEFFICIENT THERMIQUE DU MILIEU SOL-VEGETATION.
    ! PCT        : THERMICAL COEFFICIENT OF SOIL-VEGETATION MIDDEL.
    ! PEMIS      : EMISSIVITE DE SURFACE COURANTE.
    ! PEMIS      : MODEL SURFACE LONGWAVE EMISSIVITY.
    ! PFCHSP     : FLUX DE CHALEUR DE LA SURFACE VERS LE SOL PROFOND.
    ! PFCHSP     : HEAT FLUX FROM SURFACE TO DEEP SOIL.
    ! PFCLL      : FLUX DE CHALEUR LATENTE SUR EAU LIQUIDE (OU SOL HUMIDE).
    ! PFCLL      : LATENT HEAT FLUX OVER LIQUID WATER (OR WET SOIL).
    ! PFCLN      : FLUX DE CHALEUR LATENTE SUR NEIGE (OU GLACE).
    ! PFCLN      : LATENT HEAT FLUX OVER SNOW (OR ICE).
    ! PFCS       : FLUX DE CHALEUR SENSIBLE EN SURFACE.
    ! PFCS       : SENSIBLE HEAT FLUX AT SURFACE LEVEL.
    ! PFEVI      : FLUX DE VAPEUR D'EAU SUR SOL GELE.
    ! PFEVI      : WATER VAPOUR FLUX OVER FROZEN SOIL.
    ! PFEVL      : FLUX DE VAPEUR D'EAU SUR EAU LIQUIDE (OU SOL HUMIDE).
    ! PFEVL      : WATER VAPOUR FLUX OVER LIQUID WATER (OR WET SOIL)
    ! PFEVN      : FLUX DE VAPEUR D'EAU SUR NEIGE (OU GLACE) ET SOL GELE.
    ! PFEVN      : WATER VAPOUR FLUX OVER SNOW (OR ICE) AND FROZEN SOIL.
    ! PFEVV      : FLUX D'EVAPOTRANSPIRATION.
    ! PFEVV      : EVAPOTRANSPIRATION FLUX.
    ! PFLASH     : FLASH DENSITY FLUX (FL / KM2 / S).
    ! PFTR       : FLUX DE TRANSPIRATION.
    ! PFTR       : TRANSPIRATION FLUX.
    ! PFLWSP     : FLUX D'EAU LIQUIDE DE LA SURFACE VERS LE SOL PROFOND.
    ! PFLWSP     : WATER FLUX FROM SURFACE TO DEEP SOIL.
    ! PFONTE     : FLUX D'EAU CORRESPONDANT A LA FONTE DE NEIGE EN SURFACE.
    ! PFONTE     : WATER FLUX CORRESPONDING TO SURFACE SNOW MELT.
    ! PFRSGNI    : FLUX SOLAIRE GLOBALE A LA SURFACE PERPENDICULAIRE AUX RAYONS.
    ! PFRSGNI    : GLOBAL NORMAL SOLAR FLUX AT THE SURFACE.
    ! PFRSDNI    : FLUX SOLAIRE PARALLELE A LA SURFACE PERPENDICULAIRE AUX RAYONS.
    ! PFRSDNI    : DIRECT NORMAL SOLAR FLUX AT THE SURFACE.
    ! PFRSODS    : FLUX SOLAIRE DESCENDANT EN SURFACE
    ! PFRSODS    : SURFACE DOWNWARDS SOLAR FLUX
    ! PFRSOPS    : FLUX SOLAIRE PARALLELE EN SURFACE
    ! PFRSOPS    : SURFACE PARALLEL SOLAR FLUX
    ! PFRTHDS    : FLUX IR DESCENDANT EN SURFACE
    ! PFRTHDS    : SURFACE DOWNWARDS IR FLUX
    ! PGZ0       : G FOIS LA LONGUEUR DE RUGOSITE COURANTE.
    ! PGZ0       : G * ROUGHNESS LENGTH (CURRENT).
    ! PGZ0H      : G*LONGUEUR DE RUGOSITE THERMIQUE COURANTE (SI KVCLIV >= 8)
    ! PGZ0H      : CURRENT G*THERMAL ROUGHNESS LENGTH (IF KVCLIV >= 8)
    ! PLHS       : CHALEUR LATENTE EN SURFACE.
    ! PNEIJ      : PROPORTION DE SOL ENNEIGE.
    ! PNEIJ      : FRACTION OF SOIL COVERED BY SNOW.
    ! PVEG       : FRACTION DE VEGETATION APPARENTE.
    ! PVEG       : FRACTIONAL COVER BY APPARENT VEGETATION.
    ! PQS        : HUMIDITE SPECIFIQUE DE SURFACE.
    ! PQS        : SPECIFIC HUMIDITY AT SURFACE LEVEL.
    ! PQSATS     : HUMIDITE SPECIFIQUE DE SATURATION EN SURFACE.
    ! PQSATS     : SATURATED SPECIFIC HUMIDITY AT SURFACE LEVEL.
    ! PRS        : CONSTANTE DES GAZ DE L'AIR EN SURFACE.
    ! PRUISP     : FLUX DE RUISSELLEMENT EN PROFONDEUR.
    ! PRUISP     : RUN-OFF FLUX IN SOIL.
    ! PRUISL     : FLUX DE RUISSELLEMENT DU RESERVOIR D'INTERCEPTION.
    ! PRUISL     : RUN-OFF FLUX OUT THE INTERCEPTION WATER-TANK.
    ! PRUISS     : FLUX DE RUISSELLEMENT EN SURFACE.
    ! PRUISS     : RUN-OFF FLUX AT SURFACE LEVEL.
    ! PFGEL      : FLUX DE GEL DE L'EAU DU SOL.
    ! PFGEL      : FREEZING FLUX OF SOIL WATER.
    ! PFGELS     : FLUX DE GEL DE L'EAU DU RESERVOIR DE SURFACE.
    ! PFGELS     : FREEZING FLUX OF SOIL WATER AT SURFACE LEVEL.
    ! PUCLS      : SORTIE DIAGNOSTIQUE DU VENT EN X A HUV METEO.
    ! PUCLS      : U-COMPONENT OF WIND AT 10 METERS (DIAGNOSTIC).
    ! PVCLS      : SORTIE DIAGNOSTIQUE DU VENT EN Y A HUV METEO.
    ! PVCLS      : V-COMPONENT OF WIND AT 10 METERS (DIAGNOSTIC).
    ! PNUCLS     : SORTIE DIAGNOSTIQUE DU VENT NEUTRE EN X A HUV METEO.
    ! PNUCLS     : U-COMPONENT OF NEUTRAL WIND AT 10 METERS (DIAGNOSTIC).
    ! PNVCLS     : SORTIE DIAGNOSTIQUE DU VENT NEUTRE EN Y A HUV METEO.
    ! PNVCLS     : V-COMPONENT OF NEUTRAL WIND AT 10 METERS (DIAGNOSTIC).
    ! PTCLS      : SORTIE DIAGNOSTIQUE DE LA TEMPERATURE A HTQ METEO.
    ! PTCLS      : TEMPERATURE AT 2 METERS (DIAGNOSTIC).
    ! PMRT       : SORTIE DIAGNOSTIQUE DE LA TEMPERATURE RADIANTE MOYENNE.
    ! PMRT       : MEAN RADIANT TEMPERATURE AT 2 METERS (DIAGNOSTIC).
    ! PQCLS      : SORTIE DIAGNOSTIQUE DE L'HUMIDITE SPECIFIQUE A HTQ METEO.
    ! PQCLS      : SPECIFIC HUMIDITY AT 2 METERS (DIAGNOSTIC).
    ! PRHCLS     : SORTIE DIAGNOSTIQUE DE L'HUMIDITE RELATIVE A HTQ METEO.
    ! PRHCLS     : RELATIVE HUMIDITY AT 2 METERS (DIAGNOSTIC).
    ! PCLCC      : SORTIE DIAGNOSTIQUE DE LA NEBULOSITE CONVECTIVE.
    ! PCLCC      : CONVECTIVE CLOUD COVER (DIAGNOSTIC).
    ! PCLCH      : SORTIE DIAGNOSTIQUE DE LA NEBULOSITE HAUTE.
    ! PCLCH      : HIGH CLOUD COVER (DIAGNOSTIC).
    ! PCLCL      : SORTIE DIAGNOSTIQUE DE LA NEBULOSITE BASSE.
    ! PCLCL      : LOW CLOUD COVER (DIAGNOSTIC).
    ! PCLCM      : SORTIE DIAGNOSTIQUE DE LA NEBULOSITE MOYENNE.
    ! PCLCM      : MEDIUM CLOUD COVER (DIAGNOSTIC).
    ! PCLCT      : SORTIE DIAGNOSTIQUE DE LA NEBULOSITE TOTALE.
    ! PCLCT      : TOTAL CLOUD COVER (DIAGNOSTIC).
    ! PCAPE      : CAPE.
    ! PCTOP      : PRESSION DU SOMMET DE LA CONVECTION PROFONDE
    ! PCTOP      : PRESSURE OF THE DEEP CONVECTION
    ! PCLPH      : HAUTEUR (EN METRES) DE LA COUCHE LIMITE PLANETAIRE
    ! PCLPH      : HEIGHT (IN METERS) OF THE PBL
    ! PVEIN      : VENTILATION INDEX IN THE PBL
    ! PVEIN      : INDEX DE VENTILATION DANS PBL
    ! PUGST      : SORTIE DIAGNOSTIQUE DES RAFALES EN X
    ! PUGST      : U-COMPONENT OF GUSTS (DIAGNOSTIC).
    ! PVGST      : SORTIE DIAGNOSTIQUE DES RAFALES EN Y
    ! PVGST      : V-COMPONENT OF GUSTS (DIAGNOSTIC).
    ! PDIAGH     : DIAGNOSTQUE DE LA GRELE.
    ! PDIAGH     : HAIL DIAGNOSTICS.
    ! PTDISS     : TKE dissipation tendency (m2/s3)
    
    !     Method.
    !     -------
    
    !     Externals.
    !     ----------
    
    !     Reference.
    !     ----------
    
    !     Author.
    !     -------
    !        2004-09-15: F. Bouyssel
    
    !   Modifications
    !   -------------
    !        2007-02-25: R. Brozkova, split of autoconversion and prec. evaporation
    !                                 fluxes.
    !        2007-05-07: Y. Seity, initialisation of Graupel and Hail fluxes
    !        2009-08-07: A. Alias, PFRMQ added
    !        ????-??-??  M. Mokhtari: Initialisation of the desert aerosols and introduction of LMDUST
    !        2011-09-07 J.M. Piriou: PCMT convection scheme.
    !        2011-06: M. Jerczynski - some cleaning to meet norms
    !        R. El Khatib 03-Sep-2014 Use full array syntax for an optimal use of fast memset
    !        P. Marguinaud 04-Oct-2016 Port to single precision
    !        R. Brozkova 09-2018: Initialization of PFRSGNI, PFRSDNI, PMRT and
    !                             PDIAGH
    !     ----------------------------------------------------------------------------------
    
    USE STACK_MOD
    
    IMPLICIT NONE
    
    LOGICAL, INTENT(IN) :: LDAROME
    INTEGER(KIND=JPIM), INTENT(IN) :: KLON
    INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
    INTEGER(KIND=JPIM), INTENT(IN) :: KSGST
    INTEGER(KIND=JPIM), INTENT(IN) :: KCSS
    INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
    INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
    
    REAL(KIND=JPRB), INTENT(IN) :: PVEG0(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PFRMQ(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PFTKE(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PFTKEI(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PFEFB1(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PFEFB2(KLON, 0:KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PFEFB3(KLON, 0:KLEV)
    
    
    REAL(KIND=JPRB), INTENT(OUT) :: PCPS(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PLHS(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PRS(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PLH(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PLSCPE(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PQSAT(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PQW(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PTW(KLON, KLEV)
    REAL(KIND=JPRB), INTENT(OUT) :: PCD(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PCDN(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PCH(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PC1(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PC2(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PEMIS(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PFEVI(KLON, KSGST + 1)
    REAL(KIND=JPRB), INTENT(OUT) :: PNEIJ(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PVEG(KLON)
    REAL(KIND=JPRB), INTENT(OUT) :: PQSATS(KLON)
    
    INTEGER(KIND=JPIM), INTENT(OUT) :: KCLPH(KLON)
    
    TYPE(STACK), INTENT(IN) :: YDSTACK
  END SUBROUTINE APLPAR_INIT_OPENACC
END INTERFACE
