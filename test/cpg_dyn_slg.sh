#!/bin/bash

 #set -x
 #set -e

source ~/loki_env/bin/activate
which python3

export PATH=/home/gmap/mrpm/cossevine/build_scc:$PATH
p=$(pwd)
echo $p


function resolve ()
{
  f=$1
  for view in $(cat .gmkview)
  do  
    g="src/$view/$f"
    if [ -f $g ]
    then
  #    echo $g
      echo "src/$view/"
      break
    fi  
  done
}

for f in \
  arpifs/adiab/lasure.F90   \
  arpifs/adiab/lattex_dnt.F90 \
  arpifs/adiab/lattex_tnt.F90 \
  arpifs/adiab/gptf2_expl_2tl.F90 \
  arpifs/utility/verdisint.F90 \
  arpifs/adiab/gp_spv.F90 \
  arpifs/adiab/gpinislb_part1_expl.F90 \
  arpifs/adiab/gpinislb_part3_expl.F90 \
  arpifs/adiab/gprt.F90 \
  arpifs/adiab/gpcty_expl.F90 \
  arpifs/adiab/gpgeo_expl.F90 \
  arpifs/adiab/gpgrgeo_expl.F90 \
  arpifs/adiab/gphlwi.F90 \
  arpifs/adiab/gphluv_expl.F90 \
  arpifs/adiab/gpuvs.F90 \
  arpifs/adiab/gpgrp_expl.F90 \
  arpifs/adiab/gpxx.F90 \
  arpifs/adiab/gp_tndlagadiab_uv.F90 \
  arpifs/adiab/gphpre_expl_vertfe0.F90 \
  arpifs/adiab/gphpre_expl_vertfe1.F90 \
  arpifs/adiab/gphpre_expl.F90 \
  arpifs/adiab/gpgrxyb_expl.F90 \
  arpifs/adiab/gpgw.F90 \
  arpifs/adiab/gpmpfc_expl_part1.F90 
do

  echo "==> $f <=="
  dir=$(dirname $f)
  mkdir -p loki/$dir
  g=src/
  python3 ../main.py --pathpack $p --pathview $g --pathfile $f --pathacc /loki 
  python3 ../accseq.py $p/loki/$f

done

for f in \
  arpifs/phys_dmn/dprecips_xfu.F90 \
  arpifs/dia/meanwind_xfu.F90
do

  echo "==> $f <=="
  dir=$(dirname $f)
  mkdir -p loki/$dir
  g=src/
  python3 ../main.py --pathpack $p --pathview $g --pathfile $f --pathacc /loki 
  python3 ../accseq.py $p/loki/$f


done


