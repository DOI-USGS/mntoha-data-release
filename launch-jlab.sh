#!/bin/bash

JPORT=`shuf -i 8400-9400 -n 1`

export PATH="$PATH:$HOME/miniconda3/bin"
source activate mntoha_release 

echo "ssh -N -L $JPORT:`hostname`:$JPORT $USER@tallgrass.cr.usgs.gov"

jupyter lab --ip '*' --no-browser --port $JPORT --notebook-dir=. &

wait
