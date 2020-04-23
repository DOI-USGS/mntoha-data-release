# mntoha-data-release
Thermal Optical Habitat Area data release

## Organization

Though we don't strictly adhere to these divisions, here's the general idea for the output folders:

* `out` contains git-committable yml or ind files that describe the existence of data files
* `out_data` contains data files that are directly posted to SB as-is
* `tmp` contains data files that we should be able to delete to save space without disrupting much pipeline progress

## PGDL outputs

PGDL outputs are built and posted directly from Tallgrass.

```sh
ssh tallgrass.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/mntoha-data-release
source activate mntoha_release
git pull
```

These targets should be built in the `lake-temperature-neural-networks` project:
```
2_model/log/pgdl_outputs.ind # or otherwise be sure the batch is complete
3_assess/out/posthoc_config.csv
3_assess/log/preds_pretrain.ind
3_assess/log/preds_holdout.ind
3_assess/log/preds_final.ind
```
and these targets should be built in `lake-temperature-model-prep`:
```
1_crosswalk_fetch/out/canonical_lakes_sf.rds
```
(see the READMEs in those repos for instructions on how to create and use a conda environment to build the above targets.)

Results can now be packaged and posted using this repo (but run `mkdir log`, `mkdir out_data`, `mkdir out`, and `mkdir tmp` first if needed)
```sh
Rscript -e "library(scipiper); scmake('out_data/pgdl_config.csv', force=TRUE)" > log/pgdl_config.out 2>&1
nohup Rscript -e "library(scipiper); scmake('tmp/5_pgdl_pretrain_predictions_zips.yml', force=TRUE)" > log/5_pgdl_pretrain_predictions_zips.out 2>&1 &
nohup Rscript -e "library(scipiper); scmake('out/5_pgdl_predictions_zips.yml', force=TRUE)" > log/5_pgdl_predictions_zips.out 2>&1 &
```

### Tallgrass configuration for PGDL outputs

```sh
conda update -n base -c defaults conda
conda create -n mntoha_release
source activate mntoha_release
conda install -c conda-forge proj4 gdal r-rgdal r-devtools r-maps r-mapdata r-maptools r-rgeos r-rjsonio r-RcppCNPy r-ggplot2 r-sf r-lwgeom r-dplyr r-tidyr r-readr r-progress r-BH r-hms r-generics r-lubridate r-feather r-plyr r-reticulate python
module load netcdf/gcc/64/4.6.1
```

Install more packages. Choose option 3 to not update already-installed packages.
```r
devtools::install_github('richfitz/remake')
devtools::install_github('USGS-R/scipiper')
install.packages(c('dataRetrieval', 'sbtools'))
devtools::install_github('USGS-R/meddle')
devtools::install_github('GLEON/GLMr')
devtools::install_github('USGS-R/glmtools')
```

dssecrets: On your desktop/laptop, paste https://github.com/USGS-CIDA/dssecrets/archive/master.tar.gz into a browser to download. Move to Tallgrass like so:
```sh
scp ~/Downloads/dssecrets-master.tar.gz tallgrass.cr.usgs.gov:/caldera/projects/usgs/water/iidd/datasci/lake-temp/mntoha-data-release/tmp
```
Then install when back on Tallgrass
```r
devtools::install_github("gaborcsardi/secret")
install.packages("tmp/dssecrets-master.tar.gz", repos = NULL)
```

After doing all of the above, you should be able to just load the environment with
```sh
source activate mntoha_release
```
in future sessions.
