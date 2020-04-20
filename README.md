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
source activate lakes_pgdl
git pull
```

These targets should be built in the `lake-temperature-neural-networks` project:
```
2_model/log/pgdl_outputs.ind # or otherwise be sure the batch is complete
3_assess/out/posthoc_config.csv
3_assess/log/preds_holdout.ind
```
Results can now be packaged and posted using this repo (`mkdir log` if needed)
```sh
nohup Rscript -e "library(scipiper); scmake('out_data/pgdl_config.csv', force=TRUE)" > log/pgdl_config.out 2>&1 &
nohup Rscript -e "library(scipiper); scmake('tmp/5_pgdl_pretrain_predictions_zips.yml', force=TRUE)" > log/5_pgdl_pretrain_predictions_zips.out 2>&1 &
nohup Rscript -e "library(scipiper); scmake('out/5_pgdl_predictions_zips.yml', force=TRUE)" > log/5_pgdl_predictions_zips.out 2>&1 &

```
