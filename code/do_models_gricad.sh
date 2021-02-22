#!/usr/bin/env bash

#OAR -n ddm
#OAR -l /nodes=1/core=8,walltime=48:00:00
#OAR --stdout ddm.out
#OAR --stderr ddm.err
#OAR --project deeppredspeech

source /applis/environments/conda.sh
#! conda create -n renv r-essentials
conda activate renv

#! conda install parallel
#! conda install brms
#! conda install -c conda-forge r-brms
#! conda install r-tidyverse
#! conda install r-brms

Rscript /bettik/PROJECTS/pr-deeppredspeech/R/models_gricad.R

exit 0
