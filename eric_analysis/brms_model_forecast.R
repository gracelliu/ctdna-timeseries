' brms_model_forecast.R
Perform prospective forecasting of BEDvdc using the
BRMS-based Bayesian model.

Usage:
    brms_model_forecasting.R -m MODEL -p PATIENT -t TIMEPOINT -o OUTPUT [ --recompile ]

Options:
    -m --model MODEL            Path to model file (RDS)
    -p --patient PATIENT        ID of patient
    -t --timepoint TIMEPOINT    Timepoint to model, e.g. 1, 2, 3...
    -o --output OUTPUT          Path to output table
    --recompile                 Recompile the BRMS model
' -> doc

library(dplyr)
library(readr)
library(tidyr)
library(brms)

source('brms_functions.R')

if (! interactive()) {
    library(docopt)
    args <- docopt(doc, version='BRMS model forecasting')
    print(args)
} else {
    message('Running in interactive mode. Be sure to specify args manually.')
}

model <- readRDS(args[['--model']])
my_patient <- args[['--patient']]
timepoint <- as.integer(args[['--timepoint']])

new_brms <- update_model(model, my_patient, timepoint)
get_detectability_curve(new_brms, 'newpatient') %>%
  write_csv(args[['--output']])
