' brms_model_forecast_all.R
Perform prospective forecasting of BEDvdc using the
BRMS-based Bayesian model.

Usage:
    brms_model_forecasting_all.R -m MODEL -o OUTDIR [ --recompile ]

Options:
    -m --model MODEL            Path to model file (RDS)
    -o --outdir OUTDIR          Path to output directory
    --recompile                 Recompile the BRMS model
' -> doc

if (! interactive()) {
    library(docopt)
    args <- docopt(doc, version='BRMS model forecasting')
    print(args)
} else {
    message('Running in interactive mode. Be sure to specify args manually.')
}

library(dplyr)
library(readr)
library(tidyr)
library(brms)

source('src/brms_functions.R')


model <- readRDS(args[['--model']])
timepoints <- model$data %>%
    distinct(patient, EQD2) %>%
    arrange(patient, EQD2) %>%
    group_by(patient) %>%
    mutate(timepoint = row_number()) %>%
    ungroup() %>%
    distinct(patient, timepoint)

plyr::ddply(timepoints, c('patient', 'timepoint'), function(z) {
    my_patient <- unique(z$patient)
    timepoint <- unique(z$timepoint)
    outfile <- sprintf('%s/%s_%s.Rds', args[['--outdir']], my_patient, timepoint)
    tmpfile <- sprintf('%s/.%s_%s.tmp', args[['--outdir']], my_patient, timepoint)

    if (file.exists(outfile)) {
        message(sprintf('Output already exists for patient %s, timepoint %s, skipping...', my_patient, timepoint))
    } else if (file.exists(tmpfile)) {
        message(sprintf('Tmpfile exists for patient %s, timepoint %s, skipping...', my_patient, timepoint))
    } else {
        start_time <- Sys.time()
        writeLines('running', tmpfile)
        new_brms <- update_model(model, my_patient, timepoint)
        get_followup_prediction(new_brms, 'newpatient') %>%
            saveRDS(outfile)
        end_time <- Sys.time()
        message(sprintf('Output written to %s', outfile))
        message(sprintf('Time required %s', end_time - start_time))
    }
})



