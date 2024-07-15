update_model <- function(model, my_patient, timepoint) {
    stopifnot(timepoint >= 1)

    data_singlepatient <- model$data %>%
      filter(patient == my_patient) %>%
      arrange(EQD2) %>%
      filter(row_number() %in% 1:timepoint) %>%
      mutate(patient = 'newpatient')

    newdata <- model$data %>%
      filter(patient != my_patient) %>%
      bind_rows(data_singlepatient)

    new_brms <- update(
      model,
      newdata = newdata,
      iter = 1000, chains = 4, cores = 4,
      recompile = args[['--recompile']]
    )
    return(new_brms)
}

get_followup_prediction <- function(model, my_patient) {
  model$data %>%
    filter(patient == my_patient) %>%
    select(-EQD2, -censored, -ctdna, -follow_up) %>%
    distinct() %>%
    mutate(follow_up = 1, EQD2 = 60) %>%
    posterior_predict(model, newdata = .)
}

get_detectability_curve <- function(brm_obj, selected_patient) {
  df_patient <- brm_obj$data %>%
    filter(patient == selected_patient) %>%
    select(-EQD2, -ctdna) %>%
    distinct() %>%
    crossing(EQD2 = seq(0, 100, 0.1))
  
  m_predictions <- posterior_predict(
    brm_obj,
    df_patient
  )
  
  colnames(m_predictions) <- seq(0, 100, 0.1)
  
  return(
    apply(m_predictions, 2, function(z) {sum(z > 0) / length(z)}) %>%
      as_tibble(rownames = 'EQD2') %>%
      mutate(EQD2 = as.numeric(EQD2))
  )
}

