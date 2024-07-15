
#############
# Functions #
#############
plot_conditional_effects_eqd2 <- function(model, interaction, interaction_name, indep_var='EQD2', dpar=NULL) {
  conditional_effects(model, effects = indep_var, interaction, dpar=dpar) %>%
    .[[indep_var]] %>%
    dplyr::rename(xvar = 1) %>%
    ggplot(aes(
      xvar,
      y = estimate__,
      #ymin = lower__,
      #ymax = upper__
    )) +
    #geom_ribbon(aes(
    #  fill = cond__
    #  ), alpha = 0.1
    #) +
    geom_line(aes(
      color = cond__
    )) +
    scale_color_brewer(palette = 'Set1') +
    scale_fill_brewer(palette = 'Set1') +
    labs(
      x = indep_var,
      y = 'HPV DNA level',
      color = interaction_name,
      fill = interaction_name,
    )
}


get_detectability_curve <- function(brm_obj, selected_patient) {
  df_patient <- brm_obj$data %>%
    filter(patient == selected_patient) %>%
    select(-EQD2, -ctdna) %>%
    distinct() %>%
    crossing(EQD2 = seq(0, 100, 1))
  
  m_predictions <- posterior_predict(
    brm_obj,
    df_patient
  )
  
  colnames(m_predictions) <- seq(0, 100, 1)
  
  return(
    apply(m_predictions, 2, function(z) {sum(z > 0) / length(z)}) %>%
      as_tibble(rownames = 'EQD2') %>%
      mutate(EQD2 = as.numeric(EQD2))
  )
}

summarise_rtdclear <- function(df, clearance=T, plot=F, estimate_p=0.5, upper_p=0.75, lower_p=0.25) {
  if (clearance) {
    df <- df %>% mutate(value = 1 - value)
  }
  model_spline <- with(df, smooth.spline(EQD2, value, df=10))
  
  val_RTDclear_estimate = uniroot.all(function(x) {
    predict(model_spline, x, deriv = 0)$y - estimate_p
    }, interval = c(0, 100)) %>% max()
  
  val_RTDclear_upper = uniroot.all(function(x) {
    predict(model_spline, x, deriv = 0)$y - upper_p
    }, interval = c(0, 100)) %>% max()
  
  val_RTDclear_lower = uniroot.all(function(x) {
    predict(model_spline, x, deriv = 0)$y - lower_p
    }, interval = c(0, 100)) %>% max()
  
  fit <- predict(model_spline)
  fit_spline <- tibble(
    EQD2 = fit$x,
    smoothed = fit$y
  )
    
  output = list(
    values = tibble(
      RTDclear = if_else(is.infinite(val_RTDclear_estimate), 0, val_RTDclear_estimate),
      uci = if_else(is.infinite(val_RTDclear_upper), Inf, val_RTDclear_upper),
      lci = if_else(is.infinite(val_RTDclear_lower), 0, val_RTDclear_lower)
    ),
    model_spline = model_spline,
    fit_spline = fit_spline
  )
  
  if (plot) {
    p <- tibble(
      EQD2 = predict(model_spline)$x,
      smoothed = predict(model_spline)$y
    ) %>%
      ggplot(aes(
        x = EQD2,
        y = smoothed
      )) +
      geom_point(aes(
        x = EQD2,
        y = value
        ),
        data = df,
        color = 'grey80'
      ) +
      geom_line()
    
    if (!is.infinite(val_RTDclear_estimate)) {
      p <- p + annotate(
        geom='point',
        x = val_RTDclear_estimate,
        y = estimate_p,
        size = 5,
        color = 'black'
      )
    }
    if (!is.infinite(val_RTDclear_lower)) {
      p <- p + annotate(
        geom='point',
        x = val_RTDclear_lower,
        y = lower_p,
        size = 5,
        color = 'black'
      )
    }
    if (!is.infinite(val_RTDclear_upper)) {
      p <- p + annotate(
        geom='point',
        x = val_RTDclear_upper,
        y = upper_p,
        size = 5,
        color = 'black'
      )
    }
    output$plot <- p
  }
    
  return(output)
}


get_fixed_effects <- function(brms_output, vector_fixedeffects, df_fixedeffects_names) {
  brms::as_draws_df(
    brms_output,
    vector_fixedeffects
  ) %>%
    as_tibble() %>%
    gather(parameter, estimate, -.iteration, -.chain, -.draw) %>%
    mutate(parameter = factor(parameter, levels = vector_fixedeffects)) %>%
    left_join(df_fixedeffects_names)
}


plot_trace <- function(chain_data) {
  p_traceplots <- chain_data %>%
    mutate(
      .chain = factor(.chain),
      label = sprintf('%s: %s', model, name)
    ) %>%
    ggplot(aes(
      x = .iteration,
      y = estimate,
      color = .chain
    )) +
    facet_wrap(~label, scales = 'free_y') +
    geom_line(alpha = 0.5) +
    scale_color_brewer(palette = 'Set1')
  return(p_traceplots)
}



plot_fixedeffects_compact <- function(df_chain) {
  plotdata_fixedeffects <- df_chain %>%
    group_by(parameter) %>%
    mutate(
      estimate_normalized = estimate / sd(estimate)
    ) %>%
    ungroup() %>%
    mutate(
      normalized_cut = cut(
        estimate_normalized,
        seq(min(estimate_normalized), max(estimate_normalized), 0.1)
      ),
      bin = as.numeric(normalized_cut) * 0.1 + min(estimate_normalized) - 0.05,
      parameter_group = case_when(
        model == 'Cor' ~ '4. Correlated random effects',
        grepl('Post-RT', label) ~ '3. Post-treatment levels',
        grepl('EQD2', label) ~ '2. On-treatment kinetics',
        TRUE ~ '1. Baseline HPV DNA levels'
      ) %>%
        factor(
          levels = c(
            '1. Baseline HPV DNA levels',
            '2. On-treatment kinetics',
            '3. Post-treatment levels',
            '4. Correlated random effects'
          )
        )
    )
  
  plotdata_fixedeffects_summary <- plotdata_fixedeffects %>%
    group_by(parameter, label, model, parameter_group) %>%
    summarise(
      mean = mean(estimate),
      lci = quantile(estimate, 0.025),
      uci = quantile(estimate, 0.975),
      mean_normalized = mean(estimate_normalized),
      lci_normalized = quantile(estimate_normalized, 0.025),
      uci_normalized = quantile(estimate_normalized, 0.975)
    ) %>%
    ungroup() %>%
    mutate(
      signif = !(0 > lci & 0 < uci),
      signif_marker = if_else(signif, '*', ''),
      text_label = sprintf(
        '%s (%s, %s) %s',
        signif(mean, 3),
        signif(lci, 3),
        signif(uci, 3),
        signif_marker
      ),
      text_x = if_else(mean_normalized < 0, uci_normalized + 0.3, lci_normalized - 0.3),
      text_hjust = if_else(mean_normalized < 0, 0, 1)
    )
  
  plotdata_fixedeffects_binned <- plotdata_fixedeffects %>%
    group_by(parameter, label, model, bin, parameter_group) %>%
    summarise(
      n = n()
    ) %>%
    ungroup()
  
  plotdata_fixedeffects_binned %>%
    ggplot(aes(
      x = bin,
      y = label
    )) +
    geom_tile(aes(
      fill = n
    )) +
    geom_errorbarh(aes(
      x = mean_normalized,
      xmin = lci_normalized,
      xmax = uci_normalized,
      y = label
    ),
    data = plotdata_fixedeffects_summary,
    color = 'Grey40',
    height = 0.5
    ) +
    geom_text(aes(
      x = 9,
      label = text_label,
      hjust = 0
    ), data = plotdata_fixedeffects_summary, color = 'Grey40', size = 3) +
    scale_fill_distiller(palette = 'Reds', direction = 1) +
    geom_vline(xintercept = 0, color = 'black') +
    labs(
      x = 'Posterior (normalized to SD = 1)',
      y = 'Parameter'
    ) +
    ggforce::facet_col(
      facets = vars(parameter_group), 
      scales = "free_y", 
      space = "free"
    ) +
    theme(strip.text.y = element_text(angle=0, hjust = 0)) +
    xlim(
      min(plotdata_fixedeffects_binned$bin, na.rm=T),
      22
    ) +
    theme(legend.position = 'none')
}

plot_posterior_predictive_check <- function(brms_object, show_censored=F) {
  censor_limit = min(brms_object$data$ctdna)
  
  if (show_censored) {
    df_prediction <- brms_object$data %>%
      mutate(
        ctdna = if_else(censored=='left', 0, ctdna),
        model_prediction = colMeans(posterior_predict(brms_object, newdata = NULL, ndraws = 1)) %>% as.numeric,
        follow_up = if_else(follow_up == 1, 'Follow-up', 'Pre-/On-RT')
      )
  } else {
    df_prediction <- brms_object$data %>%
      mutate(
        ctdna = if_else(censored=='left', 0, ctdna),
        model_prediction = colMeans(posterior_predict(brms_object, newdata = NULL, ndraws = 1)) %>%
          as.numeric %>%
          if_else(. < censor_limit, 0, .),
        follow_up = if_else(follow_up == 1, 'Follow-up', 'Pre-/On-RT')
      )
  }
  
  plot_model_prediction_scatter <- df_prediction %>%
    ggplot(aes(
      x = ctdna,
      y = model_prediction
    )) +
    geom_point(aes(color = follow_up)) +
    labs(
      x = 'HPV ctDNA copies/mL',
      y = 'Model Prediction',
      color = 'Timepoint'
    ) +
    geom_abline(intercept = 0, slope = 1, color = 'black') +
    scale_x_continuous(
      trans = scales::pseudo_log_trans(sigma=0.1),
      breaks = c(0, 10^(0:10)),
      labels = c('Undetect.', sprintf("%.20g", c(10^(0:10))))
    ) +
    scale_y_continuous(
      trans = scales::pseudo_log_trans(sigma=0.1),
      breaks = c(0, 10^(0:10)),
      labels = c('Undetect.', sprintf("%.20g", c(10^(0:10))))
    ) +
    labs(color = '') +
    theme(legend.position='top') +
    scale_color_brewer(palette = 'Set1')
  
  plot_model_prediction_lines <- df_prediction  %>%
    rename(
      `Actual Value` = ctdna,
      `Model Prediction` = model_prediction
    ) %>%
    gather(type, value, `Actual Value`, `Model Prediction`) %>%
    mutate(type = factor(type, levels = c('Model Prediction', 'Actual Value'))) %>%
    ggplot(aes(
      x = EQD2,
      y = value,
      group = patient
    )) +
    facet_wrap(~ type) +
    geom_line(alpha = 0.5, color = 'grey70', alpha = 0.5) +
    geom_point(aes(
      color = follow_up
    )) +
    scale_y_continuous(
      trans = scales::pseudo_log_trans(sigma=0.1),
      breaks = c(0, 10^(0:10)),
      labels = c('Undetect.', sprintf("%.20g", c(10^(0:10))))
    ) +
    labs(
      x = 'RT dose (EQD2)',
      y = 'HPV ctDNA (copies/mL)',
      color = 'Timepoint'
    ) +
    scale_color_brewer(palette = 'Set1') +
    theme(legend.position='top')
  
  return(list(
    scatterplot = plot_model_prediction_scatter,
    lineplot = plot_model_prediction_lines
  ))
}


# -------------------------------------------------------------------- #
# Posterior predictive distribution for one patient with overlaid data #
# -------------------------------------------------------------------- #

plot_posterior_prediction <- function(brms_model, chosen_patient, ymax = NULL, sigma = 0.01, fill_scale = scale_fill_gradient(low = '#FFFFFF', high='#A63603')) {
  y_breaks = 10^(seq(log10(sigma), 100, 3))
  y_scale = scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = sigma/10, base=10),
    breaks = c(0, y_breaks),
    labels = c('Undetect.', as.character(y_breaks)),
    limits = c(0, ymax)
  )
  
  detection_threshold = min(brms_model$data$ctdna)
  
  val_min_ctdna <- min(brms_model$data$ctdna)
  
  pd_test_eqd2 <- brms_model$data %>%
    filter(patient == chosen_patient) %>%
    select(-EQD2, -ctdna, -censored) %>%
    filter(follow_up == 0) %>%
    distinct() %>%
    crossing(EQD2 = 1:60)
  
  posterior_prediction_eqd2 = posterior_predict(
    brms_model,
    newdata = pd_test_eqd2
  )
  
  if (is.null(ymax)) {
    ymax = max(posterior_prediction_eqd2)
  }
  
  pd_test_followup <- brms_model$data %>%
    filter(patient == chosen_patient) %>%
    select(-EQD2, -ctdna, -censored, -follow_up) %>%
    crossing(EQD2 = 60, follow_up = seq(0, 1, 0.05))
  
  posterior_prediction_followup = posterior_predict(
    brms_model,
    newdata = pd_test_followup
  )
  
  pd_posterior_eqd2 <- pd_test_eqd2 %>% mutate(
    mean = apply(posterior_prediction_eqd2, 2, median),
    lci = apply(posterior_prediction_eqd2, 2, function(z) {sort(z)[round(length(z) * 0.025)]}),
    uci = apply(posterior_prediction_eqd2, 2, function(z) {sort(z)[round(length(z) * 0.975)]})
  )
  cut_values_eqd2 <- seq(log10(min(posterior_prediction_eqd2)), log10(max(posterior_prediction_eqd2)), 0.15)
  
  colnames(posterior_prediction_eqd2) <- pd_test_eqd2$EQD2
  p_prediction_heatmap_eqd2 <- posterior_prediction_eqd2 %>%
    log10() %>%
    apply(2, function(z) {table(cut(z, breaks=cut_values_eqd2))}) %>%
    as_tibble(rownames = 'bin') %>%
    gather(EQD2, count, -bin) %>%
    mutate(EQD2 = as.numeric(EQD2)) %>%
    mutate(
      hpv_dna_lower = gsub('.(.*?),.*', '\\1', bin) %>% as.numeric %>% {10^.},
      hpv_dna_upper = gsub('.*?,(.*)', '\\1', bin) %>% substr(1, nchar(.)-1) %>% as.numeric %>% {10^.}
    ) %>%
    ggplot() +
    geom_rect(aes(
      xmin = EQD2-0.5,
      xmax = EQD2+0.5,
      ymin = hpv_dna_lower,
      ymax = hpv_dna_upper,
      fill = count
    )) +
    fill_scale +
    y_scale +
    labs(
      x = 'RT dose (Gy EQD2)',
      y = 'HPV DNA'
    ) +
    geom_line(
      aes(
        x = EQD2,
        y = ctdna
      ),
      data = brms_model$data %>%
        filter(patient == chosen_patient, follow_up == 0),
      color = '#0000DD',
      size = 1.5
    ) +
    geom_point(
      aes(
        x = EQD2,
        y = ctdna
      ),
      data = brms_model$data %>%
        filter(patient == chosen_patient, follow_up == 0),
      color = '#0000DD',
      size = 3
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank()
    )
  
  cut_values_followup <- seq(
    log10(min(posterior_prediction_followup)),
    log10(max(posterior_prediction_followup)),
    0.15
  )
  
  colnames(posterior_prediction_followup) <- pd_test_followup$follow_up
  pd_prediction_heatmap_followup <- posterior_prediction_followup %>%
    log10() %>%
    apply(2, function(z) {table(cut(z, breaks=cut_values_followup))}) %>%
    as_tibble(rownames = 'bin') %>%
    gather(follow_up, count, -bin) %>%
    mutate(follow_up = as.numeric(follow_up)) %>%
    mutate(
      hpv_dna_lower = gsub('.(.*?),.*', '\\1', bin) %>% as.numeric %>% {10^.},
      hpv_dna_upper = gsub('.*?,(.*)', '\\1', bin) %>% substr(1, nchar(.)-1) %>% as.numeric %>% {10^.}
    )
  
  p_prediction_heatmap_followup <- pd_prediction_heatmap_followup %>%
    ggplot() +
    geom_rect(aes(
      xmin = follow_up-0.025,
      xmax = follow_up+0.025,
      ymin = hpv_dna_lower,
      ymax = hpv_dna_upper,
      fill = count
    )) +
    fill_scale +
    y_scale +
    scale_x_continuous(
      breaks = c(0, 1),
      labels = c('Post-RT', 'Follow-up'),
      limits = c(-0.2, 1.3)
    ) +
    geom_point(
      aes(
        x = follow_up,
        y = ctdna
      ),
      data = brms_model$data %>%
        filter(patient == chosen_patient, follow_up == 1) %>%
        mutate(ctdna = if_else(censored == 'left', 0, ctdna)),
      color = '#0000DD',
      size = 3
    ) +
    geom_boxplot(
      aes(x = 1, y = estimates),
      width = 0.2, color = 'black', fill = NA, outlier.shape=NA,
      data = tibble(
        estimates = posterior_prediction_followup[,ncol(posterior_prediction_followup)]
      )
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.title.x = element_blank()
    )
  
  p_histogram <- tibble(
    estimates = posterior_prediction_followup[,ncol(posterior_prediction_followup)]
  ) %>%
    ggplot(aes(
      x = 0,
      y = estimates
    )) +
    geom_violin(fill = '#de8a64', color = NA) +
    geom_boxplot(width = 0.2, color = 'black', fill = NA, outlier.shape=NA) +
    y_scale +
    scale_x_continuous(breaks = 0, label = 'pfHPVDNA\ndensity') +
    geom_hline(yintercept = detection_threshold, color = 'black') +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = element_blank()
    )
    
  
  plot_grid(
    p_prediction_heatmap_eqd2 +
      theme(legend.position = 'none'),
    p_prediction_heatmap_followup +
      theme(
        legend.position = 'none',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank()
      ),
    #p_histogram,
    nrow = 1,
    rel_widths = c(4,2),
    align = 'h'
  )
}


# ----------------------------------------- #
# Dynamic update figure for a given patient #
# ----------------------------------------- #

plot_dynamic <- function(chosen_patient, brms_obj, data_ctdna, data_rtdclear, plot_title = NULL, update_model=F) {
  if (is.null(plot_title)) {
    plot_title = sprintf('Patient: %s', chosen_patient)
  }
  
  if (update_model) {
    brms_obj <- update(
      brms_obj,
      newdata = bind_rows(
        brms_ctdna_eqd2_gtv$data %>%
          filter(patient != chosen_patient),
        brms_ctdna_eqd2_gtv$data %>%
          filter(patient == chosen_patient) %>%
          filter(EQD2 == min(EQD2)) %>%
          mutate(patient = 'newpatient')
      ),
      chains = 4,
      cores = 4,
      iter = 300
    )
    
    m_predicted <- posterior_predict(
      brms_obj,
      newdata = brms_obj$data %>%
        filter(patient == 'newpatient') %>%
        select(-ctdna, -EQD2) %>%
        crossing(EQD2 = 1:80),
      allow_new_levels = T
    )
  } else {
    m_predicted <- posterior_predict(
      brms_obj,
      newdata = brms_obj$data %>%
        filter(patient == chosen_patient) %>%
        mutate(patient = 'newpatient') %>%
        select(-ctdna, -EQD2) %>%
        crossing(EQD2 = 1:80),
      allow_new_levels = T
    )
  }
  
  bins = 10^seq(0, log10(max(m_predicted)), log10(max(m_predicted))/50)
  
  p_hpvdna <- apply(
    m_predicted,
    2,
    function(z) {
      zero_fraction = sum(z == 0) / length(z)
      bin_counts = table(cut(z, bins))
      bin_counts / sum(bin_counts) * (1-zero_fraction)
    }
  ) %>%
    as_tibble(rownames = 'bin') %>%
    mutate(
      bin_start = gsub('.(.*?),.*', '\\1', bin) %>% as.numeric,
      bin_end = gsub('.*?,(.*?)', '\\1', bin) %>% substr(1, nchar(.)-1) %>% as.numeric
    ) %>%
    gather(EQD2, value, -bin, -bin_start, -bin_end) %>%
    mutate(EQD2 = gsub('V', '', EQD2) %>% as.numeric) %>%
    ggplot() +
    geom_rect(
      aes(
        xmin = EQD2-0.5,
        xmax = EQD2+0.5,
        ymin = bin_start,
        ymax = bin_end,
        fill = value
      )
    ) +
    scale_fill_distiller(palette = 'Blues', direction=1) +
    xlim(0, data_ctdna %>% filter(patient == chosen_patient) %>% .$EQD2 %>% max) +
    scale_y_continuous(
      trans=scales::pseudo_log_trans(),
      breaks = c(0, 10^(1:10))
      #limits = c(0, data_ctdna %>% filter(patient == chosen_patient) %>% .$ctdna %>% max)
    ) +
    geom_line(
      aes(
        x = EQD2,
        y = ctdna
      ),
      data = data_ctdna %>%
        filter(patient == chosen_patient),
      color = 'black'
    ) +
    geom_point(
      aes(
        x = EQD2,
        y = ctdna
      ),
      data = data_ctdna %>%
        filter(patient == chosen_patient),
      size = 3
    ) +
    geom_text_repel(
      aes(
        x = EQD2,
        y = ctdna,
        label = value_to_string(ctdna)
      ),
      data = data_ctdna %>%
        filter(patient == chosen_patient)
    ) +
    labs(
      fill = 'Expected\nHPV DNA\ndensity'
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.x= element_blank(),
      panel.grid.major.y = element_blank(),
      panel.background = element_blank()
    )
    
  p_hpvdna_zero <- apply(
    m_predicted,
    2,
    function(z) {
      zero_fraction = sum(z == 0) / length(z)
    }
  ) %>%
    tibble(
      EQD2 = 1:ncol(m_predicted),
      zero_fraction = .
    ) %>%
    ggplot(aes(
      x = EQD2,
      y = zero_fraction
    )) +
    geom_line() +
    xlim(0, data_ctdna %>% filter(patient == chosen_patient) %>% .$EQD2 %>% max) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1)
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = 'Grey90', linetype = 'solid')
    ) +
    labs(
      y = 'p(clear)'
    )
  
  p_case_rtdclear <- data_rtdclear %>%
    filter(patient == chosen_patient) %>%
    ggplot(aes(
      x = EQD2,
      y = RTDclear
    )) +
    geom_line(color = 'grey70') +
    geom_errorbar(
      aes(
        ymin = lci,
        ymax = uci
      ),
      width = 0
    ) +
    geom_point(size=2) +
    labs(
      y = 'RTDclear (Gy)'
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linetype = 'solid'),
      panel.background = element_blank()
    )
  
  p_case_risk <- data_rtdclear %>%
    filter(patient == chosen_patient) %>%
    ggplot(aes(
      x = EQD2,
      y = recurrence_risk
    )) +
    geom_line(color = 'grey70') +
    geom_errorbar(
      aes(
        ymin = recurrence_risk_lci,
        ymax = recurrence_risk_uci,
        color = risk_group
      ),
      width = 0
    ) +
    geom_point(aes(
      color = risk_group
    ), size=2) +
    geom_text(
      aes(
        x = 0,
        y = risk_group,
        label = name
      ),
      hjust = 0,
      data = tibble(
        risk_group = vector_risk_groups[2:length(vector_risk_groups)], 
        name = vector_risk_group_names
      ),
      size = 3,
      color = 'Grey70',
      nudge_y = -4
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linetype = 'solid'),
      panel.background = element_blank()
    ) +
    labs(
      x = 'RT dose (Gy, EQD2)',
      y = 'Risk (%)',
      color = 'Risk group'
    ) +
    scale_y_continuous(
      breaks = vector_risk_groups %>% sort,
      labels = value_to_percent(vector_risk_groups/100),
      limits = c(0, 100)
    ) +
    scale_color_manual(values=palette_risk_group, drop=F)
  
  plot_grid(
    p_hpvdna + theme(legend.position = 'none') + ggtitle(plot_title),
    p_hpvdna_zero,
    p_case_rtdclear,
    p_case_risk + theme(legend.position = 'none'),
    ncol = 1,
    rel_heights = c(2.5,1,2,4),
    align = 'v'
  )
}
