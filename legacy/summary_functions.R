
# Functions
pval_to_string <- function(p) {
  if_else(p<0.001, 'p < 0.001', sprintf('p=%#.3g', p))
}

value_to_string <- function(r) {
  return(sprintf('%#.3g', r))
}

value_to_percent <- function(r) {
  return(paste0(sprintf('%#.3g%%', r*100)))
}

median_range <- function(x) {
  return(
    sprintf(
      '%s [%s, %s]',
      median(x, na.rm=T) %>% value_to_string,
      min(x, na.rm=T) %>% value_to_string,
      max(x, na.rm=T) %>% value_to_string
    )
  )
}

cor_test_string <- function(x, y, method='spearman') {
  cortest <- cor.test(x, y, method = method)
  if (method == 'spearman') {
    return(sprintf(
      'Spearman coefficient = %s, %s', 
      cortest$estimate %>% value_to_string,
      cortest$p.value %>% pval_to_string
    ))
  } else if (method == 'pearson' & length(x) > 4 & length(y) > 4) {
    return(sprintf(
      'Pearson r = %s [%s, %s], %s', 
      cortest$estimate %>% value_to_string,
      cortest$conf.int[1] %>% value_to_string,
      cortest$conf.int[2] %>% value_to_string,
      cortest$p.value %>% pval_to_string
    ))
  } else {
    return(sprintf(
      'Pearson r = %s, %s', 
      cortest$estimate %>% value_to_string,
      cortest$p.value %>% pval_to_string
    ))
  }
}

get_lor <- function(glm_output, parameter) {
  return(sprintf(
    '%s [%s], %s',
    coef(glm_output)[[parameter]] %>% value_to_string,
    confint(glm_output)[parameter, ] %>% value_to_string %>% paste(collapse = ' - '),
    glm_output %>% summary %>% coefficients %>% .[parameter, 'Pr(>|t|)'] %>% pval_to_string
  ))
}