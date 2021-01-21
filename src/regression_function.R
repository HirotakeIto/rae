#############################################
##  utility function for regression
#############################################

source("./src/lib.R")
source("./src/rdd.R")
# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

parametric_regression_felm = function(
  dfx, fm, choices_1level_factor,
  func_sampling = function(x) return(x),
  p_adjust = FALSE, num_p_adjust = NA
) {
  dfx %<>% func_sampling()
  if (cmdreg::can_do_analysis(dfx, fm) == FALSE) {
    stop("fail to check before analysis")
    }
  dfx_use = dfx %>% dplyr::select_(.dots = all.vars(fm)) %>% na.omit()
  fm_use_str = return_fm_str_excluding_1level_factor(fm = fm, data = dfx_use, choices_1level_factor = choices_1level_factor)
  result = execute_felm(fm = as.formula(fm_use_str), data = dfx_use)
  if (is.null(result)) {
    stop("result is null")
    }
  return(
    list(
      fm_use = fm_use_str,
      summary_tidy = broom::tidy(result),
      summary_glance = broom::glance(result) %>%
        dplyr::mutate(
          obs = result$N,
          fm_use = fm_use_str
        )
    )
  )
}

rdd_parametric_regression_felm = function(
  dfx, fm, choices_1level_factor,
  func_sampling = function(x) return(x),
  p_adjust = FALSE, num_p_adjust = NA
) {
  dfx %<>% get_within_wave_cutoff_about_relative_age() %>% func_sampling()
  if (cmdreg::can_do_analysis(dfx, fm) == FALSE) {
    stop("fail to check before analysis")
  }
  dfx_use = dfx %>% dplyr::select_(.dots = all.vars(fm)) %>% na.omit()
  fm_use_str = return_fm_str_excluding_1level_factor(fm = fm, data = dfx_use, choices_1level_factor = choices_1level_factor)
  result = execute_felm(fm = as.formula(fm_use_str), data = dfx_use)
  if (is.null(result)) {
    stop("result is null")
    }
  return(
    list(
      fm_use = fm_use_str,
      summary_tidy = broom::tidy(result),
      summary_glance = broom::glance(result) %>%
        dplyr::mutate(obs = result$N, fm_use = fm_use_str)
    )
  )
}

parametric_regression_felm_delta = function(
  dfx, fm, fm_delta, choices_1level_factor,
  func_sampling = function(x) return(x),
  p_adjust = FALSE, num_p_adjust = NA
) {
  dfx %<>% func_sampling()
  if (cmdreg::can_do_analysis(dfx, fm) == FALSE) {
    stop("fail to check before analysis")
    }
  dfx_use = dfx %>% dplyr::select_(.dots = all.vars(fm)) %>% na.omit()
  fm_use_str = return_fm_str_excluding_1level_factor(fm = fm, data = dfx_use, choices_1level_factor = choices_1level_factor)
  result = execute_felm(fm = as.formula(fm_use_str), data = dfx_use)
  result_delta = car::deltaMethod(result, fm_delta, rhs = 0)
  if (is.null(result)) {
    stop("result is null")
    }
  return(
    list(
      fm_use = fm_use_str,
      summary_tidy = broom::tidy(result),
      summary_glance = broom::glance(result) %>%
        dplyr::mutate(
          obs = result$N,
          fm_use = fm_use_str,
          delta_est = result_delta$Estimate,
          delta_se = result_delta$SE,
          delta_pvalue = result_delta$`Pr(>|z|)`
        )
    )
  )
}
