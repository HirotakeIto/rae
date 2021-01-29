#############################################
##  Qunantile regression
#############################################¥

library(rlang)
source("./src/data_download.R")
source("./src/analysis_component.R")
source("./src/setting.R")
source("./src/lib.R")


func_sampling_school = function(dfx, type = c("prime", "junior")) {
  if (type == "prime") return(dfx %>% dplyr::filter(grade %in% c(4, 5, 6)))
  if (type == "junior") return(dfx %>% dplyr::filter(grade %in% c(7, 8, 9)))
}

quant_regression_felm_delta = function(
  dfx, fm, fm_delta, tau, choices_1level_factor, type_sampling = c("prime", "junior")
  ) {
  # example
  # fm = zselfcontrol ~ relative_age + I(relative_age^2)
  # fm_delta = "11 * relative_age  +  11 **2 * `I(relative_age^2)`"
  # grade_num = 4
  # tau = 0.8
  # choices_1level_factor = c("grade", "year")
  # start
  dfx_use = dfx %>% func_sampling_school(type = type_sampling) %>% dplyr::select_(.dots = all.vars(fm)) %>% na.omit()
  if (cmdreg::can_do_analysis(dfx_use, fm) == FALSE) {stop("fail to check before analysis")}
  fm_use_str = return_fm_str_excluding_1level_factor(fm = fm, data = dfx_use, choices_1level_factor = choices_1level_factor)
  result = execute_quantreg(fm = as.formula(fm_use_str), data = dfx_use, tau = tau,  method = "pfn")
  if (is.null(result)) {stop("result is null")}
  vcov = quantreg::summary.rq(result, se = "iid", cov = TRUE)$cov
  result.delta = car::deltaMethod(object = result$coefficients, fm_delta, vcov. = vcov)
  result.summary = summary(result, se = "iid")
  return(
    list(
      fm_use = fm_use_str, summary_tidy = broom::tidy(result, se = "iid"),
      summary_glance = broom::glance(result) %>%
        dplyr::mutate(
          obs = nrow(result$model),
          delta_est = result.delta$Estimate,
          delta_se = result.delta$SE,
          fm_use = fm_use_str
        )
    )
  )
}


quant_regression_felm_delta_some_taus = function(
  dfx, fm, fm_delta, choices_1level_factor, tau1=0.1, tau2=0.5, tau3=0.9, type_sampling = c("prime", "junior")
) {
  # example
  # fm = zkokugo_level ~ relative_age + I(relative_age^2)
  # fm_delta = "11 * relative_age  +  11 **2 * `I(relative_age^2)`"
  # tau1 = 0.8
  # tau2 = 0.6
  # tau3 = 0.9
  # type_sampling = "prime"
  # choices_1level_factor = c("grade", "year")
  # function
  evaluate_fm_from_rqmodel = function(model.rq, fm_str) {
    para = model.rq$coefficients
    para.names = names(para)
    para.names[1] = gsub("\\(Intercept\\)", "Intercept", para.names[1])
    g. = parse(text = fm_delta)
    envir = new.env()
    for (i in 1:length(para)) {
      assign(para.names[i], para[i], envir)
    }
    est = eval(g., envir)
    names(est) = NULL
    return(est)
  }

  boot_delta = function(dfx, i) {
    tryCatch({
      dfx.index = dfx[i, ]
      res.tau1 = execute_quantreg(fm = as.formula(fm_use_str), data = dfx.index, tau = tau1,  method = "pfn")
      res.tau2 = execute_quantreg(fm = as.formula(fm_use_str), data = dfx.index, tau = tau2,  method = "pfn")
      res.tau3 = execute_quantreg(fm = as.formula(fm_use_str), data = dfx.index, tau = tau3,  method = "pfn")
      res.tau1.delta = evaluate_fm_from_rqmodel(res.tau1)
      res.tau2.delta = evaluate_fm_from_rqmodel(res.tau2)
      res.tau3.delta = evaluate_fm_from_rqmodel(res.tau3)
      return(c(
        "res.tau1.delta" = res.tau1.delta, "res.tau2.delta" = res.tau2.delta, "res.tau3.delta" = res.tau3.delta,
        "tau1-tau2" = res.tau1.delta - res.tau2.delta, "tau2-tau3" = res.tau2.delta - res.tau3.delta,
        "tau1-tau3" = res.tau1.delta - res.tau3.delta
      ))
    },
    error = function(e) {
      return(NA)
    })
  }
  # start
  dfx_use = dfx %>% func_sampling_school(type = type_sampling) %>% dplyr::select_(.dots = all.vars(fm)) %>% na.omit()
  if (cmdreg::can_do_analysis(dfx_use, fm) == FALSE) {stop("fail to check before analysis")}
  fm_use_str = return_fm_str_excluding_1level_factor(fm = fm, data = dfx_use, choices_1level_factor = choices_1level_factor)
  # regression
  res.tau1 = execute_quantreg(fm = as.formula(fm_use_str), data = dfx_use, tau = tau1,  method = "pfn")
  res.tau2 = execute_quantreg(fm = as.formula(fm_use_str), data = dfx_use, tau = tau2,  method = "pfn")
  res.tau3 = execute_quantreg(fm = as.formula(fm_use_str), data = dfx_use, tau = tau3,  method = "pfn")
  if (is.null(res.tau1)) {stop("res.tau1 is null")}
  if (is.null(res.tau2)) {stop("res.tau2 is null")}
  if (is.null(res.tau3)) {stop("res.tau3 is null")}
  summary_tidy = purrr::map(list(res.tau1, res.tau2, res.tau3), ~ broom::tidy(.x)) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(fm_use = fm_use_str, fm_delta = fm_delta)
  summary_glance = purrr::map(list(res.tau1, res.tau2, res.tau3), ~ broom::glance(.x)) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(fm_use = fm_use_str, fm_delta = fm_delta)
  # set up bootstrap
  res.boot = boot::boot(dfx_use, statistic = boot_delta, R = 500)
  conf.interval = apply(res.boot$t, 2, function(x) quantile(x, c(0.025, 0.975))) %>%
    data.frame %>%
    t %>%
    data.frame %>%
    dplyr::mutate(term = names(res.boot$t0))
  summary_boot = broom::tidy(res.boot) %>%
    dplyr::left_join(conf.interval, by = "term") %>%
    dplyr::mutate(fm_use = fm_use_str, fm_delta = fm_delta)
  boot_item = res.boot$t %>%
    data.frame() %>%
    magrittr::set_colnames(names(res.boot$t0)) %>%
    dplyr::mutate(times_boot = 1:nrow(.), fm_use = fm_use_str, fm_delta = fm_delta)
  return(
    list(
      fm_use = fm_use_str,
      summary_tidy = summary_tidy, summary_glance = summary_glance,
      summary_boot = summary_boot, boot_item = boot_item
    )
  )
}

AnalysisEnvironmentT22 = R6::R6Class(
  inherit = cmdreg::RegressionAnalysisEnvironment,
  lock_objects = FALSE,
  public = list(
    target = character(),
    tau = numeric(),
    taus = numeric(),
    type_analysis = c("rq", "rqboot"),
    type_sampling = c("prime", "junior"),
    choices_1level_factor = c("school_id_prime", "grade_prime", "year_prime"),
    # setter and getter for formula
    get_fm_template = function() "{dependent} ~ relative_age + I(relative_age^2)",
    get_fm = function() strformat(self$get_fm_template(), c(dependent = self$target)) %>% as.formula(),
    set_fm = function() warning("setter for fm is not working"),
    # formula for delta method
    g_delta_method = stringi::stri_c("11 * relative_age  +  11 **2 * `I(relative_age^2)`", sep = ""),
    fm_delta_name = "relative_age_effect",
    get_g_delta_method = function() {self$g_delta_method},
    analyze_rq = function(dfx) {
      return(quant_regression_felm_delta(
        dfx = dfx, tau = self$tau,
        fm = self$get_fm(), fm_delta = self$get_g_delta_method(),
        choices_1level_factor = self$choices_1level_factor,
        type_sampling = self$type_sampling
      ))
    },
    analyze_rqboot = function(dfx) {
      return(quant_regression_felm_delta_some_taus(
        dfx = dfx, fm = self$get_fm(), fm_delta = self$get_g_delta_method(),
        tau1 = 0.1, tau2 = 0.5, tau3 = 0.9,
        type_sampling = self$type_sampling,
        choices_1level_factor = self$choices_1level_factor
      ))
    },
    analyze_base = function(dfx) {
      if (self$type_analysis == "rq") return(self$analyze_rq(dfx))
      if (self$type_analysis == "rqboot") return(self$analyze_rqboot(dfx))
      stop("type is in parametric, rdd")
    }
  )
)

main = function(save_folder_basis) {
  # sample
  # dfx = readr::read_csv("./notebooks/Yamaguchi/RAE/data/dataset1.csv", guess_max = 2000000)
  # dt_sample = data_download$usedata_extract(data_download$setup_data(dfx), flag_dryrun = 1)
  # grades = c(9); targets = c("zmath_level"); taus = c(0.9)
  dt_sample = download_saitama(flag_dryrun = FALSE)
  # build analysis instance
  targets = c(
    c("zgakuryoku", "zkokugo_level", "zmath_level", "zeng_level", "zstrategy"),
    c("zselfcontrol", "zselfefficacy", "zdilligence")
  )
  ana_envs = c()
  type_analysis = "rq"
  for (type_sampling in c("prime", "junior")) {
    for (target in targets) {
      for (tau in c(0.1, 0.5, 0.9)) {
        ana_env = AnalysisEnvironmentT22$new(
          target = target, tau = tau, type_sampling = type_sampling,
          type_analysis = type_analysis, tag = c(type_analysis, target, type_sampling, tau)
        )
        ana_envs = c(ana_envs, ana_env)
      }
    }
  }
  type_analysis = "rqboot"
  for (type_sampling in c("prime", "junior")) {
    for (target in targets) {
      ana_env = AnalysisEnvironmentT22$new(
        target = target, type_sampling = type_sampling,
        type_analysis = type_analysis, tag = c(type_analysis, target, type_sampling)
      )
      ana_envs = c(ana_envs, ana_env)
    }
  }
  # execute analysis
  for (ana_env in ana_envs) {
    print(ana_env$name())
    ana_env$analyze(dfx = dt_sample)
  }
  # save
  summary_tidy = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(
      ~ .x$result$summary_tidy %>% dplyr::mutate(name = .x$name())
      ) %>%
    purrr::reduce(dplyr::bind_rows)
  summary_glance = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(
      ~ .x$result$summary_glance %>% dplyr::mutate(name = .x$name()) %>% dplyr::select(-logLik)
      ) %>%
    purrr::reduce(dplyr::bind_rows)
  summary_boot = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::discard(~ .x$type_analysis != "rqboot") %>%
    purrr::map(
      ~ .x$result$summary_boot %>% dplyr::mutate(name = .x$name())
      ) %>%
    purrr::reduce(dplyr::bind_rows)
  boot_item = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::discard(~ .x$type_analysis != "rqboot") %>%
    purrr::map(
      ~ .x$result$boot_item %>% dplyr::mutate(name = .x$name())
      ) %>%
    purrr::reduce(dplyr::bind_rows)
  savefolder = file.path(save_folder_basis, "t22")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))
  write.csv(x = summary_boot, file = file.path(savefolder, "summary_boot.csv"))
  write.csv(x = boot_item, file = file.path(savefolder, "boot_item.csv"))

}
