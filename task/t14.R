#############################################
##  Estimate effects of Age on *something* using Saitama Vital Statics
#############################################¥
library(rlang)
source("./src/data_download.R")
source("./src/regression_function.R")
source("./src/setting.R")

# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

func_sampling_all = function(dfx) {
  print("sampling all")
  return(dfx)
}

func_sampling_apr_march = function(dfx) {
  print("sampling apr_march")
  return(dfx %>% dplyr::filter(!!sym(relative_age_col) %in% c(0, 11)))
}

AnalysisEnvironmentT24 <- R6::R6Class(
  inherit = cmdreg::RegressionAnalysisEnvironment,
  lock_objects = FALSE,
  public = list(
    type_formula = c("ols", "no_live", "ols_liner", "no_live_liner", "rdd"),
    type_sampling = c("all", "apr_march"),
    target = character(),
    choices_1level_factor = c("year"),
    num_p_adjust = NaN,
    get_p_adjust = function() {if (self$num_p_adjust) TRUE  else FALSE},
    # setter and getter for formula
    get_fm_ols = function() "{dependent} ~ 1 + relative_age + I(relative_age^2) + as.factor(year) | lived_city | 0 | lived_city" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm_no_live = function() "{dependent} ~ 1 + relative_age + I(relative_age^2) + as.factor(year) | 0 | 0 | 0" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm_ols_liner = function() "{dependent} ~ 1 + relative_age + as.factor(year) | lived_city | 0 | lived_city" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm_no_live_liner = function() "{dependent} ~ 1 + relative_age + as.factor(year) | 0 | 0 | 0" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm_rdd = function() "{dependent} ~  1 + distance_cutoff + upper_cutoff + distance_cutoff * upper_cutoff | 0 | 0 | 0" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm = function() {
      if (self$type_formula == "ols")  return(self$get_fm_ols())
      if (self$type_formula == "no_live")  return(self$get_fm_no_live())
      if (self$type_formula == "ols_liner")  return(self$get_fm_ols_liner())
      if (self$type_formula == "no_live_liner")  return(self$get_fm_no_live_liner())
      if (self$type_formula == "rdd")  return(self$get_fm_rdd())
      stop("Error, type_formula is not valid")
    },
    # formula for delta method
    g_delta_method = stringi::stri_c("11 * relative_age  +  11 **2 * `I(relative_age^2)`", sep = ""),
    g_delta_method2 =  "11 * relative_age",
    get_g_delta_method = function() {
      if (self$type_formula %in% c("ols", "no_live")) return(self$g_delta_method)
      if (self$type_formula %in% c("ols_liner", "no_live_liner")) return(self$g_delta_method2)
      stop("Error, type_formula is not valid")
      },
    # get_func_sampling
    get_func_sampling = function() {
      if (self$type_sampling %in% c("all")) return(func_sampling_all)
      if (self$type_sampling %in% c("apr_march")) return(func_sampling_apr_march)
      stop("Error, type_sampling is not valid")
    },
    # analyze
    analyze_base_parametric_with_delta = function(dfx) {
      # browser()
      return(parametric_regression_felm_delta(
        dfx = dfx, fm = self$get_fm(), fm_delta = self$get_g_delta_method(),
        choices_1level_factor = self$choices_1level_factor, func_sampling = self$get_func_sampling()
      ))
    },
    analyze_base_parametric_rdd = function(dfx) {
      return(rdd_parametric_regression_felm(
        dfx = dfx, fm = self$get_fm(), choices_1level_factor = self$choices_1level_factor, func_sampling = self$get_func_sampling()
      ))
    },
    analyze_base = function(dfx) {
      print(paste("Analysis#### formula:", self$type_formula, ", sampling:", self$type_sampling, ", targets:", self$target, sep = " "))
      if (self$type_formula %in% c("ols", "no_live", "ols_liner", "no_live_liner"))  return(self$analyze_base_parametric_with_delta(dfx = dfx))
      if (self$type_formula %in% c("rdd"))  return(self$analyze_base_parametric_rdd(dfx = dfx))
      stop("Error, analyze_base function is not ready. ")
    }
  )
)

get_adopt_env = function(target, type_formula, type_sampling) {
  # 解析に外部から評価を与える
  targets_adopt = c(
    "age_father", "age_mother", "I(no_legitimate*100)",  # "I(no_job*100)", "I(job_big_company*100)",
    "gram", "pregnant_week", "I(single_womb*100)", "I(firstborn*100)", "I(girl*100)",
    "I(job1*100)", "I(job2*100)", "I(job3*100)", "I(job4*100)", "I(job5*100)", "I(job6*100)", "I(job7V*100)"
  )
  if ((target %in% targets_adopt) & (type_formula %in% c("ols")) & (type_sampling %in% c("all"))) {
    return(list(is_adopt = TRUE, tag_adopt = "table14_parametroric"))
  }
  if ((target %in% targets_adopt) & (type_formula %in% c("no_live")) & (type_sampling %in% c("all"))) {
    return(list(is_adopt = TRUE, tag_adopt = "table14_parametroric_no_live"))
  }
  if ((target %in% targets_adopt) & (type_formula %in% c("no_live_liner")) & (type_sampling %in% c("all"))) {
    return(list(is_adopt = TRUE, tag_adopt = "table14_parametroric_no_live_liner"))
  }
  if ((target %in% targets_adopt) & (type_formula %in% c("rdd")) & (type_sampling %in% c("all"))) {
    return(list(is_adopt = TRUE, tag_adopt = "table14_rdd"))
  }
  return(list(is_adopt = FALSE))
}

get_ana_envs_for_data1 = function(if_amagasaki=FALSE) {
  targets = c(
    "age_father", "age_mother", "I(no_legitimate*100)",  # "I(no_job*100)", "I(job_big_company*100)",
    "gram", "pregnant_week", "I(single_womb*100)", "I(firstborn*100)", "I(girl*100)",
    "I(job1*100)", "I(job2*100)", "I(job3*100)", "I(job4*100)", "I(job5*100)", "I(job6*100)", "I(job7V*100)"
    # "I(under_20_parent*100)", "I(low_gram*100)",  "I(early_birth*100)", "I(job_company*100)", "I(have_job*100)"
  )
  types = if (if_amagasaki == FALSE) c("ols", "ols_liner", "no_live", "no_live_liner") else c("no_live", "no_live_liner")
  ana_envs = c()
  for (target in targets) {
    for (type_formula in types) {  # "no_live", "no_live_liner"
      for (type_sampling in c("all")) {
        adopt_env = get_adopt_env(target = target, type_formula = type_formula, type_sampling = type_sampling)
        ana_env = AnalysisEnvironmentT24$new(
          target = target, type_formula = type_formula, type_sampling = type_sampling, tag = c(type_sampling, type_formula, target),
          is_adopt = adopt_env$is_adopt, tag_adopt = adopt_env$tag_adopt
        )
        ana_envs = c(ana_envs, ana_env)
      }
    }
  }
  ana_envs
}

get_ana_envs_for_data2 = function() {
  ana_envs = c()
  targets = c("I(is_white_work_father*100)", "I(is_white_work_mother*100)", "I(no_job_father*100)", "I(no_job_mother*100)")
  for (target in targets) {
    for (type_formula in c("ols", "ols_liner")) {  # "no_live", "no_live_liner"
      for (type_sampling in c("all")) {  # , "rdd"
        adopt_env = get_adopt_env(target = target, type_formula = type_formula, type_sampling = type_sampling)
        ana_env = AnalysisEnvironmentT24$new(
          target = target, type_formula = type_formula, type_sampling = type_sampling, tag = c(type_sampling, type_formula, target),
          is_adopt = adopt_env$is_adopt, tag_adopt = adopt_env$tag_adopt
        )
        ana_envs = c(ana_envs, ana_env)
      }
    }
  }
  ana_envs
}

main = function(save_folder_basis) {
  savefolder = file.path(save_folder_basis, "t14_ra")
  df = download_saitama_birth1()
  df2 = download_saitama_birth2()
  ana_envs1 = get_ana_envs_for_data1()
  lapply(ana_envs1, function(ana) ana$analyze(dfx = df))
  ana_envs2 = get_ana_envs_for_data2()
  lapply(ana_envs2, function(ana) ana$analyze(dfx = df2))
  ana_envs = c(ana_envs1, ana_envs2)
  # save
  summary_tidy = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(~ .x$result$summary_tidy %>% dplyr::mutate(
      name = .x$name(), is_adopt = .x$is_adopt, tag_adopt = .x$tag_adopt, num_p_adjust = .x$num_p_adjust
      )) %>%
    purrr::reduce(dplyr::bind_rows)
  summary_glance = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(~ .x$result$summary_glance %>% dplyr::mutate(
      name = .x$name(), is_adopt = .x$is_adopt, tag_adopt = .x$tag_adopt, num_p_adjust = .x$num_p_adjust
      )) %>%
    purrr::reduce(dplyr::bind_rows)
  # (ad hoc) get pval_adjust
  summary_tidy %<>%
    dplyr::filter((is_adopt == TRUE & term == "upper_cutoff"))  %>%
    dplyr::group_by(tag_adopt) %>%
    dplyr::mutate(
      p_value_adjust = p.adjust(p.value, method = "BH"), p_value_adjust_num = dplyr::n()
      ) %>% # p.valueにリストが入る
    dplyr::ungroup() %>%
    dplyr::bind_rows(
      summary_tidy %>% dplyr::filter(!(is_adopt == TRUE & term == "upper_cutoff"))
    )
  summary_glance %<>%
    dplyr::filter((is_adopt == TRUE))  %>%
    dplyr::group_by(tag_adopt) %>%
    dplyr::mutate(
      p_value_adjust = p.adjust(delta_pvalue, method = "BH"), p_value_adjust_num = dplyr::n()
      ) %>% # p.valueにリストが入る
    dplyr::ungroup() %>%
    dplyr::bind_rows(
      summary_glance %>% dplyr::filter(!(is_adopt == TRUE))
    )
  # save
  savefolder = file.path(save_folder_basis, "t14")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))
  # check
  # purrr::map(ana_envs, ~.x$status)
  # purrr::map(ana_envs, ~.x$result$fm_use)
  # p.adjust(summary_tidy %>% dplyr::filter(is_adopt == TRUE, term == "upper_cutoff") %>% .$p.value, method = "BH")
  # p.adjust(summary_glance %>% dplyr::filter(is_adopt == TRUE) %>% .$delta_pvalue, method = "BH")
  #######
  ## get descriptives
  #######
  df = download_saitama_birth1()
  res1 = df %>%
    psych::describe(skew = FALSE, ranges = FALSE) %>%
    dplyr::mutate(col = row.names(.))
  df = download_saitama_birth2()
  res2 = df %>%
    psych::describe(skew = FALSE, ranges = FALSE) %>%
    dplyr::mutate(col = row.names(.))
  res = dplyr::bind_rows(res1, res2)
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = res, file = file.path(savefolder, "summary_stat.csv"))
}
