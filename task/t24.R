#### 戸田氏の個人データの分析

library(rlang)
source("./src/data_download.R")
source("./src/regression_function.R")
source("./src/setting.R")

# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

AnalysisEnvironmentT24 <- R6::R6Class(
  inherit = cmdreg::RegressionAnalysisEnvironment,
  lock_objects = FALSE,
  public = list(
    type = "parametric", # "parametric" or "rdd"
    target = character(),
    choices_1level_factor = c("school_id", "grade", "year"),
    # setter and getter for formula
    fm_template_parametric = "{dependent} ~ relative_age + I(relative_age^2) |  grade + year | 0 | mst_id ",
    fm_template_rdd = "{dependent} ~  1 + distance_cutoff + upper_cutoff + distance_cutoff * upper_cutoff + as.factor(grade) + as.factor(year)",
    get_fm_template = function() if (self$type == "parametric") {self$fm_template_parametric} else {self$fm_template_rdd},
    get_fm = function() strformat(self$get_fm_template(), c(dependent = self$target)) %>% as.formula(),
    set_fm = function() warning("setter for fm is not working"),
    # formula for delta method
    g_delta_method = stringi::stri_c("11 * relative_age  +  11 **2 * `I(relative_age^2)`", sep = ""),
    fm_delta_name = "relative_age_effect",
    get_g_delta_method = function() {self$g_delta_method},
    # analysis function
    analyze_base_parametric = function(dfx) {
      # browser()
      return(parametric_regression_felm_delta(
        dfx = dfx, fm = self$get_fm(), fm_delta = self$get_g_delta_method(),
        choices_1level_factor = self$choices_1level_factor
      ))
    },
    analyze_base_rdd = function(dfx) {
      # browser()
      return(rdd_parametric_regression_felm(
        dfx = dfx, fm = self$get_fm(), choices_1level_factor = self$choices_1level_factor
      ))
    },
    analyze_base = function(dfx) {
      if (self$type == "parametric") {self$analyze_base_parametric(dfx)
      } else if (self$type == "rdd") {self$analyze_base_rdd(dfx)
      } else {stop("type is in parametric, rdd") }
    },
    # 解析に外部から評価を与える。
    # 最終的に論文に載せるときに採用しているか否かなど。pvalueの調整を行うためにこういう処理を挟む
    get_adopt_env = function() {
      targets_adopt = c("I(attendance*100)", "I(singleparent*100)", "I(publicassistance*100)")
      if ((self$target %in% targets_adopt) & (self$type %in% c("parametric"))) return(list(is_adopt = TRUE, tag_adopt = "table24_para"))
      return(list(is_adopt = FALSE))
    },
    get_is_adopt = function() {self$get_adopt_env()$is_adopt},
    get_tag_adopt = function() {self$get_adopt_env()$tag_adopt}
  )
)

main = function(save_folder_basis) {
  dt_sample = download_toda()
  # build analysis instance
  targets = c("I(attendance*100)", "I(singleparent*100)", "I(publicassistance*100)")
  ana_envs = c()
  for (type in c("rdd", "parametric")) {
    for (target in targets) {
      ana_env = AnalysisEnvironmentT24$new(target = target, type = type, tag = c(target, type))
      ana_env$analyze(dfx = dt_sample)  # execute analysis
      ana_envs = c(ana_envs, ana_env)
    }
  }
  # save
  summary_tidy = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(~ .x$result$summary_tidy %>% dplyr::mutate(
      name = .x$name(), is_adopt = .x$get_is_adopt(), tag_adopt = .x$get_tag_adopt()
    )) %>%
    purrr::reduce(dplyr::bind_rows)
  summary_glance = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(~ .x$result$summary_glance %>% dplyr::mutate(
      name = .x$name(), is_adopt = .x$get_is_adopt(), tag_adopt = .x$get_tag_adopt()
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
  savefolder = file.path(save_folder_basis, "t24")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))
  # 記述統計
  print(
    dt_sample %>% dplyr::summarise_at(c("attendance", "singleparent", "publicassistance"), mean, na.rm = TRUE) * 100
  )
}
