#### 戸田氏の進学データの分析

library(rlang)
source("./src/data_download.R")
source("./src/regression_function.R")
source("./src/setting.R")

# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`


AnalysisEnvironmentT25 <- R6::R6Class(
  inherit = cmdreg::RegressionAnalysisEnvironment,
  lock_objects = FALSE,
  public = list(
    doc = "進学データ分析用のクラス",
    type = "parametric", # "parametric" or "rdd"
    target = character(),
    choices_1level_factor = c("school_id", "grade", "year"),
    # setter and getter for formula
    fm_parametric = function() "{dependent} ~ relative_age + I(relative_age^2)  | 0 | 0 | 0 " %>%
      strformat(c(dependent = self$target)) %>%
      as.formula(),
    fm_parametric2 = function() "{dependent} ~ relative_age + I(relative_age^2) + as.factor(sex) + as.factor(school_id) + as.factor(book) + as.factor(cramschool) | 0 | 0 | 0 "  %>%
      strformat(c(dependent = self$target)) %>%
      as.formula(),
    fm_parametric3 = function() "{dependent} ~ relative_age + I(relative_age^2) + as.factor(sex) + as.factor(school_id) + as.factor(book) + as.factor(cramschool) + zkokugo_level + zmath_level + zeng_level| 0 | 0 | 0 " %>%
      strformat(c(dependent = self$target)) %>%
      as.formula(),
    fm_parametric4 = function() "{dependent} ~ relative_age + I(relative_age^2) + as.factor(sex) + as.factor(school_id) + as.factor(book) + as.factor(cramschool) + zkokugo_level + zmath_level + zeng_level + zselfefficacy | 0 | 0 | 0 " %>%
      strformat(c(dependent = self$target)) %>%
      as.formula(),
    fm_rdd = function() "{dependent} ~  1 + distance_cutoff + upper_cutoff + distance_cutoff * upper_cutoff + as.factor(sex) + as.factor(school_id) + as.factor(book) + as.factor(cramschool)" %>%
      strformat(c(dependent = self$target)) %>%
      as.formula(),
    get_fm = function() {
      if (self$type == "parametric")  return(self$fm_parametric())
      if (self$type == "parametric2")  return(self$fm_parametric2())
      if (self$type == "parametric3")  return(self$fm_parametric3())
      if (self$type == "parametric4")  return(self$fm_parametric4())
      if (self$type == "rdd") return(self$fm_rdd())
      stop("Error, formula is not valid")
    },
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
      return(rdd_parametric_regression_felm(
        dfx = dfx, fm = self$get_fm(), choices_1level_factor = self$choices_1level_factor
      ))
    },
    analyze_base = function(dfx) {
      if (self$type %in% c("parametric", "parametric2", "parametric3", "parametric4")) {
        return(self$analyze_base_parametric(dfx))
      }
      if (self$type == "rdd") return(self$analyze_base_rdd(dfx))
      stop("type is in parametric, rdd")
    }
  )
)

main = function(save_folder_basis) {
  dt_sample1 = data.table::fread(path_enter) %>%
    tibble::as_tibble() %>%
    dplyr::rename(shingaku = final_hensachi) %>%
    dplyr::select(mst_id, year, shingaku, cramschool)
  dt_sample2 = data.table::fread(path_enter) %>%
    tibble::as_tibble() %>%
    dplyr::rename(zyuken = final_hensachi) %>%
    dplyr::select(mst_id, zyuken)
  dt_sample3 = data.table::fread(path_toda) %>%
    tibble::as_tibble() %>%
    dplyr::select(mst_id, year, grade, school_id, relative_age, sex, book, cram, zkokugo_level, zmath_level, zeng_level, zstrategy, zselfefficacy)
  dt_sample = dt_sample1 %>%
    dplyr::left_join(dt_sample2, by = "mst_id") %>%
    dplyr::left_join(dt_sample3, by = c("mst_id", "year"))
  # dt_sample %>% dplyr::group_by(grade, year) %>% dplyr::summarise_all(~sum(is.na(.))) %>% dplyr::select(zselfcontrol, zdilligence, zselfefficacy, year, grade)  # na countして正しい非認知をとって来れているかどうかチェック
  # build analysis instance
  targets = c("zyuken", "shingaku", "zmath_level", "zkokugo_level", "zeng_level")
  ana_envs = c()
  for (type in c("rdd", "parametric", "parametric2", "parametric3", "parametric4")) {
    for (target in targets) {
      ana_env = AnalysisEnvironmentT25$new(target = target, type = type, tag = c(target, type))
      ana_envs = c(ana_envs, ana_env)
    }
  }
  # execute analysis
  for (ana_env in ana_envs) {
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
      ~ .x$result$summary_glance %>% dplyr::mutate(name = .x$name())
      ) %>%
    purrr::reduce(dplyr::bind_rows)
  savefolder = file.path(save_folder_basis, "t25")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))
}
