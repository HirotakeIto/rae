#############################################
##  Estimate effects of Age on *something* using Toda Data
#############################################¥

library(rlang)
source("./src/data_download.R")
source("./src/regression_function.R")
source("./src/setting.R")

# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

AnalysisEnvironmentT19 <- R6::R6Class(
  inherit = cmdreg::RegressionAnalysisEnvironment,
  lock_objects = FALSE,
  public = list(
    type_formula = c("all"),
    target = character(),
    choices_1level_factor = c("year", "grade", "book", "sex", "is_school_prime"),
    # setter and getter for formula
    get_fm1 = function() "{dependent} ~ as.factor(attendance) + as.factor(attendance) * relative_age + as.factor(attendance) * I(relative_age^2)  | as.factor(grade) + as.factor(year)  + as.factor(school_id) | 0 | school_id" %>%
      strformat(c(dependent = self$target)) %>%
      as.formula(),
    get_fm2 = function() "{dependent} ~ as.factor(attendance) + as.factor(attendance) * relative_age + as.factor(attendance) * I(relative_age^2) +  as.factor(grade) * relative_age | as.factor(grade) + as.factor(year)  + as.factor(school_id) | 0 | school_id" %>%
      strformat(c(dependent = self$target)) %>%
      as.formula(),
    get_fm = function() {
      if (self$type_formula == "fm1")  return(self$get_fm1())
      if (self$type_formula == "fm2")  return(self$get_fm2())
      stop("Error, type_formula is not valid")
    },
    # formula for delta method
    g_delta_method = stringi::stri_c(
      "   (11 * `as.factor(attendance)1:relative_age` +  11 **2 * `as.factor(attendance)1:I(relative_age^2)`",
      "    - (0 * `as.factor(attendance)1:relative_age` +  0^2 * `as.factor(attendance)1:I(relative_age^2)`) )",
      sep = ""
    ),
    get_g_delta_method = function() {
      if (self$type_formula %in% c("fm1", "fm2")) return(self$g_delta_method)
      stop("Error, type_formula is not valid")
    },
    # analyze
    analyze_base_parametric_with_delta = function(dfx) {
      # browser()
      return(parametric_regression_felm_delta(
        dfx = dfx, fm = self$get_fm(), fm_delta = self$get_g_delta_method(),
        choices_1level_factor = self$choices_1level_factor
      ))
    },
    analyze_base = function(dfx) {
      print(paste("Analysis#### formula:", self$type_formula, ", targets:", self$target, sep = " "))
      if (self$type_formula %in% c("fm1", "fm2"))  return(self$analyze_base_parametric_with_delta(dfx = dfx))
      stop("Error, analyze_base function is not ready. ")
    },
    get_adopt_env = function() {
      # 解析に外部から評価を与える。
      targets_adopt = c(
        c("zkokugo_level", "zmath_level", "zeng_level", "zselfcontrol", "zselfefficacy", "zdilligence")
        )
      if ((self$target %in% targets_adopt) & (self$type_formula %in% c("fm2")))
        return(list(is_adopt = TRUE, tag_adopt = paste("table19", "fm2", sep = "_")))
      return(list(is_adopt = FALSE))
    },
    get_is_adopt = function() {self$get_adopt_env()$is_adopt},
    get_tag_adopt = function() {self$get_adopt_env()$tag_adopt}
  )
)


t19_ra = function(save_folder_basis) {
  dt_sample = download_toda()
  targets = c(
    c("zgakuryoku", "zkokugo_level", "zmath_level", "zeng_level", "zstrategy"),
    c("zselfcontrol", "zselfefficacy", "zdilligence", "hourshome", "hoursprep", "studytime", "cram",  "teacherrelation", "zfriendrelation", "teacherrelation2")
  )
  analysiss = c()
  for (target in targets) {
    for (type in c("fm1", "fm2")) {
      ana = AnalysisEnvironmentT19$new(
        target = target, type_formula = type,
        tag = c(type, target)
      )
      analysiss = c(analysiss, ana)
    }
  }
  # execute
  lapply(analysiss, function(x) x$analyze(dfx = dt_sample))  # execute analysis
  # create summary
  summary_tidy = analysiss %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(~ .x$result$summary_tidy %>%
    dplyr::mutate(name = .x$name())) %>%
    purrr::reduce(dplyr::bind_rows)
  summary_glance = analysiss %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(~ .x$result$summary_glance %>% dplyr::mutate(
      name = .x$name(), is_adopt = .x$get_is_adopt(), tag_adopt = .x$get_tag_adopt()
    )) %>%
    purrr::reduce(dplyr::bind_rows)
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
  savefolder = file.path(save_folder_basis, "t19_ra")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))
}
