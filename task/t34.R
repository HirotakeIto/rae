#############################################
##  Estimate effects of Age on *something* within grade
##     subsample analysis: april and march
#############################################¥
library(rlang)
source("./src/data_download.R")
source("./src/regression_function.R")
source("./src/setting.R")

# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`


AnalysisEnvironmentT23 <- R6::R6Class(
  inherit = cmdreg::RegressionAnalysisEnvironment,
  lock_objects = FALSE,
  public = list(
    type_formula = c("basic", "basic2"),
    type_sampling = c("in_april_and_march_by_grade"),
    grade = integer(),
    target = character(),
    choices_1level_factor = c("grade", "year", "sex", "book"),
    # setter and getter for formula
    get_fm_basic = function() "{dependent} ~ is_apirl + as.factor(sex) + as.factor(book) + as.factor(year) | as.factor(school_id) | 0 | school_id" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm_basic2 = function() "{dependent} ~ is_apirl + as.factor(sex) + as.factor(year) | as.factor(school_id) | 0 | school_id" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm = function() {
      if (self$type_formula == "basic")  return(self$get_fm_basic())
      if (self$type_formula == "basic2")  return(self$get_fm_basic2())
      stop("Error, type_formula is not valid")
    },
    # get_func_sampling
    sampling_in_april_and_march_by_grade = function(dfx) {
      print(paste("sampling grade:", self$grade))
      return(
        dfx %>% 
          dplyr::filter(!!sym(grade_col) == self$grade, !!sym(relative_age_col) %in% c(0, 11)) %>%
          dplyr::mutate(
            is_apirl = dplyr::case_when(!!sym(relative_age_col)==11~1, !!sym(relative_age_col)==0~0, TRUE~NaN)
          )
        )
    },
    sampling = function(dfx) {
      if (self$type_sampling %in% c("in_april_and_march_by_grade")) return(self$sampling_in_april_and_march_by_grade(dfx))
      stop("Error, type_sampling is not valid")
    },
    # analyze
    analyze_base_parametric = function(dfx) {
      # browser()
      return(
        parametric_regression_felm(
          dfx = self$sampling(dfx = dfx), 
          fm = self$get_fm(), 
          choices_1level_factor = self$choices_1level_factor
        )
      )
    },
    analyze_base = function(dfx) {
      print(paste("Analysis#### formula:", self$type_formula, ", sampling:", self$type_sampling, ", targets:", self$target, "grade:", self$grade, sep = " "))
      if (self$type_formula %in% c("basic", "basic2"))  return(self$analyze_base_parametric(dfx = dfx))
      stop("Error, analyze_base function is not ready.")
    }
    # get_adopt_env = function() {
    #   if ((self$target %in% targets_adopt) & (self$type_formula %in% c("ra_cont")) & (self$type_sampling %in% c("all_grade")))
    #     return(list(is_adopt = TRUE, tag_adopt = paste("table8", "teacher_friend", sep = "_")))
    #   return(list(is_adopt = FALSE))
    # },
    # get_is_adopt = function() self$get_adopt_env()$is_adopt,
    # get_tag_adopt = function() self$get_adopt_env()$tag_adopt
  )
)



main = function(save_folder_basis) {
  dt_sample = download_saitama(flag_dryrun = FALSE)
  # analysis setup
  targets = c(
    c("zgakuryoku", "zkokugo_level", "zmath_level", "zeng_level", "zstrategy"),
    c("zselfcontrol", "zselfefficacy", "zdilligence"),
    c("hourshome", "hoursprep", "studytime", "cram",  "teacherrelation", "zfriendrelation", "teacherrelation2")
  )
  ana_envs = c()
  # target = "zkokugo_level"; type_formula = "basic"; type_sampling = "in_april_and_march_by_grade"; grade = 9  # for debug
  for (target in targets) {
    for (type_formula in c("basic")) { 
      for (type_sampling in c("in_april_and_march_by_grade")) {
        for (grade in 4:9) {
          ana_env = AnalysisEnvironmentT23$new(
            target = target, type_formula = type_formula, type_sampling = type_sampling, grade = grade,
            tag = c(paste("grade", grade, sep = ""), type_sampling, "t8", type_formula, target)
          )
          ana_envs = c(ana_envs, ana_env)
        }
      }
    }
  }
  targets = c(
    c("reading_time_in_a_weekdays", "smart_phone_gaming_tv_time", "lesson_time", "playing_sport", "reading_time_in_a_weekdays", "smart_phone_gaming_tv_time", "lesson_time", "playing_sport")
  )  
  for (target in targets) {
    for (type_formula in c("basic2")) { 
      for (type_sampling in c("in_april_and_march_by_grade")) {
        for (grade in 4:9) {
          ana_env = AnalysisEnvironmentT23$new(
            target = target, type_formula = type_formula, type_sampling = type_sampling, grade = grade,
            tag = c(paste("grade", grade, sep = ""), type_sampling, "t8", type_formula, target)
          )
          ana_envs = c(ana_envs, ana_env)
        }
      }
    }
  }
  # ana_env$analyze(dfx = dt_sample)  # debug
  lapply(ana_envs, function(x) x$analyze(dfx = dt_sample))  # execute analysis
  # create summary
  summary_tidy = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(~ .x$result$summary_tidy %>% dplyr::mutate(
      name = .x$name()
    )) %>%
    purrr::reduce(dplyr::bind_rows)
  summary_glance = ana_envs %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(~ .x$result$summary_glance %>% dplyr::mutate(
      name = .x$name()
    )) %>%
    purrr::reduce(dplyr::bind_rows)
  # save
  savefolder = file.path(save_folder_basis, "t34")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))
  gc(reset = TRUE)
}
