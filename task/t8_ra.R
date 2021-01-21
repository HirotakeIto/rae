#############################################
##  Estimate effects of Age on *something* within Grade
#############################################¥
library(rlang)
source("./src/data_download.R")
source("./src/regression_function.R")
source("./src/setting.R")

# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

create_func_sampling_grade_all = function(g) {
  function(dfx) {
    print(paste("sampling grade:", g))
    return(dfx %>% dplyr::filter(!!sym(grade_col) == g))
  }
}

create_func_sampling_grade_apr_march = function(g) {
  function(dfx) {
    print(paste("sampling grade:", g))
    return(dfx %>% dplyr::filter(!!sym(grade_col) == g) %>% dplyr::filter(!(!!sym(relative_age_col) %in% c(0, 11))))
  }
}

AnalysisEnvironmentT8 <- R6::R6Class(
  inherit = cmdreg::RegressionAnalysisEnvironment,
  lock_objects = FALSE,
  public = list(
    type_formula = c("ra_basic", "ra_cont", "ra_cont2"),
    type_sampling = c("all_grade", "not_apr_march_grade"),
    grade = integer(),
    target = character(),
    choices_1level_factor = c("year", "grade", "book", "sex", "is_school_prime"),
    # setter and getter for formula
    get_fm_ra_basic = function() "{dependent} ~ relative_age + I(relative_age^2) | 0 | 0 | school_id" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm_ra_cont = function() "{dependent} ~ relative_age + I(relative_age^2) + as.factor(sex) + as.factor(book) + as.factor(year) | as.factor(school_id) | 0 | school_id" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm_ra_cont2 = function() "{dependent} ~ relative_age + I(relative_age^2) + as.factor(sex) | as.factor(school_id) | 0 | school_id" %>% strformat(c(dependent = self$target)) %>% as.formula(),
    get_fm = function() {
      if (self$type_formula == "ra_basic")  return(self$get_fm_ra_basic())
      if (self$type_formula == "ra_cont")  return(self$get_fm_ra_cont())
      if (self$type_formula == "ra_cont2")  return(self$get_fm_ra_cont2())
      stop("Error, type_formula is not valid")
    },
    # formula for delta method
    g_delta_method = stringi::stri_c("11 * relative_age  +  11 **2 * `I(relative_age^2)`", sep = ""),
    get_g_delta_method = function() {
      if (self$type_formula %in% c("ra_basic", "ra_cont", "ra_cont2")) return(self$g_delta_method)
      stop("Error, type_formula is not valid")
    },
    # get_func_sampling
    get_func_sampling = function() {
      if (self$type_sampling %in% c("all_grade")) return(create_func_sampling_grade_all(self$grade))
      if (self$type_sampling %in% c("not_apr_march_grade")) return(create_func_sampling_grade_apr_march(self$grade))
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
    analyze_base = function(dfx) {
      print(paste("Analysis#### formula:", self$type_formula, ", sampling:", self$type_sampling, ", targets:", self$target, sep = " "))
      if (self$type_formula %in% c("ra_basic", "ra_cont", "ra_cont2"))  return(self$analyze_base_parametric_with_delta(dfx = dfx))
      stop("Error, analyze_base function is not ready. ")
    },
    get_adopt_env = function() {
      # 解析に外部から評価を与える。
      # 最終的に論文に載せるときに採用しているか否かなど。pvalueの調整を行うためにこういう函数を容易しておく
      # しかし本質的にこの処理は一つのクラスインスタンスのなかで閉じた処理ではなく、極めて気持ち悪い
      targets_adopt = c("zkokugo_level", "zmath_level", "zeng_level")
      if ((self$target %in% targets_adopt) & (self$type_formula %in% c("ra_basic")) & (self$type_sampling %in% c("all_grade")))
        return(list(is_adopt = TRUE, tag_adopt = paste("table8", "cognitive", "ra_basic", "all_grade", sep = "_")))
      if ((self$target %in% targets_adopt) & (self$type_formula %in% c("ra_cont")) & (self$type_sampling %in% c("all_grade")))
        return(list(is_adopt = TRUE, tag_adopt = paste("table8", "cognitive", "ra_cont", "all_grade", sep = "_")))
      if ((self$target %in% targets_adopt) & (self$type_formula %in% c("ra_cont")) & (self$type_sampling %in% c("not_apr_march_grade")))
        return(list(is_adopt = TRUE, tag_adopt = paste("table8", "cognitive", "ra_cont", "not_apr_march_grade", sep = "_")))
      targets_adopt = c("zselfcontrol", "zselfefficacy", "zdilligence")
      if ((self$target %in% targets_adopt) & (self$type_formula %in% c("ra_cont")) & (self$type_sampling %in% c("all_grade")))
        return(list(is_adopt = TRUE, tag_adopt = paste("table8", "noncognitive", sep = "_")))
      targets_adopt = c("zkokugo_growth", "zmath_growth", "zeng_growth")
      if ((self$target %in% targets_adopt) & (self$type_formula %in% c("ra_cont")) & (self$type_sampling %in% c("all_grade")))
        return(list(is_adopt = TRUE, tag_adopt = paste("table8", "cognitive_growth", sep = "_")))
      targets_adopt = c("zselfcontrol_growth", "zselfefficacy_growth", "zdilligence_growth")
      if ((self$target %in% targets_adopt) & (self$type_formula %in% c("ra_cont")) & (self$type_sampling %in% c("all_grade")))
        return(list(is_adopt = TRUE, tag_adopt = paste("table8", "noncognitive_growth", sep = "_")))
      targets_adopt1 = c("studytime", "cram")
      targets_adopt2 = c("reading_time_in_a_weekdays", "playing_sport", "lesson_time")
      if (
        (self$target %in% targets_adopt1) & (self$type_formula %in% c("ra_cont")) & (self$type_sampling %in% c("all_grade"))
        |
        (self$target %in% targets_adopt2) & (self$type_formula %in% c("ra_cont2")) & (self$type_sampling %in% c("all_grade"))
        )
        return(list(is_adopt = TRUE, tag_adopt = paste("table8", "outsideschool", sep = "_")))
      targets_adopt = c("zfriendrelation", "teacherrelation2")
      if ((self$target %in% targets_adopt) & (self$type_formula %in% c("ra_cont")) & (self$type_sampling %in% c("all_grade")))
        return(list(is_adopt = TRUE, tag_adopt = paste("table8", "teacher_friend", sep = "_")))
      return(list(is_adopt = FALSE))
    },
    get_is_adopt = function() self$get_adopt_env()$is_adopt,
    get_tag_adopt = function() self$get_adopt_env()$tag_adopt
  )
)

main = function(save_folder_basis) {
  dt_sample = download_saitama(flag_dryrun = FALSE)
  # 20200526: 算数と国語は2015年のG6で、英語はG8で、非認知能力についてはG6（最初に出てきた年）で正規化し、Table 5, 6, 7と同じものを作る。結果はSlack上で見せて欲しい（論文に入れるかは要検討）
  standalize_by_specic_year_grade = function(dfx, target, val_year, val_grade) {
    mean_sd = dfx %>%
      plyr::filter(!!sym(year_col) == val_year, !!sym(grade_col) == val_grade) %>%
      {list(mean = mean(.[[target]], na.rm = TRUE), sd = sd(.[[target]], na.rm = TRUE))}
    print(paste(target, mean_sd$meam, mean_sd$sd))
    (dfx[[target]] - mean_sd$mean) / mean_sd$sd
  }
  df_use = dt_sample %>%
    dplyr::mutate(
      kokugo_level_std = standalize_by_specic_year_grade(., target = "kokugo_level", val_year = 2015, val_grade = 6),
      math_level_std = standalize_by_specic_year_grade(., target = "math_level", val_year = 2015, val_grade = 6),
      eng_level_std = standalize_by_specic_year_grade(., target = "eng_level", val_year = 2015, val_grade = 8),
      selfcontrol_std = standalize_by_specic_year_grade(., target = "selfcontrol", val_year = 2016, val_grade = 4),
      selfefficacy_std = standalize_by_specic_year_grade(., target = "selfefficacy", val_year = 2016, val_grade = 5),
      dilligence_std = standalize_by_specic_year_grade(., target = "dilligence", val_year = 2016, val_grade = 6)
      )

  # analysis setup
  targets = c(
    c("zgakuryoku", "zkokugo_level", "zmath_level", "zeng_level", "zstrategy"),
    c("zselfcontrol", "zselfefficacy", "zdilligence"),
    c("hourshome", "hoursprep", "studytime", "cram",  "teacherrelation", "zfriendrelation", "teacherrelation2"),
    c("zkokugo_growth", "zmath_growth", "zeng_growth", "zstrategy_growth", "zselfcontrol_growth", "zselfefficacy_growth", "zdilligence_growth"),
    c("zyunan", "planning", "execution", "resource", "ninti", "effort"),
    c("reading_time_in_a_weekdays", "smart_phone_gaming_tv_time", "lesson_time", "playing_sport"),
    c("kokugo_level_std", "math_level_std", "eng_level_std", "selfcontrol_std", "selfefficacy_std", "dilligence_std")
  )
  ana_envs = c()
  # target = "reading_time_input_weekdays"; type_formula = "ra_cont2"; type_sampling = "all_grade"  # for debug
  for (target in targets) {
    for (type_formula in c("ra_basic", "ra_cont")) {  # "no_live", "ols_liner", "no_live_liner"
      for (type_sampling in c("all_grade", "not_apr_march_grade")) {
        for (grade in 4:9) {
          ana_env = AnalysisEnvironmentT8$new(
            target = target, type_formula = type_formula, type_sampling = type_sampling, grade = grade,
            tag = c(paste("grade", grade, sep = ""), type_sampling, "t8", type_formula, target)
            )
          ana_envs = c(ana_envs, ana_env)
        }
      }
    }
  }
  for (target in  c("reading_time_in_a_weekdays", "smart_phone_gaming_tv_time", "lesson_time", "playing_sport")) {
    for (type_formula in c("ra_cont2")) {
      for (type_sampling in c("all_grade")) {
        for (grade in 4:9) {
          ana_env = AnalysisEnvironmentT8$new(
            target = target, type_formula = type_formula, type_sampling = type_sampling, grade = grade,
            tag = c(paste("grade", grade, sep = ""), type_sampling, "t8", type_formula, target)
          )
          ana_envs = c(ana_envs, ana_env)
        }
      }
    }
  }
  lapply(ana_envs, function(x) x$analyze(dfx = df_use))  # execute analysis
  # create summary
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
  # 以下のコードは微妙。各々のクラスインスタンスが係数推定量にアクセスできるようにしないと行けない気がする
  # 各々のクラスインスタンスのpvalueを補正するようなコードを書いた方が良いな、、、
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
  savefolder = file.path(save_folder_basis, "t8_ra")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))
  gc(reset = TRUE)
}
