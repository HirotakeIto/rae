###################################
#
#   戸田市の人と埼玉県全体の比較
#
###################################
library(rlang)
source("./src/data_download.R")
source("./src/regression_function.R")
source("./src/setting.R")

# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

test_difference = function(dfx1, dfx2, target) {
  result = t.test(dfx1[[target]], dfx2[[target]])
  return(list(
    summary_glance = broom::glance(result) %>%
      dplyr::mutate(obs1 = sum(!is.na(dfx1[target])), obs2 = sum(!is.na(dfx2[target]))),
    summary_tidy = broom::tidy(result)))
}

AnalysisEnvironmentT28 <- R6::R6Class(
  inherit = cmdreg::RegressionAnalysisEnvironment,
  lock_objects = FALSE,
  public = list(
    grade = integer(),
    target = character(),
    type = c("mean_test"),
    # setter and getter for formula
    analyze_base_is_exist_in_next_year = function(dfx1, dfx2) {
      print(stringi::stri_c("Analysis Start :::: ", self$type, self$grade, self$target))
      return(test_difference(
        dfx1 = dfx1 %>% dplyr::filter(!!sym(grade_col) == self$grade),
        dfx2 = dfx2 %>% dplyr::filter(!!sym(grade_col) == self$grade),
        target = self$target))
    },
    analyze_base = function(dfx1, dfx2=NULL) {
      if (self$type %in% c("mean_test")) return(self$analyze_base_is_exist_in_next_year(dfx1, dfx2))
      stop("Error, formula is not valid")
    },
    get_adopt_env = function() {
      # 解析に外部から評価を与える。
      targets_adopt = c(
        c("zgakuryoku", "zkokugo_level", "zmath_level", "zeng_level", "zstrategy"),
        c("zselfcontrol", "zselfefficacy", "zdilligence"),
        c("girl", "no_book", "cram", "relative_age"),
        c("hourshome", "hoursprep", "studytime",  "teacherrelation", "zfriendrelation", "teacherrelation2"),
        c("zyunan", "planning", "execution", "resource", "ninti", "effort"),
        c("reading_time_in_a_weekdays", "smart_phone_gaming_tv_time", "lesson_time", "playing_sport")
      )
      if (self$target %in% targets_adopt & self$type == "mean_test" & self$grade == 9) {
        return(list(is_adopt = TRUE, tag_adopt = stringi::stri_c("grade9")))
      }
      return(list(is_adopt = FALSE))
    },
    get_is_adopt = function() {self$get_adopt_env()$is_adopt},
    get_tag_adopt = function() {self$get_adopt_env()$tag_adopt}
  )
)

main = function(save_folder_basis) {
  dfx1 = data.table::fread(path_main) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      sex = as.numeric(as.character(sex)),
      girl = dplyr::case_when(sex == 1 ~ 0, sex == 2 ~ 1, TRUE~ NaN),
      no_book = dplyr::case_when(book == 1 ~ 1, book %in% c(2:5) ~ 0, TRUE~ NaN),
      book = as.numeric(as.character(book)),
      cram = as.numeric(as.character(cram)),
  )
  dfx2 = readr::read_csv(path_toda, guess_max = 100000) %>%
    dplyr::mutate(
      sex = as.numeric(as.character(sex)),
      girl = dplyr::case_when(sex == 1 ~ 0, sex == 2 ~ 1, TRUE~ NaN),
      no_book = dplyr::case_when(book == 1 ~ 1, book %in% c(2:5) ~ 0, TRUE~ NaN),
      book = as.numeric(as.character(book)),
      cram = as.numeric(as.character(cram)),
    )
  # analysis start
  analysiss = c()
  # targets = c("zkokugo_level")
  targets = c(
    c("zgakuryoku", "zkokugo_level", "zmath_level", "zeng_level", "zstrategy"),
    c("zselfcontrol", "zselfefficacy", "zdilligence"),
    c("girl", "no_book", "cram", "relative_age"),
    c("hourshome", "hoursprep", "studytime",  "teacherrelation", "zfriendrelation", "teacherrelation2"),
    c("zyunan", "planning", "execution", "resource", "ninti", "effort"),
    c("reading_time_in_a_weekdays", "smart_phone_gaming_tv_time", "lesson_time", "playing_sport")
  )
  grades = 4:9
  type = "mean_test"
  for (target in targets) {
    for (grade in grades) {
      ana = AnalysisEnvironmentT28$new(
        target = target, grade = grade,  type = type,
        tag = c(type, target, grade)
      )
      analysiss = c(analysiss, ana)
    }
  }
  # execute
  lapply(analysiss, function(x) x$analyze(dfx1 = dfx1, dfx2 = dfx2))  # execute analysis
  # create summary
  summary_tidy = analysiss %>%
    purrr::discard(~ .x$status != "analyzed") %>%
    purrr::map(
      ~ .x$result$summary_tidy %>% dplyr::mutate(name = .x$name())
      ) %>%
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
      p_value_adjust = p.adjust(p.value, method = "BH"), p_value_adjust_num = dplyr::n()
    ) %>% # p.valueにリストが入る
    dplyr::ungroup() %>%
    dplyr::bind_rows(
      summary_glance %>% dplyr::filter(!(is_adopt == TRUE))
    )
  savefolder = file.path(save_folder_basis, "t28")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))
}
