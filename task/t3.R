source("./src/data_download.R")
source("./src/analysis_component.R")
source("./src/analysis.R")

# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

### Analysis of heterogenuity about sex in Relative Age
get_formula_cross_term = function(...) {
  fm_list = c()
  fm_name = c()
  fm_delta = c()
  fm_delta_name = c()
  ########
  # 1. cross term: sex
  #######
    ########
    # 1.1 liner
    #######
  g_delta_method = stringi::stri_c(
    " 11 * `as.factor(sex)2:relative_age` - 0 * `as.factor(sex)2:relative_age` ",
    sep = ""
  )
  # 1
  target_list = c(
    c("zkokugo_level", "zmath_level", "zeng_level"),
    c("zselfcontrol", "zselfefficacy", "zdilligence"),
    c("studytime", "cram",  "teacherrelation", "zfriendrelation")
  )
  fm_str_template = "{dependent} ~ as.factor(sex)*relative_age + as.factor(sex)*as.factor(book) + as.factor(sex)*as.factor(year) + as.factor(sex)*as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste("t3_sex", .x, sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ g_delta_method))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "sex_nl"))
  # 2
  target_list =  c("studytime", "cram",  "teacherrelation", "zfriendrelation")
  target_list2 =  c("zgakuryoku")
  combination = purrr::cross2(target_list, target_list2)
  fm_str_template = "{dependent} ~ as.factor(sex)*relative_age+  as.factor(sex)*{independent} + as.factor(sex)*as.factor(book) + as.factor(sex)*as.factor(year) + as.factor(sex)*as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(combination, ~ strformat(fm_str_template, c(dependent = .x[[1]], independent = .x[[2]])) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(combination,  ~ paste("t3_sex_2", .x[[1]], .x[[2]], sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ g_delta_method))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "sex_nl"))
  ########
  # 1.2 non liner
  #######
  g_delta_method = stringi::stri_c(
    "   (11 * `as.factor(sex)2:relative_age` +  11 **2 * `as.factor(sex)2:I(relative_age^2)`",
    "    - (0 * `as.factor(sex)2:relative_age` +  0^2 * `as.factor(sex)2:I(relative_age^2)`) )",
    sep = ""
  )
  # 1
  target_list = c(
    c("zkokugo_level", "zmath_level", "zeng_level"),
    c("zselfcontrol", "zselfefficacy", "zdilligence"),
    c("studytime", "cram",  "teacherrelation", "zfriendrelation")
  )
  fm_str_template = "{dependent} ~ as.factor(sex)*relative_age + as.factor(sex)*I(relative_age^2) + as.factor(sex)*as.factor(book) + as.factor(sex)*as.factor(year) + as.factor(sex)*as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste("t3_sex_nl", .x, sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ g_delta_method))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "sex_nl"))
  # 2
  target_list =  c("studytime", "cram",  "teacherrelation", "zfriendrelation")
  target_list2 =  c("zgakuryoku")
  combination = purrr::cross2(target_list, target_list2)
  fm_str_template = "{dependent} ~ as.factor(sex)*relative_age + as.factor(sex)*I(relative_age^2) +  as.factor(sex)*{independent} + as.factor(sex)*as.factor(book) + as.factor(sex)*as.factor(year) + as.factor(sex)*as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(combination, ~ strformat(fm_str_template, c(dependent = .x[[1]], independent = .x[[2]])) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(combination,  ~ paste("t3_sex_nl_2", .x[[1]], .x[[2]], sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ g_delta_method))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "sex_nl"))
  ########
  # 2. cross term: lowses
  #######
    ########
    # 1.1 liner
    #######
  g_delta_method = stringi::stri_c(
    " 11 * `as.factor(lowses)1:relative_age` - 0 * `as.factor(lowses)1:relative_age` ",
    sep = ""
  )
  # 1
  target_list = c(
    c("zkokugo_level", "zmath_level", "zeng_level"),
    c("zselfcontrol", "zselfefficacy", "zdilligence"),
    c("studytime", "cram",  "teacherrelation", "zfriendrelation")
  )
  fm_str_template = "{dependent} ~ as.factor(lowses)*relative_age + as.factor(lowses)*as.factor(book) + as.factor(lowses)*as.factor(year) + as.factor(lowses)*as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste("t3_lowses", .x, sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ g_delta_method))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "lowses_nl"))
  # 2
  target_list =  c("studytime", "cram",  "teacherrelation", "zfriendrelation")
  target_list2 =  c("zgakuryoku")
  combination = purrr::cross2(target_list, target_list2)
  fm_str_template = "{dependent} ~ as.factor(lowses)*relative_age+  as.factor(lowses)*{independent} + as.factor(lowses)*as.factor(book) + as.factor(lowses)*as.factor(year) + as.factor(lowses)*as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(combination, ~ strformat(fm_str_template, c(dependent = .x[[1]], independent = .x[[2]])) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(combination,  ~ paste("t3_lowses_2", .x[[1]], .x[[2]], sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ g_delta_method))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "lowses"))
    ########
    # 2.2 non liner
    #######
  g_delta_method = stringi::stri_c(
    "   (11 * `as.factor(lowses)1:relative_age` +  11 **2 * `as.factor(lowses)1:I(relative_age^2)`",
    "    - (0 * `as.factor(lowses)1:relative_age` +  0^2 * `as.factor(lowses)1:I(relative_age^2)`) )",
    sep = ""
  )
  # 1
  target_list = c(
    c("zkokugo_level", "zmath_level", "zeng_level"),
    c("zselfcontrol", "zselfefficacy", "zdilligence"),
    c("studytime", "cram",  "teacherrelation", "zfriendrelation")
  )
  fm_str_template = "{dependent} ~ as.factor(lowses)*relative_age + as.factor(lowses)*I(relative_age^2) + as.factor(lowses)*as.factor(book) + as.factor(lowses)*as.factor(year) + as.factor(lowses)*as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste("t3_lowses_nl", .x, sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ g_delta_method))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "lowses_nl"))
  # 2
  target_list =  c("studytime", "cram",  "teacherrelation", "zfriendrelation")
  target_list2 =  c("zgakuryoku")
  combination = purrr::cross2(target_list, target_list2)
  fm_str_template = "{dependent} ~ as.factor(lowses)*relative_age + as.factor(lowses)*I(relative_age^2) +  as.factor(lowses)*{independent} + as.factor(lowses)*as.factor(book) + as.factor(lowses)*as.factor(year) + as.factor(lowses)*as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(combination, ~ strformat(fm_str_template, c(dependent = .x[[1]], independent = .x[[2]])) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(combination,  ~ paste("t3_lowses_nl_2", .x[[1]], .x[[2]], sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ g_delta_method))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "lowses_nl"))
  # combine
  fm_info_list = list(x = fm_list,  y = fm_name, z = fm_delta, w = fm_delta_name) %>%
    purrr::pmap(function(x, y, z, w) {
      list(fm = x, name = y, delta_str = z, delta_name = w)
      })
  return(list(fm_info_list = fm_info_list))
}

get_formula_subsample = function(...) {
  fm_list = c()
  fm_name = c()
  fm_delta = c()
  fm_delta_name = c()
  #####################
  # 1th polynominal
  #####################
  g_delta_method = stringi::stri_c(
    "11 * relative_age",
    sep = ""
  )
  target_list = c(
    c("zkokugo_level", "zmath_level", "zeng_level"),
    c("zselfcontrol", "zselfefficacy", "zdilligence"),
    c("hourshome", "hoursprep", "studytime", "cram",  "teacherrelation", "zfriendrelation")
  )
  # 1
  fm_str_template = "{dependent} ~ relative_age + as.factor(sex) + as.factor(book) + as.factor(year) + as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste("t3_subsample", .x, sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ strformat(g_delta_method, c(reference = "sex"))))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "subsample"))
  # 2
  target_list =  c("hourshome", "hoursprep", "studytime", "cram",  "teacherrelation", "zfriendrelation")
  target_list2 =  c("zgakuryoku")  # "zkokugo_level", "zmath_level",
  combination = purrr::cross2(target_list, target_list2)
  fm_str_template = "{dependent} ~ relative_age + {independent} + as.factor(sex) + as.factor(book) + as.factor(year) + as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(combination, ~ strformat(fm_str_template, c(dependent = .x[[1]], independent = .x[[2]])) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(combination,  ~ paste("t9_subsample_2", .x[[1]], .x[[2]], sep = "_")))
  fm_delta = c(fm_delta, purrr::map(combination,  ~ strformat(g_delta_method, c(reference = "sex"))))
  fm_delta_name = c(fm_delta_name, purrr::map(combination, ~ "subsample"))
  #####################
  # 2th polynominal
  #####################
  g_delta_method = stringi::stri_c(
    "11 * relative_age  +  11 **2 * `I(relative_age^2)`",
    sep = ""
  )
  target_list = c(
    c("zkokugo_level", "zmath_level", "zeng_level"),
    c("zselfcontrol", "zselfefficacy", "zdilligence"),
    c("hourshome", "hoursprep", "studytime", "cram",  "teacherrelation", "zfriendrelation")
  )
  # 1
  fm_str_template = "{dependent} ~ relative_age + I(relative_age^2) + as.factor(sex) + as.factor(book) + as.factor(year) + as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste("t3_subsample_2nd", .x, sep = "_")))
  fm_delta = c(fm_delta, purrr::map(target_list,  ~ strformat(g_delta_method, c(reference = "sex"))))
  fm_delta_name = c(fm_delta_name, purrr::map(target_list, ~ "subsample"))
  # 2
  target_list =  c("hourshome", "hoursprep", "studytime", "cram",  "teacherrelation", "zfriendrelation")
  target_list2 =  c("zgakuryoku")  # "zkokugo_level", "zmath_level",
  combination = purrr::cross2(target_list, target_list2)
  fm_str_template = "{dependent} ~ relative_age + I(relative_age^2) + {independent} + as.factor(sex) + as.factor(book) + as.factor(year) + as.factor(grade) | as.factor(school_id) | 0 | school_id"
  fm_list = c(fm_list, purrr::map(combination, ~ strformat(fm_str_template, c(dependent = .x[[1]], independent = .x[[2]])) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(combination,  ~ paste("t3_subsample_2nd_2", .x[[1]], .x[[2]], sep = "_")))
  fm_delta = c(fm_delta, purrr::map(combination,  ~ strformat(g_delta_method, c(reference = "sex"))))
  fm_delta_name = c(fm_delta_name, purrr::map(combination, ~ "subsample_2nd"))
  # combine
  fm_info_list = list(x = fm_list,  y = fm_name, z = fm_delta, w = fm_delta_name) %>%
    purrr::pmap(function(x, y, z, w) {
      list(fm = x, name = y, delta_str = z, delta_name = w)
      })
  return(list(fm_info_list = fm_info_list))
}

main = function(save_folder_basis) {
  dt_sample = download_saitama()
  get_tibble_customed_cross = factory_get_tibble_customed(c(get_tibble_by_grade, get_tibble_all))
  af = AnalysisFunction$new(
    func_get_tibble = get_tibble_customed_cross,
    func_get_formula_info = get_formula_cross_term,
    func_get_analysis_result = get_analysis_result_felm_delta
  )
  research_execute = af$get_analysis_method()
  analysis = Analysis$new(dfx = dt_sample)
  research_execute(analysis = analysis)
  # # no use (subsample)
  # get_tibble_customed_subsample = factory_get_tibble_customed(c(get_tibble_by_grade_lowses, get_tibble_by_grade_sex, get_tibble_by_lowses, get_tibble_by_sex))
  # af = AnalysisFunction$new(
  #   func_get_tibble = get_tibble_customed_subsample,
  #   func_get_formula_info = get_formula_subsample,
  #   func_get_analysis_result = get_analysis_result_felm_delta
  # )
  # research_execute = af$get_analysis_method()
  # analysis2 = Analysis$new(dfx=dt_sample)
  # research_execute(analysis = analysis2)
  # save
  summary_tidy = dplyr::bind_rows(analysis$summary_tidy)  # , analysis2$summary_tidy
  summary_glance = dplyr::bind_rows(analysis$summary_glance)  # , , analysis2$summary_glance
  # pvalue adjust
  pattern_is_adapt = "^(grade|all)(_\\d){0,1}_t3_(sex|lowses)_nl_(zkokugo|zmath|zeng|zdilligence|zselfcontrol|zselfefficacy).*$"
  summary_glance %<>%
    dplyr::mutate(
      is_adopt = stringr::str_detect(name, pattern_is_adapt),
      tag_adopt = stringr::str_match(name, pattern_is_adapt)[, c(2, 4)] %>% apply(1, paste, collapse = "_")
    )
  summary_glance = summary_glance %>%
    dplyr::filter(is_adopt) %>%
    dplyr::group_by(tag_adopt) %>%
    dplyr::mutate(
      p_value_adjust = p.adjust(delta_pvalue, method = "BH"), p_value_adjust_num = dplyr::n()
    ) %>% # p.valueにリストが入る
    dplyr::ungroup() %>%
    dplyr::bind_rows(
      summary_glance %>% dplyr::filter(!(is_adopt == TRUE))
    )
  savefolder = file.path(save_folder_basis, "t3")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))
}
