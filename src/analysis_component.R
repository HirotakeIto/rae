#############################################
##  utility function for regression in tibbled dataframe
#############################################
source("./src/lib.R")
# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

# helper function for getting grouped tibble
get_tibble_all = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::group_by() %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = "all"
    )
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_school_type = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::mutate(
      grade_num = as.numeric(as.character(grade)),
      school_type = dplyr::case_when(grade_num <= 6  ~ "prime", grade_num > 6 ~ "junior", TRUE ~ NA_character_)
    ) %>%
    dplyr::group_by(school_type) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name =  stringi::stri_c("school_type", school_type, sep = "_")
    ) %>%
    dplyr::select(-school_type)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_by_grade = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::group_by(grade) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("grade", grade, sep = "_"),
      data = purrr::map2(data, grade, ~ .x %>% dplyr::mutate(grade = .y))
    ) %>%
    dplyr::select(-grade)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_by_grade_apr_month = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::filter(relative_age %in% c(0, 11)) %>%
    dplyr::mutate(
      is_mar = dplyr::if_else(relative_age == 0, 1, 0),
      is_apr = dplyr::if_else(relative_age == 11, 1, 0),
      ) %>%
    dplyr::group_by(grade) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("grade", grade, "apr_month", sep = "_")
    ) %>%
    dplyr::select(-grade)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_by_grade_not_apr_march = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::filter(!(relative_age %in% c(0, 11))) %>%
    dplyr::mutate(
      is_mar = dplyr::if_else(relative_age == 0, 1, 0),
      is_apr = dplyr::if_else(relative_age == 11, 1, 0),
    ) %>%
    dplyr::group_by(grade) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("grade", grade, "not_apr_march", sep = "_")
    ) %>%
    dplyr::select(-grade)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_by_wave = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::group_by(wave) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("wave", wave, sep = "_")
    ) %>%
    dplyr::select(-wave)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_by_sex = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::group_by(sex) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("sex", sex, sep = "_"),
      data = purrr::map2(data, sex, ~ .x %>% dplyr::mutate(sex = .y))
    ) %>%
    dplyr::select(-sex)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_by_sex = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::group_by(sex) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("sex", sex, sep = "_"),
      data = purrr::map2(data, sex, ~ .x %>% dplyr::mutate(sex = .y))
    ) %>%
    dplyr::select(-sex)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_by_lowses = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::group_by(lowses) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("lowses", lowses, sep = "_"),
      data = purrr::map2(data, lowses, ~ .x %>% dplyr::mutate(lowses = .y))
    ) %>%
    dplyr::select(-lowses)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_by_grade_sex = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::filter(!is.na(sex)) %>%
    dplyr::group_by(grade, sex) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("grade", grade, "sex", sex, sep = "_"),
      data = purrr::map2(data, grade, ~ .x %>% dplyr::mutate(grade = .y)),
      data = purrr::map2(data, sex, ~ .x %>% dplyr::mutate(sex = .y))
    ) %>%
    dplyr::select(-sex, -grade)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_by_grade_lowses = function(dfx, ...) {
  tibble_for_ana = dfx %>%
    dplyr::filter(!is.na(lowses)) %>%
    dplyr::group_by(grade, lowses) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("grade", grade, "lowses", lowses, sep = "_"),
      data = purrr::map2(data, grade, ~ .x %>% dplyr::mutate(grade = .y)),
      data = purrr::map2(data, lowses, ~ .x %>% dplyr::mutate(lowses = .y))
    ) %>%
    dplyr::select(-lowses, -grade)
  return(list(tibble_for_ana = tibble_for_ana))
}

get_tibble_customed = function(dfx, get_tibble_func_list, ...) {
  # Example
  # get_tibble_customed(dt_sample, c(get_tibble_by_grade, get_tibble_by_grade_lowses, get_tibble_by_grade_sex))
  tibble_for_ana = tibble::tribble()
  for (tibble_func in get_tibble_func_list) {
    tibble_for_ana %<>% dplyr::bind_rows(tibble_func(dfx = dfx) %>% .$tibble_for_ana)
  }
  return(list(tibble_for_ana = tibble_for_ana))
}

factory_get_tibble_customed = function(get_tibble_func_list) {
  # get_tibble_used = factory_get_tibble_customed(c(get_tibble_by_grade, get_tibble_by_grade_lowses, get_tibble_by_grade_sex))
  # get_tibble_used(dt_sample)
  function(dfx) get_tibble_customed(dfx, get_tibble_func_list)
}

# helper function for getting regression result object
get_analysis_result_felm = function(tibble_for_ana, fm_info_list, after_engineering=NULL) {
  if (is.null(after_engineering)) {
    after_engineering = function(x) return(x)
  }
  summary_tidy = tibble::tribble()
  summary_glance = tibble::tribble()
  for (fm_info in fm_info_list) {
    fm = fm_info$fm
    name = fm_info$name
    print(stringi::stri_c("NAME:::::::", name, "FUNC:::::::", fm))
    # all sample
    aaa = tibble_for_ana %>%
      dplyr::mutate(no_have_data = purrr::map_lgl(data, ~is_no_df_have_values(df = .x, fm = fm))) %>%
      dplyr::filter(no_have_data == FALSE) %>%
      dplyr::mutate(
        data = purrr::map(data, ~ (
          .x %>%
            dplyr::select_(.dots = all.vars(fm)) %>%
            na.omit() %>%
            after_engineering()
        ))
      ) %>%
      dplyr::mutate(
        fm_use_str = purrr::map_chr(data, ~ return_fm_str_excluding_1level_factor(fm = fm, data = .x)),
        result = purrr::map2(data, fm_use_str, ~execute_felm(fm = as.formula(.y), data = .x)),
        name = paste(data_name, name, sep = "_")
      ) %>%
      dplyr::select(-data)
    summary_tidy %<>% dplyr::bind_rows(helper_get_result_tidy(aaa))
    summary_glance %<>% dplyr::bind_rows(helper_get_felm_result_glance(aaa))
    gc(reset = TRUE)
  }
  return(list(summary_tidy = summary_tidy, summary_glance = summary_glance))
}

# For Delta Method
helper_get_felm_result_glance_delta = function(result, fm_delta, delta_name) {
  res_glance = result %>%
    dplyr::filter(purrr::map_lgl(result, ~ ! is.null(.x))) %>%
    dplyr::mutate(
      obs = purrr::map_dbl(result, ~ .x$N),
      col_glance = purrr::map(result, ~ broom::glance(.x)),
      delta_est = purrr::map(result, ~ car::deltaMethod(.x, fm_delta)$Estimate),
      delta_se = purrr::map(result, ~ car::deltaMethod(.x, fm_delta)$SE),
      delta_pvalue = purrr::map(result, ~ car::deltaMethod(.x, fm_delta, rhs = 0)$`Pr(>|z|)`),
      delta_name = delta_name
    ) %>%
    dplyr::select(-result) %>%
    tidyr::unnest()
  res_glance
}

get_analysis_result_felm_delta = function(tibble_for_ana, fm_info_list, after_engineering=NULL) {
  if (is.null(after_engineering)) {
    after_engineering = function(x) return(x)
  }
  summary_tidy = tibble::tribble()
  summary_glance = tibble::tribble()
  for (fm_info in fm_info_list) {
    fm = fm_info$fm
    name = fm_info$name
    fm_delta = fm_info$delta_str
    fm_delta_name = fm_info$delta_name
    print(stringi::stri_c("NAME:::::::", name, "FUNC:::::::", fm))
    # all sample
    aaa = tibble_for_ana %>%
      dplyr::mutate(no_have_data = purrr::map_lgl(data, ~is_no_df_have_values(df = .x, fm = fm))) %>%
      dplyr::filter(no_have_data == FALSE) %>%
      dplyr::mutate(
        data = purrr::map(data, ~ (
          .x %>%
            dplyr::select_(.dots = all.vars(fm)) %>%
            na.omit() %>%
            after_engineering()
        ))
      ) %>%
      dplyr::mutate(
        fm_use_str = purrr::map_chr(data, ~ return_fm_str_excluding_1level_factor(fm = fm, data = .x)),
        result = purrr::map2(data, fm_use_str, ~execute_felm(fm = as.formula(.y), data = .x)),
        name = paste(data_name, name, sep = "_")
      ) %>%
      dplyr::select(-data)
    summary_tidy %<>% dplyr::bind_rows(helper_get_result_tidy(aaa))
    summary_glance %<>% dplyr::bind_rows(helper_get_felm_result_glance_delta(aaa, fm_delta, fm_delta_name))
    gc(reset = TRUE)
  }
  return(list(summary_tidy = summary_tidy, summary_glance = summary_glance))
}