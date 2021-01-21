#############################################
##  utility function for rdd regression
#############################################
source("./src/setting.R")

get_within_wave_cutoff_about_relative_age = function(dfx, relative_age_char = relative_age_col) {
  ###
  #     birth month     :  3  2  1 12 11 10  9  8  7  6  5  4
  #    relative_age     :  0  1  2  3  4  5  6  7  8  9 10 11
  # distance from cutoff: -1 -2 -3 -4 -5 -6  5  4  3  2  1  0
  ##
  dfx %>%
    dplyr::mutate(
      distance_cutoff = dplyr::if_else(
        !!as.symbol(relative_age_char) >= 6,
        11 - !!as.symbol(relative_age_char),
        -1 - !!as.symbol(relative_age_char)
        ),
      upper_cutoff = dplyr::if_else(!!as.symbol(relative_age_char) >= 6, 1, 0)
    )
}

get_formula_info_for_estimated_effect_within = function(target_list) {
  fm_list = c()
  fm_name = c()
  fm_str_template = "{dependent} ~  1 + distance_cutoff + upper_cutoff + distance_cutoff * upper_cutoff + as.factor(year)"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste(.x, "within_grade", sep = "_")))
  fm_info = purrr::map2(fm_list, fm_name, ~ list(fm = .x, name = .y))
  return(fm_info)
}
