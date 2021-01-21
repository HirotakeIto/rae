source("./src/analysis_component.R")
source("./src/data_download.R")
source("./src/analysis.R")

# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

get_formula_info_t6 = function(...) {
  fm_list = c()
  fm_name = c()
  target_list = c(
    c("zgakuryoku", "zkokugo_level", "zmath_level", "zeng_level", "zstrategy"),
    c("zselfcontrol", "zselfefficacy", "zdilligence")
  )
  fm_str_template = "{dependent} ~ studytime + cram + teacherrelation + zfriendrelation + as.factor(year) + as.factor(grade) | 0 | 0 | school_id"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste("t6_ra_ols", .x, sep = "_")))
  fm_str_template = "{dependent} ~ studytime + cram + teacherrelation + zfriendrelation + as.factor(year) + as.factor(grade) | mst_id | 0 | school_id"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste("t6_ra_fe_basic", .x, sep = "_")))
  fm_str_template = "{dependent} ~ studytime + cram + relative_age * studytime + I(relative_age^2) * studytime + teacherrelation + zfriendrelation + as.factor(year) | mst_id | 0 | school_id"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste("t6_ra_fe", .x, sep = "_")))
  # combine
  fm_info_list = purrr::map2(fm_list, fm_name, ~ list(fm = .x, name = .y))
  return(list(fm_info_list = fm_info_list))
}


main = function(save_folder_basis) {
  dt_sample = download_saitama(flag_dryrun = TRUE)
  get_tibble_customed = function(dfx, ...) {
    tibble_for_ana = tibble::tribble()
    # tibble_for_ana  %<>% dplyr::bind_rows(get_tibble_by_wave(dfx = dfx) %>% .$tibble_for_ana)
    tibble_for_ana %<>% dplyr::bind_rows(get_tibble_school_type(dfx = dfx) %>% .$tibble_for_ana)
    tibble_for_ana %<>% dplyr::bind_rows(get_tibble_all(dfx = dfx) %>% .$tibble_for_ana)
    return(list(tibble_for_ana = tibble_for_ana))
  }
  af = AnalysisFunction$new(
    func_get_tibble = get_tibble_customed,
    func_get_formula_info = get_formula_info_t6,
    func_get_analysis_result = get_analysis_result_felm
  )
  research_execute = af$get_analysis_method()
  analysis = Analysis$new(dfx = dt_sample)
  research_execute(analysis = analysis)
  savefolder = file.path(save_folder_basis, "t6_ra")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = analysis$summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = analysis$summary_glance, file = file.path(savefolder, "summary_glance.csv"))
  gc(reset = TRUE)
}