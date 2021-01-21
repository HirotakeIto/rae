source("./src/data_download.R")
source("./src/analysis_component.R")
source("./src/analysis.R")

get_formula_info_t0 = function(...) {
  fm_list = c()
  fm_name = c()
  # t0 : Correlation
  target_list =  c(
    "zselfcontrol", "zselfefficacy", "zdilligence",
    "zyunan", "planning", "execution", "resource", "ninti", "effort"
    )
  target_list2 =  c("zkokugo_level", "zmath_level", "zgakuryoku", "zeng_level")
  combination = purrr::cross2(target_list, target_list2)
  fm_str_template = "{dependent} ~ {independent}  | 0 | 0 | 0"  #  BUG?: 最後のschool_idはfactorが効かない
  fm_list = c(fm_list, purrr::map(combination, ~ strformat(fm_str_template, c(dependent = .x[[1]], independent = .x[[2]])) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(combination,  ~ paste("t0_corr", .x[[1]], .x[[2]], sep = "_")))
  # combine
  fm_info_list = purrr::map2(fm_list, fm_name, ~ list(fm = .x, name = .y))
  return(list(fm_info_list = fm_info_list))
}


main = function(save_folder_basis) {
  dt_sample = download_saitama()
  af = AnalysisFunction$new(
    func_get_tibble = get_tibble_by_grade,
    func_get_formula_info = get_formula_info_t0,
    func_get_analysis_result = get_analysis_result_felm
  )
  research_execute = af$get_analysis_method()
  analysis = Analysis$new(dfx = dt_sample)
  research_execute(analysis = analysis)
  # save
  savefolder = file.path(save_folder_basis, "t0_ra")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = analysis$summary_tidy, file = file.path(savefolder, "summary_tidy.csv"))
  write.csv(x = analysis$summary_glance, file = file.path(savefolder, "summary_glance.csv"))
}
