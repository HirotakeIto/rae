# import & setting
`%>%` = magrittr::`%>%`
`%<>%` = magrittr::`%<>%`
setwd(rprojroot::find_rstudio_root_file())
# task start









data_download = new.env();sys.source('./notebooks/Yamaguchi/RAE/src/data_download.R', envir = data_download)
t0_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/t0_ra.R', envir = t0_ra)
t3_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/t3_ra.R', envir = t3_ra)
t6_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/t6_ra.R', envir = t6_ra)
t8_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/t8_ra.R', envir = t8_ra)
t9_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/t9_ra.R', envir = t9_ra)
t14_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/task14.R', envir = t14_ra)
t17_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/t17_ra.R', envir = t17_ra)
t18_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/t18_ra.R', envir = t18_ra)
t19_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/t19_ra.R', envir = t19_ra)
t21_ra = new.env();sys.source('./notebooks/Yamaguchi/RAE/task/t21.R', envir = t21_ra)
# Start
save_folder_basis = file.path('./notebooks/Yamaguchi/RAE/result/', format(as.POSIXlt(Sys.time(), "GMT") , format = "GMT%Y%m%d%H%M"))
# dfx = readr::read_csv('./notebooks/Yamaguchi/RAE/data/dataset1.csv', guess_max = 2000000)
dfx = data.table::fread('./notebooks/Yamaguchi/RAE/data/dataset1.csv') %>% tibble::as_tibble()
dt_sample = data_download$usedata_extract(data_download$setup_data(dfx), flag_dryrun = 0)
dt_sample = dt_sample %>%
  dplyr::mutate(
    absolute_age = as.numeric(relative_age) + (as.integer(as.numeric(as.character(grade))) + 5) * 12
  ) %>%
  dplyr::group_by(year,  school_id, grade) %>%
  dplyr::mutate(relative_age_rank = percent_rank(as.numeric(relative_age))) %>%
  dplyr::ungroup()
invisible({rm(list = c("dfx"));gc();gc()})
# # RelativeAge
t0_ra$t0_ra(dt_sample = dt_sample, save_folder_basis = save_folder_basis)
t3_ra$t3_ra(dt_sample = dt_sample, save_folder_basis = save_folder_basis)
t6_ra$t6_ra(dt_sample = dt_sample, save_folder_basis = save_folder_basis)
t8_ra$t8_ra(dt_sample = dt_sample, save_folder_basis = save_folder_basis)
t9_ra$t9_ra(dt_sample = dt_sample, save_folder_basis = save_folder_basis)
t14_ra$t14_ra(save_folder_basis = save_folder_basis)
t17_ra$t17_ra(dt_sample = dt_sample, save_folder_basis = save_folder_basis)
t17_ra$check_rdd_plot(save_folder_basis = save_folder_basis)
t18_ra$t18_ra(dt_sample = dt_sample, save_folder_basis = save_folder_basis)
t19_ra$t19_ra(save_folder_basis = save_folder_basis)
# Toda
# dfx = readr::read_csv('./notebooks/Yamaguchi/RAE/data/dataset1_toda.csv', guess_max = 2000000)
# dt_sample = data_download$usedata_extract(data_download$setup_data(dfx), flag_dryrun = 0)
# dt_sample = dt_sample %>%
#   dplyr::mutate(
#     absolute_age = as.numeric(relative_age) + (as.integer(as.numeric(as.character(grade))) + 5) * 12
#   ) %>%
#   dplyr::group_by(year,  school_id, grade) %>%
#   dplyr::mutate(relative_age_rank = percent_rank(as.numeric(relative_age))) %>%
#   dplyr::ungroup()
# invisible({rm(list = c("dfx"));gc();gc()})
t21_ra$t21_ra(save_folder_basis = save_folder_basis)