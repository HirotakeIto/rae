################################
#
#    Task Runner about R's tasks of the paper
#
################################3


# import & setting
setwd(rprojroot::find_rstudio_root_file())
source("./src/setting.R")
`%>%` = magrittr::`%>%`
`%<>%` = magrittr::`%<>%`
# task start
load_source = function(file, envir) {
  sys.source(file, envir = envir)
  envir
}
'./task/t0.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}
'./task/t3.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}
'./task/t6.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}
'./task/t8.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}
'./task/t14.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}
'./task/t17.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}  # TODO: fix
'./task/t19.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}
'./task/t22.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}  # high cost
'./task/t24.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}
'./task/t25.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}
'./task/t28.R' %>% load_source(file = ., envir = new.env()) %>% {.$main(save_folder_basis = path_result_folder)}

