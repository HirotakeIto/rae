library(rlang)
source("./src/data_download.R")
source("./src/regression_function.R")
source("./src/setting.R")

limain = function(save_folder_basis) {
  df = download_saitama_birth1()
  # df2 = download_saitama_birth2()

  get_fm_ols = function(target) "{dependent} ~ 1 + relative_age + I(relative_age^2) + as.factor(year) | lived_city | 0 | lived_city" %>% strformat(c(dependent = target)) %>% as.formula()
  get_fm_ols_liner = function(target) "{dependent} ~ 1 + relative_age + I(relative_age^2) + as.numeric(year) | lived_city | 0 | lived_city" %>% strformat(c(dependent = target)) %>% as.formula()

  targets = c("age_father", "age_mother")


  res_ols.age_father = lfe::felm(get_fm_ols('age_father'), df)
  res_ols.age_father$name = "res_ols.age_father"
  res_liner.age_father = lfe::felm(get_fm_ols_liner('age_father'), df)
  res_liner.age_father$name = "res_liner.age_father"
  res_ols.age_mother = lfe::felm(get_fm_ols('age_mother'), df)
  res_ols.age_mother$name = "res_ols.age_mother"
  res_liner.age_mother = lfe::felm(get_fm_ols_liner('age_mother'), df)
  res_liner.age_mother$name = "res_liner.age_mother"
  ana_envs = list(res_ols.age_father, res_liner.age_father, res_ols.age_mother, res_liner.age_mother)

  summary_glance = ana_envs %>%
    purrr::map(
      ~ broom::glance(.x) %>%
        dplyr::mutate(
          delta_est = car::deltaMethod(.x, "11 * relative_age  +  11 **2 * `I(relative_age^2)`", rhs = 0)$Estimate,
          delta_se = car::deltaMethod(.x, "11 * relative_age  +  11 **2 * `I(relative_age^2)`", rhs = 0)$SE,
          delta_pvalue = car::deltaMethod(.x, "11 * relative_age  +  11 **2 * `I(relative_age^2)`", rhs = 0)$`Pr(>|z|)`
        ) %>%
        dplyr::mutate(name = .x$name)
      ) %>%
    purrr::reduce(dplyr::bind_rows)
  savefolder = file.path("result", "t14_rr")
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance.csv"))

}