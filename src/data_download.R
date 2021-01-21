#############################################
##  utility function for data download
#############################################
library(rlang)
source("./src/setting.R")
# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`
# formula for data download
setup_data <- function(data) {
  factor_list = c(
    "grade", "year", "birth", "sex", "book", "school_id", "city_id",
    "birth2", "ses1", "ses2", "ses3", "ses4"
    )
  numeric_list = c(
    "strategy", "selfcontrol", "selfefficacy", "dilligence",
    "zgakuryoku", "zkokugo_level", "zmath_level", "zeng_level",
    "zkokugo_growth", "zmath_growth", "zeng_growth",
    "zstrategy", "zselfcontrol", "zselfefficacy", "zdilligence",
    "zstrategy_growth", "zselfcontrol_growth", "zselfefficacy_growth", "zdilligence_growth",
    "hourshome", "hoursprep", "cramschool", "cram",  "teacherrelation", "zfriendrelation", "teacherrelation2",
    "relative_age",
    "rate_school_attendance", "rate_graduation", "prime_prime_pct_younger_in_class", "prime_pct_younger_in_class",
    "prime_prime_zkokugo_level", "prime_prime_zmath_level", "prime_prime_zeng_level", "prime_prime_zgakuryoku",
    "prime_prime_zstrategy", "prime_prime_zdilligence", "prime_prime_zselfcontrol", "prime_prime_zselfefficacy",
    "prime_zkokugo_level", "prime_zmath_level", "prime_zeng_level", "prime_zgakuryoku",
    "prime_zstrategy", "prime_zdilligence", "prime_zselfcontrol", "prime_zselfefficacy"
    )
  for (col in factor_list) {
    if (col %in% colnames(data)) {
      data[[col]] = as.factor(data[[col]])
    } else{
      print(col)
    }
  }
  for (col in numeric_list) {
    if (col %in% colnames(data)) {
      data[[col]] = as.numeric(data[[col]])
    } else{
      print(col)
    }
  }
  return(data)
}

setup_data_general <- function(data, factor_list = c(), numeric_list = c()) {
  for (col in factor_list) {
    if (col %in% colnames(data)) {
      data[[col]] = as.factor(data[[col]])
    } else{
      print(col)
    }
  }
  for (col in numeric_list) {
    if (col %in% colnames(data)) {
      data[[col]] = as.numeric(data[[col]])
    } else{
      print(col)
    }
  }
  return(data)
}

usedata_extract <- function(data, flag_dryrun = 0) {
  if (flag_dryrun == 1) {
    target = data %>%
      dplyr::filter(!!sym(year_col) == 2018) %>%
      dplyr::select(mst_id_col) %>%
      dplyr::distinct() %>%
      dplyr::sample_n(2500) %>%
      dplyr::pull(!!sym(mst_id_col))
    dt_sample1 = data %>%
      dplyr::filter(!!sym(mst_id_col) %in% target) %>%
      dplyr::group_by(!!sym(mst_id_col)) %>%
      dplyr::mutate(count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(count >= 2) # 2018年度のデータなら、2016−2018で3年分あるとフルサンプル。だけどパネルデータであることを保証したいだけやから、これでおk
    target2 = data %>%
      dplyr::filter(!!sym(year_col) == 2018, !!sym(grade_col) == 4) %>%
      dplyr::select(mst_id_col) %>%
      dplyr::distinct() %>%
      dplyr::sample_n(250) %>%
      dplyr::pull(!!sym(mst_id_col))
    target3 = data %>%
      dplyr::filter(!!sym(year_col) == 2016, !!sym(grade_col) == 9) %>%
      dplyr::select(mst_id_col) %>%
      dplyr::distinct() %>%
      dplyr::sample_n(250) %>%
      dplyr::pull(!!sym(mst_id_col))
    dt_sample2 = data %>%
      dplyr::filter(!!sym(mst_id_col) %in% c(target2, target3))
    dt_sample = dplyr::bind_rows(dt_sample1, dt_sample2)
  } else if (flag_dryrun == 0) {
    dt_sample = data
  }
  return(dt_sample)
}

download_saitama = function(flag_dryrun=FALSE) {
  dfx = data.table::fread(path_main) %>% tibble::as_tibble()
  dt_sample = usedata_extract(setup_data(dfx), flag_dryrun = flag_dryrun) %>%
    dplyr::mutate(
      !!sym(absolute_age_col) := as.numeric(!!sym(relative_age_col)) + (as.integer(as.numeric(as.character(!!sym(grade_col)))) + 5) * 12
    ) %>%
    dplyr::group_by(!!sym(year_col), !!sym(school_id_col), !!sym(grade_col)) %>%
    dplyr::mutate(relative_age_rank = dplyr::percent_rank(as.numeric(!!sym(relative_age_col)))) %>%
    dplyr::ungroup()
  invisible({
    rm(list = c("dfx"));gc();gc()
    })
  dt_sample
}

download_toda = function() {
  dfx = data.table::fread(path_toda) %>%
    tibble::as_tibble() %>%
    setup_data() %>%
    usedata_extract(flag_dryrun = FALSE)
  dfx
}

download_saitama_birth1 = function() {
  factor_list = c(
    "lived_city", "year"
  )
  numeric_list = c(
    "relative_age",
    "age_father", "age_mother", "under_20_parent", "no_legitimate",
    "gram", "low_gram", "pregnant_week", "early_birth",
    "single_womb", "firstborn",
    "job_big_company", "job_company",
    "is_white_work_father", "is_white_work_mother", "no_job_father", "no_job_mother"
  )
  df = readr::read_csv(path_vital_stat, guess_max = 2000000) %>%
    dplyr::mutate(
      no_job = dplyr::if_else(!is.na(have_job), (have_job == 0) * 1, NaN),
      job1 = dplyr::if_else(job == "job1", 1, 0), job2 = dplyr::if_else(job == "job2", 1, 0),
      job3 = dplyr::if_else(job == "job3", 1, 0), job4 = dplyr::if_else(job == "job4", 1, 0),
      job5 = dplyr::if_else(job == "job5", 1, 0), job6 = dplyr::if_else(job == "job6", 1, 0),
      job7V = dplyr::if_else(job %in% c("job7", "jobV"), 1, 0)
      ) %>%
    setup_data_general(factor_list = factor_list, numeric_list = numeric_list)
  df
}

download_saitama_birth2 = function() {
  factor_list = c(
    "lived_city", "year"
  )
  numeric_list = c(
    "relative_age",
    "age_father", "age_mother", "under_20_parent", "no_legitimate",
    "gram", "low_gram", "pregnant_week", "early_birth",
    "single_womb", "firstborn",
    "job_big_company", "job_company",
    "is_white_work_father", "is_white_work_mother", "no_job_father", "no_job_mother"
  )
  df = readr::read_csv(path_vital_stat2, guess_max = 2000000) %>%
    setup_data_general(factor_list = factor_list, numeric_list = numeric_list)
  df
}