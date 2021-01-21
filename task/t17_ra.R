#############################################
##  Estimate effects of Age on *something* by RDD
#############################################¥
library(rlang)
library(boot)
library(ggplot2)
library(patchwork)
source("./src/data_download.R")
source("./src/regression_function.R")
source("./src/setting.R")
source("src/lib.R")


setup_zscore = function(dt) {
  standarize = function(values, avg, std) {
    return((values - avg) / std)
  }
  standarize_near_grade6 = function(df1, target) {
    # monkey patch. year, grade。さらにそれの0が3月生まれを表すこと前提
    min_year = df1 %>% dplyr::filter(!is.na(.[[target]])) %>% dplyr::pull(year) %>% na.omit() %>% as.character() %>% as.numeric() %>% min()
    min_grade = df1 %>%
      dplyr::filter(!is.na(.[[target]]), as.numeric(as.character(year)) == min_year) %>%
      dplyr::distinct(grade) %>%
      dplyr::mutate(
        grade_num = as.numeric(as.character(grade)),
        from_6 = abs(6 - grade_num)
        ) %>%
      dplyr::filter(from_6 == min(from_6)) %>%
      dplyr::pull(grade_num)
    avg = df1 %>% dplyr::filter(as.numeric(as.character(year)) == min_year, as.numeric(as.character(grade)) == min_grade) %>% dplyr::pull(target) %>% na.omit() %>% mean()
    std = df1 %>% dplyr::filter(as.numeric(as.character(year)) == min_year, as.numeric(as.character(grade)) == min_grade) %>% dplyr::pull(target) %>% na.omit() %>% sd()
    return(standarize(df1[[target]], avg, std))
  }
  return(
    dt %>%
      do(dplyr::mutate(
        .,
        gakuryoku_z = standarize_near_grade6(df1 = ., target = "gakuryoku"),
        math_level_z = standarize_near_grade6(df1 = ., target = "math_level"),
        kokugo_level_z = standarize_near_grade6(df1 = ., target = "kokugo_level"),
        eng_level_z = standarize_near_grade6(df1 = ., target = "eng_level"),
        dilligence_z = standarize_near_grade6(df1 = ., target = "dilligence"),
        selfefficacy_z = standarize_near_grade6(df1 = ., target = "selfefficacy"),
        selfcontrol_z = standarize_near_grade6(df1 = ., target = "selfcontrol"),
        strategy_z = standarize_near_grade6(df1 = ., target = "strategy"),
        teacherrelation_z = standarize_near_grade6(df1 = ., target = "teacherrelation"),
        zfriendrelation_z = standarize_near_grade6(df1 = ., target = "zfriendrelation"),
      )
    )
  )
}


get_data_seup = function() {
  setwd(rprojroot::find_rstudio_root_file())
  data_download = new.env();sys.source("./notebooks/Yamaguchi/RAE/src/data_download.R", envir = data_download)
  data_all = data.table::fread("./notebooks/Yamaguchi/RAE/data/dataset1.csv") %>%
    tibble::as_tibble() %>%
    data_download$setup_data(.) %>%
    data_download$usedata_extract(., flag_dryrun = 0)
  df_use = data_all %>%
    setup_zscore %>%
    dplyr::mutate(
      grade_int = as.integer(as.numeric(as.character(grade))),
      grade_str = stringr::str_c(as.integer(as.numeric(as.character(grade))), "th")
    ) %>%
    dplyr::mutate(
      absolute_age = as.numeric(relative_age) + (grade_int + 5) * 12,
      birthMar = dplyr::if_else(relative_age == 0, 1, 0),
      birthApr = dplyr::if_else(relative_age == 11, 1, 0)
    ) %>%
    dplyr::mutate(
      compare_pair1 = dplyr::case_when(
        relative_age == 0 ~ stringr::str_c("grade", grade_int, "th", "Mar_grade", grade_int - 1, "th", "Apr"),
        relative_age == 11 ~ stringr::str_c("grade", grade_int + 1, "th", "Mar_grade", grade_int, "th", "Apr"),
      ),
      compare_pair2 = dplyr::case_when(
        relative_age == 0 ~ stringr::str_c("grade", grade_int, "th", "Apr_grade", grade_int, "th", "Mar"),
        relative_age == 11 ~ stringr::str_c("grade", grade_int, "th", "Apr_grade", grade_int, "th", "Mar"),
      )
    )
  return(df_use)
}

get_tibble_for_effect_between_grade = function(dfx) {
  grade_list = 4:9
  grade_choice_list = purrr::map2(
    grade_list[1:length(grade_list) - 1], grade_list[2:length(grade_list)], ~ c(.x, .y)
  )
  tibble_for_ana = tibble::tribble()
  for (grade_choice in grade_choice_list) {
    tibble_for_ana = dfx %>%
      dplyr::filter(grade_int %in% grade_choice) %>%
      # dplyr::filter(relative_age %in% c(0, 11)) %>%  # いらない：https://peereffect.slack.com/archives/C8XBQ6VNK/p1557981193004100
      tidyr::nest() %>%
      dplyr::mutate(
        data_name = stringi::stri_c(grade_choice, sep = "_", collapse = "_")
      ) %>%
      dplyr::mutate(
        data = purrr::map(data, ~ (
          .x %>%
            dplyr::mutate(
              distance_cutoff = absolute_age - min(absolute_age, na.rm = TRUE) - 12,
              upper_cutoff = dplyr::if_else(distance_cutoff >= 0, 1, 0)
            )
        ))
      ) %>%
      dplyr::bind_rows(tibble_for_ana)
  }
  gc(reset = TRUE)
  return(tibble_for_ana)
}

get_formula_info_for_estimated_effect_between = function() {
  fm_list = c()
  fm_name = c()
  target_list = c(
    "gakuryoku", "kokugo_level", "math_level", "eng_level",
    "strategy_z", "selfcontrol_z", "selfefficacy_z", "dilligence_z",
    "hoursprep", "hourshome", "studytime", "cram", "teacherrelation_z", "zfriendrelation_z",
    c("reading_time_input_weekdays", "smart_phone_gaming_tv_time", "lesson_time", "playing_sport")
  )
  fm_str_template = "{dependent} ~  1 + distance_cutoff + I(distance_cutoff^2) + upper_cutoff + distance_cutoff * upper_cutoff + I(distance_cutoff^2) * upper_cutoff"
  # fm_str_template = "{dependent} ~  1 + distance_cutoff + upper_cutoff + distance_cutoff * upper_cutoff"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste(.x, "between_grade", sep = "_")))
  fm_info = purrr::map2(fm_list, fm_name, ~ list(fm = .x, name = .y))
  return(fm_info)
}

get_tibble_for_effect_within_grade = function(dfx) {
  ###
  #          birth :  3  2  1 12 11 10  9  8  7  6  5  4
  #    relative_age:  0  1  2  3  4  5  6  7  8  9 10 11
  # distance from cutoff: -1 -2 -3 -4 -5 -6  5  4  3  2  1  0
  ##
  tibble_for_ana = dfx %>%
    dplyr::mutate(
      distance_cutoff = dplyr::if_else(relative_age >= 6, 11 - relative_age, -1 - relative_age),
      upper_cutoff = dplyr::if_else(relative_age >= 6, 1, 0)
    ) %>%
    dplyr::group_by(grade) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data_name = stringi::stri_c("grade", as.character(grade))
    ) %>%
    dplyr::select(-grade)
}

get_formula_info_for_estimated_effect_within = function() {
  fm_list = c()
  fm_name = c()
  target_list = c(
    "gakuryoku", "kokugo_level", "math_level", "eng_level",
    "strategy_z", "selfcontrol_z", "selfefficacy_z", "dilligence_z",
    "hoursprep", "hourshome", "studytime", "cram", "teacherrelation_z", "zfriendrelation_z",
    c("reading_time_input_weekdays", "smart_phone_gaming_tv_time", "lesson_time", "playing_sport")
  )
  fm_str_template = "{dependent} ~  1 + distance_cutoff + upper_cutoff + distance_cutoff * upper_cutoff"
  fm_list = c(fm_list, purrr::map(target_list, ~ strformat(fm_str_template, c(dependent = .x)) %>% as.formula()))
  fm_name = c(fm_name, purrr::map(target_list,  ~ paste(.x, "within_grade", sep = "_")))
  fm_info = purrr::map2(fm_list, fm_name, ~ list(fm = .x, name = .y))
  return(fm_info)
}

t17_ra = function(dt_sample, save_folder_basis) {
  savefolder = file.path(save_folder_basis, "t17_ra")
  df_use = get_data_seup()
  ############################
  # Method1: Calc From Estimated Coef
  ############################
  # fm_info_list = get_formula_info()
  # summary_result = tibble::tribble()
  # for (fm_info in fm_info_list) {
  #   fm = fm_info$fm
  #   name = fm_info$name
  #   print(name)
  #   df_est = get_lm_data(df_use, fm)
  #   df_est_compare = df_est %>%
  #     dplyr::distinct(grade_str, absolute_age, compare_pair2, birthApr, .keep_all = FALSE) %>%
  #     na.omit %>%
  #     dplyr::mutate(
  #       from = birthApr,
  #       pair = compare_pair2
  #     ) %>%
  #     dplyr::select(-birthApr, -compare_pair2)
  #   out.i <- boot(data = df_est, statistic = difference, R = 300, fmx = fm, df_est_compare = df_est_compare)
  #   result = broom::tidy(out.i) %>%
  #     dplyr::mutate(
  #       name = name
  #     )
  #   summary_result %<>% dplyr::bind_rows(result)
  #   gc(reset = TRUE)
  # }
  # summary_result = summary_result %>%
  #   dplyr::rename(
  #     difference = statistic,
  #   ) %>%
  #   dplyr::select(-bias)
  # dir.create(savefolder, recursive = TRUE)
  # write.csv(x = summary_result, file = file.path(savefolder, "summary_tidy.csv"))
  ############################
  # Method2: Calc By RDD
  ############################
  ## get effect between grade
  fm_info_list = get_formula_info_for_estimated_effect_between()
  tibble_for_ana = get_tibble_for_effect_between_grade(dfx = df_use)
  summary_tidy = tibble::tribble()
  summary_glance = tibble::tribble()
  for (fm_info in fm_info_list) {
    fm = fm_info$fm
    name = fm_info$name
    print(name)
    aaa = tibble_for_ana %>%
      dplyr::mutate(no_have_data = purrr::map_lgl(data, ~is_no_df_have_values(df = .x, fm = fm))) %>%
      dplyr::filter(no_have_data == FALSE) %>%
      dplyr::mutate(
        data = purrr::map(data, ~ (
          .x %>%
            dplyr::select_(.dots = all.vars(fm)) %>%
            na.omit()
        ))
      ) %>%
      dplyr::mutate(
        result = purrr::map(data, ~ execute_lm(fm = fm, data = .x, ... = name)),
        name = paste(name, data_name, sep = ""),
        fm = as.character.formula(fm)
        ) %>%
      dplyr::select(-data)
    summary_tidy %<>% dplyr::bind_rows(helper_get_result_tidy(aaa))
    summary_glance %<>% dplyr::bind_rows(helper_get_result_glance(aaa))
    gc(reset = TRUE)
  }
  fm_info_list = get_formula_info_for_estimated_effect_within()
  tibble_for_ana = get_tibble_for_effect_within_grade(dfx = df_use)
  for (fm_info in fm_info_list) {
    fm = fm_info$fm
    name = fm_info$name
    print(name)
    aaa = tibble_for_ana %>%
      dplyr::mutate(no_have_data = purrr::map_lgl(data, ~is_no_df_have_values(df = .x, fm = fm))) %>%
      dplyr::filter(no_have_data == FALSE) %>%
      dplyr::mutate(
        data = purrr::map(data, ~ (
          .x %>%
            dplyr::select_(.dots = all.vars(fm)) %>%
            na.omit()
        ))
      ) %>%
      dplyr::mutate(
        result = purrr::map(data, ~ execute_lm(fm = fm, data = .x, ... = name)),
        name = paste(name, data_name, sep = ""),
        fm = as.character.formula(fm)
      ) %>%
      dplyr::select(-data)
    summary_tidy %<>% dplyr::bind_rows(helper_get_result_tidy(aaa))
    summary_glance %<>% dplyr::bind_rows(helper_get_result_glance(aaa))
    gc(reset = TRUE)
  }
  # save
  dir.create(savefolder, recursive = TRUE)
  write.csv(x = summary_tidy, file = file.path(savefolder, "summary_tidy2.csv"))
  write.csv(x = summary_glance, file = file.path(savefolder, "summary_glance2.csv"))
}


check_rdd_plot = function(save_folder_basis) {
  get_plot = function(
    lmobject, dfx.average, absolute_age_min, absolute_age_max, absolute_age_cutoff0,
    xlab = NULL, ylab = NULL
    )
    # > dfx.average
    # # A tibble: 24 x 2
    # absolute_age  score
    # 1          108 -1.34
    # 2          109 -1.29
    # 3          110 -1.23
    # 4          111 -1.17
    #
    {
    dfx.pre1 = data.frame(
      absolute_age = seq(absolute_age_min, absolute_age_cutoff0, by = 0.1),
      upper_cutoff = 0
    ) %>%
      dplyr::mutate(distance_cutoff = absolute_age - absolute_age_cutoff0) %>%
      dplyr::mutate(pre = predict(lmobject, .), se = predict(lmobject, ., se.fit = TRUE)$se.fit)
    dfx.pre2 = data.frame(
      absolute_age = seq(absolute_age_cutoff0, absolute_age_max, by = 0.1), 
      upper_cutoff = 1
    ) %>%
      dplyr::mutate(distance_cutoff = absolute_age - absolute_age_cutoff0) %>%
      dplyr::mutate(pre = predict(lmobject, .), se = predict(lmobject, ., se.fit = TRUE)$se.fit)
    p = ggplot(NULL)
    p = p + geom_line(data = dfx.pre1, aes(x = absolute_age, y = pre, color = "b"))
    p = p + geom_line(data = dfx.pre2, aes(x = absolute_age, y = pre, color = "r"))
    p = p + geom_point(data = dfx.average, aes(x = absolute_age, y = score))
    p = p + geom_vline(xintercept = absolute_age_cutoff0, linetype = "dashed")
    p = p + xlim(absolute_age_min, absolute_age_max)
    p = p + theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.minor = element_line(color = NA),
      panel.grid.major = element_line(color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15)
    ) + xlab(NULL) + ylab(ylab) + ggtitle(xlab)
    p
  }
  get_blank_plot = function(absolute_age_min, absolute_age_max) {
    df <- data.frame()
    p = ggplot(NULL)
    p = p + geom_line()
    # p = p + xlim(absolute_age_min, absolute_age_max)
    p = p + theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.minor = element_line(color = NA),
      panel.grid.major = element_line(color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15)
    ) + xlab(NULL) + ylab(ylab) + ggtitle(xlab)
    p
  }
  result_model_off = function(lmobject) {l
    mobject$model = NULL; lmobject
    }
  df_use = get_data_seup()
  df_use %<>% dplyr::filter(year != 2015)
  fm_info_list = get_formula_info_for_estimated_effect_between()
  tibble_for_ana = get_tibble_for_effect_between_grade(dfx = df_use)
  summary_tidy = tibble::tribble()
  for (fm_info in fm_info_list) {
    right.vars = c(purrr::keep(labels(terms(fm_info$fm)), function(x) {x %in% all.vars(fm_info$fm)}), "absolute_age")
    left.vars = purrr::discard(all.vars(fm_info$fm), function(x) {x %in% right.vars})
    left.vars_sym = sym(left.vars)
    aaa = tibble_for_ana %>%
      dplyr::mutate(
        result = purrr::map(data, ~ execute_lm(fm = fm_info$fm, data = .x, ... = fm_info$name)),
        name = paste(fm_info$name, data_name, sep = ""),
        fm = as.character.formula(fm_info$fm),
        left.vars = left.vars
      ) %>%
      dplyr::mutate(
        absolute_age_min = purrr::map_dbl(data, ~ min(.x[["absolute_age"]])),
        absolute_age_max = purrr::map_dbl(data, ~ max(.x[["absolute_age"]])),
        absolute_age_cutoff0 = purrr::map_dbl(data, ~  .x %>% dplyr::filter(distance_cutoff == 0) %>% dplyr::pull(absolute_age) %>% min()),
        count_upper_cutoff = purrr::map_dbl(result, ~ length(unique(.x[["model"]][["upper_cutoff"]])))
      ) %>%
      dplyr::mutate(
        dfx.average = purrr::map(data, ~ .x %>%
          dplyr::group_by(absolute_age) %>%
          dplyr::summarise(score = mean(!!left.vars_sym, na.rm = TRUE))
        ),
        result = dplyr::if_else(count_upper_cutoff == 2, result_model_off(result), NULL)
        ) %>%
      dplyr::select(-data)
    summary_tidy %<>% dplyr::bind_rows(aaa)
  }

  plot_info_list = list(
    c(value = "4_5", label = "Grade 4 vs Grade 5", is_grade_left = TRUE),
    c(value = "5_6", label = "Grade 5 vs Grade 6", is_grade_left = FALSE),
    c(value = "6_7", label = "Grade 6 vs Grade 7", is_grade_left = FALSE),
    c(value = "7_8", label = "Grade 7 vs Grade 8", is_grade_left = FALSE),
    c(value = "8_9", label = "Grade 8 vs Grade 9", is_grade_left = FALSE)
  )

  subject_info_list_lisr = list(
    list(name = "cognitive", subject_info_list = list(
      list(subject = "kokugo_level", name = "Japanese", is_subject_top = TRUE),
      list(subject = "math_level", name = "Math", is_subject_top = FALSE),
      list(subject = "eng_level", name = "English", is_subject_top = FALSE)
    )),
    list(name = "noncognitive", subject_info_list = list(
      list(subject = "selfcontrol_z", name = "Self-control", is_subject_top = TRUE),
      list(subject = "selfefficacy_z", name = "Self-efficacy", is_subject_top = FALSE),
      list(subject = "dilligence_z", name = "Consciousness", is_subject_top = FALSE)
    )),
    list(name = "inputs", subject_info_list = list(
      list(subject = "studytime", name = "Study time", is_subject_top = TRUE),
      list(subject = "cram", name = "Prep school", is_subject_top = FALSE),
      list(subject = "teacherrelation_z", name = "Teacher", is_subject_top = FALSE),
      list(subject = "zfriendrelation_z", name = "Friend", is_subject_top = FALSE)
    )),
    list(name = "outside", subject_info_list = list(
      list(subject = "reading_time_input_weekdays", name = "Reading time", is_subject_top = TRUE),
      list(subject = "lesson_time", name = "Lesson time", is_subject_top = FALSE),
      list(subject = "playing_sport", name = "Playing sports time", is_subject_top = FALSE),
      list(subject = "smart_phone_gaming_tv_time", name = "Watching TV or SP or game time", is_subject_top = FALSE)
    ))
  )
  for (subject_info_list_info in subject_info_list_lisr) {
    subject_info_list = subject_info_list_info[["subject_info_list"]]
    filename = subject_info_list_info[["name"]]
    p_all = ggplot(NULL)
    for (subject_info in subject_info_list) {
      p_line = ggplot(NULL)
      for (plot_info in plot_info_list) {
        use_result = summary_tidy %>% dplyr::filter(left.vars == subject_info[["subject"]], data_name == plot_info[["value"]])
        lm_object.use = use_result %>% dplyr::pull(result) %>% .[[1]]
        dfx.average.use = use_result %>% dplyr::pull(dfx.average) %>% .[[1]]
        absolute_age_min.use = use_result %>% dplyr::pull(absolute_age_min) %>% .[[1]] - 2
        absolute_age_max.use = use_result %>% dplyr::pull(absolute_age_max) %>% .[[1]] + 2
        absolute_age_cutoff0.use = use_result %>% dplyr::pull(absolute_age_cutoff0) %>% .[[1]]
        xlab =  if (subject_info[["is_subject_top"]]) plot_info[["label"]] else NULL
        ylab =  if (plot_info[["is_grade_left"]]) subject_info[["name"]] else NULL
        if (is.null(lm_object.use)) {
          p = get_blank_plot(absolute_age_min.use, absolute_age_max.use)
          p_line = p_line | p
          next
        }
        p = get_plot(
          lmobject = lm_object.use, dfx.average = dfx.average.use, absolute_age_min = absolute_age_min.use,
          absolute_age_max = absolute_age_max.use, absolute_age_cutoff0 = absolute_age_cutoff0.use,
          xlab = xlab, ylab = ylab
        )
        p_line = p_line | p
      }
      p_all = p_all / (p_line)
      is_subject_top = FALSE
    }
    p_all = p_all +
      theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
    savefolder = file.path(save_folder_basis, "fig", "rdd")
    dir.create(savefolder, recursive = TRUE)
    save_path = file.path(savefolder, paste(filename, "mpg1.pdf", sep = "_"))
    ggsave(file = save_path, p_all, width = 3 * length(plot_info_list), height = 3 * length(subject_info_list), limitsize = FALSE)
    gc()
  }
}
