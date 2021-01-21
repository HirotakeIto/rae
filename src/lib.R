# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`
# helper for formula
as.character.formula = function(x) Reduce(paste, deparse(x))

return_fm_str_excluding_1level_factor = function(
  fm, data, choices_1level_factor = c("year", "grade", "book", "sex", "is_school_prime")
) {
  # remove_variable_from_formula_str = function(fm_str, col) {
  #   stringi::stri_replace(
  #     fm_new,
  #     regex = strformat(
  #       "\\+\\s*({col}|as.factor\\({col}\\))", list(col = col)),
  #     replacement = "")
  # }
  remove_variable_from_formula_str = function(fm_str, col) {
    stringi::stri_replace_all(
      fm_str,
      regex = strformat("\\+((?![\\+\\|]).)*{col}((?![\\+\\|]).)*", list(col = col)),
      replacement = "")
  }
  fm_new = as.character.formula(fm)
  removed = c()
  for (col in choices_1level_factor) {
    if (col %in% colnames(data)) {
      # colのfactorが一個しかないならformulaからcolを削除する
      if (dplyr::n_distinct(data[[col]]) == 1) {
        fm_new = remove_variable_from_formula_str(fm_str = fm_new, col = col)
        removed = c(removed, col)
      }
    } else {
      # # そもそもデータに存在しなくても削除する
      # if (grepl(col, fm_new)) {
      #   fm_new = remove_variable_from_formula_str(fm_str = fm_new, col = col)
      #   removed = c(removed, col)
      # }
    }
  }
  if (length(removed) > 0) {
    cat(stringi::stri_c(
      stringi::stri_c(removed, collapse = ", "),
      "   is removed. formula converted to ",
      fm_new,
      "\n",
      sep = ""))
  }
  return(fm_new)
}

# helper for getting information about dataframe
is_no_df_have_values = function(df, fm) df[all.vars(fm)] %>% na.omit() %>% dim() %>% .[[1]] == 0

is_no_df_have_values_vars = function(df, all_vars) df[all_vars] %>% na.omit() %>% dim() %>% .[[1]] == 0

have_using_columns_completely = function(dfx, fm) is_no_df_have_values(df = dfx, fm = fm) == FALSE

have_all_columns_using = function(dfx, fm) min(all.vars(fm) %in% names(dfx)) == 1

can_do_analysis = function(dfx, fm) have_using_columns_completely(dfx, fm) & have_all_columns_using(dfx, fm)

# helper for exextion of regression
execute_felm = function(fm, data, ...) {
  tryCatch({
    response = lfe::felm(fm, data = data)
    return(response)
  },
  error = function(e) {
    print(paste("Fail:::::", paste(deparse(fm), ":::::", c(...), collapse = ""), sep = ""))
    print(e$message)
    return(NULL)
  },
  silent = TRUE
  )
}

execute_lm = function(fm, data, ...) {
  tryCatch({
    response = lm(fm, data = data)
    return(response)
  },
  error = function(e) {
    print(paste("Fail:::::", paste(deparse(fm), ":::::", c(...), collapse = ""), sep = ""))
    print(e$message)
    return(NULL)
  },
  silent = TRUE
  )
}

execute_quantreg = function(fm, data, ...) {
  tryCatch({
    response = quantreg::rq(fm, data = data, ...)
    return(response)
  },
  error = function(e) {
    print(paste("Fail:::::", paste(deparse(fm), ":::::", c(...), collapse = ""), sep = ""))
    print(e$message)
    return(NULL)
  },
  silent = TRUE
  )
}

# helper for result object about regression
get_rsq = function(res) {
  if (class(res) == "felm") {
    return(unname(summary(res)[["P.adj.r.squared"]]))
  }
}

get_result = function(nested_dt, formula_info_list) {
  # nested_dtにdataというカラムがあることを前提にします
  # formula_info_listはfmとnameという名前の要素を持つベクトルの集まり
  # ハードコーディングだけど考えている時間勿体無いからこうします
  summary_result = tibble::tribble()
  for (formula_info in formula_info_list) {
    fm = formula_info$fm
    name = formula_info$name
    print(name)
    aaa = nested_dt %>%
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
        result = purrr::map(data, ~ execute_felm(fm = fm, data = .x, name = name)),
        name = name,
        fm = as.character.formula(fm)) %>%
      dplyr::select(-data)
    summary_result %<>% dplyr::bind_rows(aaa)
  }
  return(summary_result)
}

helper_get_result_tidy = function(result) {
  res_tidy = result %>%
    dplyr::filter(purrr::map_lgl(result, ~ ! is.null(.x))) %>%
    dplyr::mutate(col_tidy = purrr::map(result, ~ broom::tidy(.x))) %>%
    dplyr::select(-result) %>%
    tidyr::unnest()
  res_tidy
}

helper_get_result_glance = function(result) {
  res_glance = result %>%
    dplyr::filter(purrr::map_lgl(result, ~ ! is.null(.x))) %>%
    dplyr::mutate(
      col_glance = purrr::map(result, ~ broom::glance(.x))
      ) %>%
    dplyr::select(-result) %>%
    tidyr::unnest()
  res_glance
}

helper_get_result_tidy = function(result) {
  res_tidy = result %>%
    dplyr::filter(purrr::map_lgl(result, ~ ! is.null(.x))) %>%
    dplyr::mutate(col_tidy = purrr::map(result, ~ broom::tidy(.x))) %>%
    dplyr::select(-result) %>%
    tidyr::unnest()
  res_tidy
}

helper_get_result_glance = function(result) {
  res_glance = result %>%
    dplyr::filter(purrr::map_lgl(result, ~ ! is.null(.x))) %>%
    dplyr::mutate(
      col_glance = purrr::map(result, ~ broom::glance(.x))
    ) %>%
    dplyr::select(-result) %>%
    tidyr::unnest()
  res_glance
}

helper_get_felm_result_glance = function(result) {
  res_glance = result %>%
    dplyr::filter(purrr::map_lgl(result, ~ ! is.null(.x))) %>%
    dplyr::mutate(
      obs = purrr::map_dbl(result, ~ .x$N),
      col_glance = purrr::map(result, ~ broom::glance(.x))
    ) %>%
    dplyr::select(-result) %>%
    tidyr::unnest()
  res_glance
}

helper_get_result_stage1_tidy = function(result, res_tidy) {
  res_tidy = result %>%
    dplyr::filter(purrr::map_lgl(result, ~ ! is.null(.x))) %>%
    dplyr::mutate(stage1 = purrr::map(result, ~ .x$stage1)) %>%
    dplyr::mutate(tidy = purrr::map(stage1, ~broom::tidy(.x))) %>%
    dplyr::select(-result, -stage1) %>%
    tidyr::unnest(tidy)
  res_tidy
}

helper_get_result_stage1_glance = function(result) {
  res_glance = result %>%
    dplyr::filter(purrr::map_lgl(result, ~ ! is.null(.x))) %>%
    dplyr::mutate(stage1 = purrr::map(result, ~ .x$stage1)) %>%
    dplyr::mutate(glance = purrr::map(stage1, ~broom::glance(.x))) %>%
    dplyr::mutate(
      obs = purrr::map_dbl(result, ~ .x$N),
      iv1fstat = purrr::map_dbl(stage1, ~ .x$iv1fstat[[1]][["F"]])
    ) %>%
    dplyr::select(-result, -stage1) %>%
    tidyr::unnest(glance)
  res_glance
}

# other's function
str_random =  function(n = 5000) {
  a = do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

strformat = function(str, vals) {
  vars = stringi::stri_match_all(str, regex = "\\{.*?\\}", vectorize_all = FALSE)[[1]][, 1]
  x = str
  for (i in seq_along(names(vals))) {
    var_name = names(vals)[i]
    var_code = paste0("{", var_name, "}")
    x = stringi::stri_replace_all_fixed(x, var_code, vals[[var_name]], vectorize_all = TRUE)
  }
  return(x)
}
