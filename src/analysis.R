#############################################
##  utility class for controler for regression
#############################################

source("./src/lib.R")
# setting
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

Analysis = R6::R6Class(
  classname = "Analysis",
  lock_objects = FALSE,
  public = list(
    initialize = function(...) {
      args = list(...)
      for (name in names(args)) {
        self[[name]] = args[[name]]
        }
    },
    add_attr = function(name, value) {
      self[[name]] = value
    }
  )
)

AnalysisFunction = R6::R6Class(
  classname = "AnalysisFunction",
  lock_objects = TRUE,
  public = list(
    func_get_tibble = NULL,
    func_get_formula_info = NULL,
    func_get_analysis_result = NULL,
    initialize = function(
      func_get_tibble = NULL,
      func_get_formula_info = NULL,
      func_get_analysis_result = NULL
      ) {
      self$func_get_tibble = func_get_tibble
      self$func_get_formula_info = func_get_formula_info
      self$func_get_analysis_result = func_get_analysis_result
    },
    get_tibble = function(analysis) {
      private$execute_func_using_analysis_attr(analysis, self$func_get_tibble)
    },
    get_formula_info = function(analysis) {
      private$execute_func_using_analysis_attr(analysis, self$func_get_formula_info)
    },
    get_analysis_result = function(analysis) {
      private$execute_func_using_analysis_attr(analysis, self$func_get_analysis_result)
    },
    get_analysis_method = function() {
      private$create_analysis_method(
        get_tibble = self$get_tibble,
        get_formula_list = self$get_formula_info,
        get_analysis_result = self$get_analysis_result
      )
    }
  ),
  private = list(
    execute_func_using_analysis_attr = function(analysis, func) {
      use_attr_name = private$get_func_input_name(func)
      use_attr = private$get_use_attr_name_list_from_object(analysis, use_attr_name)
      res = do.call(func, use_attr)
      for (name in names(res)) {
        analysis[[name]] = res[[name]]
        }  # 結果をanalysisに格納
    },
    get_func_input_name = function(func) {
      func %>% formals %>% names %>% purrr::discard(~ .x %in% c("..."))
    },
    get_use_attr_name_list_from_object = function(obj, use_attr_name_list) {
      purrr::map(use_attr_name_list, ~ purrr::pluck(obj, .x)) %>% setNames(use_attr_name_list)
    },
    create_analysis_method = function(get_tibble, get_formula_list, get_analysis_result, ...) {
      func = function(analysis) {
        do.call(get_tibble, args = list(analysis))
        do.call(get_formula_list, args = list(analysis))
        do.call(get_analysis_result, args = list(analysis))
        return(analysis)
      }
      return(func)
    }
  )
)
