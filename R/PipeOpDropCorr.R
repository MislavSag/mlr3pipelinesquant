#' PipeOpDropCorr
#'
#' @description
#' `PipeOpDropCorr` is an R6 class inheriting from `PipeOpTaskPreprocSimple`. It filters out
#' features from a dataset that are highly correlated with each other, based on a specified
#' correlation threshold.
#'
#' @section Construction:
#' ```
#' PipeOpDropCorr$new(id = "drop.corr", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"drop.corr"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Parameters:
#' The parameters are:
#' * `use` :: `character(1)`\cr
#'   Method for handling missing values in correlation computation. Default is `"everything"`.
#' * `method` :: `character(1)`\cr
#'   Correlation method, one of `"pearson"`, `"kendall"`, or `"spearman"`. Default is `"pearson"`.
#' * `cutoff` :: `numeric(1)`\cr
#'   The cutoff threshold for correlation. Features with a correlation higher than this value are dropped. Default is 0.99.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from `PipeOpTaskPreprocSimple`.
#'
#' The output is the input [`Task`][mlr3::Task] with highly correlated features removed.
#'
#' @section State:
#' The `$state` is a named `list` containing:
#' * `cnames` :: `character`\cr
#'   The names of the columns that are retained (i.e., those with correlation below `cutoff`).
#'
#' @section Internals:
#' The operator calculates a correlation matrix and removes features that are highly correlated,
#' as defined by the `cutoff` parameter. The correlation method and handling of missing values
#' are determined by the `method` and `use` parameters, respectively.
#'
#' @section Methods:
#' Methods inherited from `PipeOpTaskPreprocSimple`.
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' po_drop_corr = PipeOpDropCorr$new()
#'
#' po_drop_corr$train(list(task))[[1]]$data()
#'
#' @family PipeOps
#' @export
PipeOpDropCorr = R6::R6Class(
  "PipeOpDropCorr",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "drop.const", param_vals = list()) {
      ps = ParamSet$new(list(
        ParamFct$new("use", c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"), default = "everything"),
        ParamFct$new("method", c("pearson", "kendall", "spearman"), default = "pearson"),
        ParamDbl$new("cutoff", lower = 0, upper = 1, default = 0.99)
      ))
      ps$values = list(use = "everything", method = "pearson", cutoff = 0.99)
      super$initialize(id = id, param_set = ps, param_vals = param_vals, feature_types = c("numeric"))
    }
  ),

  private = list(
    .get_state = function(task) {
      # debug
      # pv = list(
      #   use = "everything",
      #   method = "pearson",
      #   cutoff = 0.9
      # )

      fn = task$feature_types[type == self$feature_types, id]
      data = task$data(cols = fn)
      pv = self$param_set$values

      cm = mlr3misc::invoke(stats::cor, x = data, use = pv$use, method = pv$method)
      cm[upper.tri(cm)] <- 0
      diag(cm) <- 0
      cm <- abs(cm)
      remove_cols <- colnames(data)[apply(cm, 2, function(x) any(x > pv$cutoff))]
      keep_cols <- setdiff(fn, remove_cols)
      list(cnames = keep_cols)
    },

    .transform = function(task) {
      task$select(self$state$cnames)
    }
  )
)

mlr_pipeops$add("drop.corr", PipeOpDropCorr)
