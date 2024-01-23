#' PipeOpUniform
#'
#' @description
#' `PipeOpUniform` is an R6 class inheriting from `PipeOpTaskPreproc`. It applies a
#' uniformization transformation to numeric features of a dataset. The transformation
#' is based on the empirical cumulative distribution functions (ECDFs). Group-based
#' uniformization can also be performed if `groups` are specified.
#'
#' @section Construction:
#' ```
#' PipeOpUniform$new(id = "uniformization", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"uniformization"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from `PipeOpTaskPreproc`.
#'
#' The output is the input [`Task`][mlr3::Task] with numeric features transformed via ECDF.
#'
#' @section State:
#' The `$state` is a named `list` containing:
#' * `ecdf_` :: list of functions\cr
#'   The empirical cumulative distribution functions computed for each feature.
#'
#' @section Internals:
#' Depending on whether `groups` are provided, the PipeOp computes ECDFs for each feature
#' either for the entire dataset or within each group. These ECDFs are then used to transform
#' the feature values to follow a uniform distribution.
#'
#' @section Methods:
#' Methods inherited from `PipeOpTaskPreproc`.
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' po_uniform = PipeOpUniform$new()
#'
#' po_uniform$train(list(task))[[1]]$data()
#'
#' @family PipeOps
#' @export
PipeOpUniform = R6::R6Class(
  "PipeOpUniform",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    groups = NULL,
    initialize = function(id = "uniformization", param_vals = list()) {
      super$initialize(id,
                       param_vals = param_vals,
                       feature_types = c("numeric", "integer"))
    }
  ),

  private = list(
    .select_cols = function(task) {
      self$groups = task$groups
      task$feature_names
    },

    .train_dt = function(dt, levels, target) {
      # state variables
      if (!(is.null(self$groups))) {
        row_ids  = self$groups[group == self$groups[nrow(self$groups), group], row_id]
        ecdf_ = mlr3misc::map(dt[row_ids], ecdf)
      } else {
        ecdf_ = mlr3misc::map(dt, ecdf)
      }
      self$state = list(ecdf_ = ecdf_)

      # dt object train
      if (!(is.null(self$groups))) {
        dt = dt[, lapply(.SD, function(x)
          as.vector(ecdf(x)(x))), by = self$groups[, group]]
        dt = dt[,-1]
      } else {
        dt = dt[, lapply(.SD, function(x)
          ecdf(x)(x))]
      }
      dt
    },

    .predict_dt = function(dt, levels) {
      dt[, Map(function(a, b)
        b(a), .SD, self$state$ecdf_)]
    }
  )
)

mlr_pipeops$add("uniformization", PipeOpUniform)
