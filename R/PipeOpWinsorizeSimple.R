#' PipeOpWinsorizeSimple
#'
#' @description
#' `PipeOpWinsorizeSimple` is an R6 class inheriting from `PipeOpTaskPreprocSimple`. It applies
#' winsorization to numeric features of a dataset by limiting extreme values based on specified quantiles.
#'
#' @section Construction:
#' ```
#' PipeOpWinsorizeSimple$new(id = "winsorization", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"winsorization"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Parameters:
#' The parameters are:
#' * `probs_low` :: `numeric(1)`\cr
#'   Lower quantile probability for winsorization. Default is 0.05.
#' * `probs_high` :: `numeric(1)`\cr
#'   Upper quantile probability for winsorization. Default is 0.95.
#' * `na.rm` :: `logical(1)`\cr
#'   Whether to remove NA values in the computation of quantiles. Default is TRUE.
#' * `qtype` :: `integer(1)`\cr
#'   Type of quantile calculation. Default is 7 (see `?quantile` for types).
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from `PipeOpTaskPreprocSimple`.
#'
#' The output is the input [`Task`][mlr3::Task] with numeric features winsorized based on the specified quantiles.
#'
#' @section State:
#' The `$state` is a named `list` containing:
#' * `minvals` :: `numeric`\cr
#'   The lower bound values for each feature calculated during training.
#' * `maxvals` :: `numeric`\cr
#'   The upper bound values for each feature calculated during training.
#'
#' @section Internals:
#' The operator calculates the specified lower and upper quantiles for each feature and uses them
#' to limit extreme values during both training and prediction.
#'
#' @section Methods:
#' Methods inherited from `PipeOpTaskPreprocSimple`.
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' po_winsorize = PipeOpWinsorizeSimple$new()
#'
#' po_winsorize$train(list(task))[[1]]$data()
#'
#' @family PipeOps
#' @export
PipeOpWinsorizeSimple = R6::R6Class(
  "PipeOpWinsorizeSimple",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    groups = NULL,
    initialize = function(id = "winsorization", param_vals = list()) {
      ps = ParamSet$new(list(
        ParamDbl$new("probs_low", lower = 0, upper = 1, default = 0.05, tags = c("winsorize_tag")),
        ParamDbl$new("probs_high", lower = 0, upper = 1, default = 0.95, tags = c("winsorize_tag")),
        ParamLgl$new("na.rm", default = TRUE, tags = c("winsorize_tag")),
        ParamInt$new("qtype", lower = 1L, upper = 9L, default = 7L, tags = c("winsorize_tag"))
      ))
      ps$values = list(qtype = 7L, na.rm = TRUE, probs_low = 0.95, probs_high = 0.05)
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("numeric"))
    }
  ),

  private = list(

    .get_state_dt = function(dt, levels, target) {
      # debug
      # task = copy(tsk_aroundzero_month)
      # dt = tsk_aroundzero_month$data()
      # cols = tsk_aroundzero_month$feature_types[type %in% c("numeric", "integer"), id]
      # dt = dt[, ..cols]
      # pv = list(
      #   probs_low = 0.01, probs_high = 0.99, na.rm = TRUE, qtype = 7
      # )
      # self = list()

      # params
      pv = self$param_set$get_values(tags = "winsorize_tag")

      # state variables
      q = dt[, lapply(.SD,
                      quantile,
                      probs = c(pv$probs_low, pv$probs_high),
                      na.rm = pv$na.rm,
                      type = pv$qtype)]
      list(
        minvals = q[1],
        maxvals = q[2]
      )
    },

    .transform_dt  = function(dt, levels) {
      dt = dt[, Map(function(a, b) data.table::fifelse(a < b, b, a), .SD, self$state$minvals)]
      dt = dt[, Map(function(a, b) data.table::fifelse(a > b, b, a), .SD, self$state$maxvals)]
      dt
    }
  )
)

mlr_pipeops$add("winsorization", PipeOpWinsorizeSimple)
