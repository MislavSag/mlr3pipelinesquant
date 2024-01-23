#' PipeOpDropNACol
#'
#' @description
#' `PipeOpDropNACol` is an R6 class inheriting from `PipeOpTaskPreprocSimple`. It removes columns
#' with a high proportion of missing values (NA) from the dataset. The threshold for dropping
#' a column is specified by the `cutoff` parameter.
#'
#' @section Construction:
#' ```
#' PipeOpDropNACol$new(id = "drop.nacol", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"drop.nacol"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Parameters:
#' The parameters are:
#' * `cutoff` :: `numeric(1)`\cr
#'   Proportion threshold for dropping a column. Columns with a proportion of missing
#'   values higher than `cutoff` are dropped. Range: (0, 1). Default: 0.2.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from `PipeOpTaskPreprocSimple` .
#'
#' The output is the input [`Task`][mlr3::Task] with columns having a high NA proportion (above `cutoff`) removed.
#'
#' @section State:
#' The `$state` is a named `list` containing:
#' * `cnames` :: `character`\cr
#'   The names of the columns that are retained (i.e., those with NA proportion below `cutoff`).
#'
#' @section Internals:
#' The operator calculates the proportion of NAs in each column and compares it with the `cutoff`
#' parameter. Columns exceeding this threshold are excluded from the resulting dataset.
#'
#' @section Methods:
#' Methods inherited from `PipeOpTaskPreprocSimple`/`PipeOp`.
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' # Assume iris task has NA values for demonstration
#' po_drop_nacol = PipeOpDropNACol$new()
#'
#' po_drop_nacol$train(list(task))[[1]]$data()
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @export
PipeOpDropNACol = R6::R6Class(
  "PipeOpDropNACol",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "drop.nacol", param_vals = list()) {
      ps = ParamSet$new(list(
        ParamDbl$new("cutoff", lower = 0, upper = 1, default = 0.05, tags = c("dropnacol_tag"))
      ))
      ps$values = list(cutoff = 0.2)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    }
  ),

  private = list(
    .get_state = function(task) {
      pv = self$param_set$get_values(tags = "dropnacol_tag")
      features_names = task$feature_names
      data = task$data(cols = features_names)
      keep = sapply(data, function(column) (sum(is.na(column))) / length(column) < pv$cutoff)
      list(cnames = colnames(data)[keep])
    },

    .transform = function(task) {
      task$select(self$state$cnames)
    }
  )
)

mlr_pipeops$add("drop.nacol", PipeOpDropNACol)
