#' PipeOpDropNA
#'
#' @description
#' `PipeOpDropNA` is an R6 class inheriting from `PipeOpTaskPreproc`. It filters out
#' rows from the dataset that contain any missing values (NA) in the features.
#'
#' @section Construction:
#' ```
#' PipeOpDropNA$new(id = "drop.na")
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"drop.na"`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from `PipeOpTaskPreproc`.
#'
#' The output is the input [`Task`][mlr3::Task] with rows containing NA values removed.
#'
#' @section State:
#' The `$state` is an empty list as this PipeOp does not store state information.
#'
#' @section Internals:
#' The operator identifies rows with any NA values in the features and excludes them from the task.
#' This is done in both training and prediction phases.
#'
#' @section Methods:
#' Methods inherited from `PipeOpTaskPreproc`.
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' # Assume iris task has NA values for demonstration
#' po_drop_na = PipeOpDropNA$new()
#'
#' po_drop_na$train(list(task))[[1]]$data()
#'
#' @family PipeOps
#' @export
PipeOpDropNA = R6::R6Class(
  "PipeOpDropNA",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "drop.na") {
      super$initialize(id)
    }
  ),

  private = list(
    .train_task = function(task) {
      self$state = list()
      private$compute_exclude(task)
    },

    .predict_task = function(task) {
      private$compute_exclude(task)
    },

    compute_exclude = function(task) {
      featuredata = task$data(cols = task$feature_names)
      exclude = apply(is.na(featuredata), 1, any)
      task$filter(task$row_ids[!exclude])
    }
  )
)

mlr_pipeops$add("drop.na", PipeOpDropNA)
