context("GraphLearner")

test_that("basic graphlearner tests", {
  skip_on_cran()  # takes too long
  task = mlr_tasks$get("iris")

  lrn = mlr_learners$get("classif.rpart")
  gr = PipeOpLearner$new(lrn)

  glrn = GraphLearner$new(gr)
  expect_true(run_experiment(task, glrn)$ok)

  glrn = GraphLearner$new(gr)
  expect_learner(glrn)
  glrn$train(task)

  expect_prediction_classif({
    graphpred = glrn$predict(task)
  })
  expect_equal(graphpred,
    lrn$train(task)$predict(task))

  expect_prediction_classif({
    graphpred = glrn$predict(task)
  })
  expect_equal(graphpred, lrn$predict(task))

  set.seed(1)
  lrn = mlr_learners$get("classif.rpart")
  resgraphlrn = resample(task, lrn, mlr_resamplings$get("cv"))
  set.seed(1)
  resjustlrn = resample(task, lrn, mlr_resamplings$get("cv"))
  expect_equal(resgraphlrn$prediction(), resjustlrn$prediction())

  gr2 = PipeOpScale$new() %>>% PipeOpLearner$new(lrn)
  glrn2 = GraphLearner$new(gr2)
  glrn2_clone = glrn2$clone(deep = TRUE)
  expect_learner(glrn2)
  expect_true(run_experiment(task, glrn)$ok)
  glrn2$train(task)
  glrn2_clone$state = glrn2$state
#  glrn2_clone$state$log = glrn2_clone$state$log$clone(deep = TRUE)  # FIXME: this can go when mlr-org/mlr3#343 is fixed
#  glrn2_clone$state$model$classif.rpart$log = glrn2_clone$state$model$classif.rpart$log$clone(deep = TRUE)  # FIXME: this can go when mlr-org/mlr3#343 is fixed
  expect_deep_clone(glrn2_clone, glrn2$clone(deep = TRUE))
  expect_prediction_classif({
    graphpred2 = glrn2$predict(task)
  })

  expect_equal(glrn2$predict(task), glrn2_clone$predict(task))

  scidf = cbind(scale(iris[1:4]), iris[5])
  scalediris = TaskClassif$new("scalediris", as_data_backend(scidf), "Species")

  dblrn = mlr_learners$get("classif.debug")
  dblrn$param_set$values$save_tasks = TRUE

  dbgr = GraphLearner$new(PipeOpScale$new() %>>% PipeOpLearner$new(dblrn))


  expect_equal(dbgr$train(task), dbgr)

  # debuglearner predict() modifies model, but PipeOpLearner does not accept
  # model changes in predict phase, so would ordinarily discard the change.
  # Here we swap the debuglearner model by an environment, which gets updated
  # by-reference, so we can get the $task_predict slot eventually.
  dbmodels = as.environment(dbgr$model$classif.debug$model)
  dbgr$state$model$classif.debug$model = dbmodels

  dbgr$predict(task)

  expect_equal(dbmodels$task_train$data(), scalediris$data())
  expect_equal(dbmodels$task_predict$data(), scalediris$data())
})

test_that("GraphLearner clone_graph FALSE", {

  # prepare graph
  gr1 = po("pca") %>>% lrn("classif.rpart")
  gr1$train(tsk("iris"))
  expect_true(gr1$is_trained)

  gl = GraphLearner$new(gr1, clone_graph = FALSE)

  # graph is not cloned
  expect_identical(gl$graph, gr1)

  # GraphLearner$initialize resets graph state
  expect_false(gr1$is_trained)

  # compare result of training with a subset of iris
  gl$train(tsk("iris")$filter(1:110))

  # gr1 state is not set by this
  expect_false(gr1$is_trained)

  # train gr1 with a *different* task than gl
  gr1$train(tsk("iris"))

  # simulate pipeline with iris subset to get expected GraphLearner prediction result
  pp = po("pca")
  expected_prediction = lrn("classif.rpart")$train(pp$train(list(tsk("iris")$filter(1:110)))[[1]])$predict(pp$predict(list(tsk("iris")))[[1]])

  # check that predicting on iris subset gives different result from gr1$predict()
  expect_false(isTRUE(all.equal(gr1$predict(tsk("iris"))[[1]], expected_prediction)))
  expect_true(gr1$is_trained)

  # check that the GraphLearner predicts what we expect
  expect_true(isTRUE(all.equal(gl$predict(tsk("iris")), expected_prediction)))

  expect_false(gr1$is_trained)  # predicting with GraphLearner resets Graph state

  expect_identical(gl$graph, gr1)

  # check that as_learner respects `clone` now
  gl = as_learner(gr1, clone = FALSE)
  expect_identical(gl$graph, gr1)

})

test_that("graphlearner parameters behave as they should", {
  dblrn = mlr_learners$get("classif.debug")
  dblrn$param_set$values$save_tasks = TRUE

  dbgr = PipeOpScale$new() %>>% PipeOpLearner$new(dblrn)

  expect_subset(c("scale.center", "scale.scale", "classif.debug.x"), names(dbgr$param_set$params))

  dbgr$param_set$values$classif.debug.x = 1

  expect_equal(dbgr$param_set$values$classif.debug.x, 1)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 1)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 1)

  dbgr$pipeops$classif.debug$param_set$values$x = 0

  expect_equal(dbgr$param_set$values$classif.debug.x, 0)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 0)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 0)

  dbgr$pipeops$classif.debug$learner$param_set$values$x = 0.5

  expect_equal(dbgr$param_set$values$classif.debug.x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 0.5)

  expect_error({
    dbgr$param_set$values$classif.debug.x = "a"
  })
  expect_error({
    dbgr$pipeops$classif.debug$param_set$values$x = "a"
  })
  expect_error({
    dbgr$pipeops$classif.debug$learner$param_set$values$x = "a"
  })

  expect_equal(dbgr$param_set$values$classif.debug.x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 0.5)

  dblrn = mlr_learners$get("classif.debug")
  dblrn$param_set$values$message_train = 1
  dblrn$param_set$values$message_predict = 1
  dblrn$param_set$values$warning_train = 1
  dblrn$param_set$values$warning_predict = 1

  pol = PipeOpLearner$new(dblrn, param_vals = list(message_predict = 0, warning_train = 0, warning_predict = 0))

  gl = GraphLearner$new(pol, param_vals = list(classif.debug.warning_train = 1, classif.debug.warning_predict = 1))

  gl$param_set$values$classif.debug.warning_predict = 0

  expect_mapequal(gl$param_set$values,
    list(classif.debug.message_predict = 0, classif.debug.message_train = 1, classif.debug.warning_predict = 0, classif.debug.warning_train = 1))
})

test_that("graphlearner type inference", {
  skip_on_cran()  # takes too long
  # default: classif
  lrn = GraphLearner$new(mlr_pipeops$get("nop"))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  ###########
  # classif #
  ###########

  # inference from pipeoplearner
  lrn = GraphLearner$new(mlr_pipeops$get("learner", lrn("classif.rpart")))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  # inference from output only
  lrn = GraphLearner$new(mlr_pipeops$get("copy", 1) %>>% mlr_pipeops$get("learner", lrn("classif.rpart")))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  # inference from input only
  lrn = GraphLearner$new(mlr_pipeops$get("learner", lrn("classif.rpart")) %>>% mlr_pipeops$get("copy", 1))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  # inference when multiple input, but one is a Task
  # inference when multiple input, but one is a Task

  lrn = GraphLearner$new(gunion(list(mlr_pipeops$get("learner", lrn("regr.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch"))
  expect_equal(lrn$task_type, "regr")
  # expect_equal(lrn$predict_type, "response")

  ###########
  # Errors  #
  ###########

  # input, output mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", lrn("regr.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", lrn("classif.rpart"))
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  gr = gunion(list(mlr_pipeops$get("learner", lrn("classif.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", lrn("regr.rpart"))
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  # input two mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", lrn("classif.rpart")), mlr_pipeops$get("learner", lrn("regr.rpart")))) %>>% mlr_pipeops$get("unbranch")
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  # input two mismatching types
  expect_error(GraphLearner$new(PipeOpScale$new()), "output type not.*Prediction.*or compatible")

  ###########################
  # Target Transformations  #
  ###########################

  lrn = GraphLearner$new(ppl("targettrafo", graph = lrn("classif.rpart"), trafo_pipeop = PipeOpTargetMutate$new()))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  lrn = GraphLearner$new(ppl("targettrafo", graph = lrn("regr.rpart"), trafo_pipeop = PipeOpTargetMutate$new()))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  lrn = GraphLearner$new(ppl("targettrafo", graph = lrn("regr.rpart"), trafo_pipeop = PipeOpTargetTrafoScaleRange$new()))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")
})

test_that("graphlearner type inference - branched", {
  skip_on_cran()  # takes too long

  # default: classif

  lrn = GraphLearner$new(gunion(list(
      mlr_pipeops$get(id = "l1", "learner", lrn("classif.rpart")),
      po("nop") %>>% mlr_pipeops$get(id = "l2", "learner", lrn("classif.rpart"))

    )) %>>%
    po("classifavg") %>>%
    po(id = "n2", "nop"))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  ###########
  # regr    #
  ###########

  # inference from pipeoplearner
  lrn = GraphLearner$new(mlr_pipeops$get("learner", lrn("regr.rpart")))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  # inference from output only
  lrn = GraphLearner$new(mlr_pipeops$get("copy", 1) %>>% mlr_pipeops$get("learner", lrn("regr.rpart")))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  # inference from input only
  lrn = GraphLearner$new(mlr_pipeops$get("learner", lrn("regr.rpart")) %>>% mlr_pipeops$get("copy", 1))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  # inference when multiple input, but one is a Task
  lrn = GraphLearner$new(gunion(list(
      mlr_pipeops$get(id = "l1", "learner", lrn("regr.rpart")),
      po("nop") %>>% mlr_pipeops$get(id = "l2", "learner", lrn("regr.rpart"))
    )) %>>%
    po("regravg") %>>%
    po(id = "n2", "nop"))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  ###########
  # Errors  #
  ###########

  # input, output mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", lrn("regr.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", lrn("classif.rpart"))
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  gr = gunion(list(mlr_pipeops$get("learner", lrn("classif.rpart")), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", lrn("regr.rpart"))
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  # input two mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", lrn("classif.rpart")), mlr_pipeops$get("learner", lrn("regr.rpart")))) %>>% mlr_pipeops$get("unbranch")
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  # input two mismatching types
  expect_error(GraphLearner$new(PipeOpScale$new()), "output type not.*Prediction.*or compatible")

})

test_that("graphlearner predict type inference", {
  skip_on_cran()  # takes too long
  # Getter:

  # Classification
  lrp = po(lrn("classif.rpart", predict_type = "prob"))
  lrr = po(lrn("classif.rpart"))
  lfp = po(lrn("classif.featureless", predict_type = "prob"))
  lfr = po(lrn("classif.featureless"))
  nop = po("nop")

  # linear
  lrn = GraphLearner$new(lrp)
  expect_equal(lrn$predict_type, "prob")
  lrn = GraphLearner$new(lrr)
  expect_equal(lrn$predict_type, "response")
  lrn = GraphLearner$new(lrp %>>% nop)
  expect_equal(lrn$predict_type, "prob")

  # averager
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrr, 3L) %>>% po("classifavg"))
  expect_equal(lrn$predict_type, "response")
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrp, 3L) %>>% po("classifavg"))
  expect_equal(lrn$predict_type, "prob")

  # branching
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrp, lfp)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "prob")
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrr, lfr)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "response")
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrp, lfr)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "response")

  # with additional NOP in branch
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrp %>>% nop, lfp)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "prob")

  # Regression
  lrrp = po(lrn("regr.featureless", predict_type = "se"))
  lrrr = po(lrn("regr.rpart"))
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrrr, 3L) %>>% po("regravg"))
  expect_equal(lrn$predict_type, "response")
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrrp, 3L) %>>% po("regravg"))
  expect_equal(lrn$predict_type, "se")

  lrn = GraphLearner$new(lrrp %>>% nop)
  expect_equal(lrn$predict_type, "se")


  # Setter:
  lrp = po(lrn("classif.rpart", predict_type = "prob"))

  lrn = GraphLearner$new(lrp)
  lrn$predict_type = "prob"
  expect_equal(lrn$predict_type, "prob")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "prob")

  lrn = GraphLearner$new(lrp)
  lrn$predict_type = "response"
  expect_equal(lrn$predict_type, "response")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "response")

  lrn = GraphLearner$new(lrp)
  lrn$predict_type = "prob"
  expect_equal(lrn$predict_type, "prob")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "prob")

  lrn = GraphLearner$new(lrp %>>% po("nop"))
  lrn$predict_type = "response"
  expect_equal(lrn$predict_type, "response")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "response")

  # averager
  lrn = GraphLearner$new(pipeline_greplicate(po("subsample") %>>% lrp %>>% nop, 3L) %>>% po("classifavg"))
  lrn$predict_type = "response"
  expect_equal(lrn$predict_type, "response")
  expect_true(all(map_chr(lrn$graph$pipeops[paste(lrp$id, 1:3, sep = "_")], "predict_type") == "response"))

  # branching
  lrn = GraphLearner$new(po("branch", 2) %>>% gunion(list(lrp, lfp %>>% nop)) %>>% po("unbranch"))
  expect_equal(lrn$predict_type, "prob")
  lrn$predict_type = "response"
  expect_equal(lrn$predict_type, "response")
  expect_equal(lrn$graph$pipeops[[lrp$id]]$predict_type, "response")
  expect_equal(lrn$graph$pipeops[[lfp$id]]$predict_type, "response")

  # Setter on construction
  lrn = GraphLearner$new(lrr, predict_type = "prob")
  expect_equal(lrr$predict_type, "response")
  expect_equal(lrn$predict_type, "prob")
  expect_equal(lrn$graph$pipeops[[lrr$id]]$predict_type, "prob")

  # Errors:
  expect_error({lrrp = po(lrn("classif.featureless", predict_type = "se"))})
})


test_that("GraphLearner model", {
  graph = po("pca") %>>% lrn("classif.rpart")
  graph2 = graph$clone(deep = TRUE)
  graph_orig = graph$clone(deep = TRUE)

  lr = GraphLearner$new(graph)

  expect_equal(lr$graph, graph)
  expect_equal(lr$graph_model, graph)

  graph2$train(tsk("iris"))

  lr$train(tsk("iris"))

  expect_equal(graph, graph_orig)
  expect_null(graph$state$pca)

  # behind-the-scenes param_set cache ruins expect_equal if we don't do this:
  graph_orig$param_set

  expect_equal(lr$graph, graph_orig)

  graph2$state$classif.rpart$train_time = 0
  lr$state$model$classif.rpart$train_time = 0

  expect_equal(lr$graph_model, graph2)

  imp = graph2$pipeops$classif.rpart$learner_model$importance()

  expect_equal(lr$graph_model$pipeops$classif.rpart$learner_model$importance(), imp)


})

test_that("predict() function for Graph", {

  lx = as_graph(lrn("classif.rpart"))

  lx$train(tsk("iris"))

  p1 = lx$pipeops$classif.rpart$learner_model$predict(tsk("iris"))

  expect_equal(predict(lx, tsk("iris")), p1)

  expect_error(predict(lx, iris[1:4]), "Could not create a classif-task for plain prediction data")

  lx = as_graph(lrn("regr.rpart"))

  lx$train(tsk("boston_housing_classic"))

  p1 = lx$pipeops$regr.rpart$learner_model$predict(tsk("boston_housing_classic"))

  expect_equal(predict(lx, tsk("boston_housing_classic")), p1)

  expect_equal(
    predict(lx, tsk("boston_housing_classic")$data(cols = tsk("boston_housing_classic")$feature_names)),
    p1$response
  )


})

test_that("base_learner() works", {
  # graph containing single PipeOpLearner
  x = as_learner(as_graph(lrn("classif.rpart")))
  # untrained
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)
  # trained:
  x$train(tsk("iris"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)

  # graph consisting of PCA, rpart, threshold
  x = as_learner(po("pca") %>>% lrn("classif.rpart") %>>% po("threshold"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)
  # trained:
  x$train(tsk("iris"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$classif.rpart$learner_model)

  # graph inside a graph
  x = as_learner(po("pca") %>>% as_learner(po("scale") %>>% lrn("classif.rpart")) %>>% po("threshold"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$graph_model$pipeops$scale.classif.rpart$learner_model)
  expect_identical(x$base_learner(2), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$scale.classif.rpart$learner_model$graph_model$pipeops$classif.rpart$learner_model)
  x$train(tsk("iris"))
  expect_learner(x$base_learner())
  expect_identical(x$base_learner(0), x)
  expect_identical(x$base_learner(1), x$graph_model$pipeops$scale.classif.rpart$learner_model)
  expect_identical(x$base_learner(2), x$base_learner())
  expect_identical(x$base_learner(), x$graph_model$pipeops$scale.classif.rpart$learner_model$graph_model$pipeops$classif.rpart$learner_model)

  # branching: currently not supported
  branching_learner = as_learner(ppl("branch", lrns(c("classif.rpart", "classif.debug"))))
  expect_error(branching_learner$base_learner(), "Graph has no unique PipeOp containing a Learner")

  # bogus GraphLearner with no PipeOpLearner inside.
  expect_error(as_learner(po("nop"))$base_learner(), "No Learner PipeOp found.")

})


test_that("GraphLearner hashes", {


  learner1 = as_learner(ppl("robustify") %>>% lrn("regr.rpart"))
  learner1dash = as_learner(ppl("robustify") %>>% lrn("regr.rpart"))

  expect_string(learner1$hash)
  expect_string(learner1$phash)

  expect_equal(learner1$hash, learner1dash$hash)
  expect_equal(learner1$phash, learner1dash$phash)

  learner1dash$graph$pipeops$regr.rpart$param_set$values$xval = 1

  expect_string(all.equal(learner1$hash, learner1dash$hash), "mismatch")
  expect_equal(learner1$phash, learner1dash$phash)

  learner2 = as_learner(po("pca") %>>% lrn("regr.rpart"))

  expect_string(all.equal(learner1$hash, learner2$hash), "mismatch")
  expect_string(all.equal(learner1$phash, learner2$phash), "mismatch")

  learner1$id = "myid"
  learner2$id = "myid"

  expect_string(all.equal(learner1$hash, learner2$hash), "mismatch")
  expect_string(all.equal(learner1$phash, learner2$phash), "mismatch")


  # construction argument dependent hashes
  expect_string(all.equal(po("copy", 2)$hash, po("copy", 3)$hash), "mismatch")


  lr1 <- lrn("classif.rpart")
  lr2 <- lrn("classif.rpart", fallback = lrn("classif.rpart"))

  expect_string(all.equal(lr1$hash, lr2$hash), "mismatch")
  expect_string(all.equal(lr1$phash, lr2$phash), "mismatch")

  lr1 <- as_learner(as_pipeop(lr1))
  lr2 <- as_learner(as_pipeop(lr2))

  expect_string(all.equal(lr1$hash, lr2$hash), "mismatch")
  expect_string(all.equal(lr1$phash, lr2$phash), "mismatch")

  lr1 <- as_learner(as_pipeop(lr1))
  lr2 <- as_learner(as_pipeop(lr2))

  expect_string(all.equal(lr1$hash, lr2$hash), "mismatch")
  expect_string(all.equal(lr1$phash, lr2$phash), "mismatch")

})


