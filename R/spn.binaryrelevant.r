source("spn.learn.r")
source("spn.credal.predict.r")
source('spn.clara2.r')

################################################################
spn.binary.relevance <- function(train_data,
                                 test_data,
                                 classcol,
                                 eps = 0.0,
                                 verb = FALSE,
                                 ncores = 1) {
  #### Learning SPN structure model
  ptm <- proc.time()
  spn_select <- spn.learn(
    train_data$data,
    ncat = train_data$ncat,
    maxv = train_data$maxv,
    minv = train_data$minv,
    verb = verb,
    classcol = classcol,
    thr = 0.01,
    height = 1000000
  )
  if(verb){
    time <- proc.time() - ptm
    time <- as.numeric(time['sys.self'] + time['user.self'])
    cat(paste("Time of learning:::", time, "\n"))
  }
  
  #### Testing performance class-selective SPN
  cur_res_select <- spn.credal.predict(
    spn = spn_select,
    data = test_data,
    classcol = classcol,
    eps = eps,
    verb = verb,
    ncores = ncores
  )
  return(cur_res_select)
}

################################################################
spn.multilabel <- function(data_train,
                           data_test,
                           nb.labels,
                           eps = 0.0,
                           verb = FALSE,
                           ncores = 1) {
  ncols <- ncol(data_train)
  # merge train and test data for discretization
  .data_train  <- cbind(data_train, TRUE)
  colnames(.data_train)[ncols + 1] <- "train"
  .data_test <- cbind(data_test, FALSE)
  colnames(.data_test)[ncols + 1] <- "train"
  .data  <- rbind(.data_train, .data_test)
  
  .features.train  <-
    .data[, -((ncol(.data) - nb.labels):ncol(.data))]
  .labels.train <-
    .data[, ((ncol(.data) - nb.labels + 1):ncol(.data) - 1)]
  rs.multilabel <- NULL
  for (i in 1:nb.labels) {
    .br.classify.data <- cbind(.features.train, .labels.train[i])
    .br.classify.data <- getDummies(.br.classify.data)
    classcol <- ncol(.br.classify.data)
    summ.train <-
      suppressWarnings(spn.learncats(.br.classify.data, classcol = classcol))
    summ.train$data <- summ.train$data[.data$train == TRUE, ]
    .br.classify.test <- .br.classify.data[.data$train == FALSE, ]
    rs <- spn.binary.relevance(
      train_data = summ.train,
      test_data = .br.classify.test,
      classcol = classcol,
      eps = eps,
      ncores = ncores,
      verb = verb
    )
    rs.multilabel <- cbind(rs.multilabel, rs)
  }
  return(rs.multilabel)
}

