source("spn.learn.r")
source("spn.credal.predict.r")
source("spn.utils.r")
source('spn.clara2.r')
################################################################
spn.binary.relevance <- function(.idx.label,
                                 .features.train,
                                 .labels.train,
                                 nb.labels,
                                 num.intervals = NULL,
                                 eps = 0.0,
                                 verb = FALSE,
                                 idm_version = FALSE,
                                 ncores = 1) {
  .br.classify.data <- cbind(.features.train,
                             .labels.train[.idx.label],
                             .labels.train[ncol(.labels.train)])
  .idx.train <- ncol(.br.classify.data)
  # discretization by intervals
  if (!is.null(num.intervals))
    .br.classify.data <- discretization_eqfreq(.br.classify.data,
                                               numint = num.intervals,
                                               ncols = .idx.train - 2)
  # training and test row indicators
  .vals.train <- .br.classify.data[, .idx.train]
  # converting columns in dummies values
  .br.classify.data <- getDummies(.br.classify.data[,-.idx.train])
  # column index class
  classcol <- ncol(.br.classify.data)
  summary.train <-
    spn.learncats(.br.classify.data, classcol = classcol)
  summary.train$data <- summary.train$data[.vals.train == TRUE,]
  .br.classify.test <- .br.classify.data[.vals.train == FALSE,]
  rs.multilabel <- spn.binary.relevance.wrapper(
    train_data = summary.train,
    test_data = .br.classify.test,
    classcol = classcol,
    eps = eps,
    ncores = ncores,
    verb = verb,
    idm_version = idm_version
  )
  return(rs.multilabel)
}

################################################################
spn.binary.relevance.wrapper <- function(train_data,
                                         test_data,
                                         classcol,
                                         eps = 0.0,
                                         verb = FALSE,
                                         idm_version = FALSE,
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
  if (verb) {
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
    idm_version = idm_version,
    ncores = ncores
  )
  return(cur_res_select)
}
