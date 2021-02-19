source("spn.learn.r")
source("spn.credal.predict.r")
source('spn.clara2.r')

################################################################
discretization_eqfreq <-
  function(dataset, numint, ncols = ncol(dataset)) {
    numitem <- nrow(dataset)
    for (i in 1:ncols) {
      floatdata <- as.numeric(dataset[, i])
      sort.idx <- sort(floatdata, index.return = TRUE)
      dataset <- dataset[sort.idx$ix, ]
      floatdata <- floatdata[sort.idx$ix]
      cutpoint <- c()
      newname <- c()
      for (j in 1:numint) {
        .temp <- as.integer(j * (numitem / (numint))) - 1
        cutpoint <- c(cutpoint, dataset[.temp, i])
      }
      for (j in 1:numint) {
        if (j == 1) {
          string <- toString(cutpoint[j])
          string <- substring(string, 1, min(nchar(string), 7))
          newname <- c(newname, paste0('<=', string))
        } else{
          if (j == numint) {
            string <- toString(cutpoint[j - 1])
            string <- substring(string, 1, min(nchar(string), 7))
            newname <- c(newname, paste0('>', string))
          } else{
            string1 <- toString(cutpoint[j - 1])
            string2 <- toString(cutpoint[j])
            string1 <- substring(string1, 1, min(nchar(string1), 7))
            string2 <- substring(string2, 1, min(nchar(string2), 7))
            newname <-
              c(newname, paste0('(', string1, ';', string2, ']'))
          }
        }
      }
      for (j in 1:numint) {
        if (j == 1) {
          dataset[(floatdata <= as.numeric(cutpoint[j])), i] <- newname[j]
        } else{
          if (j == numint) {
            dataset[(floatdata > as.numeric(cutpoint[j - 1])), i] <- newname[j]
          } else{
            dataset[(floatdata > as.numeric(cutpoint[j - 1])) &
                      (floatdata <= as.numeric(cutpoint[j])), i] <-
              newname[j]
          }
        }
      }
    }
    return(dataset)
  }
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
                           num.intervals = NULL,
                           ncores = 1) {
  ncols <- ncol(data_train)
  # merge train and test data for discretization
  .data_train  <- cbind(data_train, TRUE)
  colnames(.data_train)[ncols + 1] <- "train"
  .data_test <- cbind(data_test, FALSE)
  colnames(.data_test)[ncols + 1] <- "train"
  .data  <- rbind(.data_train, .data_test)
  
  .features.train  <-
    .data[,-((ncol(.data) - nb.labels):ncol(.data))]
  .labels.train <-
    .data[, ((ncol(.data) - nb.labels + 1):ncol(.data) - 1)]
  rs.multilabel <- NULL
  for (i in 1:nb.labels) {
    .br.classify.data <- cbind(.features.train, .labels.train[i], .data$train)
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
    summary.train <- spn.learncats(.br.classify.data, classcol = classcol)
    summary.train$data <- summary.train$data[.vals.train == TRUE,]
    .br.classify.test <- .br.classify.data[.vals.train == FALSE,]
    rs <- spn.binary.relevance(
      train_data = summary.train,
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
