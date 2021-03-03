source("spn.binaryrelevant.r")
require("parallel")
require("foreach")
require("doParallel")

################################################################
spn.multilabel <- function(data_train,
                           data_test,
                           nb.labels,
                           eps = 0.0,
                           verb = FALSE,
                           idm_version = FALSE,
                           num.intervals = NULL,
                           ncores = 1,
                           ncore.learn = 1) {
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
    .data[, ((ncol(.data) - nb.labels):ncol(.data))]
  rs.multilabel <- NULL
  ################################################################
  # removing variables not used for parallel processus
  # save(list=ls(all.names = TRUE), file='before_session.RData')
  # remove local variables
  rm(list=Filter(exists,c(".data_test", ".data", ".data_train")))
  # remove data set not used afterwards
  rm(list=Filter(exists,c("data_train", "data_test")))
  # garbage collection
  gc()
  # save(list=ls(all.names = TRUE), file='after_session.RData')
  ################################################################
  if (ncore.learn > 1) {
    cl <- autoStopCluster(makeCluster(ncore.learn)) # outfile="" -> redirection stdout
    registerDoParallel(cl, cores = ncore.learn)
    rs.multilabel <-
      foreach(
        i = 1:nb.labels,
        .packages = c("cluster"),
        .combine = cbind
      ) %dopar% {
        source("spn.binaryrelevant.r")
        cat(paste0(Sys.time(), ":::Label", .labels.train[i], "\n"))
        spn.binary.relevance(
          .idx.label = i,
          .features.train = .features.train,
          .labels.train = .labels.train,
          nb.labels = nb.labels,
          num.intervals = num.intervals,
          eps = eps,
          verb = verb,
          idm_version = idm_version,
          ncores = ncores
        )
      }
    gc() # stopCluster(cl) (wihout autoStopCluster)
  } else {
    for (i in 1:nb.labels) {
      rs <- spn.binary.relevance(
        .idx.label = i,
        .features.train = .features.train,
        .labels.train = .labels.train,
        nb.labels = nb.labels,
        num.intervals = num.intervals,
        eps = eps,
        verb = verb,
        idm_version = idm_version,
        ncores = ncores
      )
      rs.multilabel <- cbind(rs.multilabel, rs)
    }
  }
  return(rs.multilabel)
}
