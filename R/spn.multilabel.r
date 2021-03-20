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
    .data[,-((ncol(.data) - nb.labels):ncol(.data))]
  .labels.train <-
    .data[, ((ncol(.data) - nb.labels):ncol(.data))]
  rs.multilabel <- NULL
  if (ncore.learn > 1) {
    # outfile="" -> redirection stdout
    cl <- autoStopCluster(makeCluster(ncore.learn, outfile="br.error.log")) 
    registerDoParallel(cl)
    rs.multilabel <-
      foreach(
        i = 1:nb.labels,
        .packages = c("cluster"),
        .combine = cbind
      ) %dopar% {
        cat(paste0(Sys.time(), ":::Label:::", i, "\n"))
        tryCatch({
          source("spn.binaryrelevant.r", local = TRUE)
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
        }, error = function(e) {
          message(paste0("Label ", i, ", A caused error learning step:", e))
          stop("Error in learning step, verify br.error.log file, for further details.")
        }, finally = {
          message(paste0("Label ", i, " has finished corretly"))
        })
      }
    gc()
    # stopCluster(cl) (wihout autoStopCluster)
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
