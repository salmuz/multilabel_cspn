require("parallel")
require("foreach")
require("doParallel")
source("spn.maximality.r")

################################################################
spn.credal.predict <- function(spn,
                               data,
                               classcol = ncol(data),
                               eps = 0.0,
                               verb = FALSE,
                               idm_version = FALSE,
                               ncores = detectCores() - 1) {
  nclass <- spn$ncat[classcol]
  if (nclass < 2)
    stop('class must be discrete')
  nr <- nrow(data)
  if (ncores > 1) {
    cl <- makeForkCluster(ncores, outfile="br.credal.log")
    registerDoParallel(cl)
    res <-
      foreach(
        i = 1:nr,
        .combine = rbind
      ) %dopar% {
        rs_partial <- NULL
        tryCatch({
          source("spn.maximality.r", local = TRUE)
          ptm <- proc.time()
          rs_partial <- spn.predict(
            spn = spn,
            data = as.numeric(data[i,]),
            classcol = classcol,
            verb = verb,
            eps = eps,
            idm_version = idm_version
          )
          time <- proc.time() - ptm
          timing <- as.numeric(time['sys.self'] + time['user.self'])
          cat(paste("Time of inference:::", timing, "\n"))
        }, error = function(e) {
          message(paste0('A caused error', e))
          stop("For further details go to br.creda.log file")
        })
        rs_partial
      }
    #gc() #(wiht autoStopCluster)
    stopCluster(cl)
  } else {
    res <- c()
    for (i in 1:nr) {
      ################################################################
      # starting time inference
      ptm <- proc.time()
      ################################################################
      res <- rbind(
        res,
        spn.predict(
          spn = spn,
          data = as.numeric(data[i,]),
          classcol = classcol,
          verb = verb,
          eps = eps,
          idm_version = idm_version
        )
      )
      ################################################################
      # stopping time inference
      if (verb) {
        time <- proc.time() - ptm
        timing <- as.numeric(time['sys.self'] + time['user.self'])
        cat(paste("Time of inference:::", timing, "\n"))
      }
      ################################################################
    }
  }
  
  return(res)
}
