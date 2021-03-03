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
    cl <- autoStopCluster(makeCluster(ncores))
    registerDoParallel(cl, cores = ncores)
    res <-
      foreach(
        i = 1:nr,
        .packages = c("cluster"),
        .combine = rbind
      ) %dopar% {
        if (i %% 20 == 1)
          cat(paste(Sys.time(), ":::Predict", i - 1, "\n"))
        source("spn.maximality.r")
        spn.predict(
          spn = spn,
          data = as.numeric(data[i,]),
          classcol = classcol,
          verb = verb,
          eps = eps,
          idm_version = idm_version
        )
      }
    gc() # stopCluster(cl) (wihout autoStopCluster)
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
