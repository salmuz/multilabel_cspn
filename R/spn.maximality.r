source('spn.utils.r')
source('spn.value.max.r')
source('spn.value.min.r')
library(rlang)
library(tictoc)

# This function only works with a binary credal classification
spn.predict <- function(spn,
                        data,
                        classcol = length(data),
                        verb = FALSE,
                        eps = 0.0) {
  nclass <- spn$ncat[classcol]
  if (nclass < 2)
    stop('class must be discrete')
  res <- c()
  cfg <- list()
  len <- length(data)
  cfg$scope <-
    sort(c(sample(setdiff(1:len, classcol), len - 1), classcol))
  cfg$value <- data[cfg$scope]
  ###########################################################
  lower.marginal <- c()
  upper.marginal <- c()
  precise.marginal <- c()
  for (j in 1:nclass) {
    cfg$value[which(classcol == cfg$scope)] <- j
    spn.max <-
      spn.value.max(spn, cfg, eps = eps, eps.gauss = eps)$res
    spn.min <-
      spn.value.min(spn, cfg, eps = eps, eps.gauss = eps)$res
    if (abs(spn.min) == Inf | abs(spn.max) == Inf) {
      stop('Infinite value in the inference step!!!')
    }
    lower.marginal <- c(lower.marginal, spn.min)
    upper.marginal <- c(upper.marginal, spn.max)
    precise.marginal <-
      c(precise.marginal, spn.value.max(spn, cfg)$res)
  }
  ###########################################################
  # @salmuz verify if lower.marginal(x) > upper.marginal(x)
  # @salmuz (maximality)
  ###########################################################
  lower.cond <- c(-Inf, -Inf)
  lower.cond[1] <- exp(lower.marginal[1] -
                         logsumexp(c(lower.marginal[1],
                                     upper.marginal[2])))
  lower.cond[2] <- exp(lower.marginal[2] -
                         logsumexp(c(lower.marginal[2],
                                     upper.marginal[1])))
  
  # class 1 is (0) and class 2 is (1)
  credal.class <- (-1)
  if (lower.cond[1] > 0.5) {
    credal.class <- 1
  } else{
    if (lower.cond[2] > 0.5) {
      credal.class <- 2
    } else{
      credal.class <- 3
    }
  }
  ###########################################################
  evi.marginal.log <- logsumexp(precise.marginal)
  res <- c(data[classcol],
           credal.class,
           lower.cond,
           exp(precise.marginal - evi.marginal.log))
  
  return(res)
}
