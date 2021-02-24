# Copyright (c) 2020 - Yontan Carranza Alarcon (salmuz@gmail.com)
# Two version robusness to CSPN with 
#     (1) Imprecise Dirichlet Model
#     (2) E-contamination (Copyright (c) 2019  C. P. de Campos (cassiopc@acm.org))

# version original of optimization with e-contamination (without modifications)
e.contamination <- function(weight, eps, x, decreasing = FALSE) {
  vals.low <- weight * (1 - eps)
  
  total <- 1 - sum(vals.low)
  if (total < -1e-8)
    stop(paste('total', total))
  vals <- vals.low
  
  i.set <- order(x, decreasing = decreasing)
  for (i in i.set) {
    if (total > eps) {
      vals[i] <- vals[i] + eps
      total <- total - eps
    } else {
      vals[i] <- vals[i] + total
      break
    }
  }
  return(vals)
}

idm.contamination <-
  function(weight, total_weight, eps, x, decreasing = FALSE) {
    #' Here the weight is defined as:  weight_ji := N_j/N_i, where N_i := total_weight
    #' The imprecise dirichelt is defined as: 
    #'         w_ij = (N_j + eps*v)/(N_i + eps), v \in simplex.
    vals <- (weight*total_weight) / (total_weight + eps)
    # The optimal solution is the one where w takes the upper value
    # on the i-index (w_i) which is i:=max/min_i x,(the position of
    # the value maximum/minimum in x)
    i.set <- order(x, decreasing = decreasing)
    # max element x is the w_ith with upper value
    vals[i.set[1]] <- vals[i.set[1]] + eps / (total_weight + eps)
    return(vals)
  }


reduce.cfg <- function(scope, cfg) {
  res <- list()
  res$scope <- intersect(scope, cfg$scope)
  l <- length(res$scope)
  if (l == 0)
    res$value <- c()
  else {
    res$value <- 1:l
    for (i in 1:l) {
      pos <- which(cfg$scope == res$scope[i])
      res$value[i] <- cfg$value[pos]
    }
  }
  return(res)
}
