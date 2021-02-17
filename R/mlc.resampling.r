################################################################
library(cluster)
library(doParallel)
library(foreach)
library(e1071)
library(rlang)
library(tictoc)
library(RWeka)
library(stringr)
library(optparse)
source("spn.binaryrelevant.r")
################################################################
getDummies <- function(data) {
  for (var in colnames(data)) {
    data[[var]] <- as.integer(as.factor(data[[var]]))
  }
  return(data)
}
################################################################
# init parameters
VERB <- FALSE
NCORES <- 1
# init_data
dataset <- "emotions"
nb.labels <- 6
max.resampling <- 50
ROOT <- paste0("/Users/salmuz/Downloads/", dataset, "/")
out_results <- "~/Downloads/mydata.csv"

# resampling parameters
epsilons <- seq(0.1, 0.9, 0.1)
pcts <- seq(10, 90, 10)

# loops
for (resampling in 1:max.resampling) {
  for (pct in pcts) {
    in_train <- paste0(dataset, "_train_", resampling, "_", pct, ".arff")
    in_test <- paste0(dataset, "_test_", resampling, "_", pct, ".arff")
    cat(paste(Sys.time(), 'Train file', in_train, sep = ":::"), '\n')
    cat(paste(Sys.time(), 'Test file', in_test, sep = ":::"), '\n')
    
    # data set
    data_train <- read.arff(paste0(ROOT, in_train))
    data_test <- read.arff(paste0(ROOT, in_test))
    
    for (eps in epsilons) {
      cat(paste(Sys.time(), 'Resampling', resampling, 
                "Percentage", pct, 
                'Epsilon', eps, sep = ":::"), '\n')
      rs <- spn.multilabel(
        data_train = data_train,
        data_test = data_test,
        nb.labels = nb.labels,
        eps = eps,
        verb = VERB,
        ncores = NCORES
      )
      rs <- cbind(pct, eps, resampling, rs)
      write.table(
        x = rs,
        file = out_results,
        append = TRUE,
        sep = ",",
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE
      )
    }
  }
}