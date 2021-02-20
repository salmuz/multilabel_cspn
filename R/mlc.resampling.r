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
source("spn.multilabel.r")
################################################################
# init parameters
VERB <- TRUE
NCORES <- 1
NCORES_LEARN <- 1
NUM_INTERVALS <- 5
# init_data
dataset <- "emotions"
nb.labels <- 6
max.resampling <- 50
ROOT <- paste0("/Users/salmuz/Downloads/", dataset, "/")
out_results <- "~/Downloads/mydata.csv"

# ROOT <- paste0("/home/lab/ycarranz/datasets_mlc/resampling/", dataset, "/")
# out_results <- "/home/lab/ycarranz/results_mlc/results_emotions_cspn_brut_resampling_miss00.csv"

# resampling parameters
epsilons <- seq(0.02, 0.1, 0.02)
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
        ncores = NCORES,
        num.intervals = NUM_INTERVALS,
        ncore.learn = NCORES_LEARN
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
