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
# GLOBAL: init parameters
VERB <- TRUE
NCORES <- 1
NCORES_LEARN <- 1
NUM_INTERVALS <- 6
IDM_VERSION <- FALSE
ROOT <- "..." # ROOT path to output results and to get datasets

# DATASET: init_data
dataset <- "medical"
nb.labels <- 45

# RESAMPLING: resampling number
max.resampling <- 50
# root datasets training 
ROOT_DATASET <- paste0(ROOT, "/datasets_mlc/resampling/", dataset, "/")
# creation output-file by type cautious  
type <- ifelse(IDM_VERSION, "idm", "econt")
out_results <- paste0(
  ROOT,
  "/results_mlc/results_",
  data,
  "_cspn_brut_resampling_",
  type,
  "_",
  NUM_INTERVALS,
  "disc.csv"
)

# imprecision/percentage parameters 
epsilons <- seq(0.1, 0.3, 0.1)
pcts <- seq(10, 90, 10)

# logging 
print(paste0("Version IDM?", IDM_VERSION, " -> ", out_results))
# loops
for (resampling in 1:max.resampling) {
  for (pct in pcts) {
    in_train <- paste0(dataset, "_train_", resampling, "_", pct, ".arff")
    in_test <- paste0(dataset, "_test_", resampling, "_", pct, ".arff")
    cat(paste(Sys.time(), 'Train file', in_train, sep = ":::"), '\n')
    cat(paste(Sys.time(), 'Test file', in_test, sep = ":::"), '\n')
    
    # data set
    data_train <- read.arff(paste0(ROOT_DATASET, in_train))
    data_test <- read.arff(paste0(ROOT_DATASET, in_test))
    
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
        ncore.learn = NCORES_LEARN,
        idm_version = IDM_VERSION
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

