library(shiny)
library(parallel)
library(gtools)
library(DT)
source("LCA.R")
source("DataHandling.R")
source("multiLCA.R")
source("FitIndices.R")

#funLCA <- c("emLCA", "compLik", "assignProb",
#            "randomTheta", "updateTheta", "d", "k")

cores <- detectCores() - 1
cl <- makeCluster(cores)
