library(shiny)
library(parallel)
library(gtools)
library(DT)
library(reshape2)
library(dplyr)
library(plotly)
source("LCA.R")
source("DataHandling.R")
source("multiLCA.R")
source("FitIndices.R")
source("plots.R")
#funLCA <- c("emLCA", "compLik", "assignProb",
#            "randomTheta", "updateTheta", "d", "k")

cores <- detectCores() - 1
cl <- makeCluster(cores)
