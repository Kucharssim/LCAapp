library(shiny)
library(parallel)
library(gtools)
library(DT)
library(reshape2)
library(dplyr)
library(plotly)

source("R/LCA.R")
source("R/DataHandling.R")
source("R/multiLCA.R")
source("R/FitIndices.R")
source("R/plots.R")

cores <- detectCores() - 1
cl <- makeCluster(cores)
