library(shiny)
library(parallel)
library(gtools)
library(DT)
library(reshape2)
library(dplyr)
library(ggplot2)
library(plotly)

source("R/LCA.R")
source("R/DataHandling.R")
source("R/multiLCA.R")
source("R/FitIndices.R")
source("R/plots.R")

cores <- detectCores()
if(cores > 1) { cores <- cores - 1 }
cl <- makeCluster(cores)
