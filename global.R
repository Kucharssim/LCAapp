if(!require('parallel')){install.packages('parallel')}
if(!require('gtools')){install.packages('gtools')}
if(!require('DT')){install.packages('DT')}
if(!require('reshape2')){install.packages('reshape2')}
if(!require('dplyr')){install.packages('dplyr')}
if(!require('plotly')){install.packages('plotly')}

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
