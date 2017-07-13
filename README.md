# Shiny App of Latent Class Analysis

detailed documentation can be found in documents/Documentation.pdf


run locally in the R Studio session as regular Shiny app, or run from GitHub with:

if(!require('shiny')) {install.packages('shiny')}

if(!require('parallel')) {install.packages('parallel')}

if(!require('gtools')) {install.packages('gtools')}

if(!require('DT')) {install.packages('DT')}

if(!require('reshape2')) {install.packages('reshape2')}

if(!require('dplyr')) {install.packages('dplyr')}

if(!require('ggplot2')) {install.packages('ggplot2')}

if(!require('plotly')) {install.packages('plotly')}

shiny::runGitHub("kucharssim/LCAapp")
