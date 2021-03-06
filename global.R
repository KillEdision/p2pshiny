library(bit)
library(ff)
library(shiny)
library(shinyjs)
library(ggplot2)
library(ffbase)
library(GGally)
library(ROCR)
library(randomForest)
library(grid)
library(tabplot)
library(shinyAce)
library(rmarkdown)
library(magrittr)
library(leaflet)
library(printr)

load('www/data/loan_dim.rda')
load('www/data/chengshi.rda')
load('www/data/cities.rda')
load('www/data/fact.rda')
load('www/data/jiguan.rda')
load('www/data/melteddata.rda')
load('www/data/user_dim.rda')
