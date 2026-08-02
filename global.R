#============================#
#### 1. Required packages ####
#============================#

library(shiny)
library(tidyverse)
library(readr)
library(tidyr)
library(plotly)
library(bslib)
library(shinythemes)
library(shinyjs)
library(MLmetrics)
library(scales)
library(readxl)
library(DT)

#=========================#
#### 2. Global Options ####
#=========================#
options(shiny.maxRequestSize = 20 * 2048^2)
`%!in%` = Negate(`%in%`)

#=============================================#
#### 3. Recursive Sourcing of Dependencies ####
#=============================================#
file_sources <- list.files(path = "R", 
                           pattern = "\\.R$", 
                           full.names = TRUE, 
                           recursive = TRUE)

sapply(file_sources, source)