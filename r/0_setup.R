# Clean slate
rm(list = ls())

# Libraries
library(paws); library(paws.database); library(dplyr)
library(DBI); library(RPostgres); library(tidycensus)
library(tidyverse); library(ipumsr); library(arrow)
library(here); library(spatstat); library(Hmisc)
library(haven); library(quantreg); library(dineq)
library(beepr); library(ggplot2); library(lubridate)
library(plyr); library(ggpubr); library(plotly)
library(openxlsx); library(leaflet); library(leaflet.extras)
library(sf); library(htmlwidgets); library(htmltools)
library(stargazer); library(precommit)

#TrueRoll brand colors
tr_colors <- c("#3C2A98", "#55BD08", "#fc8d59", "#8e0152", "#8856a7")
