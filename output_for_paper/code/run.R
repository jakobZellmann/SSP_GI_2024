# load packages & functions -----------------------------------------------------------
rm(list = ls())
pacman::p_load(readxl, wcde, pwt10, tidyverse, countrycode, patchwork, stargazer, BMS)
source('code/auxilliary.R')


# load projection ---------------------------------------------------------

d_p <- read.csv('input/SSP_GI_2024_proj.CSV')
d_hist <- readRDS('input/d_hist.RDS')

# plot --------------------------------------------------------------------

for(c in unique(d_p$iso3c)){
  
  p_GI(d_p, d_hist, c, T)
  
}



# latex output of regression tables ---------------------------------------

