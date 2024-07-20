# load packages & functions -----------------------------------------------------------
rm(list = ls())
pacman::p_load(readxl, wcde, pwt10, tidyverse, countrycode, patchwork, stargazer, BMS)
source('code/auxilliary.R')

# data ---------------------------------------------------------------

# read
d_hist <- readRDS('output/d_hist.RDS')
d_proj <- readRDS('output/d_proj.RDS')

# within transform
d_h_WI <- within_trans(d_hist, T)
d_p_WI <- within_trans(d_proj, F)

# estimate model averaging ---------------------------------------------------------
m_est <- list()
# interate over dependet variables
for (y in c('st_cap_or', 'st_cap_ha', 'rspct', 'gee', 'gi_wb')) {
 
  m_est[[y]] <- m_aver(d_h_WI, y)
   
}
  


# parameter search --------------------------------------------------------

# copy d_proj
p_n_fe <- d_proj

# search for time fixed effect parameter

# initialize tfe_para
tfe_para <- cbind(rep(0.1, 5), rep(0.1, 5), rep(0, 5), rep(0, 5), rep(0.1, 5))
colnames(tfe_para) <- c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5')
rownames(tfe_para) <- c('st_cap_or', 'st_cap_ha', 'rspct', 'gee', 'gi_wb')

# project without fe
for (y in c('st_cap_or', 'st_cap_ha', 'rspct', 'gee', 'gi_wb')) {
  
  # tmp container
  p_tmp <- c()
  
  # iterate over dependent variables
  for (s in c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5')) {
    
    # estimate and project
    p <- proj(SSP = s,
                  y = y,
                  m_est = m_est[[y]],
                  tfe = F,
                  cfe = F,
                  tfe_para = tfe_para,
                  d_h_WI = d_h_WI,
                  d_p_WI = d_p_WI)
    

    
    # store projection
    p_tmp <- rbind(p_tmp, p)
    # d_proj_tmp_not_s <- subset(d_proj, scen != s)
    # d_proj_tmp_not_s[, y] <- NA
    # d_proj_tmp_s <- subset(d_proj, scen == s)
    # d_proj_tmp_s <- left_join(d_proj_tmp_s, p, by = c('iso3c', 'year', 'scen'))
    # d_proj <- rbind(d_proj_tmp_not_s, d_proj_tmp_s)
    
  }
  
  # store
  p_n_fe <- left_join(p_n_fe, p_tmp, by = c('iso3c', 'year', 'scen'))
  
}



# set effect size aim
d_s_a <- c(SSP1 = .1, SSP2 =0.05, SSP3 = 0, SSP4 =  0, SSP5 =  0.08)

# update para for tfe
for (y in c('st_cap_or', 'st_cap_ha', 'rspct', 'gee', 'gi_wb')) {
  
  # iterate over dependent variables
  for (SSP in c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5')) {
    
    # parameter for determination of search
    t <- F
    while(!t) {
      
      # store output of update_tfe
      tmp <- update_tfe(SSP,
                        y,
                        d_s_a,
                        tfe_para,
                        p_n_fe)
      
      # update t
      t <- tmp$search
      
      # update para
      tfe_para <- tmp$tfe_para
      
      # print progress
      print(list(SSP, y, tfe_para))
      
    }
    
    
  }
  
}





# project -----------------------------------------------------------------

# inclusion of fixed effects
FE <- T

d_p <- d_proj
#iterate over scenario
for (y in c('st_cap_or', 'st_cap_ha', 'rspct', 'gee', 'gi_wb')) {
  
  # tmp container
  p_tmp <- c()
  
  # iterate over dependent variables
  for (s in c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5')) {
    
    # project
    p <- proj(SSP = s,
              y = y,
              m_est = m_est[[y]],
              tfe = FE,
              cfe = FE,
              tfe_para = tfe_para,
              d_h_WI = d_h_WI,
              d_p_WI = d_p_WI)
    
    # store projection
    p_tmp <- rbind(p_tmp, p)
    
  }
  
  # store
  d_p <- left_join(d_p, p_tmp, by = c('iso3c', 'year', 'scen'))
  
}


# smooth projections
d_p <- smooth(d_p, d_hist)



# store -------------------------------------------------------------------

write.csv(select(d_p, c(scen, iso3c, year, st_cap_or, st_cap_ha, rspct, gee, gi_wb)), "output/SSP_GI_2024_proj.CSV")
