# within_trans ------------------------------------------------------------

within_trans <- function(d, h) {
  
  
  if (h == T) {
    
    for (y in c('st_cap_or', 'st_cap_ha', 'rspct', 'gee', 'gi_wb')) {

      # apply logit
      d[unlist(d[,y]) %in% c(0,1) ,y] <- NA
      d[,y] <- log(d[,y]/(1-d[,y]))

    }
    
    # cal mean
    d <- d %>%
      group_by(iso3c) %>%
      summarise(across(everything(), mean, na.rm=TRUE)) %>%
      left_join(d, ., c('iso3c'), suffix = c("", "_FE"))
    
    # demean
    names <- names(d)[!(names(d)  %in% c('iso3c', 'year')) & str_detect(names(d), 'FE', negate = T)]
    
    
  } else {
      
    # cal mean
    d <- d %>%
      group_by(scen, iso3c) %>%
      summarise(across(everything(), mean, na.rm=TRUE)) %>%
      left_join(d, ., c('scen', 'iso3c'), suffix = c("", "_FE"))
    
    # demean
    names <- names(d)[!(names(d)  %in% c('scen', 'iso3c', 'year')) & str_detect(names(d), 'FE', negate = T)]
      
    }

  
  
  for (n in names) {
    
    d[,n] <- d[,n] - d[,paste(n, 'FE', sep = '_')]
    
  }
  
  # return
  return(d)

}

# model averaging --------------------------------------------------------

m_aver <- function(d_h_WI, y) {
  
  # within transform across countries
  d <- d_h_WI
  
  # select relevant columns
  d <- na.omit(d[, c(y, 'iso3c', 'year', 'log_gdp_pc', 'e1_s', 'e2_s', 'e3_s', 'e4_s', 'e5_s', 'e6_s', 'e1_s_gg', 'e2_s_gg', 'e3_s_gg', 'e4_s_gg', 'e5_s_gg', 'e6_s_gg', 'mys', 'mys_gg')])
  
  # formula
  f <- as.formula(paste(y, '~ -1 + as.factor(year) + log_gdp_pc + e1_s + e2_s + e3_s + e4_s + e5_s + e6_s + e1_s_gg + e2_s_gg + e3_s_gg + e4_s_gg + e5_s_gg + e6_s_gg + mys + mys_gg', sep = ''))
  # f <- as.formula(paste(y, '~ + as.factor(year)', sep = ''))
  
  # model matrix
  mm <- model.matrix(f, d)[,-1]
  mm <- cbind(d[, y], mm)
  
  # estimate
  est <- bms(mm, mprior = "uniform", g="UIP")
  #est <-  bic.glm(f, data = d, glm.family = gaussian())
  
  # return
  return(est)

  
}



# project TFE -------------------------------------------------------------

project_time_FE <- function(SSP, y, p, tfe_para) {
  

  # time series
  TFE_proj <- data.frame(year = seq(2020, 2100, 5),
                         tfe = cumsum(rep(tfe_para[y, SSP], 17))
  )
  
  # d_proj + TFE_proj
  out <- left_join(p, TFE_proj, by = 'year')
  out[, y] <- out[, y] + out$tfe
  out$tfe <- NULL
  
  # return
  return(out)
  
}


# project country fixed effects -------------------------------------------

project_country_FE <- function(SSP, y, p, m_est, d_p_WI, d_h_WI) {
  
  # subset for scen and year
  d_p <- subset(d_p_WI, scen == SSP & year == 2020)
  d_h <- subset(d_h_WI, year == 2015, c(c('iso3c'), y, paste(y, 'FE', sep='_')))
  
  
  # add intercept
  d_p$`(Intercept)` <- 1
  
  # get posterior coef
  coef <- coef(m_est)[, 'Post Mean']
  
  # only use non time FE coef
  c <- coef[str_detect(names(coef), 'year', negate = T)]
  
  # project
  pp <- as.matrix(d_p[, names(c)]) %*% as.matrix(c)
  
  # add TFE from 2015
  pp <- pp + coef[str_detect(names(coef), '2015')]
  
  # add keys
  pp <- data.frame(iso3c = d_p$iso3c,
                  year = d_p$year,
                  p = pp)
  
  # subset for relevant variables
  pp <- left_join(pp, d_h, by = c('iso3c'))
  pp <- pp[!is.nan(unlist(pp[, paste(y, 'FE', sep='_')])), ]
  
  # calculate intercept
  pp$c <- pp[,y] - pp$p
  
  # mapping of SSP scenario and year of institutional convergence
  scen_CFE <- data.frame(SSP1 = 2180, SSP2 = 2250, SSP3 = Inf, SSP4 = Inf , SSP5 = 2130)
  # scen_CFE <- data.frame(SSP1 = Inf, SSP2 = Inf, SSP3 = Inf, SSP4 = Inf , SSP5 = Inf)
  
  # get CFE
  cfe <- subset(d_h_WI, year == 2020, c('iso3c', paste(y, 'FE', sep = '_')))
  
  # choose max CFE
  m_cfe <- quantile(cfe[, paste(y, 'FE', sep = '_')], .8, na.rm = T)
  
  # iterate over countries
  c <- unique(p$iso3c)
  out <- c()
  for (cc in c) {
    
    # extract country FE
    cfe_cc <- cfe[cfe$iso3c == cc, paste(y, 'FE', sep = '_')]
    
    # extract intercept 
    c_cc <- pp[pp$iso3c == cc, 'c']
    
    if (cfe_cc < m_cfe) {
      
      # calculate average change of country FE for 5 year period
      ## aim by for linear growth
      cfe_cc_g_l <- 5*(m_cfe - cfe_cc)/(scen_CFE[SSP] - 2015)
    
      ## slope under affine growth assumption
      cfe_cc_g <- cfe_cc_g_l - c_cc/17
      cfe_cc_proj_TS <- cumsum(rep(cfe_cc_g, 17)) + c_cc
      
    } else {
      
      # no convergence for countries with high GI at beginning of period
      cfe_cc_proj_TS <- rep(0, 17) + c_cc
      
    }
    
    
    # projection data + K_g_cc_proj_TS
    p_cc <- subset(p, iso3c == cc)
    p_cc$cfe <- cfe_cc_proj_TS
    
    # store tmp
    out <- rbind(out, p_cc)
    
  }
  
  # add country FE to projection
  out[, y] <- out[, y] + out$cfe
  out$cfe <- NULL
  
  # return 
  return(out)
  
}


# project -----------------------------------------------------------------
proj <- function(SSP, y, m_est, tfe, cfe, tfe_para, d_h_WI, d_p_WI) {
  
  # subset for scen
  d_p <- subset(d_p_WI, scen == SSP)
  
  # add intercept
  d_p$`(Intercept)` <- 1
  
  # get posterior coef
  coef <- coef(m_est)[, 'Post Mean']
  
  # only use non time FE coef
  c <- coef[str_detect(names(coef), 'year', negate = T)]
  
  # project
  p <- as.matrix(d_p[, names(c)]) %*% as.matrix(c)
  
  # add TFE from 2015
  p <- p + coef[str_detect(names(coef), '2015')]
  
  # add keys
  p <- data.frame(iso3c = d_p$iso3c,
                  year = d_p$year,
                  scen = d_p$scen,
                  p = p)
  # rename
  names(p)[4] <- y
  
  # add CFE
  d_FE <- subset(d_h_WI, year == 2020)
  p <- left_join(p, d_FE[,c("iso3c", paste(y, 'FE', sep='_'))], by = c('iso3c'))
  p <- p[!is.nan(unlist(p[, paste(y, 'FE', sep='_')])), ]
  p[,y] <- p[, y] + p[, paste(y, 'FE', sep='_')]
  # p[,y] <- p[, paste(y, 'FE', sep='_')]
  p[, paste(y, 'FE', sep='_')] <- NULL
  
  # continue with TFE and CFE
  
  # include fe
  if (tfe) {
    
    # add time FE 
    p <- project_time_FE(SSP, y, p, tfe_para)
    
  }
  
  if (cfe) {
    
    # add CFE
    p <- project_country_FE(SSP, y, p, m_est, d_p_WI, d_h_WI)
    
  }

  # sigmoid
  p[, y] <- 1/(1 + exp(-p[, y]))
  
  # return
  return(p)
  
  
}



# update_tfe --------------------------------------------------------------

update_tfe <- function(SSP, y, d_s_a, tfe_para, p_n_fe) {
  
  # projections without fe
  pnfe <- subset(p_n_fe, scen == SSP, c(c('iso3c', 'year', 'scen'), y))
  
  # project with tfe
  ptfe <- proj(SSP = SSP,
               y = y,
               m_est = m_est[[y]],
               tfe = T,
               cfe = F,
               tfe_para = tfe_para,
               d_h_WI = d_h_WI,
               d_p_WI = d_p_WI)
  
  # merge data
  p_comp <- left_join(ptfe, pnfe, c('iso3c', 'year', 'scen'))
  
  # subset for last period
  p_comp_2100 <- subset(p_comp, year == 2100)
  
  # average diff
  a_d <- mean(p_comp_2100[,4] / p_comp_2100[,5]) - 1
  
  print(a_d)
  # increase tfe_para
  if (abs(a_d) < .9 * abs(d_s_a[SSP])) {
    
    # update para
    tfe_para[y, SSP] <- tfe_para[y, SSP] + sign(a_d) * .001
    
    # update term
    term <- F
    
  }
  # decrease tfe_para
  else if (abs(a_d) > 1.1 * abs(d_s_a[SSP])) {
    
    # update para
    tfe_para[y, SSP] <- tfe_para[y, SSP] - sign(a_d) * .001
    
    # update term
    term <- F
    
  } else {
    
    term <- T
    
  }
  
  # return parameter if search is terminated and tfe_para
  return(list(search = term, tfe_para = tfe_para))
  
}



# smooth ------------------------------------------------------------------

smooth <- function(d_p, d_hist) {
  
  # historic 2015 data
  d_h_2015 <- subset(d_hist, year == 2015)
  
  # 
  d <- left_join(d_p, d_h_2015, c('iso3c'), suffix = c("", "_2015"))
  
  for (y in c('st_cap_or', 'st_cap_ha', 'rspct', 'gee', 'gi_wb')) {
    
    # 2020 = 3/4 * 2015 + 1/4 * 2020
    d[d$year == 2020 , y] <- 1/2 * d[d$year == 2020 , y] + 1/2 * d[d$year == 2020 , paste(y, '2015', sep = '_')]
    
    # 2025 = 1/2 * 2015 + 1/2 * 2020
    d[d$year == 2025 , y] <- 1/2 * d[d$year == 2025 , y] + 1/2 * d[d$year == 2020 , y]
    
    # 2020 = 1/4 * 2015 + 3/4 * 2020
    d[d$year == 2030 , y] <- 1/2 * d[d$year == 2030 , y] + 1/2 * d[d$year == 2025 , y]
    
    # 2020 = 1/4 * 2015 + 3/4 * 2020
    d[d$year == 2035 , y] <- 1/2 * d[d$year == 2035 , y] + 1/2 * d[d$year == 2030 , y]
    
    # 2020 = 1/4 * 2015 + 3/4 * 2020
    d[d$year == 2040 , y] <- 1/2 * d[d$year == 2040 , y] + 1/2 * d[d$year == 2035 , y]
    
  }

  # return
  return(d)
  
}