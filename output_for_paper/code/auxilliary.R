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

# plots --------------------------------------------------------------------

p_GI <- function(d_proj, d_h, c, FE) {
  
  # iterate over dep var
  for (y in c('st_cap_or', 'st_cap_ha', 'rspct', 'gee', 'gi_wb')) {
    
    # historic data
    h <- subset(d_hist, iso3c == c & year %in% seq(1980, 2015, 5), c('year', y))
    h <- bind_rows(replicate(5, h, simplify = FALSE))
    h$scen <- c(rep('SSP1', 8), rep('SSP2', 8), rep('SSP3', 8), rep('SSP4', 8), rep('SSP5', 8))
    h <- h[ ,c(1, 3, 2)]
    
    # projected
    p <- d_proj[d_proj$iso3c == c, c('year', 'scen', y)]
    
    # h + p
    hp <- rbind(h, p)
    names(hp)[3] <- 'value'
    
    # plot
    assign(paste('p', y, sep = '_'), ggplot(hp, aes(x = year, y = value, col = scen)) +
             geom_line() +
             geom_vline(xintercept = 2015, linetype="solid",
                        color = "blue", size=.5) +
             ggtitle(y))
    
    
  }
  
  combined <- p_gi_wb + p_st_cap_ha + p_st_cap_or + p_rspct + p_gee & theme(legend.position = "bottom")
  combined + plot_layout(guides = "collect")
  

    
  pdf(paste('output/plots/projections/', c,".pdf", sep = ""))
  print(combined + plot_layout(guides = "collect"))
  dev.off()

  
  
  return(combined)
  
  
  
}
