
# load packages -----------------------------------------------------------

pacman::p_load(readxl, wcde, pwt10, tidyverse, countrycode, zoo)


# read data ---------------------------------------------------------------

# read historic data
d_hist <- read.csv('input/hist.csv')

# edu share
edu_share_h <- read.csv('input/wcde_edu_hist.csv', skip = 8)

# edu share gap
edu_share_gg_h <- read.csv('input/wcde_edu_gg_hist.csv', skip = 8)

# mys hist
mys_h <- read.csv('input/wcde_mys_hist.csv', skip = 8)

# mys hist gap
mys_gg_h <- read.csv('input/wcde_mys_gg_hist.csv', skip = 8)

# government indicators
sheets <- readxl::excel_sheets('input/wgidataset.xlsx') 
gi_raw <- lapply(sheets[-1], function(x) readxl::read_excel('input/wgidataset.xlsx', sheet = x, skip = 13, col_names = F, na = '#N/A')) 


# edu share
edu_share_p <- read.csv('input/wcde_edu_proj.csv', skip = 8)

# edu share gap
edu_share_gg_p <- read.csv('input/wcde_edu_gg_proj.csv', skip = 8)

# mys hist
mys_p <- read.csv('input/wcde_mys_proj.csv', skip = 8)

# mys hist gap
mys_gg_p <- read.csv('input/wcde_mys_gg_proj.csv', skip = 8)

# gdp
gdp_ssp <- read.csv('input/gdp_ssp_GI.csv')



# process -----------------------------------------------------------------


# historic data -----------------------------------------------------------

#GI
gi <- data.frame(iso3c = d_hist$countrycode,
                 year = d_hist$year)


for (i in 1:length(gi_raw)) {
  
  # select every column 3rd mod 4
  gi_raw[[i]] <- gi_raw[[i]][-2, c(2, seq(3, ncol(gi_raw[[i]]), 6))]
  
  # change entry
  gi_raw[[i]][1,1] <- "iso3c"
  
  # change names
  names(gi_raw[[i]]) <- as.character(gi_raw[[i]][1,])
  
  # omit 1st row
  gi_raw[[i]] <- gi_raw[[i]][-1, ]
  
  # pivot longer
  gi_raw[[i]] <- pivot_longer(gi_raw[[i]], !iso3c, names_to = 'year', values_to = 'gi')
  
  # convert data type
  gi_raw[[i]]$year <- as.integer(gi_raw[[i]]$year)
  
  # merge
  gi <- left_join(gi, gi_raw[[i]], by = c('iso3c', 'year'))
  
}

# average columns and apply logit
gi_wb <- data.frame(iso3c = gi$iso3c,
                    year = gi$year,
                    gi_wb = 1/(1 + exp( - rowMeans(apply(gi[,3:ncol(gi)], 2, as.numeric)))))



# MODIFY HISTORIC DATA
d_hist <- d_hist %>%
  
  # for 1970 on wards
  filter(year >= 1970) %>%
  
  # rename variables
  rename(iso3c = countrycode,
         st_cap_or = st.cap.or,
         st_cap_ha = st.cap.ha,
         gdp_pc = gdp.pc,
         ps_rel_f = fem_ps_2064,
         ps_rel = ps_2064,
         mys_gg = ggap) %>%
  
  # add log of gdp_cap
  mutate(log_gdp_pc = log(gdp_pc)) %>%
  
  # merge gi_wb
  left_join(., gi_wb, by = c('iso3c', 'year')) %>%
  
  # select variables
  select(!c(X, pop, rgdpe, mys_gg, ps_rel_f, ps_rel, gdp_pc))




# edu share hist
edu_s_h <- edu_share_h %>%
  
  # select observation over age >= 15
  filter(Education %in% c("No Education", "Incomplete Primary", "Primary", "Lower Secondary", "Upper Secondary", "Post Secondary")) %>%
  
  # iso3c
  mutate(iso3c = countrycode(Area, 'country.name', 'iso3c'),
         Education = Education %>% fct_recode(e1_s = "No Education",
                                        e2_s = "Incomplete Primary",
                                        e3_s = "Primary",
                                        e4_s = "Lower Secondary",
                                        e5_s = "Upper Secondary",
                                        e6_s = "Post Secondary")) %>%
  
  
  # omit NAs
  drop_na() %>%
  
  # rename
  rename(year = Year,
         edu = Distribution) %>%
  
  # select relevant variables
  select(c(iso3c, year,  Education, edu)) %>%
  
  # pivot
  pivot_wider(names_from = Education, values_from = edu)




# edu share hist gender gap
edu_s_gg_h <- edu_share_gg_h %>%
  
  # iso3c
  mutate(iso3c = countrycode(Area, 'country.name', 'iso3c'),
         Education = Education %>% fct_recode(e1_s_gg = "No Education",
                                              e2_s_gg = "Incomplete Primary",
                                              e3_s_gg = "Primary",
                                              e4_s_gg = "Lower Secondary",
                                              e5_s_gg = "Upper Secondary",
                                              e6_s_gg = "Post Secondary")) %>%
  
  # omit NAs
  drop_na() %>%
  
  # rename
  rename(year = Year,
         edu = Percentage.Points) %>%
  
  # select relevant variables
  select(c(iso3c, year,  Education, edu)) %>%
  
  # pivot
  pivot_wider(names_from = Education, values_from = edu)



# MYS
mys_h <- mys_h %>%
  
  # iso3c
  mutate(iso3c = countrycode(Area, 'country.name', 'iso3c')) %>%
  
  # omit NAs
  drop_na() %>%
  
  # rename
  rename(year = Year,
         mys = Years) %>%
  
  # select relvant varaibles
  select(c(iso3c, year, mys))


# MYS GAP
mys_gg_h <- mys_gg_h %>%
  
  # iso3c
  mutate(iso3c = countrycode(Area, 'country.name', 'iso3c')) %>%
  
  # omit NAs
  drop_na() %>%
  
  # rename
  rename(year = Year,
         mys_gg = Years) %>%
  
  # select relvant varaibles
  select(c(iso3c, year, mys_gg))


# merge historic data
d_hist <- d_hist %>%
  
  # + edu
  left_join(., edu_s_h, by = c('iso3c', 'year')) %>%
  
  # + edu_s
  left_join(., edu_s_gg_h, by = c('iso3c', 'year'))%>%
  
  # + mys
  left_join(., mys_h, by = c('iso3c', 'year')) %>%
  
  # + mys_gg
  left_join(., mys_gg_h, by = c('iso3c', 'year'))


# interpolate dependent variables
d_hist <- d_hist %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate_at(vars(e1_s, e2_s, e3_s, e4_s, e5_s, e6_s, e1_s_gg, e2_s_gg, e3_s_gg, e4_s_gg, e5_s_gg, e6_s_gg, mys, mys_gg), na.approx, na.rm=FALSE) 



# projected data ----------------------------------------------------------

# edu share proj
edu_s_p <- edu_share_p %>%
  
  # select observation over age >= 15
  filter(Education %in% c("No Education", "Incomplete Primary", "Primary", "Lower Secondary", "Upper Secondary", "Post Secondary")) %>%
  
  # iso3c
  mutate(iso3c = countrycode(Area, 'country.name', 'iso3c'),
         Education = Education %>% fct_recode(e1_s = "No Education",
                                              e2_s = "Incomplete Primary",
                                              e3_s = "Primary",
                                              e4_s = "Lower Secondary",
                                              e5_s = "Upper Secondary",
                                              e6_s = "Post Secondary")) %>%
  
  
  # omit NAs
  drop_na() %>%
  
  # rename
  rename(year = Year,
         edu = Distribution,
         scen = Scenario) %>%
  
  # select relevant variables
  select(c(iso3c, year, scen,  Education, edu)) %>%
  
  # pivot
  pivot_wider(names_from = Education, values_from = edu)




# edu share gender gap proj
edu_s_gg_p <- edu_share_gg_p %>%
  
  # iso3c
  mutate(iso3c = countrycode(Area, 'country.name', 'iso3c'),
         Education = Education %>% fct_recode(e1_s_gg = "No Education",
                                              e2_s_gg = "Incomplete Primary",
                                              e3_s_gg = "Primary",
                                              e4_s_gg = "Lower Secondary",
                                              e5_s_gg = "Upper Secondary",
                                              e6_s_gg = "Post Secondary")) %>%
  
  # omit NAs
  drop_na() %>%
  
  # rename
  rename(year = Year,
         edu = Percentage.Points,
         scen = Scenario) %>%
  
  # select relevant variables
  select(c(iso3c, year, scen,  Education, edu)) %>%
  
  # pivot
  pivot_wider(names_from = Education, values_from = edu)



# MYS
mys_p <- mys_p %>%
  
  # iso3c
  mutate(iso3c = countrycode(Area, 'country.name', 'iso3c')) %>%
  
  # omit NAs
  drop_na() %>%
  
  # rename
  rename(year = Year,
         mys = Years,
         scen = Scenario) %>%
  
  # select relevant variables
  select(c(iso3c, year, scen, mys))


# MYS GAP
mys_gg_p <- mys_gg_p %>%
  
  # iso3c
  mutate(iso3c = countrycode(Area, 'country.name', 'iso3c')) %>%
  
  # omit NAs
  drop_na() %>%
  
  # rename
  rename(year = Year,
         mys_gg = Years,
         scen = Scenario) %>%
  
  # select relevant variables
  select(c(iso3c, year, scen, mys_gg))


# GDP SSP
names(gdp_ssp)[1] <- 'scen'
gdp_ssp$gdp_pc <- gdp_ssp$gdp_pc * 1000
gdp_ssp$log_gdp_pc <- log(gdp_ssp$gdp_pc)


# MERGE PORJECTION DATA

# keys
iso3c <- setdiff(unique(gdp_ssp$iso3c), c("AFG", "CIV", "COM", "ERI", "FJI", "GNB", "HTI", "IRQ", "KNA", "LBR", "MHL", "PLW", "PRK", "RWA", "SLB", "SLE", "SOM", "SSD", "SYR", "TKM", "TUV", "VUT", "YEM", "ZAR"))
year <- seq(2020, 2100, 5)
scen <- c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5')



d_proj <- expand_grid(iso3c, year, scen) %>%
  
  # + gdp pc
  left_join(.,gdp_ssp, by = c('iso3c', 'year', 'scen')) %>%
  
  # + edu_s
  left_join(., edu_s_p, by = c('iso3c', 'year', 'scen')) %>%
  
  # + edu_s_gg
  left_join(., edu_s_gg_p, by = c('iso3c', 'year', 'scen')) %>%
  
  # + MYS
  left_join(., mys_p, by = c('iso3c', 'year', 'scen')) %>%
  
  # + MYS gender gap
  left_join(., mys_gg_p, by = c('iso3c', 'year', 'scen'))







# write data --------------------------------------------------------------

saveRDS(d_hist, 'output/d_hist.RDS')
saveRDS(d_proj, 'output/d_proj.RDS')
