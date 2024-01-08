# Radbeh Heravi



# delete previous stuff
rm(list=ls(all=TRUE))
ls()


# load packages
library("tidyverse")
library("haven")
library("rlang")
library("broom")
library("clubSandwich")


# visit page below
# https://github.com/jrnold/masteringmetrics/blob/master/masteringmetrics/data/deaths.rda
# download mlda.rda
# open the file downloaded
# now you just have imported the data, named "deaths"

setwd("E:/Stuff/Daily Stuffs/University/MA/Mastering 'Metrics class/MM- table5.2&5.3")
load("deaths.rda")



# In these regressions, we will use both indicator variables for year as well as a trend,
# so make a factor version of the year variable.
deaths <- mutate(deaths, year_fct = factor(year))





################### Table 5.3 ################### 
# Regression DD Estimates of MLDA-Induced Deaths among 18-20 year-olds, from 1970-1983
dtypes <- c("all" = "All deaths",
            "MVA" = "Motor vehicle accidents",
            "suicide" = "Suicide",
            "internal" = "All internal causes")


# Estimate the DD for MLDA for all causes of death in 18-20 year olds. 
# Run the regression with lm and calculate the cluster robust standard errors using 
# sandwich::vcovCL. Subset the data.
data <- filter(deaths, year <= 1983, agegr == "18-20 yrs", dtype == "all")


# Run the OLS model 
mod <- lm(mrate ~ 0 + legal + state + year_fct, data = data)



data <- filter(deaths, year <= 1983, agegr == "18-20 yrs", dtype == "all")
mod <- lm(mrate ~ 0 + legal + state + year_fct, data = data)
# step 1
vcov1 <- vcovCR(mod, cluster = data[["state"]], type = "CR2")
# step 2
coef_t <- coef_test(mod, vcov = vcov1)
# step 3
coef_t1 <- rownames_to_column(coef_t, var = "term")
# step 4
coef_t2 <- as_tibble(coef_t1)
# step 5
coef_t3 <- select(coef_t2, term, estimate = beta, std.error = SE)
# step 6
coef_tfil <- filter(coef_t3, term == "legal")
coef_tfil


z <- matrix(c("ntnw", "std.error", "tnw", "std.error", "ntw" , "std.error"
              , rep(0,24)), nrow = 6 ,ncol = 5)
z <- as.data.frame(z)

d <- c("all", "MVA", "suicide", "internal")

for (i in 1:4) {
  
  data <- filter(deaths, year <= 1983, agegr == "18-20 yrs", dtype == d[i])
  
  # step 1
  vcov_ntnw <- vcovCR(lm(mrate ~ 0 + legal + state + year_fct, data = data)
                      , cluster = data[["state"]], type = "CR2")
  vcov_tnw <- vcovCR(lm(mrate ~ 0 + legal + year_fct + state + state:year, data = data)
                     , cluster = data[["state"]], type = "CR2")
  vcov_ntw <- vcovCR(lm(mrate ~ 0 + legal + year_fct + state, data = data, weights = pop)
                     , cluster = data[["state"]], type = "CR2")
  # step 2
  coef_ntnw <- coef_test(lm(mrate ~ 0 + legal + state + year_fct, data = data)
                         , vcov = vcov_ntnw)
  coef_tnw <- coef_test(lm(mrate ~ 0 + legal + year_fct + state + state:year, data = data)
                        , vcov = vcov_tnw)
  coef_ntw <- coef_test(lm(mrate ~ 0 + legal + year_fct + state, data = data, weights = pop)
                        , vcov = vcov_ntw)
  # step 3
  coef_ntnw3 <- rownames_to_column(coef_ntnw, var = "term")
  coef_tnw3 <- rownames_to_column(coef_tnw, var = "term")
  coef_ntw3 <- rownames_to_column(coef_ntw, var = "term")
  
  # step 4
  coef_ntnw4 <- select(as_tibble(coef_ntnw3), term, estimate = beta, std.error = SE)
  coef_tnw4 <- select(as_tibble(coef_tnw3), term, estimate = beta, std.error = SE)
  coef_ntw4 <- select(as_tibble(coef_ntw3), term, estimate = beta, std.error = SE)
  
  # step 5
  filter_ntnw <- filter(coef_ntnw4, term == "legal")
  filter_tnw <- filter(coef_tnw4, term == "legal")
  filter_ntw <- filter(coef_ntw4, term == "legal")
  
  # step 6
  z[1,i+1] <- round(filter_ntnw$estimate,2)
  z[2,i+1] <- round(filter_ntnw$std.error,2)
  z[3,i+1] <- round(filter_tnw$estimate,2)
  z[4,i+1] <- round(filter_tnw$std.error,2)
  z[5,i+1] <- round(filter_ntw$estimate,2)
  z[6,i+1] <- round(filter_ntw$std.error,2)
  colnames(z) <- c("name","all(l)","MVA(l)","suicide(l)", "internal(l)")
}

z




################### Table 5.3 ################### 

z1 <- matrix(c("nt", "std.error", "t", "std.error", rep(0,16)), nrow = 4 ,ncol = 5)
z1 <- as.data.frame(z1)
z2 <- matrix(c("nt", "std.error", "t", "std.error", rep(0,16)), nrow = 4 ,ncol = 5)
z2 <- as.data.frame(z2)
d <- c("all", "MVA", "suicide", "internal")
b <- c("legal", "beertaxa")

for (i in 1:4) {
  for (j in 1:2) {
    
  data <- filter(deaths, year <= 1983, agegr == "18-20 yrs",
                 dtype == d[i], !is.na(beertaxa))
  
  # step 1
  vcov_nt <- vcovCR(lm(mrate ~ 0 + legal + beertaxa + year_fct + state, data = data)
                    , cluster = data[["state"]], type = "CR2")
  vcov_t <- vcovCR(lm(mrate ~ 0 + legal + beertaxa + year_fct + state + state:year,
                      data = data), cluster = data[["state"]], type = "CR2")
  
  # step 2
  coef_nt <- coef_test(lm(mrate ~ 0 + legal + beertaxa + year_fct + state, data = data)
                       , vcov = vcov_nt)
  coef_t <- coef_test(lm(mrate ~ 0 + legal + beertaxa + year_fct + state + state:year,
                         data = data)
                      , vcov = vcov_t)
  
  # step 3
  coef_nt3 <- rownames_to_column(coef_nt, var = "term")
  coef_t3 <- rownames_to_column(coef_t, var = "term")
  
  # step 4
  coef_nt4 <- select(as_tibble(coef_nt3), term, estimate = beta, std.error = SE)
  coef_t4 <- select(as_tibble(coef_t3), term, estimate = beta, std.error = SE)
  
  
  # step 5
  filter_nt <- filter(coef_nt4, term == b [j])
  filter_t <- filter(coef_t4, term == b [j])
  
  # step 6
  if(j == 1) {
    z1[1,i+1] <- round(filter_nt$estimate,2)
    z1[2,i+1] <- round(filter_nt$std.error,2)
    z1[3,i+1] <- round(filter_t$estimate,2)
    z1[4,i+1] <- round(filter_t$std.error,2)
    
  } else {
    z2[1,i+1] <- round(filter_nt$estimate,2)
    z2[2,i+1] <- round(filter_nt$std.error,2)
    z2[3,i+1] <- round(filter_t$estimate,2)
    z2[4,i+1] <- round(filter_t$std.error,2)
  }
  }
  
  z3 <- cbind(z1, z2[,2:5])
  colnames(z3) <- c("name","all(l)","MVA(l)","suicide(l)",
                    "internal(l)", "all(b)","MVA(b)","suicide(b)",
                    "internal(b)")
  
}

z3






