rm(list = ls())
gc()

library(rstan)
library(lubridate)
library(dplyr)
library(tidyr)
library(EnvStats)

library(purrr)
library(tidyverse)

source('prepare_data.R')

StanModel = 'model'

load('data/city_mobility.rdata')

ifr.by.country <- read_csv('data/popt_ifr.csv')

region.stat <- tibble(
  region = c('Paris', 'Moscow', 'London', "Saint Petersburg - Russia", "New York City", "Berlin", "Madrid"),
  density = c( 21498,   8537, 4542,  3708, 10194, 3944, 5400),
  population = c(2148000, 12476000, 8982000, 5300000, 8398000, 3769000, 6642000)
)

day.start <- min( map_dbl(cases1, ~  match(T, .x$deaths != 0) ) ) - 40

stopifnot(all(day.start >1))
cases2 <- map(cases1, ~ .x %>% slice(day.start:n(),) )


# Checking the covariates -------------------------------------------------

# map(names(cases2), ~cases2[[.x]] %>% mutate(city=.x) %>% select(-deaths) ) %>% bind_rows() %>% select(-transit) %>% 
#   ggplot(aes(x=date, col=city))+geom_line(aes(y=driving)) + theme_bw()+ theme(panel.grid.minor = element_blank(), legend.position="bottom") + scale_y_continuous(breaks=seq(0, 1.5, by=0.1))
# 
# map(names(cases2), ~cases2[[.x]] %>% mutate(city=.x) %>% select(-deaths) ) %>% bind_rows() %>% select(-transit) %>% 
#   ggplot(aes(x=date, col=city))+geom_line(aes(y=walking)) + theme_bw()+ theme(panel.grid.minor = element_blank(), legend.position="bottom") + scale_y_continuous(breaks=seq(0, 1.5, by=0.1))
# 
# map(names(cases2), ~cases2[[.x]] %>% mutate(city=.x) %>% select(deaths, city, date) ) %>% bind_rows()  %>% mutate(deaths=deaths+1) %>% 
#   ggplot(aes(x=date, col=city))+geom_line(aes(y=deaths)) + theme_bw()+ theme(panel.grid.minor = element_blank(), legend.position="bottom") + scale_y_log10()


# Processing data ---------------------------------------------------------
# cases.selected <- cases2[c('New York City', 'Moscow', 'Paris', 'Madrid')]

# options(mc.cores = parallel::detectCores())
options(mc.cores = 15)
rstan_options(auto_write = TRUE)
m <- stan_model(paste0('model.stan'))

#we want to be able to launch multiple experiments and than compare the results:
exp.spec <- list(
  e1 = list(cities = c('New York City', 'Moscow'),    expname = 'e1'),
  e2 = list(cities = c('New York City', 'Moscow', 'Madrid'),    expname = 'e2'),
  e4 = list(cities = c('New York City', 'Moscow', 'Paris', 'Madrid', 'London'),    expname = 'e3'),
  e5 = list(cities = setdiff(names(cases2),'Berlin'),    expname = str_c('e', length(names(cases2))) )
)

DEBUG <- F

if (DEBUG){
  cases.selected <- cases2[exp.spec$e1$cities]
  stan_data <- process_covariates_city(region.data = cases.selected, 
    region.stat = region.stat, ifr.stats = ifr.by.country, forecast = 10)
  fit.debug = sampling(m,data=stan_data,iter=50, warmup=20, chains=1, thin=1, control = list(adapt_delta = 0.96, max_treedepth = 11))
}

# for (exp.i in exp.spec ){
for (exp.i in exp.spec[c('e4', 'e5')] ){ #if we want to run only few of experiments
	
  print(now())
  cases.selected <- cases2[exp.i$cities]
  stan_data <- process_covariates_city(region.data = cases.selected, 
                                       region.stat = region.stat, ifr.stats = ifr.by.country, forecast = 10)
  
  fit = sampling(m,data=stan_data,iter=2000, warmup=1000, chains=5, thin=1,control = list(adapt_delta = 0.96, max_treedepth = 12))
  # out = rstan::extract(fit)
  expname <- str_c('out/e2fit',length(names(cases.selected)), '.rdata.xz')
  save(fit, cases.selected, stan_data, file=expname, compress = 'xz')
}
