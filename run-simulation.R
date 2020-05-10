rm(list = ls())
gc()

library(rstan)
library(lubridate)
library(dplyr)
library(tidyr)
library(EnvStats)

library(purrr)
library(tidyverse)

source('process-covariates-cities_joint_time.R')

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

exp.spec <- list(
  e1 = list(cities = c('New York City', 'Moscow'),    expname = 'e1'),
  e2 = list(cities = c('New York City', 'Moscow', 'Madrid'),    expname = 'e2'),
  e4 = list(cities = c('New York City', 'Moscow', 'Paris', 'Madrid', 'London'),    expname = 'e3'),
  e5 = list(cities = names(cases2),    expname = str_c('e', length(names(cases2))) )
)

if (F){
  cases.selected <- cases2[exp.spec$e1$cities]
  stan_data <- process_covariates_city(region.data = cases.selected, 
    region.stat = region.stat, ifr.stats = ifr.by.country, forecast = 10)
  fit.debug = sampling(m,data=stan_data,iter=50, warmup=20, chains=1, thin=1, control = list(adapt_delta = 0.96, max_treedepth = 11))
}

for (exp.i in exp.spec[c('e1')] ){
  print(now())
  cases.selected <- cases2[exp.i$cities]
  stan_data <- process_covariates_city(region.data = cases.selected, 
                                       region.stat = region.stat, ifr.stats = ifr.by.country, forecast = 10)
  
  fit = sampling(m,data=stan_data,iter=1500, warmup=800, chains=6,thin=1,control = list(adapt_delta = 0.96, max_treedepth = 12))
  # out = rstan::extract(fit)
  expname <- str_c('out/efit',length(names(cases.selected)), '.rdata.xz')
  save(fit, cases.selected, stan_data, file=expname, compress = 'xz')
}

# # Warning messages:
# Warning messages:
#   1: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.96 may help. See
# http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
# 2: Examine the pairs() plot to diagnose sampling problems
# 
# 3: The largest R-hat is NA, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#r-hat 
# 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#bulk-ess 
# 5: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#tail-ess 
# pairs(fit, pars = c("y", "alpha[1]", "alpha[2]"), las = 1) 


