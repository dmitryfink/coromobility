rm(list = ls())

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

map(names(cases2), ~cases2[[.x]] %>% mutate(city=.x) %>% select(-deaths) ) %>% bind_rows() %>% select(-transit) %>% 
  ggplot(aes(x=date, col=city))+geom_line(aes(y=driving)) + theme_bw()+ theme(panel.grid.minor = element_blank(), legend.position="bottom") + scale_y_continuous(breaks=seq(0, 1.5, by=0.1))

map(names(cases2), ~cases2[[.x]] %>% mutate(city=.x) %>% select(-deaths) ) %>% bind_rows() %>% select(-transit) %>% 
  ggplot(aes(x=date, col=city))+geom_line(aes(y=walking)) + theme_bw()+ theme(panel.grid.minor = element_blank(), legend.position="bottom") + scale_y_continuous(breaks=seq(0, 1.5, by=0.1))


# Processing data ---------------------------------------------------------

processed_data <- process_covariates_city(region.data = cases2, 
    region.stat = region.stat, ifr.stats = ifr.by.country, forecast = 10)


stan_data = processed_data$stan_data
stan_data$deaths

stan_data$N

stan_data$pop_density

stan_data$pop


str(stan_data$X)
stan_data$X[,1,]


# Running HMC -------------------------------------------------------------

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
invisible(map(stan_data, ~ print(str(.x) )))

m <- stan_model(paste0('stan-models/',StanModel,'.stan'))
# fit.debug = sampling(m, data=stan_data,iter=40,warmup=20,chains=1)

fit = sampling(m,data=stan_data,iter=1500,warmup=700,chains=4,thin=1,control = list(adapt_delta = 0.96, max_treedepth = 11))

# 
# Warning messages:
# 1: There were 57 divergent transitions after warmup. Increasing adapt_delta above 0.96 may help. See
# http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
# 2: There were 1304 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 11. See
# http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 3: Examine the pairs() plot to diagnose sampling problems
#  #  
# pairs(fit, pars = c("y", "alpha[1]", "alpha[2]"), las = 1) 

print(fit)

out = rstan::extract(fit)

save(out, cases2, stan_data, file='stan.out3.rdata.xz', compress = 'xz')

