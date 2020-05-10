rm(list = ls())
gc()

library(ggplot2)
library(bayesplot)
library(dplyr)
library(tidyverse)
library(rstan)
library(furrr)

c2 <- new.env()
load('out/e2.rdata.xz', envir = c2)

c21 <- new.env()
load('out/efit2.rdata.xz', envir = c21)

c31 <- new.env()
load('out/efit3.rdata.xz', envir = c31)

c51 <- new.env()
load('out/efit5.rdata.xz', envir = c51)

c71 <- new.env()
load('out/efit7.rdata.xz', envir = c71)


eex <- c71
# eex <- c7
# eex <- c4

eex$out <- extract(eex$fit)

r.names <- names(eex$cases.selected)
exp.name <- paste(r.names, collapse=' ')
print(exp.name)


# exploring parameters ----------------------------------------------------


mcmc_areas(eex$fit, regex_pars = '^densi', prob = 0.5) 
mcmc_areas(eex$fit, regex_pars = '^densi', prob = 0.5) + scale_y_discrete(labels = r.names)
mcmc_areas(eex$fit, regex_pars = 'alpha\\[\\d*,1', prob = 0.5, ) + scale_y_discrete(labels = r.names)
mcmc_areas(eex$fit, regex_pars = 'alpha\\[\\d*,2', prob = 0.5, ) + scale_y_discrete(labels = r.names)
mcmc_areas(eex$fit, regex_pars = 'alpha_bias', prob = 0.5, ) + scale_y_discrete(labels = r.names)

mcmc_areas(eex$fit, regex_pars = '^y', prob = 0.5)
mcmc_dens(eex$fit, regex_pars = '^y', prob = 0.5)
mcmc_dens(eex$fit, regex_pars = 'alpha\\[')

library(tidybayes)



# calculating the data ----------------------------------------------------

last.observation <- eex$cases.selected$Moscow$date[length(eex$cases.selected$Moscow$date)]

timeline <- c( eex$cases.selected$Moscow$date, 
  seq(from =  last.observation + 1, by=1,length.out = 10)
)

prediction.m <- apply(eex$out$predicted_daily_new_cases, c(2,3), median) %>% as_tibble %>% set_names(r.names) %>% 
  mutate(date = timeline)

Rt.m <- apply(eex$out$Rt_adj, c(2,3), median) %>% as_tibble %>% set_names(r.names) %>% mutate(date = timeline) 

e_deaths.m <- apply(eex$out$E_deaths, c(2,3), median) %>% as_tibble %>% set_names(r.names) %>% 
  mutate(date = timeline)
jo.death.data <- eex$stan_data$deaths %>% mutate(date = timeline) %>% filter_if(is.double, function(x)x!=-1) %>% mutate(dtype='actual') %>% bind_rows(e_deaths.m %>% mutate(dtype='estimated'))

# plotting the results ----------------------------------------------------

jo.death.data %>% pivot_longer(cols = c(-dtype, -date)) %>% mutate_if(is.numeric, function(x)x+1) %>% 
  ggplot(aes(x=date, y=value, color=dtype, group=dtype))+geom_line()+facet_wrap(~name) + theme_bw() + theme(legend.position="bottom") + 
  ggtitle('Estimated and reported deaths') + scale_y_log10()

prediction.m %>% pivot_longer(cols = -date) %>% mutate(value = value + 1) %>% 
  ggplot(aes(x = date, y=value, col=name)) + geom_line() + 
    geom_vline(aes(xintercept=last.observation)) + theme_bw() +
    theme(panel.grid.minor = element_blank(), legend.position="bottom") + 
    scale_y_log10(labels = scales::comma, 
      breaks=scales::trans_breaks("log10", function(x) 10^x)) +
    annotation_logticks(sides = "l")+ ggtitle('Number of infected per day, log scale')


prediction.m %>% mutate_if(is.numeric, cumsum) %>% pivot_longer(cols = -date) %>% mutate(value=value+1) %>% 
  ggplot(aes(x = date, y=value, col=name)) + geom_line() + geom_vline(aes(xintercept=last.observation)) +
    scale_y_log10(labels = scales::comma,
      breaks=scales::trans_breaks("log10", function(x) 10^x)) +
    theme_bw() + theme( legend.position="bottom") + 
    annotation_logticks(sides = "l") + ggtitle('Cumulative number of cases, log')

Rt.m %>% pivot_longer(cols=-date) %>% ggplot(aes(x=date, y=value, col=name)) + geom_line() + 
  theme_bw() + theme(legend.position="bottom") + ggtitle('Rt') + scale_y_log10()



eex$stan_data$deaths %>% mutate(date = timeline) %>% pivot_longer(cols = -date) %>% ggplot(aes(x = date, y=value, col=name)) + geom_line() + geom_vline(aes(xintercept=last.observation))+ theme_bw() +
    theme(panel.grid.minor = element_blank(), legend.position="bottom") 
  
e_deaths.m %>% pivot_longer(cols = -date) %>% ggplot(aes(x = date, y=value, col=name)) + geom_line() + geom_vline(aes(xintercept=last.observation))+ theme_bw() +
    theme(panel.grid.minor = element_blank(), legend.position="bottom")



# plotting covariates against death rates ---------------------------------

jo.death.data %>% pivot_longer(cols = c(-dtype, -date)) %>% 
  ggplot(aes(x=date, y=value, color=dtype, group=dtype))+geom_line()+facet_wrap(~name, scales = 'free_y') + theme_bw() + theme(legend.position="bottom") + ggtitle('Estimated and reported deaths')

t(eex$stan_data$X[,,1]) %>% as_tibble %>%  mutate(date = timeline) %>% pivot_longer(cols=-date) %>% ggplot(aes(x=date, y=value, col=name)) + geom_line() + theme_bw() + theme(legend.position="bottom")
t(eex$stan_data$X[,,2]) %>% as_tibble %>%  mutate(date = timeline) %>% pivot_longer(cols=-date) %>% ggplot(aes(x=date, y=value, col=name)) + geom_line() + theme_bw() + theme(legend.position="bottom")

t(eex$stan_data$X[,,1]) %>% as_tibble %>%  mutate(date = timeline) %>% pivot_longer(cols=-date) %>% mutate(dtype = 'mobility') %>% 
  bind_rows(
        jo.death.data %>% filter(dtype == 'actual') %>% select(-dtype) %>% pivot_longer(-date)%>% mutate(dtype = 'deaths', value=log(1+value))
        ) %>% ggplot(aes(x=date, y=value, group=name, col=name)) + facet_wrap(~dtype, nrow=2, scales = 'free_y') + geom_line()


