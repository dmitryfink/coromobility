rm(list = ls())

library(ggplot2)
library(bayesplot)
library(dplyr)
library(tidyverse)
# library(rstan)

load(file='out/stan.out3.rdata.xz')


mcmc_intervals(out$y %>% `colnames<-`(names(cases2)), prob = 0.5)
mcmc_intervals(out$alpha %>% `colnames<-`(paste0('a', 1:3)), prob = 0.5)

mcmc_intervals(out$mu %>% `colnames<-`(names(cases2)), prob = 0.5)

density_power <- median(out$density_power)
mu_norm <- sweep(out$mu, MARGIN=2,  stan_data$pop_density^density_power, '*')
colnames(mu_norm) <- names(cases2)
mcmc_intervals(mu_norm, prob = 0.5)

mu_norm <- out$mu * t(outer(stan_data$pop_density, out$density_power))
colnames(mu_norm) <- names(cases2)
mcmc_intervals(mu_norm, prob = 0.5)

hist(out$beta_power )
hist(out$density_power )

prediction = out$predicted_daily_new_cases
str(prediction )

last.observation <- cases2$Moscow$date[length(cases2$Moscow$date)]

timeline <- c( cases2$Moscow$date, 
  seq(from =  last.observation + 1, by=1,length.out = 10)
)
prediction.m <- apply(prediction, c(2,3), median) %>% as_tibble %>% set_names(names(cases2)) %>% 
  mutate(date = timeline)

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

# prediction.m %>% mutate_if(is.numeric, cumsum) %>% pivot_longer(cols = -date) %>% mutate(value=value+1)%>% 
#   ggplot(aes(x = date, y=value, col=name)) + geom_line() + geom_vline(aes(xintercept=last.observation)) + 
#     theme_bw() + theme( legend.position="bottom") +  scale_y_continuous(labels = scales::comma)
# 

Rt.m <- apply(out$Rt_adj, c(2,3), median) %>% as_tibble %>% set_names(names(cases2)) %>% mutate(date = timeline) 
Rt.m %>% pivot_longer(cols=-date) %>% ggplot(aes(x=date, y=value, col=name)) + geom_line() + scale_y_log10() + 
  theme_bw() + theme(legend.position="bottom") + ggtitle('Rt')

t(stan_data$X[,,1]) %>% as_tibble %>%  mutate(date = timeline) %>% pivot_longer(cols=-date) %>% ggplot(aes(x=date, y=value, col=name)) + geom_line() + theme_bw() + theme(legend.position="bottom")
t(stan_data$X[,,2]) %>% as_tibble %>%  mutate(date = timeline) %>% pivot_longer(cols=-date) %>% ggplot(aes(x=date, y=value, col=name)) + geom_line() + theme_bw() + theme(legend.position="bottom")

estimated.deaths = out$E_deaths
e_deaths.m <- apply(estimated.deaths, c(2,3), median) %>% as_tibble %>% set_names(names(cases2)) %>% 
  mutate(date = timeline)

stan_data$deaths %>% mutate(date = timeline) %>% pivot_longer(cols = -date) %>% ggplot(aes(x = date, y=value, col=name)) + geom_line() + geom_vline(aes(xintercept=last.observation))+ theme_bw() +
    theme(panel.grid.minor = element_blank(), legend.position="bottom") 
  
e_deaths.m %>% pivot_longer(cols = -date) %>% ggplot(aes(x = date, y=value, col=name)) + geom_line() + geom_vline(aes(xintercept=last.observation))+ theme_bw() +
    theme(panel.grid.minor = element_blank(), legend.position="bottom")

jo.death.data <- stan_data$deaths %>% mutate(date = timeline) %>% dplyr::filter_if(is.double, function(x)x!=-1) %>% mutate(dtype='actual') %>% bind_rows(e_deaths.m %>% mutate(dtype='estimated'))

jo.death.data %>% pivot_longer(cols = c(-dtype, -date)) %>% 
  ggplot(aes(x=date, y=value, color=dtype, group=dtype))+geom_line()+facet_wrap(~name, scales = 'free_y') + theme_bw() + theme(legend.position="bottom") + ggtitle('Estimated and reported deaths')








