rm(list = ls())
gc()

library(ggplot2)
library(bayesplot)
library(dplyr)
library(tidyverse)
library(rstan)
library(furrr)

#making it easy to compare experiments results

c2 <- new.env()
load('out/e2fit2.rdata.xz', envir = c2)

c3 <- new.env()
load('out/e2fit3.rdata.xz', envir = c3)

c5 <- new.env()
load('out/e2fit5.rdata.xz', envir = c5)

c6 <- new.env()
load('out/e2fit6.rdata.xz', envir = c6)


eex <- c6 # this is the experiment we would like to vizualise
# eex <- c7
# eex <- c4

eex$out <- extract(eex$fit)
r.names <- names(eex$cases.selected)
exp.name <- paste(r.names, collapse=' ')
print(exp.name)


# exploring parameters ----------------------------------------------------

mcmc_areas(eex$fit, regex_pars = 'alpha\\[\\d*,1', prob = 0.5, ) + scale_y_discrete(labels = r.names)
mcmc_areas(eex$fit, regex_pars = 'alpha\\[\\d*,2', prob = 0.5, ) + scale_y_discrete(labels = r.names)
mcmc_areas(eex$fit, regex_pars = 'alpha_bias', prob = 0.5, ) + scale_y_discrete(labels = r.names)

mcmc_dens(eex$fit, regex_pars = '^y', prob = 0.5)
mcmc_dens(eex$fit, regex_pars = 'alpha\\[')

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

g1 <- jo.death.data %>% pivot_longer(cols = c(-dtype, -date)) %>% mutate_if(is.numeric, function(x)x+1) %>% 
  ggplot(aes(x=date, y=value, color=dtype, group=dtype))+geom_line()+facet_wrap(~name) + theme_bw() + theme(legend.position="bottom") + 
  ggtitle('Estimated and reported deaths') + scale_y_log10()
print(g1)
ggsave(file = 'charts/df_estimated_deaths.png', plot = g1, device = png())
dev.off()

g2 <- prediction.m %>% pivot_longer(cols = -date) %>% mutate(value = value + 1) %>% 
  ggplot(aes(x = date, y=value, col=name)) + geom_line() + 
    geom_vline(aes(xintercept=last.observation)) + theme_bw() +
    theme(panel.grid.minor = element_blank(), legend.position="bottom") + 
    scale_y_log10(labels = scales::comma, 
      breaks=scales::trans_breaks("log10", function(x) 10^x)) +
    annotation_logticks(sides = "l")+ ggtitle('Number of infected per day, log scale')
ggsave(file = 'charts/df_estimated_infected.png', plot = g2, device = png())
print(g2)
dev.off()

g3 <- prediction.m %>% mutate_if(is.numeric, cumsum) %>% pivot_longer(cols = -date) %>% mutate(value=value+1) %>% 
  ggplot(aes(x = date, y=value, col=name)) + geom_line() + geom_vline(aes(xintercept=last.observation)) +
    scale_y_log10(labels = scales::comma,
      breaks=scales::trans_breaks("log10", function(x) 10^x)) +
    theme_bw() + theme( legend.position="bottom") + 
    annotation_logticks(sides = "l") + ggtitle('Cumulative number of cases, log')
print(g3)
ggsave(file = 'charts/df_estimated_cuminfected.png', plot = g3, device = png())
dev.off()

g4 <- Rt.m %>% pivot_longer(cols=-date) %>% ggplot(aes(x=date, y=value, col=name)) + geom_line() + 
  theme_bw() + theme(legend.position="bottom") + ggtitle('Rt') + scale_y_log10()
ggsave(file = 'charts/df_estimated_Rt.png', plot = g4, device = png())
print(g4)
dev.off()


# plotting covariates against death rates ---------------------------------

jo.death.data %>% pivot_longer(cols = c(-dtype, -date)) %>% 
  ggplot(aes(x=date, y=value, color=dtype, group=dtype))+geom_line()+facet_wrap(~name, scales = 'free_y') + theme_bw() + theme(legend.position="bottom") + ggtitle('Estimated and reported deaths')

t(eex$stan_data$X[,,1]) %>% as_tibble %>%  mutate(date = timeline) %>% pivot_longer(cols=-date) %>% ggplot(aes(x=date, y=value, col=name)) + geom_line() + theme_bw() + theme(legend.position="bottom")

t(eex$stan_data$X[,,2]) %>% as_tibble %>%  mutate(date = timeline) %>% pivot_longer(cols=-date) %>% ggplot(aes(x=date, y=value, col=name)) + geom_line() + theme_bw() + theme(legend.position="bottom")

t(eex$stan_data$X[,,1]) %>% as_tibble %>%  mutate(date = timeline) %>% pivot_longer(cols=-date) %>% mutate(dtype = 'mobility') %>% 
  bind_rows(
        jo.death.data %>% filter(dtype == 'actual') %>% select(-dtype) %>% pivot_longer(-date)%>% mutate(dtype = 'deaths', value=log(1+value))
        ) %>% ggplot(aes(x=date, y=value, group=name, col=name)) + facet_wrap(~dtype, nrow=2, scales = 'free_y') + geom_line() +theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position="bottom") +ggtitle('LOG deahs vs Apple mobility')


eex$stan_data$deaths %>% mutate(date = timeline) %>% pivot_longer(cols = -date) %>% ggplot(aes(x = date, y=value, col=name)) + geom_line() + geom_vline(aes(xintercept=last.observation))+ theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position="bottom") + ggtitle('Deaths')

e_deaths.m %>% pivot_longer(cols = -date) %>% ggplot(aes(x = date, y=value, col=name)) + geom_line() + geom_vline(aes(xintercept=last.observation))+ theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position="bottom")+ ggtitle('Expected Deaths')
