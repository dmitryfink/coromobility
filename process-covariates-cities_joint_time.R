library(rstan)
# library(data.table)
library(lubridate)
# library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(scales)
library(stringr)
library(abind)
library(zoo)

process_covariates_city <- function(region.data, region.stat, ifr.stats, forecast = 10){
  N <- nrow(region.data[[1]])
  N2 <- N + forecast
  M <- length(region.data)
  
  stopifnot(all(map_int(region.data, ~nrow(.x) == N))) # check that all the regions have the same amount of data
  
  serial.interval = read.csv("data/serial_interval.csv")
  
  # Pads serial interval with 0 if N2 is greater than the length of the serial
  # interval array
  if (N2 > length(serial.interval$fit)) {
    print('padding serial interval')
    pad_serial.interval <- data.frame(
      "X"=(length(serial.interval$fit)+1):N2,
      "fit"=rep(1e-17, max(N2-length(serial.interval$fit), 0 ))
    )
    serial.interval = rbind(serial.interval, pad_serial.interval)
  }
  # various distributions required for modeling
  mean1 <- 5.1; cv1 <- 0.86; # infection to onset
  mean2 <- 17.8; cv2 <- 0.45 # onset to death
  x1 <- rgammaAlt(1e6,mean1,cv1) # infection-to-onset distribution
  x2 <- rgammaAlt(1e6,mean2,cv2) # onset-to-death distribution
  
  ecdf.saved <- ecdf(x1+x2)
  
  # dates <- list()
  # reported_cases <- list()
  # 
  IFRs <- rep(mean( ifr.stats$ifr ), M)
  fmx <- map(IFRs, function(IFR){
    convolution = function(u) (IFR * ecdf.saved(u))
    
    f = rep(0,N2) # f is the probability of dying on day i given infection
    f[1] = (convolution(1.5) - convolution(0))
    for(i in 2:N2) {
      f[i] = (convolution(i+.5) - convolution(i-.5)) 
    }
    f
  }) %>% bind_cols()
  key.stat <- c('date', 'deaths')
  feature.names <- c('walking', 'driving')
  # set_mx_cols <- function(mx, new.colnames){
  #   colnames(mx) <- new.colnames
  #   mx
  # }
  
  epidemic.start.idxs <- map_dbl(region.data, ~which(cumsum(.x$deaths)>=10)[1]) - 30
  stopifnot(all(epidemic.start.idxs > 1))
  #transform features to avoid multicollinearity:
  conv.features.names <- c('mean', 'spread')
  pca.matrix <- matrix(c(0.5,0.5,1,-1), ncol=2, dimnames=list(NULL,conv.features.names ))
  region.data <- map(region.data, ~ .x %>% select(!!c(key.stat, feature.names)) %>% bind_cols(.x[feature.names] %>% as.matrix %*% pca.matrix %>% as_tibble) %>% select( - !!feature.names)) 
  region.data <- map(
    region.data, ~ .x %>% mutate(deaths = stats::filter(deaths, rep(1/4,4), sides = 1) %>% as.numeric %>% round %>% na.fill0(0))
  )
  
  deaths.mx <- map( region.data, ~ .x['deaths'] %>% add_row(deaths = rep(-1, forecast)) ) %>% 
    bind_cols() %>% set_names(names(region.data))
  
  stan_data <- list(M     = M, 
                    N2    = N2,
                   deaths = deaths.mx,
                   f      = fmx,
                   SI     = serial.interval$fit[1:N2],
                   X      = abind(map( region.data, ~ .x[,conv.features.names] %>% bind_rows(NA *.x[1:forecast,conv.features.names]) %>% na.locf), along=-1), #add new data as placeholders for the values to be forecasted
                   EpidemicStart = epidemic.start.idxs,
                   pop = region.stat[match( names(region.data), region.stat$region) %>% na.omit , 'population', drop= T],
                   pop_density = region.stat[match( names(region.data), region.stat$region) %>% na.omit , 'density', drop= T]
    )
  stan_data$deaths <- round(stan_data$deaths)
  stan_data$pop_density <- stan_data$pop_density / mean(stan_data$pop_density)
  stan_data$x=1:N2
  stan_data$N0 <- 4
  stan_data$N <- rep(N, M)
  stan_data$P = length(feature.names)
  stopifnot(dim(stan_data$X) == c(stan_data$M, stan_data$N2, stan_data$P ))
  stan_data
}

