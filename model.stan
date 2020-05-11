functions {
  
  vector relu_power(vector x, real power) {
    vector[rows(x)] result;
    for (i in 1:rows(x)){
      if ( x[i] > 0 ) result[i] = x[i] ^ power;
      else result[i] = 0;
    }
    return result;
  }
  
}


data {
  int <lower=1> M; // number of cities
  int <lower=1> P; // number of covariates
  int <lower=1> N0; // index of the first day, since which we need to impute infections
  int<lower=1> N[M]; // index of the last observed data for city m
  int<lower=1> N2; // days of observed data + number of days to forecast
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  matrix[N2, M] f; // h * s, daily density of fatality rate
  matrix[N2, P] X[M]; // features matrix, currently mean mobility and spread between mobility driving and walking
  int EpidemicStart[M];
  real pop[M];
  real pop_density[M];
  real SI[N2]; // fixed pre-calculated SI using emprical data from Neil
}

transformed data {
  // matrix[N2, P+2] Xa[M]; // features matrix
  vector[N2] SI_rev; // SI in reverse order
  vector[N2] f_rev[M]; // f in reversed order
  
  // for (i in 1:M){
  //   Xa[i] = append_col(X[i], rep_matrix(1.0, N2, 1));  // adding bias
  // };  
  
  for(i in 1:N2)
    SI_rev[i] = SI[N2-i+1];
    
  for(m in 1:M){
    for(i in 1:N2) {
     f_rev[m, i] = f[N2-i+1,m];
    }
  }
}

parameters {
  real<lower=0> mu[M]; // intercept for Rt
  vector[P] alpha[M];  //number of features
  real alpha_bias[M];
  real<lower=0> gamma;
  real<lower=0> kappa;
  real<lower=0> y[M]; // average number of new cases per day, used as a prior
  real<lower=0> phi;
  real<lower=0> tau;
  real <lower=0> ifr_noise[M];
  // real <lower=0, upper=10> beta_power;
  // real <lower=-1, upper=2> density_power[M];
}

transformed parameters {
  matrix[N2, M] predicted_daily_new_cases = rep_matrix(0,N2,M);   // number of NEW daily cases
  matrix[N2, M] E_deaths  = rep_matrix(0,N2,M);
  matrix[N2, M] Rt = rep_matrix(0,N2,M);
  matrix[N2, M] Rt_adj = Rt;
      
  {
    matrix[N2,M] total_cases = rep_matrix(0,N2,M);
    
    for (m in 1:M){

      predicted_daily_new_cases[(EpidemicStart[m]-N0+1):EpidemicStart[m], m] = rep_vector(y[m], N0);
      
      total_cases[2:EpidemicStart[m],m] = cumulative_sum(predicted_daily_new_cases[2:EpidemicStart[m],m]); // total number of cases per region
      
      // model 1:
      Rt[,m] = mu[m] * exp( X[m] * alpha[m] + alpha_bias[m] );
      
      // model 2:
      // Rt[,m] = mu[m] * exp( X[m] * alpha[m] + alpha_bias[m] ) * (pop_density[m]^density_power[m]) ;
      
      // model 3:
      // Rt[,m] = mu[m] * relu_power( Xa[m] * alpha[m] + alpha_bias[m], beta_power) * (pop_density[m]^density_power[m]) ;
      
      Rt_adj[1:EpidemicStart[m],m] = Rt[1:EpidemicStart[m],m];
      for (i in (EpidemicStart[m]+1):N2) {
        real serial_transmission = dot_product(sub_col(predicted_daily_new_cases, 1, m, i-1), tail(SI_rev, i-1));
        total_cases[i,m] = total_cases[i-1,m] + predicted_daily_new_cases[i-1,m];
        Rt_adj[i,m] = ((pop[m]-total_cases[i,m]) / pop[m]) * Rt[i,m];
        predicted_daily_new_cases[i, m] = Rt_adj[i,m] * serial_transmission; // !!!!!!!!!!!!!!!!!!
      }
      E_deaths[1, m]= 1e-15 * predicted_daily_new_cases[1,m];
      for (i in 2:N2){
        E_deaths[i,m] = ifr_noise[m] * dot_product(sub_col(predicted_daily_new_cases, 1, m, i-1), tail(f_rev[m], i-1));
      }
    }
  }
}

model {
  tau ~ exponential(0.03);
  for (m in 1:M){
    y[m] ~ exponential(1/tau);
  }
  // beta_power ~ normal(0.6,1);
  for ( i in 1:M){
    alpha[i][1] ~ normal(1,0.5);
    alpha[i][2] ~ normal(0,0.5);
  }
  alpha_bias ~ normal(-1,1);
  gamma ~ normal(0, .2);

  phi ~ normal(0,5);
  kappa ~ normal(0,0.5);
  mu ~ normal(3.28, kappa); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319
  // density_power ~ normal(0.7,1); //necessary for other models
  ifr_noise ~ normal(1,0.1);
  for(m in 1:M){
    deaths[EpidemicStart[m]:N[m], m] ~ neg_binomial_2(E_deaths[EpidemicStart[m]:N[m], m], phi);
  }
}
