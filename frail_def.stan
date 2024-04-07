
data {
  int<lower=0> n;
  int<lower=0> J;
  int<lower=0> p;
  matrix[n,J] time;
  matrix[n,p] X;
  matrix[n,J] is_censored;
}

parameters {
  vector[p] beta;
  vector<lower=0>[n] w;  
  real<lower=0> psi;
  real<lower=0> alpha;
}


model {
  for (i in 1:n) {
    for (j in 1:J) {
      if(is_censored[i,j]==1){
        target += weibull_lccdf(time[i,j]| alpha, (w[i]*exp(beta[1]*X[i,1] + beta[2]*X[i,2]))^(-1/alpha));
      } else{
        time[i,j] ~ weibull(alpha,(w[i]*exp(beta[1]*X[i,1] + beta[2]*X[i,2]))^(-1/alpha));
      }
    }
  }
  alpha ~ uniform(0, 10);  // Shape parameter prior
  beta ~ normal(0, 1000);   // Scale parameter prior
  w ~ gamma(psi, psi);  // Frailty parameter prior
  psi ~ gamma(0.01,0.01);
}

generated quantities{
  real<lower=0> lambda;
  real<lower=0> hr;
  lambda = exp(beta[1]);
  hr = exp(beta[2]);
}
 
