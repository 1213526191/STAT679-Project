// mean_math ~ cltype+(1|sch)+ses+eth+(1|birthq)

data {
 int<lower=1> N; //number of data points
 int<lower=1> J; //number of schools
 int<lower=1> L; //number of birthq
 int<lower=1> K; //number of fixed variable
 real math[N]; //math score
 matrix[N,K] X;
 int<lower=1, upper=J> sch[N]; //school id
 int<lower=1, upper=L> birthq[N]; //birthq
}

parameters {
 vector[K] beta; //fixed intercept and slope
 vector[J] u; //school intercepts
 vector[L] v; //school birthq
 real<lower=0> sigma_e; //error sd
 real<lower=0> sigma_u; //school sd
 real<lower=0> sigma_v; //birthq sd
}

transformed parameters
{
  vector[N] mu;
  for ( i in 1:N )
  {
    mu[i] = X[i,] * beta;
  }
}

model {
 real mu2;
 //priors
 sigma_u ~ cauchy(0,10);
 sigma_v ~ cauchy(0,10);
 sigma_e ~ cauchy(0,10);
 u ~ normal(0, sigma_u); //school random effects
 v ~ normal(0, sigma_v); //school random birthq
 // likelihood
 for (i in 1:N){
   mu2 = u[sch[i]] + v[birthq[i]] + mu[i];
   math[i] ~ normal(mu2, sigma_e);
  }
}

