data {
  int<lower=1> ngames;       
  int<lower=1> nteams;       
  int h[ngames];             
  int a[ngames];             
  int<lower=0> y1[ngames];            
  int<lower=0> y2[ngames];
  real beta_0_mu;
  real<lower=0> beta_0_sd;
  real home_mu;
  real<lower=0>home_sd;
  real att_mu;
  real def_mu;
  real sd_att_mu;
  real sd_att_sig;
  real sd_def_mu;
  real sd_def_sig;
  real p_zero_h_alpha;
  real p_zero_h_beta;
  real p_zero_a_alpha;
  real p_zero_a_beta;
}

parameters {
  real beta_0;
  real home;                 
  vector[nteams] att;   
  vector[nteams] def;  
  real<lower=0> sd_att;    
  real<lower=0> sd_def;
  real<lower=0, upper=1> p_zero_h;
  real<lower=0, upper=1> p_zero_a;
}

model {
  for (g in 1:ngames) {
    if (y1[g] == 0){
      target += log_sum_exp(
        bernoulli_lpmf(1 | p_zero_h),
        bernoulli_lpmf(0 | p_zero_h) + poisson_log_lpmf(y1[g] | beta_0 + home + att[h[g]] + def[a[g]])
      );
    }
    else {
      target += bernoulli_lpmf(0 | p_zero_h) + poisson_log_lpmf(y1[g] | beta_0 + home + att[h[g]] + def[a[g]]);
    }
    if (y2[g] == 0){
      target += log_sum_exp(
        bernoulli_lpmf(1 | p_zero_a),
        bernoulli_lpmf(0 | p_zero_a) + poisson_log_lpmf(y2[g] | beta_0 + att[a[g]] + def[h[g]])
      );
    }
    else {
      target += bernoulli_lpmf(0 | p_zero_a) + poisson_log_lpmf(y2[g] | beta_0 + att[a[g]] + def[h[g]]);
    } 
  }

  beta_0 ~ normal(beta_0_mu, beta_0_sd);
  home ~ normal(home_mu, home_sd);
  sd_att ~ cauchy(sd_att_mu, sd_att_sig);
  sd_def ~ cauchy(sd_def_mu, sd_def_sig);
  att ~ normal(att_mu, sd_att);
  def ~ normal(def_mu, sd_def);
  p_zero_h ~ beta(p_zero_h_alpha, p_zero_h_beta);
  p_zero_a ~ beta(p_zero_a_alpha, p_zero_a_beta);
}


generated quantities {
  int y1_pred[ngames];
  int y2_pred[ngames];

  for (g in 1:ngames) {
    y1_pred[g] = bernoulli_rng(1 - p_zero_h) * poisson_log_rng(beta_0 + home + att[h[g]] + def[a[g]]);
    y2_pred[g] = bernoulli_rng(1 - p_zero_a) * poisson_log_rng(beta_0 + att[a[g]] + def[h[g]]);
  }
}

