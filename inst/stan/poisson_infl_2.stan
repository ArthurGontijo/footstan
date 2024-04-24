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
  real att_h_mu; real sd_att_h_mu; real sd_att_h_sig;
  real att_a_mu; real sd_att_a_mu; real sd_att_a_sig;
  real def_h_mu; real sd_def_h_mu; real sd_def_h_sig;
  real def_a_mu; real sd_def_a_mu; real sd_def_a_sig;
  real p_zero_h_alpha;
  real p_zero_h_beta;
  real p_zero_a_alpha;
  real p_zero_a_beta;
}

parameters {
  real beta_0;
  real home;                 
  vector[nteams] att_h;
  vector[nteams] att_a;
  vector[nteams] def_h;
  vector[nteams] def_a;  
  real<lower=0> sd_att_h;
  real<lower=0> sd_att_a;
  real<lower=0> sd_def_h;
  real<lower=0> sd_def_a;
  real<lower=0, upper=1> p_zero_h;
  real<lower=0, upper=1> p_zero_a;
}

model {
  for (g in 1:ngames) {
    if (y1[g] == 0){
      target += log_sum_exp(
        bernoulli_lpmf(1 | p_zero_h),
        bernoulli_lpmf(0 | p_zero_h) + poisson_log_lpmf(y1[g] | beta_0 + home + att_h[h[g]] + def_a[a[g]])
      );
    }
    else {
      target += bernoulli_lpmf(0 | p_zero_h) + poisson_log_lpmf(y1[g] | beta_0 + home + att_h[h[g]] + def_a[a[g]]);
    }
    if (y2[g] == 0){
      target += log_sum_exp(
        bernoulli_lpmf(1 | p_zero_a),
        bernoulli_lpmf(0 | p_zero_a) + poisson_log_lpmf(y2[g] | beta_0 + att_a[a[g]] + def_h[h[g]])
      );
    }
    else {
      target += bernoulli_lpmf(0 | p_zero_a) + poisson_log_lpmf(y2[g] | beta_0 + att_a[a[g]] + def_h[h[g]]);
    } 
  }

  beta_0 ~ normal(beta_0_mu, beta_0_sd);
  home ~ normal(home_mu, home_sd);
  sd_att_h ~ cauchy(sd_att_h_mu, sd_att_h_sig);
  sd_att_a ~ cauchy(sd_att_a_mu, sd_att_a_sig);
  
  sd_def_h ~ cauchy(sd_def_h_mu, sd_def_h_sig);
  sd_def_a ~ cauchy(sd_def_a_mu, sd_def_a_sig);
  
  
  att_h ~ normal(att_h_mu, sd_att_h);
  att_a ~ normal(att_a_mu, sd_att_a);
  
  def_h ~ normal(def_h_mu, sd_def_h);
  def_a ~ normal(def_a_mu, sd_def_a);
  p_zero_h ~ beta(p_zero_h_alpha, p_zero_h_beta);
  p_zero_a ~ beta(p_zero_a_alpha, p_zero_a_beta);
}


generated quantities {
  int y1_pred[ngames];
  int y2_pred[ngames];
  vector[ngames] log_lik;

  for (g in 1:ngames) {
    y1_pred[g] = bernoulli_rng(1 - p_zero_h) * poisson_log_rng(beta_0 + home + att_h[h[g]] + def_a[a[g]]);
    y2_pred[g] = bernoulli_rng(1 - p_zero_a) * poisson_log_rng(beta_0 + att_a[a[g]] + def_h[h[g]]);
    if (y1[g] == 0){
      log_lik[g] = log_sum_exp(
        bernoulli_lpmf(1 | p_zero_h),
        bernoulli_lpmf(0 | p_zero_h) + poisson_log_lpmf(y1[g] | beta_0 + home + att_h[h[g]] + def_a[a[g]])
      );
    }
    else {
      log_lik[g] = bernoulli_lpmf(0 | p_zero_h) + poisson_log_lpmf(y1[g] | beta_0 + home + att_h[h[g]] + def_a[a[g]]);
    }
    if (y2[g] == 0){
      log_lik[g] += log_sum_exp(
        bernoulli_lpmf(1 | p_zero_a),
        bernoulli_lpmf(0 | p_zero_a) + poisson_log_lpmf(y2[g] | beta_0 + att_a[a[g]] + def_h[h[g]])
      );
    }
    else {
      log_lik[g] += bernoulli_lpmf(0 | p_zero_a) + poisson_log_lpmf(y2[g] | beta_0 + att_a[a[g]] + def_h[h[g]]);
    }
  }
}

