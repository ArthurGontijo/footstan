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
  real phi_home_mu;
  real phi_home_sig;
  real phi_away_mu;
  real phi_away_sig;
}

parameters {
  real beta_0;
  real home;
  vector[nteams] att;
  vector[nteams] def;
  real<lower=0> sd_att;
  real<lower=0> sd_def;
  real<lower=0> phi_home;
  real<lower=0> phi_away;
}

model {
  for (g in 1:ngames) {
    y1[g] ~ neg_binomial_2_log(beta_0 + home + att[h[g]] + def[a[g]], phi_home);
    y2[g] ~ neg_binomial_2_log(beta_0 + att[a[g]] + def[h[g]], phi_away);
  }

  beta_0 ~ normal(beta_0_mu, beta_0_sd);
  home ~ normal(home_mu, home_sd);
  phi_home ~ cauchy(phi_home_mu, phi_home_sig);
  phi_away ~ cauchy(phi_away_mu, phi_away_sig);
  sd_att ~ cauchy(sd_att_mu, sd_att_sig);
  sd_def ~ cauchy(sd_def_mu, sd_def_sig);
  att ~ normal(att_mu, sd_att);
  def ~ normal(def_mu, sd_def);
}

generated quantities {
  int y1_pred[ngames];
  int y2_pred[ngames];
  vector[ngames] log_lik;

  for (g in 1:ngames) {
    y1_pred[g] = neg_binomial_2_log_rng(beta_0 + home + att[h[g]] + def[a[g]], phi_home);
    y2_pred[g] = neg_binomial_2_log_rng(beta_0 + att[a[g]] + def[h[g]], phi_away);

    log_lik[g] = neg_binomial_2_log_lpmf(y1[g] | beta_0 + home + att[h[g]] + def[a[g]], phi_home) +
                 neg_binomial_2_log_lpmf(y2[g] | beta_0 + att[a[g]] + def[h[g]], phi_away);
  }
}
