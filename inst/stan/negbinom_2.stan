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

  real phi_home_mu;
  real phi_home_sig;
  real phi_away_mu;
  real phi_away_sig;
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
  real<lower=0> phi_home;
  real<lower=0> phi_away;
}

model {
  for (g in 1:ngames) {
    y1[g] ~ neg_binomial_2_log(beta_0 + home + att_h[h[g]] + def_a[a[g]], phi_home);
    y2[g] ~ neg_binomial_2_log(beta_0 + att_a[a[g]] + def_h[h[g]], phi_away);
  }

  beta_0 ~ normal(beta_0_mu, beta_0_sd);
  home ~ normal(home_mu, home_sd);

  phi_home ~ cauchy(phi_home_mu, phi_home_sig);
  phi_away ~ cauchy(phi_away_mu, phi_away_sig);

  sd_att_h ~ cauchy(sd_att_h_mu, sd_att_h_sig);
  sd_att_a ~ cauchy(sd_att_a_mu, sd_att_a_sig);

  sd_def_h ~ cauchy(sd_def_h_mu, sd_def_h_sig);
  sd_def_a ~ cauchy(sd_def_a_mu, sd_def_a_sig);

  att_h ~ normal(att_h_mu, sd_att_h);
  att_a ~ normal(att_a_mu, sd_att_a);

  def_h ~ normal(def_h_mu, sd_def_h);
  def_a ~ normal(def_a_mu, sd_def_a);
}

generated quantities {
  int y1_pred[ngames];
  int y2_pred[ngames];
  vector[ngames] log_lik;

  for (g in 1:ngames) {
    y1_pred[g] = neg_binomial_2_log_rng(beta_0 + home + att_h[h[g]] + def_a[a[g]], phi_home);
    y2_pred[g] = neg_binomial_2_log_rng(beta_0 + att_a[a[g]] + def_h[h[g]], phi_away);

    log_lik[g] = neg_binomial_2_log_lpmf(y1[g] | beta_0 + home + att_h[h[g]] + def_a[a[g]], phi_home) +
                 neg_binomial_2_log_lpmf(y2[g] | beta_0 + att_a[a[g]] + def_h[h[g]], phi_away);
  }
}
