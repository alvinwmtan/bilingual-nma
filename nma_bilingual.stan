// Bayesian contrast-based NMA: Monolingual vs Bilingual Vocabulary
// Arms: 1=mono (reference), 2=bi_L1, 3=bi_total, 4=bi_conceptual
//
// Key features:
//  - Contrast likelihood on SMD y (Hedges g or Cohen d) with known se_y
//  - Study heterogeneity: independent per bilingual arm (tau[k])
//  - Within-cohort correlation across bilingual arms via u_ppt (MVN)
//  - Scalar cohort intercept v_ppt (captures cohort-level shifts; cancels if paired)
//  - Arm-differential meta-regression: (beta_trt - beta_ref) * X

functions {
  // Return arm-specific covariate slope vector (mono arm has zero slopes)
  row_vector beta_of_arm(int arm, matrix beta, int P) {
    row_vector[P] out;
    if (arm == 1) out = rep_row_vector(0.0, P);
    else out = beta[arm - 1];
    return out;
  }

  // Absolute effect of an arm for a given study/cohort (ppt_id).
  // Mono (arm=1): only v_ppt
  // Bilingual arms: mu_d[k] + delta_study[s,k] + u_ppt[i,k] + v_ppt
  real arm_effect(int arm, int s, int i,
                  vector mu_d,
                  matrix delta_study,
                  matrix u_ppt,
                  vector v_ppt) {
    real base = v_ppt[i];
    if (arm == 1) return base;
    {
      int k = arm - 1;  // bilingual index: 1..3
      return mu_d[k] + delta_study[s, k] + u_ppt[i, k] + base;
    }
  }
}

data {
  int<lower=1> N_obs;
  int<lower=1> N_study;
  int<lower=1> N_ppt;
  int<lower=1> K;        // total arms (=4)
  int<lower=0> P;        // covariate columns

  vector[N_obs] y;       // SMD (g or d): trt - ref
  vector[N_obs] se_y;    // known SE of y

  array[N_obs] int<lower=1, upper=K>       arm_trt;
  array[N_obs] int<lower=1, upper=K>       arm_ref;
  array[N_obs] int<lower=1, upper=N_study> study_id;
  array[N_obs] int<lower=1, upper=N_ppt>   ppt_trt_id;
  array[N_obs] int<lower=1, upper=N_ppt>   ppt_ref_id;
  array[N_obs] int<lower=0, upper=1>       paired;

  matrix[N_obs, P] X;    // no intercept column
}

parameters {
  // Treatment effects (bilingual vs mono)
  vector[K-1] mu_d;                // [1]=bi_L1, [2]=bi_total, [3]=bi_conceptual

  // Arm-differential meta-regression slopes (bilingual arms only; mono = 0)
  matrix[K-1, P] beta;

  // Between-study heterogeneity (independent per bilingual arm)
  vector<lower=0>[K-1] tau;
  matrix[N_study, K-1] z_study;    // non-centred

  // Scalar cohort intercept (applies to all arms)
  vector[N_ppt] v_ppt;
  real<lower=0> sigma_ppt;

  // Within-cohort correlated residuals across bilingual arms
  cholesky_factor_corr[K-1] L_Omega;
  vector<lower=0>[K-1]      sigma_within;
  matrix[N_ppt, K-1]        z_ppt;  // non-centred
  
  // SD inflation
  real<lower=0> sigma_add_paired;
  real<lower=0> sigma_add_unpaired;
}

transformed parameters {
  matrix[N_study, K-1] delta_study;
  matrix[N_ppt, K-1]   u_ppt;

  for (k in 1:(K-1))
    delta_study[, k] = tau[k] * z_study[, k];

  {
    matrix[K-1, K-1] A = diag_pre_multiply(sigma_within, L_Omega);
    for (i in 1:N_ppt)
      u_ppt[i] = (A * z_ppt[i]')';
  }
}

model {
  // Priors
  mu_d ~ normal(0, 1);

  // Slopes: shrink fairly strongly unless covariates are well-scaled
  to_vector(beta) ~ normal(0, 0.5);

  tau          ~ normal(0, 0.3);
  sigma_within ~ normal(0, 0.3);
  sigma_ppt    ~ normal(0, 0.3);
  
  sigma_add_paired   ~ normal(0, 0.2);
  sigma_add_unpaired ~ normal(0, 0.2);

  to_vector(z_study) ~ std_normal();
  to_vector(z_ppt)   ~ std_normal();

  L_Omega ~ lkj_corr_cholesky(2);
  v_ppt   ~ normal(0, sigma_ppt);
  

  // Likelihood
  for (i in 1:N_obs) {
    real eta_base =
      arm_effect(arm_trt[i], study_id[i], ppt_trt_id[i], mu_d, delta_study, u_ppt, v_ppt)
    - arm_effect(arm_ref[i], study_id[i], ppt_ref_id[i], mu_d, delta_study, u_ppt, v_ppt);

    row_vector[P] b_trt = beta_of_arm(arm_trt[i], beta, P);
    row_vector[P] b_ref = beta_of_arm(arm_ref[i], beta, P);

    real eta = eta_base + dot_product( X[i], (b_trt - b_ref)' );
    
    real sigma_add = paired[i] == 1 ? sigma_add_paired : sigma_add_unpaired;
    real sigma_i   = sqrt(square(se_y[i]) + square(sigma_add));
    
    y[i] ~ normal(eta, sigma_i);
  }
}

generated quantities {
  corr_matrix[K-1] Omega = multiply_lower_tri_self_transpose(L_Omega);

  real d_total_vs_L1         = mu_d[2] - mu_d[1];
  real d_conceptual_vs_L1    = mu_d[3] - mu_d[1];
  real d_conceptual_vs_total = mu_d[3] - mu_d[2];
}
