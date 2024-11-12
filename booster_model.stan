functions {
  // function to build splines
  vector build_b_spline(array[] real t, array[] real ext_knots, int index, int order);
  vector build_b_spline(array[] real t, array[] real ext_knots, int index, int order) {
    vector[size(t)] b_spline;
    vector[size(t)] w1 = rep_vector(0, size(t));
    vector[size(t)] w2 = rep_vector(0, size(t));
    if (order==1)
      for (i in 1:size(t))
        b_spline[i] = (ext_knots[index] <= t[i]) && (t[i] < ext_knots[index+1]);
    else {
      if (ext_knots[index] != ext_knots[index+order-1])
        w1 = (to_vector(t) - rep_vector(ext_knots[index], size(t))) /
             (ext_knots[index+order-1] - ext_knots[index]);
      if (ext_knots[index+1] != ext_knots[index+order])
        w2 = 1 - (to_vector(t) - rep_vector(ext_knots[index+1], size(t))) /
                 (ext_knots[index+order] - ext_knots[index+1]);
      b_spline = w1 .* build_b_spline(t, ext_knots, index, order-1) +
                 w2 .* build_b_spline(t, ext_knots, index+1, order-1);
    }
    return b_spline;
  }
}

data {
  int N;                        // number of respondents
  int N_age;                    // number of unique ages
  int S;                        // number of sex groups
  int I;                        // number of Indigenous status groups
  int K;                        // number of vaccine groups
  int P;                        // number of medical conditions
  int V;                        // number of concomitant vaccine
  int Q;                        // number of time epochs
  array[N_age] real ages;       // ordered unique ages
  array[N] int x;               // ages
  array[N] int sex;             // sex groups
  array[N] int ind;             // Indigenous status groups
  array[N] int vax;             // vaccine groups
  array[N] vector[P] W;         // medical conditions
  array[N] vector[V] C;         // concomitant vaccines
  array[N] vector[Q] Z;         // time epochs
  array[N] int y;               // vector of outcomes
  int num_knots;                // num of knots
  vector[num_knots] knots;      // sequence of knots
  int spline_degree;            // degree of spline (is equal to order - 1)
  real alpha_prior_mn;          // prior mean for intercept
  real<lower=0> alpha_prior_sd; // prior sd for intercept
}

transformed data {
  int num_basis = num_knots + spline_degree - 1; // total number of B-splines
  matrix[num_basis, N_age] B;                    // matrix of B-splines
  vector[spline_degree + num_knots] ext_knots_temp;
  vector[2*spline_degree + num_knots] ext_knots; // set of extended knots
  ext_knots_temp = append_row(rep_vector(knots[1], spline_degree), knots);
  ext_knots = append_row(ext_knots_temp, rep_vector(knots[num_knots], spline_degree));
  for (index in 1:num_basis){
    B[index,:] = to_row_vector(build_b_spline(ages, to_array_1d(ext_knots), index, spline_degree + 1));
  }
  B[num_knots + spline_degree - 1, N] = 1;
}

parameters {
  real alpha;                                 // intercept term
  array[S, I, K] row_vector[num_basis] a_ast; // tmp variables for splines
  array[S, I, K] real<lower=0> tau;           // tmp variables for splines
  row_vector[P] beta;                         // medical condition parameters
  array[K] row_vector[V] delta;               // concomitant vaccine parameters
  vector[Q - 1] rho;                          // tmp variables for time epochs
  vector[Q - 1] omega;                        // hyperparameters for epochs
}

transformed parameters {
  array[S, I, K] row_vector[num_basis] a; // parameters for splines
  array[S, I, K] vector[N_age] f;         // complete spline
  row_vector[Q] gamma;                    // time epoch parameters

  // setup splines

  for(s in 1:S){
    for(i in 1:I){
      for(k in 1:K){
        a[s][i][k] = a_ast[s][i][k]*tau[s][i][k];
        f[s][i][k] = to_vector(a[s][i][k]*B);
      }
    }
  }

  // setup time epoch parameters

  gamma[1] = 0;
  for(q in 2:Q) gamma[q] = gamma[q - 1] + rho[q - 1]*omega[q - 1];
}

model {
  target += normal_lpdf(alpha | alpha_prior_mn, alpha_prior_sd);
  for(s in 1:S){
    for(i in 1:I){
      for(k in 1:K){
        target += std_normal_lpdf(a_ast[s][i][k]);
        target += std_normal_lpdf(tau[s][i][k]);
      }
    }
  }
  target += std_normal_lpdf(beta);
  for(k in 1:K) target += std_normal_lpdf(delta[k]);
  target += std_normal_lpdf(rho);
  target += inv_gamma_lpdf(omega | 3, 1);

  for(r in 1:N) target += bernoulli_logit_lpmf(y[r] | alpha + f[sex[r]][ind[r]][vax[r]][x[r]] + beta*W[r] + delta[vax[r]]*C[r] + gamma*Z[r]);
}
