data {
    int<lower=0> N;
    int<lower=0> M;
    matrix[N, M] X;
    int<lower=0, upper=1> y[N];
}

parameters {
    real beta0;
    vector[M] beta; 
}

model {
    for (i in 1:N)
        y[i] ~ bernoulli(inv_logit (beta0 + dot_product(X[i] , beta)));
}
