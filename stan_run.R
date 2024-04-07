library(ggplot2)
library(dplyr)
library(rstan)
library(bayesplot)
library(frailtyHL)
data("kidney", package = "frailtyHL")
head(kidney)
n <- length(unique(kidney$id))
J <- 2
# Survival and censoring times
time <- kidney$time
cens <- time
time[kidney$status == 0] <- NA # Censored
is.censored <- as.numeric(is.na(time))
# Matrix format
time <- matrix(time, n, J, byrow = TRUE)
cens <- matrix(cens, n, J, byrow = TRUE)
is.censored <- matrix(is.censored, n, J, byrow = TRUE)
sex <- kidney$sex[seq(1, 2 * n, 2)] - 1 # Reference = male
X <- model.matrix(~ sex)
#Put it in a list
list_frail <- list(n = n, J = J, time = cens, X = X, 
                   is_censored = is.censored, p = ncol(X))
#run the stan model
options(mc.cores = parallel::detectCores())
fit <- stan("stan_models/frail_def.stan", iter = 200000, chains = 4,
            data = list_frail, thin=10)
print(fit)
posterior <- as.data.frame(fit)
setwd("/Users/arnaugarcia/Desktop/Q2/surv_analysis_II/2. Multivariate surv analysis/work")
save(posterior, file = "posterior_frail.Rdata")

