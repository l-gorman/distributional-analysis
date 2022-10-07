
# Setup -------------------------------------------------------------------
library(brms) 
library(ggplot2)
library(parallel)
library(optparse)

set.seed(404)

# Setting Distributions Params for Coefficients
B_mu <- c( 0.2) 
B_sigma <- c( 0.02)
# Note it is nu for gamlss.dist, but alpha for brms
B_nu <- c( 0.1)

n = 100
iter = 5000


# Generating Coefficients for X values
mu <- X %*% B_mu 
sigma <- exp(X %*% B_sigma)
nu <- X %*% B_nu



# Loading in model fit lss_brm (run on blue crystal)
load("outputs/2022-10-06/n_100iter_5000/distributional_fit.rda")

sim_data <- lss_brm$data












