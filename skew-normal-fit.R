
# Setup -------------------------------------------------------------------
library(brms) 
library(ggplot2)
library(parallel)

set.seed(404)
n_cores <- detectCores(all.tests = FALSE, logical = TRUE)


# option_list = list(
#   make_option(c("-n", "--number"), action="store", default=NA, type='character',
#               help="just a variable named a"),
#   make_option(c("-b", "--bvar"), action="store", default=NA, type='character',
#               help="just a variable named b"),
#   make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
#               help="Should the program print extra stuff out? [default %default]"),
#   make_option(c("-q", "--quiet"), action="store_false", dest="verbose",
#               help="Make the program not be verbose."),
#   make_option(c("-c", "--cvar"), action="store", default="this is c",
#               help="a variable named c, with a default [default %default]")  
# )


# Data Generation ---------------------------------------------------------
N <- 100
X <- cbind(runif(N, 0, 100)) # Generate N uniform random numbers between 0-100

# Setting Distributions Params for Coefficients
B_mu <- c( 0.2) 
B_sigma <- c( 0.02)
# Note it is nu for gamlss.dist, but alpha for brms
B_nu <- c( 0.1)

# Generating Coefficients for X values
mu <- X %*% B_mu 
sigma <- exp(X %*% B_sigma)
nu <- X %*% B_nu




# Generating y-values
y <- rskew_normal(n=N, mu = mu, sigma = sigma, alpha = nu)


sim_data <- data.frame(y, X)

# Plotting the data
ggplot() +
  geom_point(data = sim_data,mapping = aes(x = X, y = y)) +
  labs(x= "X_rand", y = "Y_sim")


#
lss_fm <- bf(
  y ~ X ,
  sigma ~ X,
  alpha ~ X
)

get_prior(formula=lss_fm, lss_fm, data=sim_data,family = skew_normal(),autocor = NULL)

lss_brm <- brm(lss_fm, 
               data = sim_data, 
               family = skew_normal(
                 link = "identity", 
                 link_sigma = "log", 
                 link_alpha = "identity"),
               control=list(adapt_delta = 0.99),
               cores = 4,
               prior = c(
                 prior("normal(0, 10)", class = Intercept),
                 prior("normal(0, 1000)", class = b, coef = X),
                 
                 prior("normal(0, 10)", dpar = sigma),
                 prior("normal(0, 2)", dpar = alpha)
               ),init = 0,
               seed = 404
               
               # prior = c(
               #   set_prior(prior = "normal(0, 5)",class = "b",coef  = "X2",),
               #   set_prior(prior = "normal(0, 5)",class = "Intercept"),
               # 
               #   
               #   set_prior(prior = "normal(0, 5)",class = "b",dpar = "nu"),#,lb = -5,ub = 5),
               #   set_prior(prior = "normal(0, 5)",class = "Intercept",dpar = "nu"),#,lb = -5,ub = 5),
               #   
               #   set_prior(prior = "normal(0, 5)",class = "b",dpar = "sigma"),#,lb = -5,ub = 5),
               #   set_prior(prior = "normal(0, 5)",class = "Intercept",dpar = "sigma")#,lb = -5,ub = 5)
               # )
)

save(lss_brm, file = "./outputs/distributional_fit.rda")


png(filename = "./outputs/plot_diagnostics.png")
plot(lss_brm, ask = F) 
dev.off()

png(filename = "./outputs/pp_check.png")
pp_check(lss_brm)
dev.off()


# Summarising fit
readr::write_file(paste0(as.character(summary(lss_brm)), collapse = "\n"),"./outputs/fit_summary.txt")
# readr::write_file(r(prior_su %>% mmary(lss_brm)),"./outputs/prior_summary.txt")


# Prediction
pred_df <- data.frame(X = seq(0, 100))
lss_post_pred <- cbind(pred_df, predict(lss_brm, newdata = pred_df))

prediction_plot <- ggplot() +
  geom_point(data=sim_data, aes(y=y, x=X, shape="Data Point"))+
  scale_shape_manual("",values=c(16))+
  
  geom_ribbon(data=lss_post_pred,aes(x=X, ymin=Q2.5, ymax=Q97.5,fill = "Confidence Interval"), alpha=0.5)+
  scale_fill_manual("",values="grey12") +
  
  geom_line(data=lss_post_pred,aes(x=X, y=Estimate, color="Estimate"))+
  scale_colour_manual(c("",""),values=c("black","black"))

ggsave("./outputs/prediction_plot.png", plot = prediction_plot)



