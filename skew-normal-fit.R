# Setup -------------------------------------------------------------------
library(brms) 
library(ggplot2)
library(parallel)
library(optparse)

set.seed(404)

option_list = list(
  make_option(c("-n", "--number"),  type='integer',
               help="just a variable named a"),
  make_option(c("-i", "--iter"),  type='integer',
              help="Iterations"),
  make_option(c("-d", "--directory"), type='character',
               help="The directory where the file will be stored")
  

)
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);


# Data Generation ---------------------------------------------------------
N <- opt$number
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

lss_fm <- bf(
  y ~ X ,
  sigma ~ X,
  alpha ~ X
)

# get_prior(formula=lss_fm, lss_fm, data=sim_data,family = skew_normal(),autocor = NULL)

lss_brm <- brm(lss_fm, 
               data = sim_data, 
               family = skew_normal(
                 link = "identity", 
                 link_sigma = "log", 
                 link_alpha = "identity"),
               control=list(adapt_delta = 0.99),
               cores = 4,
               iter = opt$iter,
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




main_folder <- paste0("./outputs/",opt$directory)
sub_folder <- paste0("./outputs/",opt$directory,"/n_",opt$number, "iter_",opt$iter)
dir.create(path=main_folder,showWarnings = F)
dir.create(path=sub_folder,showWarnings = F)



save(lss_brm, file = paste0(sub_folder,"/distributional_fit.rda"))


png(filename = paste0(sub_folder,"/mcmc_plot.png"))
brms::mcmc_plot(lss_brm)
dev.off()

png(filename = paste0(sub_folder,"/pp_check.png"))
pp_check(lss_brm)
dev.off()


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

ggsave( paste0(sub_folder,"/prediction_plot.png"), plot = prediction_plot)


sink(paste0(sub_folder,"/fit_diagnostics.txt"))

cat("\n")
cat(paste0("Number of Samples:", opt$number,"\n"))
cat(paste0("Number of Iterations:", opt$iter,"\n"))

cat(paste0("Fit Summary\n"))

summary(lss_brm)
cat("\n")

cat(paste0("Prior Summary\n"))
prior_summary(lss_brm)
cat("\n")

sink()
