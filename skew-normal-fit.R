# Setup -------------------------------------------------------------------
library(brms) 
library(ggplot2)
library(parallel)
library(optparse)
library(magrittr)
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

opt <- list(
  number=100,
  iter="2000",
  directory="three_way_comparison"
)


# Data Generation ---------------------------------------------------------
N <- opt$number
X <- cbind(1, runif(N, 0, 100)) # Generate N uniform random numbers between 0-100

# Setting Distributions Params for Coefficients
B_mu <- c(1, 0.2) # Intercept and coefficient
B_sigma <- c(0.05, 0.02) # Intercept and coefficient
# Note it is nu for gamlss.dist, but alpha for brms
B_nu <- c(0.2,0.1) # Intercept and coefficient

# Generating Coefficients for X values
mu <- X %*% B_mu 
sigma <- exp(X %*% B_sigma)
nu <- X %*% B_nu




# Generating y-values
y <- rskew_normal(n=N, mu = mu, sigma = sigma, alpha = nu)


sim_data <- data.frame(y, X)

# Plotting the data





main_folder <- paste0("./outputs/",opt$directory)
sub_folder <- paste0("./outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter)

conv_folder <- paste0("./outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter,"/simple_regression")
dist_folder <- paste0("./outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter,"/free_variance")
lss_folder <- paste0("./outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter,"/location_scale_shape")


dir.create(path=main_folder,showWarnings = F)
dir.create(path=sub_folder,showWarnings = F)

dir.create(path=conv_folder,showWarnings = F)
dir.create(path=dist_folder,showWarnings = F)
dir.create(path=lss_folder,showWarnings = F)






# Plotting Main Outputs ---------------------------------------------------

data_plot <- ggplot() +
  geom_point(data = sim_data,mapping = aes(x = X2, y = y)) +
  labs(x= "X_rand", y = "Y_sim")







# Simple Regression --------------------------------------
# Formula
conv_fm <- bf(
  y ~ X2
)

#get_prior(conv_fm,sim_data)

# Fitting Model
conv_brm <- brm(conv_fm, 
                data = sim_data, 
                cores = 4,
                control=list(adapt_delta = 0.99),
                iter = opt$iter,
                
                init = 0,
                seed = 404,
                prior = c(
                  prior("normal(0, 10)", class = Intercept),
                  prior("normal(0, 1000)", class = b, coef = X2),
                  prior("normal(0, 10)", class = sigma)
                )
                
)

save(conv_brm, file = paste0(conv_folder,"/distributional_fit.rda"))

png(filename = paste0(conv_folder,"/mcmc_plot.png"))
brms::mcmc_plot(conv_brm)
dev.off()

png(filename = paste0(conv_folder,"/pp_check.png"))
pp_check(conv_brm)
dev.off()


pred_df <- data.frame(X2 = seq(0, 100))
conv_post_pred <- cbind(pred_df, predict(conv_brm, newdata = pred_df))


conv_plot <- ggplot() +
  geom_point(data=sim_data, aes(y=y, x=X2, shape="Data Point"))+
  scale_shape_manual("",values=c(16))+
  
  geom_ribbon(data=conv_post_pred,aes(x=X2, ymin=Q2.5, ymax=Q97.5,fill = "Confidence Interval (2.5, 97.5)"), alpha=0.5)+
  scale_fill_manual("",values="grey12") +
  
  geom_line(data=conv_post_pred,aes(x=X2, y=Estimate, color="Estimate"))+
  scale_colour_manual(c("",""),values=c("black","black"))+
  labs(title = "Predictions for Simple Regression Model")
ggsave( paste0(conv_folder,"/prediction_plot.png"), plot = conv_plot)


sink(paste0(conv_folder,"/fit_diagnostics.txt"))

cat("\n")

cat(paste0("Fit Summary\n"))

summary(conv_brm)
cat("\n")

cat(paste0("Prior Summary\n"))
prior_summary(conv_brm)
cat("\n")

sink()


# Model With Changing Variance --------------------------------------
dist_fm <- bf(
  y ~ X2,
  sigma ~ X2
)
dist_brm <- brm(dist_fm, 
                data = sim_data,
                cores = 4,
                control=list(adapt_delta = 0.99),
                iter = opt$iter,
                
                init = 0,
                seed = 404,
                prior = c(
                  prior("normal(0, 10)", class = Intercept),
                  prior("normal(0, 1000)", class = b, coef = X2),
                  prior("normal(0, 10)", dpar = sigma)
                ))


save(dist_brm, file = paste0(dist_folder,"/distributional_fit.rda"))

png(filename = paste0(dist_folder,"/mcmc_plot.png"))
brms::mcmc_plot(dist_brm)
dev.off()

png(filename = paste0(dist_folder,"/pp_check.png"))
pp_check(dist_brm)
dev.off()

pred_df <- data.frame(X2 = seq(0, 100))
dist_post_pred <- cbind(pred_df, predict(dist_brm, newdata = pred_df))
# head(dist_post_pred)


dist_plot <- ggplot() +
  geom_point(data=sim_data, aes(y=y, x=X2, shape="Data Point"))+
  scale_shape_manual("",values=c(16))+
  
  geom_ribbon(data=dist_post_pred,aes(x=X2, ymin=Q2.5, ymax=Q97.5,fill = "Confidence Interval (2.5, 97.5)"), alpha=0.5)+
  scale_fill_manual("",values="grey12") +
  
  geom_line(data=dist_post_pred,aes(x=X2, y=Estimate, color="Estimate"))+
  scale_colour_manual(c("",""),values=c("black","black"))+
  labs(title = "Predictions for Distributional (Free Variance)")
ggsave( paste0(dist_folder,"/prediction_plot.png"), plot = dist_plot)

sink(paste0(dist_folder,"/fit_diagnostics.txt"))

cat("\n")

cat(paste0("Fit Summary\n"))

summary(dist_brm)
cat("\n")

cat(paste0("Prior Summary\n"))
prior_summary(dist_brm)
cat("\n")

sink()


# Model For Location Scale and Shape --------------------------------------
lss_fm <- bf(
  y ~ X2 ,
  sigma ~ X2,
  alpha ~ X2
)

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
                 prior("normal(0, 1000)", class = b, coef = X2),
                 
                 prior("normal(0, 10)", dpar = sigma),
                 prior("normal(0, 2)", dpar = alpha)
               ),init = 0,
               seed = 404
               
)

save(lss_brm, file = paste0(lss_folder,"/distributional_fit.rda"))

png(filename = paste0(lss_folder,"/mcmc_plot.png"))
brms::mcmc_plot(lss_brm)
dev.off()

png(filename = paste0(lss_folder,"/pp_check.png"))
pp_check(lss_brm)
dev.off()

pred_df <- data.frame(X2 = seq(0, 100))
lss_post_pred <- cbind(pred_df, predict(lss_brm, newdata = pred_df))

lss_plot <- ggplot() +
  geom_point(data=sim_data, aes(y=y, x=X2, shape="Data Point"))+
  scale_shape_manual("",values=c(16))+
  
  geom_ribbon(data=lss_post_pred,aes(x=X2, ymin=Q2.5, ymax=Q97.5,fill = "(2.5, 97.5)"), alpha=0.5)+
  scale_fill_manual("",values="grey12") +
  
  geom_line(data=lss_post_pred,aes(x=X2, y=Estimate, color="Estimate"))+
  scale_colour_manual(c("",""),values=c("black","black"))

ggsave( paste0(lss_folder,"/prediction_plot.png"), plot = lss_plot)


sink(paste0(lss_folder,"/fit_diagnostics.txt"))

cat("\n")

cat(paste0("Fit Summary\n"))

summary(lss_brm)
cat("\n")

cat(paste0("Prior Summary\n"))
prior_summary(lss_brm)
cat("\n")

sink()





# General Outputs ---------------------------------------------------------

sink(paste0(sub_folder,"/fit_params.txt"))

cat("\n")
cat(paste0("Number of Samples:", opt$number,"\n"))
cat(paste0("Number of Iterations:", opt$iter,"\n"))

B_mu <- c(1, 0.2) # Intercept and coefficient
B_sigma <- c(0.05, 0.02) # Intercept and coefficient
# Note it is nu for gamlss.dist, but alpha for brms
B_nu <- c(0.2,0.1) # Intercept and coefficient

cat("Mu \n")
B_mu

cat("Sigma \n")
B_sigma

cat("Nu (alpha) \n")
B_nu
sink()


# Model Comparison (Against True)

conv_fixef <- data.frame(fixef(conv_brm))
conv_fixef$variable <- rownames(fixef(conv_brm))
tmp_sigma <- data.frame(log(t(summary(conv_brm)$spec_pars[1:4])),variable = 'sigma_Intercept')
tmp_sigma$names <- colnames(conv_fixef)[c(1:4)] 
tmp_sigma <- tmp_sigma %>% 
  tidyr::pivot_wider(
    id_cols = variable,
    names_from = names, 
    values_from = sigma,
    id_expand = F)
conv_fixef <- rbind(conv_fixef, tmp_sigma)
conv_fixef$Model <- 'Conventional'

dist_fixef <- data.frame(fixef(dist_brm))
dist_fixef$variable <- rownames(fixef(dist_brm))
dist_fixef$Model <- 'Free Variance'


lss_fixef <- data.frame(fixef(lss_brm))
lss_fixef$variable <- rownames(fixef(lss_brm))
lss_fixef$Model <- "LSS"

true_fixef <- data.frame(
  variable = c('mu_Intercept', 'mu_X2', 'sigma_Intercept', 'sigma_X2', 'alpha_Intercept', 'alpha_X2'),
  value = c(B_mu, B_sigma, B_nu)
)
true_fixef$variable <- factor(true_fixef$variable, ordered = T, levels =true_fixef$variable)

all_fixef <- rbind(conv_fixef, dist_fixef, lss_fixef)

all_fixef$variable[all_fixef$variable == 'Intercept'] <- 'mu_Intercept'
all_fixef$variable[all_fixef$variable == 'X2'] <- 'mu_X2'

summary_plot <- ggplot() +
  geom_crossbar(data = all_fixef, aes(x=Model, y = Estimate, ymin = Q2.5, ymax=Q97.5),
                width=0.2, fill='darkcyan', color='darkcyan', alpha=0.7) +
  geom_hline(data = true_fixef, aes(yintercept=value)) +
  facet_wrap('variable', scales='free_y') +
  labs(y = "Estimate") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))


ggsave( paste0(subfolder,"/fit_summary"), plot = summary_plot)




