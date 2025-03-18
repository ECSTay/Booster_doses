library(data.table)
library(cmdstanr)
library(matrixStats)
library(forcats)
library(stringr)

conds <- c("pmh_heart_dis", "pmh_bp", "pmh_diabetes", "pmh_lung",
           "pmh_obesity", "pmh_kidney", "pmh_liver", "pmh_cancer", "pmh_blood_cancer", 
           "pmh_chemo_rad", "pmh_transplant_organ", "pmh_transplant_bone", "pmh_neuro", "pmh_inflam", "pmh_immunodef")
cond_names <- c("Heart disease",
                "Poorly controlled blood pressure", "Diabetes", "Chronic lung disease",
                paste0("Obesity (BMI ", "\U2265", "40)"), "Chronic kidney failure", "Chronic liver disease",
                "Cancer (Solid organ)", "Haematologial cancer", "Receiving chemotherapy or radiotherapy",
                "Organ transplant recipient", "Bone marrow transplant recipient", "Neurological condition",
                "Chronic inflammatory condition", "Immunodeficiency")
concoms <- c("concom_flu","concom_dtpa","concom_shingrix","concom_prevenar_13", "concom_other")

#load("C:/Users/ETay/Documents/Booster_doses/Data/booster_sd_20241217.Rda")
load("booster_sd_20241217.Rda")
dat <- booster_sd

###analysis for MA - for sex/Indig, concoms, chronic medical conditions

N <- dat[,.N]                                              ## number of respondents
N_age <- dat[, length(unique(age))]           ## number of unique ages
S <- dat[, nlevels(sex)] #Females = 1, Males = 2           ## number of sex groups
I <- dat[, nlevels(indig)]                                 ## number of Indigenous status groups
K <- 7                                                     ## number of vaccine brands #7
P <- length(conds)                                         ## number of medical conditions
V <- 5 #TBD                                                ## number of concomitant vaccine

Q <- dat[, nlevels(epoch)]
ages <- dat[, as.numeric(names(table(age)))]
x <- dat$age
sex <- dat$sex                                            ## should be 1,2,etc.
ind <- dat$indig                                          ## 1 = Non, 2 = Aboriginal or Torres Strait Islander
vax <- as.numeric(as.factor(dat$vax_brand))               ## vaccine groups## should be 1,2,etc.

W <- as.matrix(dat[, ..conds])                           ## list all conditions and ensure all 0 or 1
C <- as.matrix(dat[, ..concoms])                         ## list all concoms and ensure all 0 or 1

Z <- as.matrix(dat[, .(as.integer(epoch == 4), 
                       as.integer(epoch == 3), 
                       as.integer(epoch == 2), 
                       as.integer(epoch == 1))]) 


y <- dat$ma


num_knots <- 8
knots <- seq(min(ages), max(ages), length.out = num_knots)
spline_degree <- 3

## these will depend on the outcome
alpha_prior_mn <- -2
alpha_prior_sd <- 1

stan_data <- list(N = N, N_age = N_age, S = S, I = I, K = K,  P = P, V = V, Q = Q,
                  ages = ages, x =x, sex = sex, ind = ind, vax = vax, W = W, C = C, Z = Z, y = y,
                  num_knots = num_knots, knots = knots, spline_degree = spline_degree,
                  alpha_prior_mn = alpha_prior_mn, alpha_prior_sd = alpha_prior_sd)

#mod <- cmdstan_model(write_stan_file(readLines("C:/Users/ETay/Documents/booster_model.stan")))
mod <- cmdstan_model(write_stan_file(readLines("booster_model.stan")))

fit <- mod$sample(data = stan_data, chains = 4, parallel_chains = 4)

postr <- posterior::as_draws_matrix(fit$draws())

saveRDS(postr, file = "posterior_MA.rds")

# > table(vax)
# vax
# 1     2     3     4     5     6     7 
# 19271 84769 71590   396  9998 25360 20700 
