
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
concoms <- c("influenza","dtpa","shingrix","prev")


dat <- load("C:/Users/ETay/Documents/Booster doses/Data/booster_sd_sample_20241111.Rda")


###analysis for Figure 1 (MA)


dat <- booster_sd
dat <- dat[!is.na(dat$sex),]
dat <- dat[!is.na(dat$epoch),]
#lists for the output



N <- dat[,.N]                                              ## number of respondents
N_age <- dat[, length(unique(age))]           ## number of unique ages
S <- dat[, nlevels(sex)] #Females = 1, Males = 2           ## number of sex groups
I <- dat[, nlevels(indig)]                                 ## number of Indigenous status groups
K <- 3                                                     ## number of vaccine groups #3
P <- length(conds)                                         ## number of medical conditions
V <- 4 #TBD                                                ## number of concomitant vaccine

Q <- dat[, nlevels(epoch)]
ages <- dat[, as.numeric(names(table(age)))]
x <- dat$age
sex <- dat$sex                                            ## should be 1,2,etc.
ind <- dat$indig                                          ## 1 = Non, 2 = Aboriginal or Torres Strait Islander
vax <- dat$vax_group                                     ## vaccine groups## should be 1,2,etc.

W <- as.matrix(dat[, ..conds])                           ## list all conditions and ensure all 0 or 1
C <- as.matrix(dat[, ..concoms])                         ## list all concoms and ensure all 0 or 1

Z <- as.matrix(dat[, .(as.integer(epoch == 3), 
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

mod <- cmdstan_model(write_stan_file(readLines("Z:/Manual Stuff/COVID-19 Booster doses/Analysis code/booster_model.stan")))
mod <- cmdstan_model(write_stan_file(readLines("C:/Users/ETay/Documents/booster_model.stan")))
##if no postr
postr <- readRDS(file = "Fig1_postr_20241112.RDS")
## eventually put in for loop

fit <- mod$sample(data = stan_data, chains = 8, parallel_chains = 8)
postr <- posterior::as_draws_matrix(fit$draws())

saveRDS(postr, file = "Fig1_postr_20241112.RDS")

postr <- postr[,str_detect(colnames(postr), "alpha|f|beta|delta")]

pred <- array(dim = c(nrow(postr), S, I, K, N_age),
              dimnames = list(Draw = 1:nrow(postr), Sex = 1:S, Indig = 1:I, Vax = 1:K, Age = 1:N_age))

for(s in 1:2){
  for(i in 1:2){
    for(k in 1:3){
      for(a in 1:N_age){
        pred[,s,i,k,a] <- postr[,"alpha"] + postr[,paste0("f[", s, ",", i, ",", k, ",", a, "]")]
      }
    }
  }
}

# for(s in 1:2){
#   for(i in 1:2){
#     for(k in 1:3){
#       for(a in 1:N_age){
#         if(sum(sex == s & ind == i & vax == k & x == ages[a]) > 0){
#           tmp <- matrix(W[sex == s & ind == i & vax == k & x == ages[a],], ncol = length(conds))
#           prop <- colMeans(tmp)
#         } else {
#           prop <- rep(1/length(conds), length(conds))
#         }
#         pred[,s,i,k,a] <- postr[,"alpha"] + 
#                           postr[,paste0("f[", s, ",", i, ",", k, ",", a, "]")] + 
#                           as.vector(prop%*%t(postr[,paste0("beta[", 1:length(conds), "]")]))
#       }
#     }
#   }
# }

pred_vis <- as.data.table(pred)
pred_vis[, `:=`(
  value = plogis(value),
  Age = factor(Age, levels = as.character(1:86), labels = ages),
  Vax = factor(Vax, labels = c("Pf", "Mod", "Nvx")),
  Indig = factor(Indig, labels = c("Indig", "Non Indig")),
  Sex = factor(Sex, labels = c("Female", "Male"))
)]

pred_vis <-  pred_vis[, .(mn = mean(value),
                          lower = quantile(value, probs = 0.025),
                          upper = quantile(value, probs = 0.975)), by = .(Sex, Indig, Vax, Age)]

ggplot(pred_vis, aes(x = as.numeric(as.character(Age)), colour = Vax, fill = Vax, group = Vax)) +
  facet_grid(Sex ~ Indig) +
  geom_line(aes(y = mn)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)


p_marg <- sapply(1:nrow(?), function(r) plogis(postr[,"alpha"] #+ spline
                                                               + as.vector(W[r,]%*%t(postr[,str_detect(colnames(postr),"beta")]))       ##conds
                                                               + as.vector(C[r,]%*%t(postr[,str_detect(colnames(postr),"delta")]))      ##concoms
                                                               + as.vector(Z[r,]%*%t(postr[,str_detect(colnames(postr),"gamma")]))))    ##epochs

n_sex <- nlevels(dat$sex)
n_indig <- nlevels(dat$indig)


#obtain ests



for(brand_dose in c("Comirnaty (Pfizer)", "Spikevax (Moderna)", "Nuvaxovid (Novavax)")){
  

}

#vis_list

#plot vis_list



  ### Figure 2 (MAfor medical conditions)
  
  ### Figure 3 (Any_event for solicited symptoms)
#Local = 0.3
#Fatigue = 0.2
#Headache = 0.15
#muscle or joint pain = 0.17
#Gi = 0.05
#Fever = 0.06



  ### Figure 4 (MA  for concomitant vaccines)
  
  ### Figure 5 (Impact on work/study/normal activity loss - on routine activities)

#mpact = 0.07


             
