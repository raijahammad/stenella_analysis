# I am going to make nine species-level estimates, meaning I wil fit
# three hierarchical Bayesian LR models (in JAGS!); the difference is that
# each models will include all three species simultaneously so the species-
# specific dynamic will come from the shared hyperdistribution

# NOTES for AAM:
# Mature i = (0) if animal i is immature and (1) is animal i is mature
# the predictor is age, meaning the probability that an animal is mature should
# increase with its age

# NOTES for LAM:
# Mature i = (0) if animal i is immature and (1) is animal i is mature
# the predictor is length, meaning the probability that an animal is mature
# should increase with its length

# NOTES for LAB:
# Born i = (0) if animal i is a fetus and (1) if animal i is a neonate
# the predictor is the animal's length, so as length increases, the probability
# that an observation is a neonate versus a fetus should increase

# what is partial pooling? JAGS will tell me what the slopes and ints generally
# look like across the genus as well as estimate a separate slope and int for
# each species
# we need this because coeruleoalba has a vastly smaller pool of data
# meaning, in a hierarchical model, coeruleoalba's parameters will be informed
# by its own observations as well as the overarching pattern in the other two


# this is my LAB data which has "length", "born", and "species"
birth_all.sp
str(birth_all.sp) # results:
'data.frame':	6501 obs. of  4 variables:
  $ Length  : num  4.1 14.5 17.9 52 9.5 ...
$ Born    : num  0 0 0 0 0 0 0 0 0 0 ...
$ species : Factor w/ 3 levels "attenuata","coeruleoalba",..: 1 1 1 1 1 1 1 1 1 1 ...
$ Length_z: num [1:6501, 1] -1.731 -1.459 -1.37 -0.478 -1.59 ...
..- attr(*, "scaled:center")= num 70.3
..- attr(*, "scaled:scale")= num 38.2

# now i'm checking for missing values JIC
colSums(is.na(birth_all.sp))
is.na(birth_all.sp)
# nice we're totally good on that

# checking "born" values
table(birth_all.sp$Born, useNA = "ifany")
# looks good, only two values

# checking species and their sample sizes
table(birth_all.sp$species, useNA = "ifany")
attenuata coeruleoalba longirostris 
4565           40         1896 
# as expected
# looking at the outcomes
table(
  birth_all.sp$species,
  birth_all.sp$Born
)
0    1
attenuata    2572 1993
coeruleoalba   25   15
longirostris  921  975
# all good

# checking over length distribution
summary(birth_all.sp$Length)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.10   37.70   78.00   70.29  104.00  124.00 
# min is smallest observed length
# max is the largest observed length
# median means half of the observations are shorter than that value
# mean is simply the average
# but we know all of this already


# now I'm looking at lengths separately by species and birth category
library(dplyr)
birth_all.sp %>%
  group_by(species, Born) %>%
  summarise(
    n = n(),
    mean_length = mean(Length, na.rm = TRUE),
    sd_length = sd(Length, na.rm = TRUE),
    min_length = min(Length, na.rm = TRUE),
    max_length = max(Length, na.rm = TRUE),
    .groups = "drop"
  )
1 attenuata     0  2572        42.9      27.5      0.100
2 attenuata     1  1993       104.       13.0     54    
3 coeruleo…     0    25        51.7      24.1      3.30 
4 coeruleo…     1    15       110.       11.8     91    
5 longiros…     0   921        37.6      26.3      0.100
6 longiros…     1   975       105.       14.9     34  

# now I'm visualizing
library(ggplot2)
ggplot(
  birth_all.sp,
  aes(
    x = Length,
    y = Born
  )
) +
  geom_jitter(
    height = 0.05,
    width = 0,
    alpha = 0.2
  ) +
  facet_wrap(
    ~ species,
    scales = "free_x"
  ) +
  labs(
    title = "Fetus and Neonate Lengths by Species",
    x = "Length",
    y = "Birth category: 0 = fetus, 1 = neonate"
  ) +
  theme_minimal()
# everything looks good

# removing unusable rows to be safer than sorry (bc i checked earlier)
birth_jags_data <- birth_all.sp %>%
  filter(
    !is.na(Length),
    !is.na(Born),
    !is.na(species)
  )
# this keeps the rows that satisfy the listed conditions
# !is.na means "not missing"

# now I'm forcing "born" to be an integer
birth_jags_data$Born <- as.integer(birth_jags_data$Born)
# what this does is change the column to integer values, which we do bc JAGS
# expects its Bernoulli outcomes to be numeric zeroes and ones, as we've estab.
unique(birth_jags_data$Born)
# confirmed that it works!

# setting the order of the species
birth_jags_data$species <- factor(
  birth_jags_data$species,
  levels = c(
    "attenuata",
    "longirostris",
    "coeruleoalba"
  )
)
levels(birth_jags_data$species)
# it works, now its att, long, and coe
# we do this because JAGS needs numeric indices so att will be 1, and so on

# now I'm actually creating the numeric species index
birth_jags_data$species_id <- as.integer(
  birth_jags_data$species
)
table(
  birth_jags_data$species,
  birth_jags_data$species_id
)
1    2    3
attenuata    4565    0    0
longirostris    0 1896    0
coeruleoalba    0    0   40
# it worked! (1) attenuata, (2) longirostris, and (3) coeruleoalba

# doing a final checkover of everything
dim(birth_jags_data)
[1] 6501    5
head(birth_jags_data)
Length Born   species   Length_z species_id
1    4.1    0 attenuata -1.7310814          1
2   14.5    0 attenuata -1.4590746          1
3   17.9    0 attenuata -1.3701494          1
4   52.0    0 attenuata -0.4782812          1
5    9.5    0 attenuata -1.5898471          1
6    4.1    0 attenuata -1.7310814          1
table(birth_jags_data)
table(
  birth_jags_data$species,
  birth_jags_data$Born
)
0    1
attenuata    2572 1993
longirostris  921  975
coeruleoalba   25   15
summary(birth_jags_data)
anyNA(birth_jags_data) # looks for any missing values
# all checks look good!


library(dplyr)
library(ggplot2)
birth_jags_data <- birth_all.sp %>%
  filter(
    !is.na(Length),
    !is.na(Born),
    !is.na(species)
  )
birth_jags_data$Born <- as.integer(
  birth_jags_data$Born
)
birth_jags_data$species <- factor(
  birth_jags_data$species,
  levels = c(
    "attenuata",
    "longirostris",
    "coeruleoalba"
  )
)
birth_jags_data$species <- as.integer(
  birth_jags_data$species
)
dim(birth_jags_data)
head(birth_jags_data)
table(birth_jags_data$Born)
table(birth_jags_data$species)
table(
  birth_jags_data$species,
  birth_jags_data$Born
)
summary(birth_jags_data$Length)
anyNA(birth_jags_data)
# I made a cleaned dataset for birth_jags_data
# columns are "Length", "Born", "species", and "species_id"

# making a data list for JAGS
jags_data_birth <- list(
  N = nrow(birth_jags_data),
  S = length(unique(birth_jags_data$species)),
  Length = birth_jags_data$Length,
  Born = birth_jags_data$Born,
  species_id = birth_jags_data$species
)
str(jags_data_birth)

# I have to standardize length
birth_jags_data$Length_z <- as.numeric(scale(birth_jags_data$Length))
jags_data_birth$Length_z <- birth_jags_data$Length_z
mean(jags_data_birth$Length_z) # [1] 8.13378e-16
sd(jags_data_birth$Length_z) # [1] 1
summary(jags_data_birth$Length_z) 
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-1.8357 -0.8523  0.2017  0.0000  0.8818  1.4048 


# yay now for the JAGS model!!! Except I'm just doing a generic one first
birth_model_string <- "
model {

  # likelihood -> one line for every individual
  for (i in 1:N) {

    Born[i] ~ dbern(p[i])

    logit(p[i]) <- alpha[species_id[i]] +
                   beta[species_id[i]] * Length_z[i]
  }

  # species-level parameters
  for (s in 1:S) {


    alpha[s] ~ dnorm(mu_alpha, tau_alpha) # mu_alpha is the avg into across sp
# sigma_alpha is just describing how different the sp ints are
# tau is the signma_alpha precision
    beta[s] ~ dnorm(mu_beta, tau_beta) # every species gets its own slope
  }

  # hyperpriors for the avg slope and intercept
  mu_alpha ~ dnorm(0, 0.01)

  mu_beta ~ dnorm(0, 0.01)

  # hyperpriors for variation amongst species
  sigma_alpha ~ dunif(0, 10)

  sigma_beta ~ dunif(0, 10)

  # i'm converting standard deviations into JAGS precisions
  tau_alpha <- 1 / pow(sigma_alpha, 2)

  tau_beta <- 1 / pow(sigma_beta, 2)
}
"
birth_model_string

# obviously
library(rjags)

# now I am compiling the model!
birth_jags_model <- jags.model(
  textConnection(birth_model_string), # this gives JAGS the text
  data = jags_data_birth,
  n.chains = 3, # the three independent Markov chains
  n.adapt = 1000 # iterations for adaptation
)
str(jags_data_birth)
# frustrated because my data contains both Length and Length_z
jags_data_birth$Length <- NULL
str(jags_data_birth)
# rendering Length as null, we only want Length_z

# now I am recompiling the model
birth_jags_model <- jags.model(
  textConnection(birth_model_string),
  data = jags_data_birth,
  n.chains = 3,
  n.adapt = 1000
)
str(jags_data_birth)

# now to make my model work for me; first, I will be sampling for the posterior
# distribution (Bayesian inference)
library(coda)
birth_samples <- coda.samples(
  model = birth_jags_model,
  variable.names = c(
    "alpha",
    "beta",
    "mu_alpha",
    "mu_beta",
    "sigma_alpha",
    "sigma_beta"
  ),
  n.iter = 10000
)
plot(birth_samples)
gelman.diag(birth_samples)
Potential scale reduction factors:
  Point est. Upper C.I.
alpha[1]             1       1.01
alpha[2]             1       1.00
alpha[3]             1       1.00
beta[1]              1       1.01
beta[2]              1       1.00
beta[3]              1       1.01
mu_alpha             1       1.00
mu_beta              1       1.00
sigma_alpha          1       1.00
sigma_beta           1       1.00
Multivariate psrf
1
# gelman-rubin diagnostic looks good; shows it didn't converge

#  merging all of the chains and iterations into one matrix
birth_post <- as.matrix(birth_samples)
dim(birth_post)
[1] 30000    10


# I'm converting the standardized length into cm
LAB_z <- data.frame(
  attenuata    = -birth_post[, "alpha[1]"] / birth_post[, "beta[1]"],
  coeruleoalba = -birth_post[, "alpha[2]"] / birth_post[, "beta[2]"],
  longirostris = -birth_post[, "alpha[3]"] / birth_post[, "beta[3]"]
)
# vlaues used when Length was standardized
length_mean <- mean(birth_all.sp$Length)
length_sd   <- sd(birth_all.sp$Length)
# now I'm actually converting standardized estimates back into cm
LAB_cm <- LAB_z * length_sd + length_mean
head(LAB_cm)
attenuata coeruleoalba longirostris
1  82.81252     78.34914     91.23417
2  83.44943     78.33375     86.62145
3  83.08192     77.83537     90.05331
4  83.15300     77.97376     92.62948
5  83.03934     78.40770     92.55286
6  82.82279     77.55710     90.39707
# it worked, showing us plausible values (in cm) for LAB

# now, to simply summarize the posterior
apply(LAB_cm, 2, summary)
attenuata coeruleoalba longirostris
Min.     81.95825     76.07854     72.99896
1st Qu.  82.85887     77.60029     86.44542
Median   83.02777     77.89368     88.55968
Mean     83.02949     77.89439     88.48844
3rd Qu.  83.20033     78.19130     90.57189
Max.     84.05271     79.67942    103.57321
apply(LAB_cm, 2, quantile, probs = c(0.025, 0.5, 0.975))
attenuata coeruleoalba longirostris
2.5%   82.53606     77.02971     82.39673
50%    83.02777     77.89368     88.55968
97.5%  83.52474     78.76202     94.33523
# NOTES for ATT:
# 83.0cm is the most probable birth length and there is a 95% CI that truth BL
# is somewhere between 82.5cm and 83.5cm (very narrow interval)
# NOTES for LONG:
# estimated birth length around 77.9cm with a CI of 77.0cm to 78.8cm; still a 
# very tight interval for 40 observations
# NOTES for COE:
#

# WAIT SPECIES GOT OUT OF ORDER -- temp fix:
levels(birth_jags_data$species)
table(birth_jags_data$species, birth_jags_data$species_id)
colnames(LAB_z) <- c(
  "attenuata",
  "longirostris",
  "coeruleoalba"
)
colnames(LAB_cm) <- c(
  "attenuata",
  "longirostris",
  "coeruleoalba"
)
apply(
  LAB_cm,
  2,
  quantile,
  probs = c(0.025, 0.5, 0.975)
)
attenuata longirostris coeruleoalba
2.5%   82.53606     77.02971     82.39673
50%    83.02777     77.89368     88.55968
97.5%  83.52474     78.76202     94.33523