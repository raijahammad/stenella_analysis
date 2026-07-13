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

# creating my LAM dataset
# checking over my reference dataset
table(stenella_clean$Species)
attenuata coeruleoalba longirostris 
4805           35         3275 
table(stenella_clean$Species, stenella_clean$isMature_bin)
0    1
attenuata    1379 3426
coeruleoalba   15   20
longirostris 1343 1932
summary(stenella_clean$TotalLength)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
80.0   168.0   177.0   177.1   187.0   231.0 
# now for the actual LAM dataset
LAM_data <- stenella_clean %>%
  transmute(
    Length = TotalLength,
    Mature = isMature_bin,
    species = Species
  ) %>%
  filter(
    !is.na(Length),
    !is.na(Mature),
    !is.na(species)
  )
str(LAM_data)
table(LAM_data$species, LAM_data$Mature)
summary(LAM_data$Length)

# forcing species order since last time was a bust
LAM_data$species <- factor(
  LAM_data$species,
  levels = c(
    "attenuata",
    "longirostris",
    "coeruleoalba"
  )
)
LAM_data$species_id <- as.numeric(LAM_data$species)
levels(LAM_data$species)
table(LAM_data$species, LAM_data$species_id)
# it worked

# next step is standardizing the lengths to make the est easier; we do this by
# turning them into z-scores bc JAGS samples efficiently when predictors are 
# centered around 0 and have a SD of 1
# Length z = (Length - mean Length)/SD(Length)
length_mean <- mean(LAM_data$Length)
length_sd <- sd(LAM_data$Length)
LAM_data$Length_z <-
  (LAM_data$Length - length_mean) / length_sd
summary(LAM_data$Length_z)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-6.498779 -0.606281 -0.003639  0.000000  0.665963  3.612212
mean(LAM_data$Length_z)
[1] -3.093014e-15
sd(LAM_data$Length_z)
[1] 1
# it worked
LAM_data$N <- length(LAM_data$Mature)
LAM_data <- list(
  N = length(LAM_jags$Mature),
  S = length(unique(LAM_jags$species_id)),
  Length_z = as.numeric(LAM_jags$Length_z),
  Mature = as.numeric(LAM_jags$Mature),
  species_id = as.integer(LAM_jags$species_id),
  length_mean = mean(LAM_jags$Length),
  length_sd = sd(LAM_jags$Length)
)
str(LAM_data)

# JAGS data list
# this is for the JAGS input only
LAM_jags <- list(
  N = nrow(LAM_data),
  S = 3,
  Length = LAM_data$Length,
  Length_z = LAM_data$Length_z,
  Mature = LAM_data$Mature,
  species_id = LAM_data$species_id
)
str(LAM_jags)

# now for the string model
LAM_model_string <- "
model {

  # the likelihood function, it is the probability model we want to estimate
  for (i in 1:N) {

    Mature[i] ~ dbern(p[i]) # Bernoulli distribution (dbern) has only two
    # possible outcomes ( 0 or 1 in this case, mature or immature)
    # if p = 0.80 then there is an 80% chance for 1 which is mature, and thus a
    # 20% chance for 0 which is immature
    # the maturity status of dolphin i comes from the Bernoulli distribution
    # with probability pi of being mature

    logit(p[i]) <-
      alpha[species_id[i]] +
      beta[species_id[i]] * Length_z[i]
  } # equation of a line -> y = alpha + beta(x)
# the [species_id[i]] is what makes my model hierarchical; each species gets
# its own logistic regression line, but note that intercepts and slopes are not
# estimated independently
# allows coeruleoalba with 35 observations to share information with att & long

  # species-specific parameters
  for (s in 1:S) { # our loop for species; s = 3 for JAGS will repeat the 
  # following equations three times (once for each species)

    alpha[s] ~ dnorm(mu_alpha, tau_alpha)

    beta[s] ~ dnorm(mu_beta, tau_beta)
  } # our regular LR gave us slopes & ints independently for each species, and
  # we saw with coeruleoalba that it left us with unstable values simply bc of
  # the lack of data; this code assumes that each species' intercept comes from
  # a normal distribution
  
  # hyperpriors for intercepts
  mu_alpha ~ dnorm(0, 0.01) # this line is the prior distribution
  # mu_alpha is the center of the normal distribution
  # basically, it is the average intercept across all three species (pop mean)
  sigma_alpha ~ dunif(0, 10) # estimating how different the species are from
  # one another; it is the spread, so it should tell us how similar the ints are
  tau_alpha <- 1 / pow(sigma_alpha, 2)

  # hyperpriors for slopes
  mu_beta ~ dnorm(0, 0.01)
  sigma_beta ~ dunif(0, 10)
  tau_beta <- 1 / pow(sigma_beta, 2)

  # Length at 50% maturity on the standardized scale
  for (s in 1:S) {
    LAM_z[s] <- -alpha[s] / beta[s]

    # Convert back into centimeters
    LAM_cm[s] <- LAM_z[s] * length_sd + length_mean
  }
}
"

# NOTE: logit(p) = log(p/1-p)
# the logit transformation takes probabilities between 0 and 1 then converts
# them to values that range from negative infinity to positive infinity
# probabilities near 0 will become large negative numbers and probabilities
# near 1 become large positive numbers, also a probability of 0.5 corresponds
# to a logit of 0

# ALSO NOTE, by standardizing the lengths to a z-score, a length_z = 0 means the
# dolphin is of average length
# this means that the intercepts tells us the log-odds of being mature for an 
# average-length dolphin

# What is precision? It is the inverse of variance; it's debatable as to 
# whether it's informative or not.

# creating the JAGS model
# I had to create initial values that explicitly start every species' slope
# above zero
LAM_inits <- function() {
  list(
    mu_alpha = rnorm(1, 0, 1),
    mu_beta = runif(1, 0.5, 1.5),
    
    sigma_alpha = runif(1, 0.5, 2),
    sigma_beta = runif(1, 0.2, 1),
    
    alpha = rnorm(LAM_data$S, 0, 1),
    beta = runif(LAM_data$S, 0.5, 1.5)
  )
}
LAM_model <- jags.model(
  textConnection(LAM_model_string),
  data = LAM_data,
  inits = LAM_inits,
  n.chains = 3,
  n.adapt = 1000
)
# it seems to have worked

# now I'm drawing posterior samples
update(LAM_model, 5000)
# collecting them
LAM_samples <- coda.samples(
  model = LAM_model,
  variable.names = c(
    "alpha",
    "beta",
    "LAM_cm"
  ),
  n.iter = 10000,
  thin = 5
)
summary(LAM_samples)

# Results:
# Species ID 1: 175.57cm (Est. LAM)
# Species ID 2: 166.19cm (Est. LAM)
# Species ID 3: 186.65 cm (Est. LAM)







