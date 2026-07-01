# install necessary packages
install.packages("brms")
library(brms)

# hard coding so neonates are classified as individuals under 125cm
NEONATE_MAX_LENGTH <- 125

# making a fetal dataset
birth_fetus <- dolphin %>%
  filter(!is.na(FetusLength_Standard)) %>%
  transmute(
    Length = FetusLength_Standard,
    Born = 0
  )
# making a neonate dataset
birth_neonate <- dolphin %>%
  filter(
    !is.na(TotalLength),
    TotalLength < NEONATE_MAX_LENGTH
  ) %>%
  transmute(
    Length = TotalLength,
    Born = 1
  )
# now combining the two
birth_data <- bind_rows(birth_fetus, birth_neonate)

# checking over results
table(birth_data$Born)
summary(birth_data$Length)


# frequentist logistic regression
birth_model <- glm(
  Born ~ Length,
  family = binomial,
  data = birth_data
)
summary(birth_model)

# Bayesian logistic regression
bayes_birth_model <- brm(
  Born ~ Length,
  data = birth_data,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(normal(0, 2), class = "b")
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)
summary(bayes_birth_model)

# now estimate length at birth!
length_at_birth <- -coef(birth_model)[1] / coef(birth_model)[2]

length_at_birth
