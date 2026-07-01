# Load the necessary packages
library(tidyverse)
install.packages("brms")
library(brms)

# Clean maturity values
stenella_clean <- Stenella %>%
  filter(
    IsMature %in% c("Y", "N"),
    !is.na(Age),
    !is.na(TotalLength_FIELD)
  ) %>%
  mutate(
    isMature_bin = if_else(IsMature == "Y", 1, 0)
  )

# Regular logistic regression
glm_age_att <- glm(
  isMature_bin ~ Age,
  family = binomial,
  data = #speciesofinterest
)

glm_length_att <- glm(
  isMature_bin ~ TotalLength_FIELD,
  family = binomial,
  data = #speciesofinterest
)

# Bayesian logistic regression
bayes_age_att <- brm(
  isMature_bin ~ Age,
  family = bernoulli(),
  data = #speciesofinterest
)

bayes_length_att <- brm(
  isMature_bin ~ TotalLength_FIELD,
  family = bernoulli(),
  data = #speciesofinterest
)


# check things over
summary(glm_age_att)
summary(glm_length_att)

# Length Logistic Curve Plot
ggplot(attenuata, aes(x = TotalLength_FIELD, y = isMature_bin)) +
  geom_jitter(height = 0.05, alpha = 0.2) +
  stat_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    color = "blue"
  ) +
  labs(
    x = "Total Length (Field)",
    y = "Probability of Maturity",
    title = "Length at Maturity"
  ) +
  theme_classic()


# Length Logistic Curve Plot
ggplot(attenuata, aes(x = Age, y = isMature_bin)) +
  geom_jitter(height = 0.05, alpha = 0.2) +
  stat_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    color = "blue"
  ) +
  labs(
    x = "Age",
    y = "Probability of Maturity",
    title = "Age at Maturity"
  ) +
  theme_classic()
