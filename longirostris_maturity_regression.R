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
glm_age_long <- glm(
  isMature_bin ~ Age,
  family = binomial,
  data = longirostris
)

glm_length_long <- glm(
  isMature_bin ~ TotalLength_FIELD,
  family = binomial,
  data = longirostris
)

# Bayesian logistic regression
bayes_age_long <- brm(
  isMature_bin ~ Age,
  family = bernoulli(),
  data = longirostris
)

bayes_length_long <- brm(
  isMature_bin ~ TotalLength_FIELD,
  family = bernoulli(),
  data = longirostris
)


# check things over
summary(glm_age_long)
summary(glm_length_long)

# Length Logistic Curve Plot
ggplot(longirostris, aes(x = TotalLength_FIELD, y = isMature_bin)) +
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


# Age Logistic Curve Plot
ggplot(longirostris, aes(x = Age, y = isMature_bin)) +
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


# Results for Age at Maturity:
#Call:
#  glm(formula = isMature_bin ~ Age, family = binomial, data = longirostris)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -4.68937    0.17736  -26.44   <2e-16 ***
#  Age          0.63747    0.02294   27.79   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 4450.9  on 3289  degrees of freedom
#Residual deviance: 2448.4  on 3288  degrees of freedom
#(10906 observations deleted due to missingness)
#AIC: 2452.4

#Number of Fisher Scoring iterations: 6

# My Notes:
# Coefficient is 0.63747 which is nice.
# p-value is <2e-16
# Deviance:
  # null = 4450.9
  # residual = 2448.4
  


# Results for Length at Maturity:
#Call:
#  glm(formula = isMature_bin ~ TotalLength_FIELD, family = binomial, 
#      data = longirostris)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -29.200934   0.856158  -34.11   <2e-16 ***
#  TotalLength_FIELD   0.175298   0.005077   34.53   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 7382.5  on 5445  degrees of freedom
#Residual deviance: 5135.1  on 5444  degrees of freedom
#(8750 observations deleted due to missingness)
  
  # My Notes:
  # Coefficient is 0.175298 which is also nice.
  # p-value is <2e-16
  # Deviance:
  # null = 7382.5
  # residual = 5135.1