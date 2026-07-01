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
  data = attenuata
)

glm_length_att <- glm(
  isMature_bin ~ TotalLength_FIELD,
  family = binomial,
  data = attenuata
)

# Bayesian logistic regression
bayes_age_att <- brm(
  isMature_bin ~ Age,
  family = bernoulli(),
  data = attenuata
)

bayes_length_att <- brm(
  isMature_bin ~ TotalLength_FIELD,
  family = bernoulli(),
  data = attenuata
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

# NOTES:

# Age at Maturity:
#Call:
#  glm(formula = isMature_bin ~ Age, family = binomial, data = attenuata)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -3.15241    0.13094  -24.07   <2e-16 ***
#  Age          0.42362    0.01469   28.84   <2e-16 ***
  ---
  #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  # (Dispersion parameter for binomial family taken to be 1)

  #Null deviance: 5943.8  on 4912  degrees of freedom
  #Residual deviance: 4075.9  on 4911  degrees of freedom
  #(23558 observations deleted due to missingness)
  #AIC: 4079.9
  
# the fitted equation is logit(p) = -3.152 + 0.424 x Age where p is the
# probability that the dolphin is mature
# Age coefficient (0.424) is positive which is expected. As age increases,
# the likelihood that a dolphin is mature also increases.
# p-value = Pr(>|z|) < 2e-16 ; this value is extremely small, meaning that
# age is a strong predictor of maturity
# Residual deviance:
# null deviance = 5943.8
# residual deviance = 4075.9, the model reduced the deviance quite a bit, so
# age explains a substantial amount of the variation in maturity



# Length at Maturity:
Call:
  glm(formula = isMature_bin ~ TotalLength_FIELD, family = binomial, 
      data = attenuata)

#Coefficients:
  #Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -42.841119   0.862777  -49.66   <2e-16 ***
#  TotalLength_FIELD   0.244173   0.004836   50.49   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 14114.3  on 11629  degrees of freedom
#Residual deviance:  6284.6  on 11628  degrees of freedom
#(16841 observations deleted due to missingness)
#AIC: 6288.6
  
# (Coefficients) TotalLength = 0.244173, which is positive. Longer dolphins are
# more likely to be mature.
# p-value = <2e-16, which is very significant. 
# Deviance:
# null deviance = 1411.4
# residual deviance = 6284.6
# note that 16841 observations were deleted due to missingness


# PRE-EDIT NOTES (CHANGED TotalLength TO TotalLength_FIELD):
# What's weird is that sample size for Length and Age differ a LOT
sum(!is.na(attenuata$Age))
sum(!is.na(attenuata$TotalLength))
# Age is 7947
# TotalLength is 265
# Age model used more because many dolphins have Age & Y/N Maturity
# Length model used a smaller amount bc less dolphins have TL & Y/N Maturity

names(attenuata)[grep("Length", names(attenuata))]
sum(!is.na(attenuata$TotalLength))
sum(!is.na(attenuata$TotalLength_FIELD))
sum(!is.na(attenuata$TotalLength_LAB))
# OK so TotalLength_FIELD has way more values, I'm going to use that.
