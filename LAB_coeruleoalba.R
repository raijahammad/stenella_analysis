# hard coding so neonates are classified as individuals under 125cm
NEONATE_MAX_LENGTH <- 125

# making a fetal dataset
birth_fetus_coe <- coeruleoalba %>%
  filter(!is.na(FetusLength_Standard)) %>%
  transmute(
    Length = FetusLength_Standard,
    Born = 0
  )
# making a neonate dataset
birth_neonate_coe <- coeruleoalba %>%
  filter(
    !is.na(TotalLength),
    TotalLength < NEONATE_MAX_LENGTH
  ) %>%
  transmute(
    Length = TotalLength,
    Born = 1
  )
# now combining the two
birth_data_coe <- bind_rows(birth_fetus_coe, birth_neonate_coe)

# checking over results
table(birth_data_coe$Born)
summary(birth_data_coe$Length)


# regular logistic regression
birth_model_coe <- glm(
  Born ~ Length,
  family = binomial,
  data = birth_data_coe
)
summary(birth_model_coe)

# Bayesian logistic regression
bayes_birth_model_coe <- brm(
  Born ~ Length,
  data = birth_data_coe,
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
summary(bayes_birth_model_coe)

# now estimate length at birth!
length_at_birth_coe <- -coef(birth_model_coe)[1] / coef(birth_model_coe)[2]

length_at_birth_coe
# estimated length at birth is 89.51658 

# let's visualize
library(ggplot2)

ggplot(birth_data_coe, aes(x = Length, y = Born)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  stat_smooth(
    method = "glm",
    method.args = list(family = binomial),
    se = TRUE
  ) +
  labs(
    title = "Estimated Length at Birth: S. coeruleoalba",
    x = "Length",
    y = "Probability Born"
  ) +
  theme_minimal()

# Results for Summary:
#Call:
#  glm(formula = Born ~ Length, family = binomial, data = birth_data_coe)
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)  -1125.90  299040.61  -0.004    0.997
#Length          12.58    3336.49   0.004    0.997

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 5.2925e+01  on 39  degrees of freedom
#Residual deviance: 2.9132e-08  on 38  degrees of freedom
#AIC: 4


# My Notes:
# slope is 12.58, so for every 1cm increase in length, the odds
# a being a born dolphin increases by 12.58?? probably not true...
# as length increases, the model predicts a higher probably of being born.

# p-value is 0.997
# Deviance:
# null: 5.2925e+01
# residual: 2.9132e-08

# the sample size for coeruleoalba is super small (40) so results are a little
# wonky
# SE is insanely high (for length = 3336.49 and for intercept = 299040.61)