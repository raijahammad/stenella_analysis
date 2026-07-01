# hard coding so neonates are classified as individuals under 125cm
NEONATE_MAX_LENGTH <- 125

# making a fetal dataset
birth_fetus_long <- longirostris %>%
  filter(!is.na(FetusLength_Standard)) %>%
  transmute(
    Length = FetusLength_Standard,
    Born = 0
  )
# making a neonate dataset
birth_neonate_long <- longirostris %>%
  filter(
    !is.na(TotalLength),
    TotalLength < NEONATE_MAX_LENGTH
  ) %>%
  transmute(
    Length = TotalLength,
    Born = 1
  )
# now combining the two
birth_data_long <- bind_rows(birth_fetus_long, birth_neonate_long)

# checking over results
table(birth_data_long$Born)
summary(birth_data_long$Length)


# regular logistic regression
birth_model_long <- glm(
  Born ~ Length,
  family = binomial,
  data = birth_data_long
)
summary(birth_model_long)

# Bayesian logistic regression
bayes_birth_model_long <- brm(
  Born ~ Length,
  data = birth_data_long,
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
summary(bayes_birth_model_long)

# now estimate length at birth!
length_at_birth_long <- -coef(birth_model_long)[1] / coef(birth_model_long)[2]

length_at_birth_long
# estimated LAB for longirostris is 77.87058

# let's visualize
library(ggplot2)

ggplot(birth_data_long, aes(x = Length, y = Born)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  stat_smooth(
    method = "glm",
    method.args = list(family = binomial),
    se = TRUE
  ) +
  labs(
    title = "Estimated Length at Birth: S. longirostris",
    x = "Length",
    y = "Probability Born"
  ) +
  theme_minimal()




#Results:
#Call:
#  glm(formula = Born ~ Length, family = binomial, data = birth_data_long)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -22.59808    1.80883  -12.49   <2e-16 ***
#  Length        0.29020    0.02317   12.52   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 2626.88  on 1895  degrees of freedom
#Residual deviance:  382.84  on 1894  degrees of freedom
#AIC: 386.84


# My Notes:
# slope is 0.29020, so for every 1cm increase in length, the odds
# a being a born dolphin increases by 0.29020
# as length increases, the model predicts a higher probably of being born.

# p-value is <2e-16 so length is statistically significant as a predictor
# Deviance:
# null: 2626.88
# residual: 382.84
