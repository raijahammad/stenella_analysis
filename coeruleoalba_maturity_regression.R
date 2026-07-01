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
glm_age_coe <- glm(
  isMature_bin ~ Age,
  family = binomial,
  data = coeruleoalba
)

glm_length_coe <- glm(
  isMature_bin ~ TotalLength_FIELD,
  family = binomial,
  data = coeruleoalba
)

# Bayesian logistic regression
bayes_age_coe <- brm(
  isMature_bin ~ Age,
  family = bernoulli(),
  data = coeruleoalba
)

bayes_length_coe <- brm(
  isMature_bin ~ TotalLength_FIELD,
  family = bernoulli(),
  data = coeruleoalba
)


# check things over
summary(glm_age_coe)
summary(glm_length_coe)

# Length Logistic Curve Plot
ggplot(coeruleoalba, aes(x = TotalLength_FIELD, y = isMature_bin)) +
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
ggplot(coeruleoalba, aes(x = Age, y = isMature_bin)) +
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

# NOTE: coeruleoalba models are based on only 48 dolphins for age
# and 110 dolphins for length, longirostris & attenuata are based on thousands
# results from these models are less stable

# Results for Age at Maturity
#Call:
#  glm(formula = isMature_bin ~ Age, family = binomial, data = coeruleoalba)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)   -7.005      2.366  -2.961  0.00307 **
#  Age            1.000      0.307   3.258  0.00112 **
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 66.459  on 47  degrees of freedom
#Residual deviance: 18.826  on 46  degrees of freedom
#(309 observations deleted due to missingness)
#AIC: 22.826

  # My Notes:
  # Coefficient/Estimate is 1.000 which could be misleading...is this due to 
  # sample size as I suspect or is the biology of coeruleoalba truly different?
  # Intercept SE is 2.37; Age SE is 0.307 which is much larger than attenuata's
  # 95% CI is roughly 0.40 to 1.60 (using 1.000 +/- 1.96(0.307)), the lower end
  # of that interval (0.40) is very close to attenuata's estimate of 0.418, so
  # this is likely because of sample size 
  
  # p-value is 0.00112, which is a tad high relative to the other two species
  # but still statistically significant
  # Deviance:
  # null = 66.459
  # residual = 18.826
  
  
  
  
# Results for Length at Maturity
#Call:
#  glm(formula = isMature_bin ~ TotalLength_FIELD, family = binomial, 
 #     data = coeruleoalba)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -35.0549     7.0381  -4.981 6.33e-07 ***
#  TotalLength_FIELD   0.1837     0.0366   5.020 5.16e-07 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 151.910  on 109  degrees of freedom
#Residual deviance:  87.747  on 108  degrees of freedom
#(247 observations deleted due to missingness)
#AIC: 91.747
  
  # My Notes:
  # Length results are more normal because sample size is twice as much than Age
  # Coefficient/Estimate is 0.1837 which 
  # Intercept SE is 7.04; Length SE is 0.036
  # p-value is 5.16e-07, which makes more sense & aligns more closely to the
  # other two species
  # Deviance:
  # null = 151.910
  # residual = 87.747
  
  
  
# because of these strange results, I'm going to look at the distribution of
# mature vs. immature dolphins
table(coeruleoalba$isMature_bin)
# the logistic regression didn't use all 128 specimens because only 48 had age
table(coeruleoalba$Age, coeruleoalba$isMature_bin)
# from about 0 to 6.5 years, every dolphin is immature until 6.6 when we get
# the first mature dolphin -- so there's a very stark curve, which explains our
# steep slope (1.000)

# ALSO NOTE:
# there are two old yet immature dolphins that interest me
# Age = 7.8 & Maturity = N
# Age = 11.0 & Maturity = N
# they have a notable influence on the fitted curve because of the small sample
# size. Possible explanations? Not sure yet. Perhaps human error?
# Or genuinely late-maturing individuals? Meaning natural variation?




