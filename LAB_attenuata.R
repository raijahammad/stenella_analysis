# hard coding so neonates are classified as individuals under 125cm
NEONATE_MAX_LENGTH <- 125

# making a fetal dataset
birth_fetus_att <- attenuata %>%
  filter(!is.na(FetusLength_Standard)) %>%
  transmute(
    Length = FetusLength_Standard,
    Born = 0
  )
# making a neonate dataset
birth_neonate_att <- attenuata %>%
  filter(
    !is.na(TotalLength),
    TotalLength < NEONATE_MAX_LENGTH
  ) %>%
  transmute(
    Length = TotalLength,
    Born = 1
  )
# now combining the two
birth_data_att <- bind_rows(birth_fetus_att, birth_neonate_att)

# checking over results
table(birth_data_att$Born)
summary(birth_data_att$Length)


# this is the model itself
birth_model_att <- glm(
  Born ~ Length,
  family = binomial,
  data = birth_data_att
)

# checking things out
summary(birth_model_att)

# now estimate length at birth!
length_at_birth_att <- -coef(birth_model_att)[1] / coef(birth_model_att)[2]

length_at_birth_att
# estimated length at birth is 83.03021cm

# let's visualize:
library(ggplot2)

ggplot(birth_data_att, aes(x = Length, y = Born)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  stat_smooth(
    method = "glm",
    method.args = list(family = binomial),
    se = TRUE
  ) +
  labs(
    title = "Estimated Length at Birth: S. attenuata",
    x = "Length",
    y = "Probability Born"
  ) +
  theme_minimal()


# Results for Summary:
#Call:
#  glm(formula = Born ~ Length, family = binomial, data = birth_data_att)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -26.3254     1.2528  -21.01   <2e-16 ***
#  Length        0.3171     0.0150   21.14   <2e-16 ***
  ---
  #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  #(Dispersion parameter for binomial family taken to be 1)

  #Null deviance: 6254.8  on 4564  degrees of freedom
  #Residual deviance: 1022.8  on 4563  degrees of freedom
  #AIC: 1026.8

# My Notes:
# slope is 0.3171, so for every 1cm increase in length, the odds
  # a being a born dolphin increases by 0.3171
  # as length increases, the model predicts a higher probably of being born.
  
 # p-value is <2e-16 so length is statistically significant as a predictor
# Deviance:
  # null: 6254.8
  # residual: 1022.8
  