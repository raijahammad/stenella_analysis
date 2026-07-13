library(dplyr)
library(brms)

# combined all of my birth data for each individual species into one data set
# bind_rows is a function from the dplyr package that i used to combine
# my data frames vertically (so I stacked their rows)
# mutate() is a function used to create, modify, or delete columns in a frame
birth_all.sp <- bind_rows(
  birth_data_att %>% mutate(species = "attenuata"),
  birth_data_long %>% mutate(species = "longirostris"),
  birth_data_coe %>% mutate(species = "coeruleoalba")
)

# species are a categorical variable so factoring them turns them into
# categories; this is because brms expects the grouping variable to be
# categorical
birth_all.sp$species <- factor(birth_all.sp$species)

# making sure everything looks right
str(birth_all.sp)
summary(birth_all.sp)
table(birth_all.sp$species)
table(birth_all.sp$Born)


# now i'm standardizing length
birth_all.sp$Length_z <- scale(birth_all.sp$Length)
# and now i'm checking that it worked
summary(birth_all.sp$Length_z)
mean(birth_all.sp$Length_z)
sd(birth_all.sp$Length_z)

# fitting the hierarchical logistic regression so each species has its own
# slope
library(brms)
birth_model_hier <- brm(
  Born ~ Length_z + (1 | species),
  data = birth_all.sp,
  family = bernoulli(link = "logit"),
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 1234
)

# results:
#> summary(birth_all.sp$Length_z)
#V1         
#Min.   :-1.8357  
#1st Qu.:-0.8523  
#Median : 0.2017  
#Mean   : 0.0000  
#3rd Qu.: 0.8818  
#Max.   : 1.4048  
#> mean(birth_all.sp$Length_z)
#[1] 8.13378e-16
#> sd(birth_all.sp$Length_z)
#[1] 1

# Notes:
# mean is statistically zero

# (1 | species) is the hierarchical portion, each species has its own int

birth_model_hier <- brm(
  Born ~ Length_z + (1 | species),
  data = birth_all.sp,
  family = bernoulli(link = "logit"),
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 1234
)

summary(birth_model_hier)

# results:
#~species (Number of levels: 3) 
#Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#sd(Intercept)     2.17      1.14     0.70     5.16 1.01      748      436

#Regression Coefficients:
#  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept    -3.33      1.35    -5.98    -0.52 1.01      699      491
#Length_z     11.85      0.49    10.92    12.84 1.00     1953     1107

# sd(Intercept) is the sd of the species intercepts
# credible interval is pretty wide, but i'm guessing (hoping) that it's bc
# there are only three species
# our estimate is 11.85 which is perfect. as SL increases, the log-odds of 
# being born incrase dramatically 

# there is a very strong positive relationship between body length and being
# born; species differ in where their birth curves are located

# let's plot
species_conditions <- data.frame(
  species = levels(birth_all.sp$species)
)

birth_effects <- conditional_effects(
  birth_model_hier,
  effects = "Length_z",
  conditions = species_conditions,
  re_formula = NULL
)

plot(
  birth_effects,
  points = TRUE,
  point_args = list(alpha = 0.1)
)

# adding scatter to my plot just to see how it varies
library(ggplot2)

ggplot(birth_all.sp,
       aes(x = as.numeric(Length_z),
           y = Born)) +
  
  geom_jitter(
    width = 0,
    height = 0.05,
    alpha = 0.15,
    size = 1.2
  ) +
  
  stat_smooth(
    method = "glm",
    method.args = list(family = binomial),
    se = FALSE,
    color = "blue",
    linewidth = 1
  ) +
  
  facet_wrap(~ species) +
  
  labs(
    title = "Probability of Birth by Standardized Length",
    x = "Standardized Length",
    y = "Born (0 = No, 1 = Yes)"
  ) +
  
  theme_minimal()

# checking summary again because that's just good practice
pp_check(birth_model_hier)
plot(birth_model_hier)

# now i'm looking at the rate of transition from fetus to neonate for each
birth_model_hier2 <- brm(
  Born ~ Length_z + (1 + Length_z | species),
  data = birth_all.sp,
  family = bernoulli(),
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 1234
)
summary(birth_model_hier2)

# checking up on convergence
plot(birth_model_hier2)
# i'm struggling with convergence; gemini advises to increase adapt_delta to 0.8

#
birth_model_hier2 <- brm(
  Born ~ Length_z + (1 + Length_z | species),
  data = birth_all.sp,
  family = bernoulli(),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  seed = 1234,
  
  control = list(adapt_delta = 0.99)
)

# posterior predictive
pp_check(birth_model_hier2)

# comparing the two models
loo(birth_model_hier, birth_model_hier2)

# refitting
control = list(adapt_delta = 0.99)

birth_model_hier2 <- brm(
  Born ~ Length_z + (1 + Length_z | species),
  data = birth_all.sp,
  family = bernoulli(),
  
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 1234,
  
  control = list(adapt_delta = 0.99)
)


print(hello)

# statistically no difference between JAGS and STAN,
# JAGS is more user friendly
# analysis paralysis is BAD


# estimates for each species using the hyper-distributions
# change z score, same data from LR is fine (look into brms struggles)
# slope should not be the same

# possible differences in intercept (assuming slope is the same), NOT using z
# score
# looking for the estimates
# cmdSTAN something? stop using brms, because that write the STAN code
# then package data (decode STAN code in brms)
# perhaps a way to get the estimates in brms? stick with brms for now
# brms does everything for you

# then compare to logistic regression analyses for each species
# length at maturity
# age @ maturity
# length @ birth


# what i have:
# individual logistic regression for 3 spp

# what i need:
# hierarchical estimates for 3 spp

# start thinking about what else needs to go into poster (maturity, neonates)
# post stat analysis, bring back to biology of the species







