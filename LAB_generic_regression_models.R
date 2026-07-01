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


# this is the model itself
birth_model <- glm(
  Born ~ Length,
  family = binomial,
  data = birth_data
)

# checking things out
summary(birth_model)

# now estimate length at birth!
length_at_birth <- -coef(birth_model)[1] / coef(birth_model)[2]

length_at_birth
