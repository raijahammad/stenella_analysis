# 1. load the necessary packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# 2. read the og tables (only doing this bc of the new project)
animal <- read.csv("data/tbl_Animal_2026-05-11.csv")
age <- read.csv("data/tbl_Age_2026-05-11.csv")
reproduction <- read.csv("data/tbl_Reproduction_2026-05-11.csv")
morphology <- read.csv("data/tbl_Morphology_2026-05-11.csv")
species <- read.csv("data/table_species_2026-05-11.csv")

# 3. combining the datasets
data. <- animal %>%
  left_join(age, by = "Specimen") %>%
  left_join(reproduction, by = "Specimen") %>%
  left_join(morphology, by = "Specimen")


# 4. made a vector for SpeciesID and SpCode for all of Stenella
Stenella.Sp.ID <- species %>%
  filter(Genus == "Stenella") %>%
  select(SpeciesID) %>%
  as.vector() %>%
  unlist()

# 5. cleaning the dataset
data.clean <- data. %>%
  mutate(
    isMature_bin = if_else(IsMature == "Y", 1, 0),
    TotalLength = TotalLength_FIELD
  )

# 6. now I'm using the Stenella vector to filter out the animals I don't want
data.clean %>%
  filter(SpeciesID %in% Stenella.Sp.ID) -> Stenella

# 7. joining species and Stenella
Stenella <- Stenella %>%
  left_join(
    species %>% select(SpeciesID, Genus, Species, CommonName),
    by = "SpeciesID"
  )

# 8. individual dataset for the three species
attenuata <- Stenella %>%
  filter(Species == "attenuata")


longirostris <- Stenella %>%
  filter(Species == "longirostris")

coeruleoalba <- Stenella %>%
  filter(Species == "coeruleoalba")


# THE FOLLOWING IS TO FILTER COASTAL ANIMALS

coast <- ne_coastline(
  scale = "medium",
  returnclass = "sf"
)

# Function to add distance to coast and keep offshore animals
make_offshore <- function(species_data) {
  
  species_sf <- species_data %>%
    filter(
      !is.na(Latitude),
      !is.na(Longitude)
    ) %>%
    st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )
  
  dist_to_coast <- st_distance(species_sf, coast)
  
  min_dist <- apply(dist_to_coast, 1, min)
  
  species_sf$dist_to_coast_km <- as.numeric(min_dist) / 1000
  
  species_offshore <- species_sf %>%
    filter(dist_to_coast_km >= 25)
  
  return(species_offshore)
}

# Apply to each species
attenuata_offshore <- make_offshore(attenuata)

longirostris_offshore <- make_offshore(longirostris)

coeruleoalba_offshore <- make_offshore(coeruleoalba)