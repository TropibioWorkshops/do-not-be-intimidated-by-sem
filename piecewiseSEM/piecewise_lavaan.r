# Information ------------------------------------------------------------------
# Structural Equation Models (SEM) with PiecewiseSEM and Lavaan
# Script developed by Fernanda Alves-Martins and Javier Martínez-Arribas
# Developed in June, 2024

# Install packages
libs = c("piecewiseSEM","lavaan","ggplot2","car","lme4","dplyr")
# install.lib = libs[!libs %in% installed.packages()]
# for(libs in install.lib) install.packages(libs, dependences = TRUE)

# Load libraries ---------------------------------------------------------------
lapply(libs, require, character.only = TRUE)
rm(install.lib, libs)

# Ex.1: Apply a SEM to test the hypotheses that the total plant cover 
# depends on fire severity, and that fire severity, in turn, is influenced by 
# the age of the plants (Grace and Keeley, 2006, Ecol App)

# Example available at https://jslefche.github.io/sem_book/index.html 
data(keeley) 

# WITH PiecewiseSEM
# Especify the models
keeley_psem = psem(
  lm(cover ~ firesev, data = keeley),
  lm(firesev ~ age, data = keeley),
  data = keeley
)

# D-separation and SEM's results
summary(keeley_psem)

# WITH Lavaan
# Especify the models
keeley_lav = '
cover ~ firesev
firesev ~ age
'
# Fit SEM's 
keeley_sem = sem(keeley_lav, data = keeley)

# SEM's results
summary(keeley_sem, standardized = T)




# Ex.2: Apply a SEM to disentangle the mechanisms underlying 
# insectivorous bird diversity in a tropical forest system 

# Import the dataset
df = read.csv("ecolData.csv", sep=";")

# WITH PiecewiseSEM
# Especify the models
mod = psem(
  lm(tree_diversity ~ distance_city + habitat_type, df),
  lm(arthr_biomass ~ tree_diversity, df),
  lm(bird_diversity ~ arthr_biomass, df)
)
summary(mod)

# Modify the models
mod1 = psem(
  lm(tree_diversity ~ distance_city + habitat_type, df),
  lm(arthr_biomass ~ tree_diversity + habitat_type, df),
  lm(bird_diversity ~ arthr_biomass, df)
)
summary(mod1)

# Modify the models
mod2 = psem(
  lm(tree_diversity ~ distance_city + habitat_type, df),
  lm(arthr_biomass ~ tree_diversity + habitat_type, df),
  lm(bird_diversity ~ arthr_biomass + habitat_type, df)
)
summary(mod2)

# Modify the models
mod3 = psem(
  lm(tree_diversity ~ distance_city + habitat_type, df),
  lm(arthr_biomass ~ tree_diversity + habitat_type, df),
  lm(bird_diversity ~ arthr_biomass + habitat_type + tree_diversity, df)
)
summary(mod3)

# WITH Lavaan
# Especify the models
mod_lav = '
tree_diversity ~ distance_city + habitat_type
arthr_biomass ~ tree_diversity
bird_diversity ~ arthr_biomass
'
# Fit SEM 
mod_sem = sem(mod_lav, data = df)

# SEM results
summary(mod_sem, standardized = T)
modindices(mod_sem, sort. = T)


# Modify the models
mod_lav1 = '
tree_diversity ~ distance_city + habitat_type
arthr_biomass ~ tree_diversity + habitat_type
bird_diversity ~ arthr_biomass
'

# Fit SEM
mod_sem1 = sem(mod_lav1, data = df)

# SEM's results
summary(mod_sem1, standardized = T)
modindices(mod_sem1, sort. = T)

# Modify the models
mod_lav2 = '
tree_diversity ~ distance_city + habitat_type
arthr_biomass ~ tree_diversity + habitat_type
bird_diversity ~ arthr_biomass + tree_diversity
'

# Fit SEM
mod_sem2 = sem(mod_lav2, data = df)

# SEM results
summary(mod_sem2, standardized = T)
modindices(mod_sem2, sort. = T)
