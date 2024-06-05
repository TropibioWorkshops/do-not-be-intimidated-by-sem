# Information ------------------------------------------------------------------
# Structural Equation Models (SEM) with PiecewiseSEM
# Script developed by Fernanda Alves-Martins and Javier Martínez-Arribas
# Developed in May, 2024

# Install packages
libs = c("ggplot2","corrplot","car","piecewiseSEM","car","DescTools","lme4","nlme","dplyr", "lavaan")
# install.lib = libs[!libs %in% installed.packages()]
# for(libs in install.lib) install.packages(libs, dependences = TRUE)

# Load libraries ---------------------------------------------------------------
lapply(libs, require, character.only = TRUE)
rm(install.lib, libs)

# Set the working directory ----------------------------------------------------

# EXERCISES --------------------------------------------------------------------

# EXERCISE 1: WITH JAVI

# EXERCISE 2: Correlation
# Hypothetical study on insectivorous bird communities in a tropical forest region

# Import data
df = read.csv("ecolData.csv", sep=";")

# Test for correlation between distance from urban centers and bird diversity
cor.test(df$distance_city, df$bird_diversity, method = "pearson") 

# Plot the correlation
ggplot(data = df, aes(x = distance_city, y = bird_diversity)) + 
  labs(x = "Distance from city (km)", y = "Bird diversity") +
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_text(x = 350, y = 15, label = "r = 0.40, P < 0.001", color = "black", size = 4) +
  geom_smooth(method = lm, se = FALSE, color = "black", linetype = "dashed") +
  theme(legend.position = "none")


# EXERCISE 3: Linear regression assumptions
# Generate two simulated datasets with a linear relationship

# Linear 1
# Parameters
x=rep(1:50,2)
a=0
b = 1
eps = rnorm(x,mean=0,sd=1)
y = a + b*x + eps
y = y - (min(y)-1)
linear1=as.data.frame(cbind(x,y)) #create the dataframe

# Declare the regression model
modReg1 = lm(y ~ x, data = linear1)

# Model diagnosis
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modReg1)

# See model results
summary(modReg1)

# Plot
ggplot(data = linear1, aes(x = x, y = y)) + 
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme(legend.position = "none")


# Linear 2
# Parameters
sigma2 = x^2.5
eps = rnorm(x,mean=0,sd=sqrt(sigma2))
y = a+b*x + eps
y = y - (min(y)-1)
linear2 = as.data.frame(cbind(x,y))#create the dataframe

# Declare the regression model
modReg2 = lm(y ~ x, data = linear2)

# Model diagnosis
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modReg2)

# See model results
summary(modReg2)

# Plot
ggplot(data = linear2, aes(x = x, y = y)) + 
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme(legend.position = "none")

# Response variable transformation
# Declare the regression model
modReg3 = lm(log(y) ~ x, data = linear2)

# Model diagnosis
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modReg3)

# See the results
summary(modReg3)

# Plot
ggplot(data = linear2, aes(x = x, y = log(y))) + 
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme(legend.position = "none")


# Ex.4: Apply a simple linear regression to test whether there is a relationship 
# between the area of the forest patch and bird diversity
# Hypothetical study on insectivorous bird communities in a tropical forest region

# Declare the regression model
modS = lm (bird_diversity ~ area, df) 

# Model diagnosis
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modS)

# See the results
summary(modS)


# Ex.5: Apply a multiple linear regression (because our response variable 
# is continuous) to understand whether bird diversity is explained by the area, 
# tree diversity, arthropod biomass (i.e., food resources), bird nests and 
# predators 
 
# Declare the regression model
modM = lm (bird_diversity ~ tree_diversity + arthr_biomass + bird_nest + predator, df) 

# Model diagnosis
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modM)
vif (modM) #measures multicollinearity among predictors

# See the results
summary(modM)


# Ex.5: Apply a piecewiseSEM to test the hypotheses that the total plant cover 
# depends on fire severity, and that fire severity, in turn, is influenced by 
# the age of the plants (Grace and Keeley, 2006, Ecol App)

# Example available at https://jslefche.github.io/sem_book/index.html 
data(keeley) 

# Especify the models
keeley_psem = psem(
  lm(cover ~ firesev, data = keeley),
  lm(firesev ~ age, data = keeley),
  data = keeley
)

# D-separation: Test of variables independence
basisSet(keeley_psem)

# Interpret models' results
summary(keeley_psem)

# Model diagnosis
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm(cover ~ firesev, data = keeley))

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm(firesev ~ age, data = keeley))

# Now, do a d-separation by hand and compare to the psem summary
modDsep = lm(cover ~ firesev + age, data = keeley)
summary(modDsep)

# Ex.6: Apply a piecewiseSEM to understand the relative importance of multi-scale
# factors on amazonian damselflies' local species richness patterns (part of 
# the dataset from Alves-Martins et al., 2018, Insect Cons Div)

# Import the dataset
df = read.csv("ZygoAMZ.csv", sep=",")

# Especify the models
set.seed(123)
mod = psem(
  glmer(abund ~ temp_season + precip_season + AET + (1 | basin), 
        data = df, family = poisson(link = "log")),
  glmer(loc_rich ~ abund + evi + width + depth + (1 | basin), 
        data = df, family = poisson(link = "log")))

# D-separation: Test of variables independence
basisSet(mod)
summary(mod)

# Model modification
# Add a path between abund and depth
mod1 = psem(
  glmer(abund ~ temp_season + precip_season + AET + depth + (1 | basin), 
        data = df, family = poisson(link = "log")),
  glmer(loc_rich ~ abund + evi + width + depth + (1 | basin), 
        data = df, family = poisson(link = "log")))
summary(mod1)

# Models diagnosis
plot(glmer(abund ~ temp_season + precip_season + AET + depth + (1 | basin), 
           data = df, family = poisson(link = "log")))

plot(glmer(loc_rich ~ abund + evi + width + depth + (1 | basin), 
           data = df, family = poisson(link = "log")))

# Interpret model results
summary(mod1)


# Ex.7: Apply a piecewiseSEM to disentangle the mechanisms underlying 
# insectivorous bird diversity in a tropical forest system 

# Import the dataset
df = read.csv("ecolData.csv", sep=";")

mod = psem(
  lm(tree_diversity ~ distance_city + habitat_type, df),
  lm(arthr_biomass ~ tree_diversity, df),
  lm(bird_diversity ~ arthr_biomass, df)
)
summary(mod)

mod1 = psem(
  lm(tree_diversity ~ distance_city + habitat_type, df),
  lm(arthr_biomass ~ tree_diversity + habitat_type, df),
  lm(bird_diversity ~ arthr_biomass, df)
)
summary(mod1)


mod2 = psem(
  lm(tree_diversity ~ distance_city + habitat_type, df),
  lm(arthr_biomass ~ tree_diversity + habitat_type, df),
  lm(bird_diversity ~ arthr_biomass + habitat_type, df)
)
summary(mod2)

mod3 = psem(
  lm(tree_diversity ~ distance_city + habitat_type, df),
  lm(arthr_biomass ~ tree_diversity + habitat_type, df),
  lm(bird_diversity ~ arthr_biomass + habitat_type + tree_diversity, df)
)
summary(mod3)

# Model diagnosis
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm(tree_diversity ~ distance_city + habitat_type, df))

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm(arthr_biomass ~ tree_diversity + habitat_type, df))

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm(bird_diversity ~ arthr_biomass + habitat_type + tree_diversity, df))

# Interpret model results
# BUG DETECTED: When a model includes a categorical predictor, the psem summary 
# IS NOT RETURNING the "Std.Estimate" for the predictor variables. 


# Ex.8: Apply a piecewiseSEM to Testing the hypothesis that diet, sex, and age 
# of a mammal species indirectly influence offspring through their effect on 
# body mass

# Create the dataset
set.seed(123)

bodyMass = c(sort(sample(140:165, 63, replace = TRUE)+ rnorm(63, mean = 0, sd = 2)),
             sort(sample(170:185, 37, replace = TRUE)+ rnorm(37, mean = 0, sd = 2)))
food = (bodyMass/5) + rnorm(100, mean = 0, sd = 2)
age = sample(2:7, 100, replace = TRUE)
sex = as.factor(c(rep("F",63),rep("M",37)))
offspring = round((bodyMass/20 + rnorm(100, mean = 0, sd = 1))/2)

df = data.frame(
  bodyMass,
  food,
  age,
  sex,
  offspring
)
rm(bodyMass,food,age,sex,offspring)

# 8.1: SEM model with a categorical variable
# Especify the models
mod1 = psem(
  lm(bodyMass ~ food + sex + age, data = df),
  lm(offspring ~ bodyMass, data = df),
  data = df
)

# Interpret models' results
summary(mod1)


# 8.2: SEM model with the categorical variable as interaction term
# Especify the models
mod2 = psem(
  lm(bodyMass ~ food + age*sex, data = df),
  lm(offspring ~ bodyMass, data = df),
  data = df
)

# Interpret models' results
summary(mod2) #When interaction terms are expressed as "age*sex" IT RETURNS AN ERROR!!


# 8.3: SEM model with the categorical variable as interaction term using 
# Multigroup Analysis

# Especify the models
mod3 = psem(
  lm(bodyMass ~ food + age, data = df),
  lm(offspring ~ bodyMass, data = df),
  data = df
)

# Interpret models' results
summary(mod3) 

# Multigroup analysis
multigroup3 = multigroup(mod3, group = "sex")


# Ex. 9: SEM model with the categorical variable as interaction term 
# In this study, Grace & Jutila (1999) investigated the factors controlling plant 
# species density in Finnish meadows. In this worked example, we will consider 
# the effects of elevation and total biomass on plant density, as well as the 
# effect of elevation on biomass (extracted from 
# https://jslefche.github.io/sem_book/multigroup-analysis.html)

data(meadows)

jutila <- psem(
  lm(rich ~ elev + mass, data = meadows),
  lm(mass ~ elev, data = meadows)
)

jutila.multigroup <- multigroup(jutila, group = "grazed")

jutila.multigroup

# From Lefcheck (https://jslefche.github.io/sem_book/multigroup-analysis.html)
# "elev --> rich ath as the only one in which coefficients do not differ 
# among groups (again, denoted by a c next to the output). Thus, in the output, 
# that coefficient is the same between groups; otherwise, the coefficients vary 
# depending on whether the meadows is grazed or ungrazed. Moreover, it seems 
# some of the paths differ in their statistical significance: 
# the rich --> mass is not significant in the grazed meadows, but is 
# significant in the ungrazed meadows"

#  piecewiseSEM output does not return a goodness-of-fit test 
# " Fisher's C = NA with P-value = NA and on 0 degrees of freedom"
# because the model is saturated (i.e., no missing paths)



