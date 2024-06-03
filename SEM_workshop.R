install.packages("lavaan", dependencies=TRUE)

# For piecewiseSEM you need to install before:
# ‘car’, ‘DiagrammeR’, ‘emmeans’, ‘igraph’, ‘lme4’, ‘multcomp’, ‘MuMIn’, ‘performance’ 
install.packages("car")
install.packages("DiagrammeR")
install.packages("emmeans")
install.packages("igraph")
install.packages("lme4")
install.packages("multcomp")
install.packages("MuMIn")
install.packages("performance")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("tibble")


install.packages("piecewiseSEM")

library(lavaan)
library(piecewiseSEM)
library(ggplot2)
library(reshape2)
library(tibble)
library(foreign)


# Plant the seed
set.seed(123)  
n <- 100

# Data generation
wolves <- rnorm(n, mean = 10, sd = 3)  # Wolves
ungulates <- 150 - 10 * wolves + rnorm(n, mean = 0, sd = 20)  # Herbivores
plants <- 50 + 5 * wolves - 2 * ungulates + rnorm(n, mean = 0, sd = 5)  # Vegetation-Cover
#altitude <- rnorm(n, mean = 2000, sd = 500)
data <- tibble(
                wolves = round(wolves),
                herbivores = round(herbivores),
                cover = round(cover),
                altitude = round(altitude)
                )

head(data)

round(cor(data),2)

# Multiple linear regresion
m1 <- lm(plants ~ ungulates + wolves, data)
summary(m1)

plot(m1)

# Path analysis
model <-'plants ~ 1 + ungulates + wolves'
m2 <- sem(model,data=data)
summary(m2,fit.measures=TRUE)

#We explain the distinct variances and covariances of the three observed variables 
#by the same number of parameters in the model.

# Mediation model
model <- '

# Direct path
  plants ~ c*wolves
# Path through mediator: Plants
  ungulates ~ a * wolves
  plants ~ b * ungulates
# Indirect effect (a*b)
  indirect := a*b
# Total effect
  total := indirect + c
'
m3 <- sem(model, data=data, std.lv=TRUE)
summary(m3,fit.measures=TRUE, standardized=TRUE)


#### CFA (Confirmatory factor analysis) ####

set.seed(123)  
n <- 1000

# Latent variables - not in dataset
maths_skills <- rnorm(n, mean = 50, sd = 10)
language_skills <- rnorm(n, mean = 60, sd = 8)
science_skills <- rnorm(n, mean = 55, sd = 12)

# General observations
maths_obs <- maths_skills + rnorm(n, mean = 0, sd = 5)
language_obs <- language_skills + rnorm(n, mean = 0, sd = 4)
science_obs <- science_skills + rnorm(n, mean = 0, sd = 6)

# Dataset
data_cfa <- tibble(
  
  algebra = round(maths_obs, 2),
  calculus = round(maths_obs + rnorm(n, mean = 0, sd = 3), 2),
  geometry = round(maths_obs + rnorm(n, mean = 0, sd = 2), 2),

  reading = round(language_obs, 2),
  writing = round(language_obs + rnorm(n, mean = 0, sd = 2.5), 2),
  listening = round(language_obs + rnorm(n, mean = 0, sd = 3), 2),
  
)

round(cor(data_cfa),2)

melted_corr_mat <- melt(corr_mat)

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value))  + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), 
            color = "black", size = 4)

model_desc <- '
  
  maths_skills =~ algebra + calculus + geometry
  language_skills =~ reading + writing + listening

'

m4 = cfa(model_desc, data=data_cfa, std.lv=TRUE)
summary(m4, fit.measures=TRUE, standardized=TRUE)



#import data
dat <- read.spss("https://stats.idre.ucla.edu/wp-content/uploads/2018/05/SAQ.sav",to.data.frame=TRUE, use.value.labels = FALSE)

#covariance of Items 3 to 5
round(cov(dat[,3:5]),2)

#one factor three items, default marker method
m1a  <- ' f  =~ q03 + q04 + q05'
onefac3items_a <- cfa(m1a, data=dat) 
summary(onefac3items_a) 

#one factor three items, variance std 
m1b  <- ' f =~ NA*q03 + q04 + q05
          f ~~ 1*f ' 
onefac3items_b <- cfa(m1b, data=dat) 
summary(onefac3items_b)

#alternative model to variance standardization 
onefac3items_a <- cfa(m1a, data=dat,std.lv=TRUE)
summary(onefac3items_a)

#obtain standardized loadings
summary(onefac3items_a,standardized=TRUE)

#alternative condensed standardized solutions
standardizedsolution(onefac3items_a)

#obtain implied variance covariance matrix 
inspect(onefac3items_a,"cov.ov")

#one factor three items, with means 
m1c <- ' f =~ q03 + q04 + q05 
              q03 ~ 1 
              q04 ~ 1 
              q05 ~ 1' 
onefac3items_c <- cfa(m1c, data=dat) 
summary(onefac3items_c)

#error with default two items
m2a <- 'f1 =~ q03 + q04' 
onefac2items <- cfa(m2a, data=dat)
summary(onefac2items)

#one factor, two items (var std) 
m2b <- 'f1 =~ a*q04 + a*q05' 
onefac2items_b <- cfa(m2b, data=dat,std.lv=TRUE) 
summary(onefac2items_b)

#one factor eight items, variance std 
m3a <- 'f =~ q01 + q02 + q03 + q04 + q05 + q06 + q07 + q08' 
onefac8items_a <- cfa(m3a, data=dat,std.lv=TRUE) 
summary(onefac8items_a, fit.measures=TRUE, standardized=TRUE)

#covariance of Items 1 through 8 
round(cor(dat[,1:8]),2)

#fit statistics 
summary(onefac8items_a, fit.measures=TRUE, standardized=TRUE)

#model chi-square 
pchisq(q=554.191,df=20,lower.tail=FALSE)

#baseline model
b1 <- ' q01 ~~ q01 
        q02 ~~ q02 
        q03 ~~ q03 
        q04 ~~ q04 
        q05 ~~ q05 
        q06 ~~ q06 
        q07 ~~ q07 
        q08 ~~ q08' 

basemodel <- cfa(b1, data=dat)
summary(basemodel)

#uncorrelated two factor solution, var std method
m4a <- 'f1 =~ q01+ q03 + q04 + q05 + q08 
        f2 =~ a*q06 + a*q07 
        f1 ~~ 0*f2 ' 
twofac7items_a <- cfa(m4a, data=dat,std.lv=TRUE)

#alternative syntax - uncorrelated two factor solution, var std method
twofac7items_a <- cfa(m4a, data=dat,std.lv=TRUE, auto.cov.lv.x=FALSE) 
summary(twofac7items_a, fit.measures=TRUE,standardized=TRUE)

#uncorrelated two factor solution, marker method
m4b <- 'f1 =~ q01+ q03 + q04 + q05 + q08 
        f2 =~ q06 + q07' 
twofac7items_b <- cfa(m4b, data=dat,std.lv=TRUE) 
summary(twofac7items_b,fit.measures=TRUE,standardized=TRUE)

#second order three factor solution, marker method
m5a <- 'f1 =~ q01+ q03 + q04 + q05 + q08 
        f2 =~ q06 + q07 
        f3 =~ 1*f1 + 1*f2 
        f3 ~~ f3' 
secondorder <- cfa(m5a, data=dat) 
summary(secondorder,fit.measures=TRUE,standardized=TRUE)

#second order three factor solution, var std method
m5b <- 'f1 =~ NA*q01+ q03 + q04 + q05 + q08 
        f2 =~ NA*q06 + q07 
        f3 =~ NA*f1 + equal("f3=~f1")*f2 
        f1 ~~ 1*f1 
        f2 ~~ 1*f2 
        f3 ~~ 1*f3' 
secondorder <- cfa(m5b, data=dat) 
summary(secondorder,fit.measures=TRUE)

#obtain the parameter table of the second order factor
inspect(secondorder,"partable")


