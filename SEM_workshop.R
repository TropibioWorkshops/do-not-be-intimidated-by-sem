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
library(MASS)


# Plant the seed
set.seed(123)  
n <- 1000

# Data generation
wolves <- rnorm(n, mean = 10, sd = 3)  # Wolves
ungulates <- 150 - 10 * wolves + rnorm(n, mean = 0, sd = 20)  # Herbivores
plants <- 50 + 5 * wolves - 2 * ungulates + rnorm(n, mean = 0, sd = 5)  # Vegetation-Cover
#altitude <- rnorm(n, mean = 2000, sd = 500)
data <- tibble(
                wolves = round(wolves),
                ungulates = round(ungulates),
                plants = round(plants)
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
# Path through mediator: Ungulates
  ungulates ~ a * wolves
  plants ~ b * ungulates
# Indirect effect (a*b)
  indirect := a*b
# Total effect
  total := indirect + c
'
m3 <- sem(model, data=data, std.lv=TRUE)
summary(m3,fit.measures=TRUE, standardized=TRUE)
semPlot::semPaths(m3)

#### CFA (Confirmatory factor analysis) ####

set.seed(123)
n <- 100

# Generate dataset
mean <- rep(0, 9)
cov_matrix <- matrix(c(1, 0.9, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                       0.9, 1, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                       0.9, 0.9, 1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                       0.1, 0.1, 0.1, 1, 0.9, 0.9, 0.1, 0.1, 0.1,
                       0.1, 0.1, 0.1, 0.9, 1, 0.9, 0.1, 0.1, 0.1,
                       0.1, 0.1, 0.1, 0.9, 0.9, 1, 0.1, 0.1, 0.1,
                       0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 1, 0.9, 0.9,
                       0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 1, 0.9,
                       0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.9, 1), 
                     nrow = 9)

data <- mvrnorm(n, mean, cov_matrix)

scale_and_translate <- function(x) {
  scaled_x <- round((x - min(x)) / (max(x) - min(x)) * 100,0)
  return(scaled_x)
}

data_cfa <- as.data.frame(apply(data, 2, scale_and_translate))

names(data_cfa) <- c("algebra", "calculus", "geometry", "reading", "writing", "listening", 
                     "computing", "statistics", "papers_published")


corr_mat <- round(cor(data_cfa),2)

melted_corr_mat <- melt(corr_mat)

ggplot(melt(data_cfa),aes(value,fill=variable))+
  geom_density(alpha=0.6)+ggtitle('Plot 2')

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value))  + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), 
            color = "black", size = 4)

model_desc <- '
  
  maths_skills =~ algebra + calculus + geometry
  language_skills =~ reading + writing + listening
  science_skills =~ computing + statistics + papers_published
'

m4 = cfa(model_desc, data=data_cfa, std.lv=TRUE)
summary(m4, fit.measures=TRUE, standardized=TRUE)
semPlot::semPaths(m4, "std")


# Example 2
#import data
dat <- read.spss("https://stats.idre.ucla.edu/wp-content/uploads/2018/05/SAQ.sav",to.data.frame=TRUE, use.value.labels = FALSE)

#covariance of Items 3 to 5
round(cov(dat[,3:5]),2)

#one factor three items, default marker method
m1a  <- ' f  =~ q03 + q04 + q05'
onefac3items_a <- cfa(m1a, data=dat) 
summary(onefac3items_a) 
semPlot::semPaths(onefac3items_a, "std")

#one factor three items, variance std 
#NA*q03 frees the loading of the first item
m1b  <- ' f =~ NA*q03 + q04 + q05
          f ~~ 1*f ' 
onefac3items_b <- cfa(m1b, data=dat) 
summary(onefac3items_b)
semPlot::semPaths(onefac3items_b, "std")

#alternative model to variance standardization 
#using std.lv=TRUE
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
semPlot::semPaths(onefac3items_c, "std")

#error with default two items
m2a <- 'f1 =~ q03 + q04' 
onefac2items <- cfa(m2a, data=dat)
summary(onefac2items)
#One solution is to use the variance standardization method, which fixes 
#the variance of the factor to one, and equate the second loading to equal the first loading

#one factor, two items (var std) 
m2b <- 'f1 =~ a*q04 + a*q05' 
onefac2items_b <- cfa(m2b, data=dat,std.lv=TRUE) 
summary(onefac2items_b)
semPlot::semPaths(onefac2items_b, "std")

#one factor eight items, variance std 
m3a <- 'f =~ q01 + q02 + q03 + q04 + q05 + q06 + q07 + q08' 
onefac8items_a <- cfa(m3a, data=dat,std.lv=TRUE) 
summary(onefac8items_a, fit.measures=TRUE, standardized=TRUE)
semPlot::semPaths(onefac8items_a, "std")

#covariance of Items 1 through 8 
round(cor(dat[,1:8]),2)
#Item 2 loads the weakest onto our SPSS Anxiety factor at -0.23 and Item 4 loads 
#the highest at 0.67. Items 2 and 3 also load in a negative direction compared to the other items

#fit statistics 
summary(onefac8items_a, fit.measures=TRUE, standardized=TRUE)

#model chi-square 
pchisq(q=554.191,df=20,lower.tail=FALSE)

#The sample size should be determined by the number of 
#parameters in your model, and the recommended ratio is 20:1
#This means that if you have 10 parameters, you should have n=200. 
#A sample size less than 100 is almost always untenable


#Uncorrelated two factor solution, var std method
#tWe assume uncorrelated (or orthogonal) factors
#Two options for identification:
  
#1-Freely estimate the loadings of the two items on the same factor but equate 
#them to be equal while setting the variance of the factor at 1

#2-Freely estimate the variance of the factor, using the marker method for the first item,
#but covary (correlate) the two-item factor with another factor

#First option because we assume uncorrelated factors
m4a <- 'f1 =~ q01+ q03 + q04 + q05 + q08 
        f2 =~ a*q06 + a*q07 
        f1 ~~ 0*f2 ' #Uncorrelated factors
twofac7items_a <- cfa(m4a, data=dat,std.lv=TRUE)

#alternative syntax - uncorrelated two factor solution, var std method
twofac7items_a <- cfa(m4a, data=dat,std.lv=TRUE, auto.cov.lv.x=FALSE) 
summary(twofac7items_a, fit.measures=TRUE,standardized=TRUE) #Freely estimates loadings: standardized=TRUE
semPlot::semPaths(twofac7items_a, "std")


#Correlated two factor solution, marker method
m4b <- 'f1 =~ q01+ q03 + q04 + q05 + q08 
        f2 =~ q06 + q07' 
twofac7items_b <- cfa(m4b, data=dat,std.lv=TRUE) 
summary(twofac7items_b,fit.measures=TRUE,standardized=TRUE) 
semPlot::semPaths(twofac7items_b, "std")


#second order three factor solution, marker method
m5a <- 'f1 =~ q01+ q03 + q04 + q05 + q08 
        f2 =~ q06 + q07 
        f3 =~ 1*f1 + 1*f2 
        f3 ~~ f3' 
secondorder <- cfa(m5a, data=dat) 
summary(secondorder,fit.measures=TRUE,standardized=TRUE)
semPlot::semPaths(secondorder, "std")


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

####   SEM   ####

### DATA PREPRATION ###
# Studying the effects of student background on academic achievement
dat <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2021/02/worland5.csv")

#Adjustment
#
# motiv: Motivation
# harm: Harmony
# stabi: Stability

#Risk
#
#ppsych: (Negative) Parental Psychology
#ses: (socio economic status) SES
#verbal: Verbal IQ

#Achievement
#
#read: Reading
#arith: Arithmetic
#spell: Spelling


#sample variance-covariance matrix 
cov(dat)

### MODEL 1: SIMPLE REGRESSION ###

#simple regression using lm() 
m1a <- lm(read ~ motiv, data=dat)
(fit1a <-summary(m1a))

### MODELS 1 AND 2: REGRESSION ###
#simple regression using lavaan
m1b <-   '
  # regressions
    read ~ 1 + motiv
  # variance (optional)
  motiv ~~ motiv
'
fit1b <- sem(m1b, data=dat)
summary(fit1b)
semPlot::semPaths(fit1b, "std")



#multiple regression
m2 <- '
  # regressions
    read ~ 1 + ppsych + motiv
    #covariance
    #ppsych ~~ motiv
'
fit2 <- sem(m2, data=dat)
summary(fit2)
semPlot::semPaths(fit2, "std")

### MODEL 3:  MULTIVARIATE REGRESSION ###

#multivariate regression (default)
m3a <- '
  # regressions
    read ~ ppsych + motiv
    arith ~ motiv
'
fit3a <- sem(m3a, data=dat)
summary(fit3a)
semPlot::semPaths(fit3a, "std")


#regression of read on psych and motiv
m3b <- lm(read ~ ppsych + motiv, data=dat)
(fit3b <- summary(m3b))

#regression of arith on motiv
m3c <- lm(arith ~ motiv, data=dat)
(fit3c <- summary(m3c))

#multivariate regression (set covariance to 0)
m3d <- '
  # regressions
    read ~ ppsych + motiv
    arith ~  motiv
  # covariance
   read ~~ 0*arith 
'
fit3d <- sem(m3d, data=dat)
summary(fit3d)


#multivariate regression (saturated model)
m3e <- '
  # regressions
    read ~ ppsych + motiv
    arith ~ ppsych + motiv
    #covariance
    ppsych ~~ motiv
    ppsych ~~ ppsych
    motiv ~~ motiv
'
fit3e <- sem(m3e, data=dat)
summary(fit3e)


### MODEL 4:  PATH ANALYSIS ###
# Path analysis allows endogenous variables to predict each other
#path analysis model 
m4a <- '
  # regressions
    read ~ ppsych + motiv
    arith ~ motiv + read
  # covariance
  #  read ~~ 0*arith 
'
fit4a <- sem(m4a, data=dat)
summary(fit4a)

#re-run with fit indexes
summary(fit4a, fit.measures=TRUE)

#model chi-square 
pchisq(q=4.870,df=1,lower.tail=FALSE)

#modification index
modindices(fit4a,sort=TRUE)

## model4a customized for degrees of freedom
m4aa <- '
  # regressions
    read ~ 1 + ppsych + motiv
    motiv ~ 1 + arith + read
  # covariance
  #  read ~~ 0*arith 
'
fit4aa <- sem(m4aa, data=dat)
summary(fit4aa)

#path analysis model after modification
m4b <- '
  # regressions
    read ~ ppsych + motiv
    arith ~ motiv + read + ppsych
'
fit4b <- sem(m4b, data=dat)
summary(fit4b)
modindices(fit4b,sort=TRUE)

#baseline model
m4c <- '
  # variances only
    read ~~ read 
    ppsych ~~ ppsych
    motiv ~~ motiv
    arith ~~ arith
'
fit4c <- sem(m4c, data=dat)
summary(fit4c, fit.measures=TRUE)

### MODEL 5:  MEASUREMENT MODEL ###

#exogenous factor analysis for adjust
m5a <- 'risk =~ verbal + ses + ppsych
          #intercepts (nu = tau) 
          verbal ~ 1
          ses ~ 1 
          ppsych ~ 1' 
fit5a <- sem(m5a, data=dat) 
summary(fit5a, standardized=TRUE)

### MODEL 6: STRUCTURAL REGRESSION ###

#structural regression (one endogenous variable)
m6a <- '
  # measurement model
    adjust =~ motiv + harm + stabi
    risk =~ verbal + ppsych + ses
    achieve =~ read + arith + spell
  # regressions
    achieve ~ adjust + risk
'
fit6a <- sem(m6a, data=dat)
summary(fit6a, standardized=TRUE, fit.measures=TRUE)

#structural regression (two endogenous variables)
m6b <- '
  # measurement model
    adjust =~ motiv + harm + stabi
    risk =~ verbal + ses + ppsych
    achieve =~ read + arith + spell
  # regressions
    adjust ~ risk 
    achieve ~ adjust + risk
'
fit6b <- sem(m6b, data=dat)
summary(fit6b, standardized=TRUE, fit.measures=TRUE)

#structural regression (observed endogenous variable)
m6c <- '
  # measurement model
    adjust =~ motiv + harm + stabi
    risk =~ verbal + ses + ppsych
  # regressions
    adjust ~ risk 
    read ~ adjust + risk
'
fit6c <- sem(m6c, data=dat)
summary(fit6c, standardized=TRUE, fit.measures=TRUE)
#inspect(fit6c,"partable")

#model6c (manual specification) 
m6cc <- '
  # measurement model
    adjust =~ motiv + harm + stabi
    risk =~ verbal + ses + ppsych
  #single indicator factor
    readf =~ 1*read
  #residuel variance to zero
    read ~~ 0*read
  # regressions
    adjust ~ risk 
    readf ~ adjust + risk
'
fit6cc <- sem(m6cc, data=dat, optim.method=list("BFGS"))
summary(fit6cc)
#inspect(fit6cc,"partable")

