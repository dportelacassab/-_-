rm(list=ls())

library("readxl")     # reading data set
library(ggplot2)      # barplots... and more

setwd('/Applications/Documents/UHaifa/Semester1/LinearModels/Assignment1/')

# 1) ####
# Combine the four sheets (main and scale, males and females)
#into a single R data frame. (Hints: You can save the sheets as four 
# .csv files and read these in to R using read.csv(). Use merge() to 
#combine the two sets of variables, and rbind() to combine males and 
#females.)

datenbank0 <- read_excel('Nutrition_data.xlsx', sheet = "main_m")
datenbank1 <- read_excel('Nutrition_data.xlsx', sheet = "main_f")
datenbank2 <- read_excel('Nutrition_data.xlsx', sheet = "scale_m")
datenbank3 <- read_excel('Nutrition_data.xlsx', sheet = "scale_f")

m = merge(datenbank0,datenbank2)
f = merge(datenbank1,datenbank3)
four = rbind(m,f)
names(four)

# * A few variables have missing values that differ from what is listed in the .rtf file.
# - grade: Treat anything >17 as missing.
four$grade[four$grade > 17] <- NA
# - exercise: Treat 9 (not 7) as missing.
four$exercise[four$exercise == 9] <- NA
# - For kq7, treat 8,9 (not 4,5) as missing
four$kq7[four$kq7 == 8 | four$kq7 == 9 ] <- NA

# 2) ####
# Use descriptive methods (frequency tables, histograms, the
# pairs() function, etc.) to check the integrity and completeness of
# the following variables: 

# PREDICTOR VARIABLES!
# region, urbanization, income, age, sex, 
# race, education (highest grade completed), 
# the five “diet” variables, exercise frequency,
# and self-reported weight status.
variables = c('bmi_sp',"region","urb","income","age","sex","race",
  "grade","dt01","dt02","dt03","dt06",
  "dt07","exercise","kq7")
databank = four[variables]

View(databank)

# Don’t forget to convert categorical variables to “factors”. (Note:
# In “real life,” questions about the data should generally be 
# referred to the investigator to determine if there are errors in
# the data and, if so, whether these can be corrected.) Only the 
# above predictor variables should be considered in the remaining 
# questions.

# region   - Region (1= Northeast;2= Midwest;3=South;4=West)
databank$region = factor(databank$region);
class(databank$region);attributes(databank$region)$levels

# urb      - Urbanization (1=Central city;2=Suburban;3=Nonmetropolitan)
databank$urb = factor(databank$urb);attributes(databank$urb)$levels

# income   - Annual income: total       (continuous)
# age      - Age in years               (continuous)

# sex      - Sex (1=male;2=female)
databank$sex = factor(databank$sex);attributes(databank$sex)$levels
# race     - Race (1= White;2=Black;3=Asian/Pacific islander;4=American Indian/Alaska Native;5=Other)
databank$race = factor(databank$race);attributes(databank$race)$levels
# grade    - Highest grade completed
databank$grade = factor(databank$grade);attributes(databank$grade)$levels
# dt01     - Diet: low cal: yes or no
databank$dt01 = factor(databank$dt01);attributes(databank$dt01)$levels
# dt02     - Diet: low fat: yes or no
databank$dt02 = factor(databank$dt02);attributes(databank$dt02)$levels
# dt03     - Diet: low salt: yes or no
databank$dt03 = factor(databank$dt03);attributes(databank$dt03)$levels
# dt06     - Diet: high fiber: yes or no
databank$dt06 = factor(databank$dt06);attributes(databank$dt06)$levels
# dt07     - Diet: diabetic: yes or no
databank$dt07 = factor(databank$dt07);attributes(databank$dt07)$levels
# exercise - Exercise frequency (1=Daily;2=5-6 time/week;3= 2-4 times/week;4=Once a week;5=1-3 times/month;6=Rarely or never;
databank$exercise = factor(databank$exercise);attributes(databank$exercise)$levels
# kq7      - Self-reported weight status (1= Overweight;2= Underweight;3=About right;4= Don't know;5=Not ascertained)
databank$kq7 = factor(databank$kq7);attributes(databank$kq7)$levels

# write.csv(databank,file = 'datenbank.csv')

# setwd('/Applications/Documents/UHaifa/Semester1/LinearModels/Assignment1/')
# databank = read.csv('datenbank.csv')
# class(databank$region)
# databank[]

n = dim(databank)[1]
p = dim(databank)[2]

Y = databank[,-3:-4]
# View(Y)

# hist(databank$income, main = 'Histogram',xlab="Income",
#      ylab="Frequency") # cmnd + shit + c to make comments here!


hist(databank$income)
shapiro.test(databank$income)
mean(databank$income)

t0 = data.frame(table(databank$income))
p0 = ggplot(t0,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('Income')+ylab('count')+ggtitle('Barplot of Income')

hist(databank$age)
summary(databank$age)
shapiro.test(databank$age)

t1 = data.frame(table(databank$region))
t1$Freq*100/n
p1 = ggplot(t1,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('Region')+ylab('count')+ggtitle('Barplot of Regions')+ 
  geom_text(
    aes(label=after_stat("count")), stat="count", nudge_y=0.125, va="bottom"
  )
p1

t2 = data.frame(table(databank$urb))
t2['perc'] = t2$Freq*100/n
t2
p2 = ggplot(t2,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('Urb')+ylab('count')+ggtitle('Barplot of Urb')

hist(databank$age)
databank$age

t3 = data.frame(table(databank$age))
t3['perc'] = t3$Freq*100/n
p3 = ggplot(t3,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('Age')+ylab('count')+ggtitle('Barplot of Age')

t4 = data.frame(table(databank$sex))
t4['perc'] = t4$Freq*100/n
p4 = ggplot(t4,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('Sex')+ylab('count')+ggtitle('Barplot of Sex')

t5 = data.frame(table(databank$race))
t5['perc'] = t5$Freq*100/n
p5 = ggplot(t5,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('Race')+ylab('count')+ggtitle('Barplot of Race')

t6 = data.frame(table(databank$grade))
t6['perc'] = t6$Freq*100/n
p6 = ggplot(t6,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('Grade')+ylab('count')+ggtitle('Barplot of Grade')

t7 = data.frame(table(databank$dt01))
t7['perc'] = t7$Freq*100/n
p7 = ggplot(t7,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('dt01')+ylab('count')+ggtitle('Barplot of dt01')

t8 = data.frame(table(databank$dt02))
t8['perc'] = t8$Freq*100/n
p8 = ggplot(t8,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('dt02')+ylab('count')+ggtitle('Barplot of dt02')

t9 = data.frame(table(databank$dt03))
t9['perc'] = t9$Freq*100/n
p9 = ggplot(t9,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('dt03')+ylab('count')+ggtitle('Barplot of dt03')

t10 = data.frame(table(databank$dt06))
t10['perc'] = t10$Freq*100/n
p10 = ggplot(t10,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('dt06')+ylab('count')+ggtitle('Barplot of dt06')

t11 = data.frame(table(databank$dt07))
t11['perc'] = t11$Freq*100/n
p11 = ggplot(t11,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('dt07')+ylab('count')+ggtitle('Barplot of dt07')

t12 = data.frame(table(databank$exercise))
t12['perc'] = t12$Freq*100/n
p12 = ggplot(t12,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('Exercise')+ylab('count')+ggtitle('Barplot of Exercise')

t13 = data.frame(table(databank$kq7))
t13['perc'] = t13$Freq*100/n
p13 = ggplot(t13,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('kq7')+ylab('count')+ggtitle('Barplot of kq7')

grob0=gridExtra::grid.arrange(p0,p3,ncol = 1)
ggsave("plots0.pdf", grob0, width = 15, height = 8)

grob1=gridExtra::grid.arrange(p1,p2,p4,p5,p6,ncol = 2)
ggsave("plots1.pdf", grob1, width = 15, height = 8)

grob2=gridExtra::grid.arrange(p7,p8,p9,p10,ncol = 2)
ggsave("plots2.pdf", grob2, width = 15, height = 8)

grob3=gridExtra::grid.arrange(p10,p11,p12,p13,ncol = 2)
ggsave("plots3.pdf", grob3, width = 15, height = 8)

# 3) ####
# For each variable, values that specify missing data 
# (e.g., “indeterminate”, “not ascertained”) should be changed to NA.

names( which(colSums(is.na( databank )) > 0) )

# we have that the variables 'grade', 'exercise' and 'kq7'
# have missing values. If a variable has more than 5% of 
# missing values then this variabel should be taken out of
# the analysis?

sum(is.na(databank$grade))*100/n
sum(is.na(databank$exercise))*100/n
sum(is.na(databank$kq7))*100/n

# All of these are just less than 1%. So we process to 
# delete these values and just work with the remaining
# data set, in order to be able to evaluate stepAIC.

databank = na.omit(databank)
names( which(colSums(is.na( databank )) > 0) )

# 4) ####
# Check the distribution of the response variable and 
# whether there is a need for a transformation. 

summary(databank$bmi_sp)
nrow(databank[databank$bmi_sp > 60, ])
# we delete these rare values!!!

databank$bmi_sp[databank$bmi_sp > 60] <- NA
summary(databank$bmi_sp)
databank = na.omit(databank)

summary(databank$bmi_sp)

ydi = ggplot(databank, aes(x = bmi_sp)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white",
                 bins = 60) +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  xlab('BMI')+ylab('Frequency')+
  ggtitle('Histogram of BMI')
ggsave("y_dist.pdf", ydi, width = 15, height = 8)

shapiro.test(databank$bmi_sp)
# with an \alpha = 0.05 > 0.00000000000000022 = p-value then there
# is enough evidence to reject H0 that the data follows
# a normal distribution.

# the distribution is skewed to the right, let's see
# if a transformation helps to fix this.

ly = log(databank$bmi_sp)
databank['transf_bmisp'] <- ly
shapiro.test(ly) #still not normally distributed
hist(ly)


# Box-Cox transformation

library(MASS) # box-cox transformation
names(databank)
summary(bcox.lm <- lm( bmi_sp ~ region + urb + income + age
                       + sex + race + grade + dt01 + dt02
                       + dt03+ dt06 + dt07 + exercise + kq7,
                       data=databank)
        ) 
boxDatabank <- boxcox(bcox.lm)
with(boxDatabank, cbind(x,y))
lambda = boxDatabank$x[which.max(boxDatabank$y)]  # optimal lambda
lambda
y_bcox <- (databank$bmi_sp^lambda - 1)/lambda

databank['bcox_bmisp'] <- y_bcox
shapiro.test(databank$bcox_bmisp)
# with an \alpha = 0.01 < 0.01735 = p-value then there
# is not enough evidence to reject H0 that the data
# follows a normal distribution. So we use this fact!


ydi0 = ggplot(databank, aes(x = bcox_bmisp)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white",
                 bins = 60) +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  xlab('BMI')+ylab('Frequency')+
  ggtitle('Histogram of boxcox transformed BMI')
ggsave("y_dist0.pdf", ydi0, width = 15, height = 8)


# this last box cox transformation even though it doesn't provides
# us with a normal distribution, it definetely get rid of the skewness
# to the right of the variable, then we use this transformed one as
# the final response variable.

# 5) ####
# Use plot() and boxplot() to examine whether BMI appears 
# to be related to the other variables listed above, and as 
# an additional check for anomalies in the data.

# Continuous
names(databank)

c_vars = databank[,c(17,4,5)]
View(c_vars)
colnames(c_vars) <- c('bcox bmisp','Income','Age')
pd = pairs(c_vars,main = "Scatter Matrix - continuos variables BMI",pch = '.', lower.panel = NULL)
ggsave("scatt_conti.pdf", sc, width = 15, height = 8)

library(psych)
options(scipen=999) # in order to avoid scientific notation in the following numbers in the plot
pairs.panels(c_vars,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses,
             ,pch = '.')
# wasn't able to save this so I did it from the interface!

# Categorical 
names(databank)
k1 = ggplot(databank, aes(x=region, y=bcox_bmisp, fill=region)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none")
k2 = ggplot(databank, aes(x=urb, y=bcox_bmisp, fill=urb)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")
k3 = ggplot(databank, aes(x=sex, y=bcox_bmisp)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)
k4 = ggplot(databank, aes(x=race, y=bcox_bmisp, fill=race)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")
k5 = ggplot(databank, aes(x=grade, y=bcox_bmisp, fill=grade)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none")
grobK0=gridExtra::grid.arrange(k1,k2,k3,k4,k5,ncol = 2)
ggsave("k.pdf", grobK0, width = 15, height = 8)

k6 = ggplot(databank, aes(x=dt01, y=bcox_bmisp, fill=dt01)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")
k7 = ggplot(databank, aes(x=dt02, y=bcox_bmisp, fill=dt02)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")
k8 = ggplot(databank, aes(x=dt03, y=bcox_bmisp, fill=dt03)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")
k9 = ggplot(databank, aes(x=dt06, y=bcox_bmisp, fill=dt06)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")
k10 = ggplot(databank, aes(x=dt07, y=bcox_bmisp, fill=dt07)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")
grobK1=gridExtra::grid.arrange(k6,k7,k8,k9,k10,ncol = 2)
ggsave("k1.pdf", grobK1, width = 15, height = 8)

k11 = ggplot(databank, aes(x=exercise, y=bcox_bmisp)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)
k12 = ggplot(databank, aes(x=kq7, y=bcox_bmisp, fill=kq7)) + 
  geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")
grobK2=gridExtra::grid.arrange(k11,k12,ncol = 2)
ggsave("k2.pdf", grobK2, width = 15, height = 8)

grobK3=gridExtra::grid.arrange(k4,k5,k12,ncol = 2)
ggsave("k3.pdf", grobK3, width = 15, height = 8)

# # razon de correlacion
# library(FactoClass)
# data(admi)
# xtable( centroids(databank$bcox_bmisp,databank[,6:8])$cr*100 );


# 6) #### 
# Regress (ORIGINAL) BMI on each of the other variables
# individually.

m1 = lm( bmi_sp ~ region , data = databank )
summary(m1)
# NOT significant!      # poor R^2

m2 = lm( bmi_sp ~ urb , data = databank )
summary(m2)
# NOT significant!      # poor R^2

m3 = lm( bmi_sp ~ income , data = databank )
summary(m3)
# SIGNIFICANT!          # poor R^2

m4 = lm( bmi_sp ~ age , data = databank )
summary(m4)
# SIGNIFICANT!          # poor R^2

m5 = lm( bmi_sp ~ sex , data = databank )
summary(m5)
# SIGNIFICANT!          # poor R^2

m6 = lm( bmi_sp ~ race , data = databank )
summary(m6)
# SIGNIFICANT!          # poor R^2

m7 = lm( bmi_sp ~ grade , data = databank )
summary(m7)
# SIGNIFICANT!
# poor R^2 so far the biggest one - 0.02618

m8 = lm( bmi_sp ~ dt01 , data = databank )
summary(m8)
# SIGNIFICANT!          # poor R^2

m9 = lm( bmi_sp ~ dt02 , data = databank )
summary(m9)
# NOT significant!      # poor R^2

m10 = lm( bmi_sp ~ dt03 , data = databank )
summary(m10)
# NOT significant!      # poor R^2

m11 = lm( bmi_sp ~ dt06 , data = databank )
summary(m11)
# NOT significant!      # poor R^2

m12 = lm( bmi_sp ~ dt07 , data = databank )
summary(m12)
# significant with a 90% level of confidence!      # poor R^2

m13 = lm( bmi_sp ~ exercise , data = databank )
summary(m13)
# NOT significant in general! Only the category 
# exercise6 with a 95% confidence    # poor R^2

m14 = lm( bmi_sp ~ kq7 , data = databank )
summary(m14)
# SIGNIFICANT!     # poor R^2




# Regress (TRANSFORMED) BMI on each of the other variables
# individually.

m1 = lm( bcox_bmisp ~ region , data = databank )
summary(m1)
# NOT significant!      # poor R^2

m2 = lm( bcox_bmisp ~ urb , data = databank )
summary(m2)
# NOT significant!      # poor R^2

m3 = lm( bcox_bmisp ~ income , data = databank )
summary(m3)
# SIGNIFICANT!          # poor R^2

m4 = lm( bcox_bmisp ~ age , data = databank )
summary(m4)
# SIGNIFICANT!          # poor R^2

m5 = lm( bcox_bmisp ~ sex , data = databank )
summary(m5)
# SIGNIFICANT!          # poor R^2

m6 = lm( bcox_bmisp ~ race , data = databank )
summary(m6)
# SIGNIFICANT!          # poor R^2

m7 = lm( bcox_bmisp ~ grade , data = databank )
summary(m7)
# SIGNIFICANT!
# poor R^2 so far the biggest one - 0.02134

m8 = lm( bcox_bmisp ~ dt01 , data = databank )
summary(m8)
# SIGNIFICANT!          # poor R^2

m9 = lm( bcox_bmisp ~ dt02 , data = databank )
summary(m9)
# SIGNIFICANT!          # poor R^2

m10 = lm( bcox_bmisp ~ dt03 , data = databank )
summary(m10)
# SIGNIFICANT!          # poor R^2

m11 = lm( bcox_bmisp ~ dt06 , data = databank )
summary(m11)
# NOT significant!      # poor R^2

m12 = lm( bcox_bmisp ~ dt07 , data = databank )
summary(m12)
# SIGNIFICANT!          # poor R^2

m13 = lm( bcox_bmisp ~ exercise , data = databank )
summary(m13)
# NOT significant in general! Only the category 
# exercise6 with a 95% confidence    # poor R^2

m14 = lm( bcox_bmisp ~ kq7 , data = databank )
summary(m14)
# SIGNIFICANT!     # poor R^2

# Almost the same conclusion could be taken from the transformed
# variable.

# 7) ####
# Build a multiple regression model for (transformed) BMI. 
# Explain the choice of variables in the final model and assess
# the model fit. There is no one “correct” approach here, but 
# you will need to justify your approach. Some strategies you 
# may consider include:

# a.	“Screen” variables based on the results of the separate
# regressions for each predictor—i.e., only predictors found to
# be significant in separate models are candidates for inclusion
# in the multiple regression.

# From the previous screening process the variables to be
# selected are 

# income, age, sex,
# race, grade, dt01, dt02, dt03,
# dt07, exercise, and kq7.

lm_screen <- lm(bcox_bmisp ~ income + age + sex + race +
                dt01 + dt02 + dt03 + dt07 + 
                kq7, data = databank)
summary(lm_screen)

# now we get to get some of the variables out of the model
# and see if that start to affect which ones get significant or not
# we'll do this with th stepAIC function!

# b.	Forward selection, backward elimination, or both, e.g.,
# using stepAIC().

library(MASS) # stepAIC function

stepAIC(lm_screen)

# by means of stepAIC we have that the following model has the
# samllest AIC of AIC=-36057.28
# At the very last step stepAIC has produced the optimal set of
# features given by

# bcox_bmisp ~ income + age + sex + race + dt01 + dt07 + kq7


# c.	Consider interaction effects.
# here ^2 creates all the two-way interactions between the variables.
lm_inter <- lm(bcox_bmisp ~ (income + age + sex +
                            race + dt01 + dt07 + kq7)^2,
               data = databank)
summary(lm_inter)

stepAIC(lm_inter)

# the stepAIC function gives us the following model
# AIC=-36205.48
# bcox_bmisp ~ income + age + sex + race + dt01 + dt07 + kq7 + 
#   income:sex + income:dt07 + income:kq7 + age:sex + age:dt01 + 
#   age:dt07 + age:kq7 + sex:race + sex:dt07 + sex:kq7 + dt07:kq7


fit0 = lm(bcox_bmisp ~ income + age + sex +
          race + dt01 + dt07 + kq7, data = databank)
summary(fit0)
fit1 = lm(bcox_bmisp ~ income + age + sex + race + dt01 +
          dt07 + kq7 + income:sex + income:dt07 + income:kq7 +
          age:sex + age:dt01 + age:dt07 + age:kq7 + sex:race +
          sex:dt07 + sex:kq7 + dt07:kq7, data = databank)
summary(fit1)
lapply(list(fit0, fit1), BIC)

source("methods.R")
qqplot.normal(fit0,200,0.01,1,"")
shapiro.test(fit0$residuals)
# Warning! Residuals don't follow a normal distribution!
Leverage.normal(fit0,1,"")
Residuos.normal(fit0,2,"")
Influence.normal(fit0,1,3,3,"")

qqplot.normal(fit1,200,0.01,1,"")
shapiro.test(fit1$residuals)
# Warning! Residuals don't follow a normal distribution!
Leverage.normal(fit1,1,"")
Residuos.normal(fit1,2,"")
Influence.normal(fit1,1,3,3,"")

options(scipen = 999)

# 8) ####
# Summarize the results in a report that includes a brief
# introduction to the problem; a description of the data
# set; an explanation of the data analysis methodology; 
# appropriate text, tables and figures (not raw R output!)
# to present the results, and a brief conclusion. Attach 
# your R code as an appendix.


