print("Isha Golakiya")
install.packages("MASS")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("gmodels")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("corrplot")
tinytex::install_tinytex()
library(Hmisc)
library(corrplot)
library(MASS)
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(gmodels)
library(ggplot2)
library(moments)
#Importing Dataset
data("cats")
summary(cats)
headtail(cats, 3)
str(cats)

#Checking Normalization
hist(cats$Bwt, xlab = "Weight", ylab = "Frequency", 
     col=c("red"),
     main = "Histogram")
print(skewness(cats$Bwt))
hist((cats$Hwt), 
     xlab = "Height(in lbs)",
     ylab = "Frequency",
     col=c("light blue"))
print(skewness(cats$Hwt))
view(cats)
#subset
male <- subset(cats,cats$Sex == "M")
summary(male)
female <- subset(cats,cats$Sex == "F")
summary(female)

headtail(male)
summary(male)
headtail(female)
summary(female)

# MALE HISTOGRAM
hist((male$Bwt), 
     xlab = "Body Weight",
     ylab = "Frequency",
     main = "Histogram for Male Bwt",
     col=c("light blue"))
print(skewness(male$Bwt))

hist((male$Hwt), 
     xlab = "Head Weight",
     ylab = "Frequency",
     main = "Histogram for Male cat",
     col=c("light blue"))
print(skewness(male$Hwt))

# Female Histogram
hist((female$Bwt), 
     xlab = "Body Weight",
     ylab = "Frequency",
     main = "Histogram for Female BWT",
     col=c("grey"))
print(skewness(female$Bwt))

hist((female$Hwt), 
     xlab = "Head weight",
     ylab = "Frequency",
     main = "Histogram for Female HWT",
     col=c("grey"))
print(skewness(female$hwt))



#scatter plot------------------------------------------------------------------------------
mh <- sample(male$Hwt)
plot(Bwt ~ mh, data=male, 
     pch=25, main="Male Body weight/ Head Weight sample")
abline(lm(male$Bwt ~ mh), col="red")


male <- sample_n(male, 30)
female <- sample_n(female, 30)
#Covaraince------------------------------------------------------------------------------- 
length(male$Sex)
male_covaraince = sum((male$Hwt - mean(male$Hwt)) * (male$Bwt - mean(male$Bwt)))
Final_male_covaraince = male_covaraince/29
Final_male_covaraince
cov(male$Hwt,male$Bwt)

length(female$Sex)
female_covaraince = sum((female$Hwt - mean(female$Hwt)) * (female$Bwt - mean(female$Bwt)))
Final_female_covaraince = female_covaraince/29
Final_female_covaraince
cov(female$Hwt,female$Bwt)

#Corelation-----------------------------------------------------------------------------
cor(male$Hwt,male$Bwt)
plot(male$Hwt,male$Bwt)
#or
sd(male$Hwt)
sd(male$Bwt)
Total_Male_Sd <- sd(male$Hwt) * sd(male$Bwt)
male_correlation = Final_male_covaraince/Total_Male_Sd
male_correlation

sd(female$Hwt)
sd(female$Bwt)
Total_female_Sd <- sd(female$Hwt) * sd(female$Bwt)
female_correlation = Final_female_covaraince/Total_female_Sd
female_correlation
#or
cor(female$Hwt,female$Bwt)
plot(female$Hwt,female$Bwt)

#Plot-------------------------------------------------------------------------------------



colnames(male)[2] <- "Male_hwt"
colnames(male)[3] <- "Male_bwt"
colnames(female)[2] <- "female_hwt"
colnames(female)[3] <- "female_bwt"

new<- cbind(male,female)
colnames(new)[4] <- "female_gender"
temp<- select(new, -c("Sex","female_gender"))

cor(temp)
corrplot(cor(temp), method = "circle") 



#Regression Co-efficient---------------------------------------------------------------------------


Regression_co_male = lm(Male_hwt ~ Male_bwt, data = male)
Regression_co_male$coefficients
summary(Regression_co_male)

Regression_co_female = lm(female_hwt ~ female_bwt, data = female)
Regression_co_female$coefficients
summary(Regression_co_female)


#Regression Model----------------------------------------------------------------------------------

Regression_Model_Male = lm(Male_bwt ~ Male_hwt, data = male)
Regression_Model_Male

Regression_Model_Female = lm(female_bwt ~ female_hwt, data = female)
Regression_Model_Female


#Plotting a scatter plot for the Bwt and Hwt for Male and Female-----------------------------
plot(Male_bwt ~ Male_hwt, data=male, 
     pch=22, 
     main="Male BWt and Hwt comparison")
abline(lm(male$Male_bwt ~ male$Male_hwt), col="red")

plot(female_bwt ~ female_hwt, data=female, 
     pch=22, main="Female BWt and Hwt comparison")
abline(lm(female$female_bwt ~ female$female_hwt), col="red")


