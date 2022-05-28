#National ITS 

######National registrations monthly#####

#Load library
library(nlme)
library(car)
library(writexl)
library(tidyverse)
library(readxl)

#Set wd

#Import Dataset 
national_NHSApp_RegistrationsCount_6_21 <- read_excel("Sum_NHSApp_RegistrationsCount_6_21.xlsx", 
                                                      +     col_types = c("date", "numeric", "numeric", 
                                                                          +         "numeric", "numeric"))
#Plot initial line plot
plot(national_NHSApp_RegistrationsCount_6_21$time, national_NHSApp_RegistrationsCount_6_21$reg_count,
     ylim=c(0,2500000),
     ylab="Registrations Count",
     xlab="Month",
     type="l",
     col="red",
     xaxt="n") 

# Add x-axis year labels
axis(1, at=1:29, labels=national_NHSApp_RegistrationsCount_6_21$month)

# Add in the points for the figure
points(national_NHSApp_RegistrationsCount_6_21$time, national_NHSApp_RegistrationsCount_6_21$reg_count,
       col="red",
       pch=20)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty=2)


##############Preliminary OLS Regression###########
#(Dont have to put in intercept term because it is implied)

model_regcount<-lm(reg_count~time+level+trend,
                      data=national_NHSApp_RegistrationsCount_6_21)
#summary
summary(model_regcount)

#CI for coefficients
confint(model_regcount)

############################
# Check for Autocorrelation
############################

#Durbin-watson test, 12 time periods
library(lmtest)
dwtest(model_regcount)
#data:  model_apptbooking
#DW = 1.4484, p-value = 0.01533
#alternative hypothesis: true autocorrelation is greater than 0

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_regcount))
acf(residuals(model_regcount),type='partial') #ACF small lag at 8 but not too big



model_regcount1 <- gls(reg_count~time+level+trend,
                 data=nat_registrationscount_month,
                 correlation=corARMA(p=8,form=~time),
                 method="ML")

summary(model_regcount1)

# Likelihood-ratio tests to check AR process
anova(model_regcount1,model_regcount)

#can keep model_regcount

model_regcount<-lm(reg_count~time+level+trend,
                   data=nat_registrationscount_month)
summary(model_regcount)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -16036.4    23166.9  -0.692   0.4952  
#time          5560.5     2548.0   2.182   0.0387 *
#level         -895.2    31916.1  -0.028   0.9778  
#trend         5870.6     3805.7   1.543   0.1355

############################
# Plot Final ITS Model
############################

# Produce the plot, first plotting the raw data points

options(scipen=999) #remove exp number format

plot(national_NHSApp_RegistrationsCount_6_21$time, national_NHSApp_RegistrationsCount_6_21$reg_count,
     ylim=c(0,2500000),
     ylab="Registration Count",
     xlab="Month",
     col="red",
     main="Registration Count ITS",
     xaxt="n") 

# Add x-axis year labels #1=xaxis
axis(1, at=1:29, labels=national_NHSApp_RegistrationsCount_6_21$month)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty="dotted")

#For observed lines use fitted()
# Plot the first line segment

#first segment: line for pre-period
lines(national_NHSApp_RegistrationsCount_6_21$time[1:15], fitted(model_regcount)[1:15], col="red", lwd=2)

#second segment: lines for post-period
lines(national_NHSApp_RegistrationsCount_6_21$time[16:29], fitted(model_regcount)[16:29], col="red", lwd=2)

#Plot Counterfactual 

segments(1,
         model_regcount$coef[1]+model_regcount$coef[2],
         29,
         model_regcount$coef[1]+model_regcount$coef[2]*29,
         lty=2,
         lwd=2,
         col='red')


#Pred value 12 mo after 1st covid lockdown= time point 28
#26 time points before last time point

pred <- fitted(model_regcount)[28]

#Estimate CF at same time point
cfac <- model_regcount$coef[1] + model_regcount$coef[2]*28

#Absolute change at final time point
pred-cfac
#= 75422.34 

#Relative change at final time point
(pred-cfac)/cfac
# = 0.5400531 

# 12 months after the first lockdown,there was an increase of an average of
# 75422 NHS app registrations. This represented a 54% increase.
