#National ITS 

######National Med Records Viewed monthly#####

#Load library
library(nlme)
library(car)
library(writexl)
library(tidyverse)
library(readxl)


options(scipen=999) #remove exp number format

#Import Dataset 
Sum_Usage_MedicalRecords_views_6_21 <- read_excel("Sum_Usage_MedicalRecords_views_6_21.xlsx", 
                                                  +     col_types = c("date", "numeric", "numeric", 
                                                                      +         "numeric", "numeric"))
#Plot initial line plot
plot(Sum_Usage_MedicalRecords_views_6_21$time, Sum_Usage_MedicalRecords_views_6_21$med_views,
     ylim=c(0,9400000),
     ylab="Medical Records Viewed",
     xlab="Month",
     type="l",
     col="red",
     xaxt="n") 

# Add x-axis year labels
axis(1, at=1:29, labels=Sum_Usage_MedicalRecords_views_6_21$month)

# Add in the points for the figure
points(Sum_Usage_MedicalRecords_views_6_21$time, Sum_Usage_MedicalRecords_views_6_21$med_views,
       col="red",
       pch=20)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty=2)

##############Preliminary OLS Regression###########
#(Dont have to put in intercept term because it is implied)

model_medviews<-lm(med_views~time+level+trend,
                     data=Sum_Usage_MedicalRecords_views_6_21)
#summary
summary(model_medviews)

#CI for coefficients
confint(model_medviews)

# 
# 
# Call:
#   lm(formula = med_views ~ time + level + trend, data = Sum_Usage_MedicalRecords_views_6_21)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1308840  -295097   -25801    37999  4919163 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   -86104     636928  -0.135  0.89355   
# time           25159      70053   0.359  0.72251   
# level       -1441297     877472  -1.643  0.11300   
# trend         371656     104629   3.552  0.00155 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1172000 on 25 degrees of freedom
# Multiple R-squared:  0.6248,	Adjusted R-squared:  0.5797 
# F-statistic: 13.88 on 3 and 25 DF,  p-value: 0.00001583
# 
# > 
#   > #CI for coefficients
#   > confint(model_medviews)
# 2.5 %    97.5 %
#   (Intercept) -1397882.4 1225673.8
# time         -119117.8  169434.9
# level       -3248483.8  365890.0
# trend         156168.5  587143.9
# ############################
# Check for Autocorrelation
############################

#Durbin-watson test, 12 time periods
library(lmtest)
dwtest(model_medviews) 
#DW = 1.6877, p-value = 0.06868
#alternative hypothesis: true autocorrelation is greater than 0 

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_medviews))
acf(residuals(model_medviews),type='partial') #no signs of a.c 

############################
# Plot Final ITS Model
############################

plot(Sum_Usage_MedicalRecords_views_6_21$time, Sum_Usage_MedicalRecords_views_6_21$med_views,
     ylim=c(0,9400000),
     ylab="Medical Records Viewed",
     xlab="Month",
     main="Medical Record Views Count ITS",
     col="red",
     xaxt="n")
     
# Add x-axis year labels #1=xaxis
axis(1, at=1:29, labels=Sum_Usage_MedicalRecords_views_6_21$month)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty="dotted")    
  

#For observed lines use fitted()
# Plot the first line segment

#first segment: line for pre-period
lines(Sum_Usage_MedicalRecords_views_6_21$time[1:15], fitted(model_medviews)[1:15], col="red", lwd=2)

#second segment: lines for post-period
lines(Sum_Usage_MedicalRecords_views_6_21$time[16:29], fitted(model_medviews)[16:29], col="red", lwd=2)


#Plot Counterfactual 
segments(1,
         model_medviews$coef[1]+model_medviews$coef[2],
         29,
         model_medviews$coef[1]+model_medviews$coef[2]*29,
         lty=2,
         lwd=2,
         col='red')

#Pred value 12 mo after 1st covid lockdown= time point 28
pred <- fitted(model_medviews)[28]

#Estimate CF at same time point
cfac <- model_medviews$coef[1] + model_medviews$coef[2]*28

#Absolute change at final time point
pred-cfac
# = 3390234 

#Relative change at final time point
(pred-cfac)/cfac
# = 5.482848 


# 12 months after the first lockdown, there was a increase of an average of
# 1641481 NHS medical record views. This represented a 265% increase.
