#National ITS 

######National Appt Booked monthly#####

#Load library
library(nlme)
library(car)
library(writexl)
library(tidyverse)
library(readxl)

options(scipen=999) #remove exp number format


#Import Dataset 
Sum_Usage_Appointments_booked_6_21 <- read_excel("ITS National/Sum_Usage_Appointments_booked_6_21.xlsx", 
                                                 +     col_types = c("date", "numeric", "numeric", 
                                                                     +         "numeric", "numeric"))

#Plot initial line plot
plot(Sum_Usage_Appointments_booked_6_21$time, Sum_Usage_Appointments_booked_6_21$appt_booked,
     ylim=c(0,45000),
     ylab="Appointments Booked",
     xlab="Month",
     type="l",
     col="red",
     xaxt="n") 

# Add x-axis year labels
axis(1, at=1:29, labels=Sum_Usage_Appointments_booked_6_21$month)

# Add in the points for the figure
points(Sum_Usage_Appointments_booked_6_21$time, Sum_Usage_Appointments_booked_6_21$appt_booked,
       col="red",
       pch=20)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty=2)

##############Preliminary OLS Regression###########
#(Dont have to put in intercept term because it is implied)

model_apptbooked<-lm(appt_booked~time+level+trend,
                     data=Sum_Usage_Appointments_booked_6_21)


model_apptbooked<-gls(appt_booked~time+level+trend,
                      data=Sum_Usage_Appointments_booked_6_21, correlation=NULL, method="ML")

#summary
summary(model_apptbooked)

#CI for coefficients
confint(model_apptbooked)



# Call:
#         lm(formula = appt_booked ~ time + level + trend, data = Sum_Usage_Appointments_booked_6_21)
# 
# Residuals:
#         Min     1Q Median     3Q    Max 
# -9353  -2528  -1553   2000  12785 
# 
# Coefficients:
#         Estimate Std. Error t value     Pr(>|t|)    
# (Intercept)  -6196.37    2641.76  -2.346       0.0272 *  
#         time          2247.35     290.56   7.735 0.0000000432 ***
#         level       -21315.76    3639.46  -5.857 0.0000041424 ***
#         trend          -22.01     433.97  -0.051       0.9599    
# ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4862 on 25 degrees of freedom
# Multiple R-squared:  0.8532,	Adjusted R-squared:  0.8356 
# F-statistic: 48.42 on 3 and 25 DF,  p-value: 0.0000000001469
# 
# > 
#         > #CI for coefficients
#         > confint(model_apptbooked)
# 2.5 %      97.5 %
#         (Intercept) -11637.1852   -755.5576
# time          1648.9367   2845.7562
# level       -28811.3620 -13820.1561
# trend         -915.7828    871.7580

############################
# Check for Autocorrelation
############################

#Durbin-watson test, 12 time periods
library(lmtest)
dwtest(model_apptbooked) 
#DW = 1.2319, p-value = 0.002166
#alternative hypothesis: true autocorrelation is greater than 0 #autocorrelation exists 

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_apptbooked))
acf(residuals(model_apptbooked),type='partial') #no signs of a.c 

############################
# Plot Final ITS Model
############################

plot(Sum_Usage_Appointments_booked_6_21$time, Sum_Usage_Appointments_booked_6_21$appt_booked,
     ylim=c(0,45000),
     ylab="Appointments Booked",
     xlab="Month",
     main="Appointments Booked Count ITS",
     col="red",
     xaxt="n") 

# Add x-axis year labels #1=xaxis
axis(1, at=1:29, labels=Sum_Usage_Appointments_booked_6_21$month)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty="dotted")

#For observed lines use fitted()
# Plot the first line segment

#first segment: line for pre-period
lines(Sum_Usage_Appointments_booked_6_21$time[1:15], fitted(model_apptbooked)[1:15], col="red", lwd=2)

#second segment: lines for post-period
lines(Sum_Usage_Appointments_booked_6_21$time[16:29], fitted(model_apptbooked)[16:29], col="red", lwd=2)

#Plot Counterfactual 
segments(1,
         model_apptbooked$coef[1]+model_apptbooked$coef[2],
         29,
         model_apptbooked$coef[1]+model_apptbooked$coef[2]*29,
         lty=2,
         lwd=2,
         col='red')

#Pred value 12 mo after 1st covid lockdown= time point 28
pred <- fitted(model_apptbooked)[28]

#Estimate CF at same time point
cfac <- model_apptbooked$coef[1] + model_apptbooked$coef[2]*28

#Absolute change at final time point
pred-cfac
#= -21601.92 

#Relative change at final time point
(pred-cfac)/cfac
# = -0.3807893 


# 12 months after the first lockdown, there was a decrease of an average of
# 21601 NHS appointment bookings. This represented a 51% decrease.
