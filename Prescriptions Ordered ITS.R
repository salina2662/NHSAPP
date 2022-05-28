#National ITS 

######National Prescriptions Ordered monthly#####

#Load library
library(nlme)
library(car)
library(writexl)
library(tidyverse)
library(readxl)

options(scipen=999) #remove exp number format


#Import Dataset 
Sum_Usage_Prescriptions_Ordered_6_21 <- read_excel("ITS National/Sum_Usage_Prescriptions_Ordered_6_21.xlsx", 
                                                   +     col_types = c("date", "numeric", "numeric", 
                                                                       +         "numeric", "numeric"))
#Plot initial line plot
plot(Sum_Usage_Prescriptions_Ordered_6_21$time, Sum_Usage_Prescriptions_Ordered_6_21$precrip_ordered,
     ylim=c(0,540000),
     ylab="Precriptions Ordered",
     xlab="Month",
     type="l",
     col="red",
     xaxt="n") 

# Add x-axis year labels
axis(1, at=1:29, labels=Sum_Usage_Prescriptions_Ordered_6_21$month)

# Add in the points for the figure
points(Sum_Usage_Prescriptions_Ordered_6_21$time, Sum_Usage_Prescriptions_Ordered_6_21$precrip_ordered,
       col="red",
       pch=20)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty=2)

##############Preliminary OLS Regression###########
#(Dont have to put in intercept term because it is implied)

model_precripordered<-lm(precrip_ordered~time+level+trend,
                   data=Sum_Usage_Prescriptions_Ordered_6_21)
#summary
summary(model_precripordered)

#CI for coefficients
confint(model_precripordered)

# 
# Call:
#         lm(formula = precrip_ordered ~ time + level + trend, data = Sum_Usage_Prescriptions_Ordered_6_21)
# 
# Residuals:
#         Min     1Q Median     3Q    Max 
# -49151  -9929  -4739   6658  91219 
# 
# Coefficients:
#         Estimate Std. Error t value      Pr(>|t|)    
# (Intercept)   -18935      13634  -1.389       0.17713    
# time            5225       1500   3.485       0.00183 ** 
#         level          27494      18783   1.464       0.15571    
# trend          19934       2240   8.900 0.00000000319 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 25090 on 25 degrees of freedom
# Multiple R-squared:  0.975,	Adjusted R-squared:  0.972 
# F-statistic: 325.2 on 3 and 25 DF,  p-value: < 0.00000000000000022
# 
# > 
#         > #CI for coefficients
#         > confint(model_precripordered)
# 2.5 %    97.5 %
#         (Intercept) -47014.534  9143.962
# time          2137.005  8313.616
# level       -11189.750 66177.681
# trend        15320.958 24546.196


############################
# Check for Autocorrelation
############################

#Durbin-watson test, 12 time periods
library(lmtest)
dwtest(model_precripordered) 
#DW = 1.4367, p-value = 0.01329
#alternative hypothesis: true autocorrelation is greater than 0 #Autocorr. detected

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_precripordered))
acf(residuals(model_precripordered),type='partial') #no signs of a.c 

############################
# Plot Final ITS Model
############################

plot(Sum_Usage_Prescriptions_Ordered_6_21$time, Sum_Usage_Prescriptions_Ordered_6_21$precrip_ordered,
     ylim=c(0,540000),
     ylab="Prescriptions Ordered",
     xlab="Month",
     col="red",
     main="Prescriptions Ordered Count ITS",
     xaxt="n")

# Add x-axis year labels #1=xaxis
axis(1, at=1:29, labels=Sum_Usage_Prescriptions_Ordered_6_21$month)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty="dotted")    

#For observed lines use fitted()
# Plot the first line segment

#first segment: line for pre-period
lines(Sum_Usage_Prescriptions_Ordered_6_21$time[1:15], fitted(model_precripordered)[1:15], col="red", lwd=2)

#second segment: lines for post-period
lines(Sum_Usage_Prescriptions_Ordered_6_21$time[16:29], fitted(model_precripordered)[16:29], col="red", lwd=2)

#Plot Counterfactual 
segments(1,
         model_precripordered$coef[1]+model_precripordered$coef[2],
         29,
         model_precripordered$coef[1]+model_precripordered$coef[2]*29,
         lty=2,
         lwd=2,
         col='red')

#Pred value 12 mo after 1st covid lockdown= time point 28
pred <- fitted(model_precripordered)[28]

#Estimate CF at same time point
cfac <- model_precripordered$coef[1] + model_precripordered$coef[2]*28

#Absolute change at final time point
pred-cfac
# = 286630.5 


#Relative change at final time point
(pred-cfac)/cfac
# = 2.250316  


# 12 months after the first lockdown, there was a increase of an average of
# 194095 NHS precriptions ordered. This represented a 152% increase.
