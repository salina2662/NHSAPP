#National ITS 

######National Logins monthly#####

#Load library
library(nlme)
library(car)
library(writexl)
library(tidyverse)
library(readxl)

options(scipen=999) #remove exp number format
 
#Import Dataset 
Sum_Usage_LoginSessions_Login_Sessions_6_21 <- read_excel("ITS National/Sum_Usage_LoginSessions_Login_Sessions_6_21.xlsx", 
                                                          +     col_types = c("date", "numeric", "numeric", 
                                                                              +         "numeric", "numeric"))
#Plot initial line plot
plot(Sum_Usage_LoginSessions_Login_Sessions_6_21$time, Sum_Usage_LoginSessions_Login_Sessions_6_21$login_count,
     ylim=c(0,17000000),
     ylab="Login Count",
     xlab="Month",
     type="l",
     col="red",
     xaxt="n") 

# Add x-axis year labels
axis(1, at=1:29, labels=Sum_Usage_LoginSessions_Login_Sessions_6_21$month)

# Add in the points for the figure
points(Sum_Usage_LoginSessions_Login_Sessions_6_21$time, Sum_Usage_LoginSessions_Login_Sessions_6_21$login_count,
       col="red",
       pch=20)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty=2)


##############Preliminary OLS Regression###########
#(Dont have to put in intercept term because it is implied)

model_logincount<-gls(login_count~time+level+trend,
                   data=Sum_Usage_LoginSessions_Login_Sessions_6_21, correlation=NULL, method="ML")

model_logincount<-lm(login_count~time+level+trend,
                      data=Sum_Usage_LoginSessions_Login_Sessions_6_21)
#summary

#summary
summary(model_logincount)

#CI for coefficients
confint(model_logincount)

#'  Call:
#'         lm(formula = login_count ~ time + level + trend, data = Sum_Usage_LoginSessions_Login_Sessions_6_21)
#' 
#' Residuals:
#'         Min       1Q   Median       3Q      Max 
#' -2488223  -360592   -59018    76467  9129451 
#' 
#' Coefficients:
#'         Estimate Std. Error t value Pr(>|t|)   
#' (Intercept)  -175074    1166156  -0.150  0.88187   
#' time           52093     128260   0.406  0.68809   
#' level       -2164386    1606569  -1.347  0.19000   
#' trend         602124     191566   3.143  0.00427 **
#'         ---
#'         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#' 
#' Residual standard error: 2146000 on 25 degrees of freedom
#' Multiple R-squared:  0.5934,	Adjusted R-squared:  0.5446 
#' F-statistic: 12.16 on 3 and 25 DF,  p-value: 0.00004227
#' 
#' 
#' 
#' 
#' 
#' #' confint(model_logincount)
#'                 2.5 %    97.5 %
#' (Intercept) -2576816.4 2226669.5
#' time         -212063.3  316249.8
#' level       -5473176.5 1144404.0
#' trend         207586.0  996661.7





############################
# Check for Autocorrelation
############################

#Durbin-watson test, 12 time periods
library(lmtest)
dwtest(model_logincount) #no autocorrelation

par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_logincount))
acf(residuals(model_logincount),type='partial')


############################
# Plot Final ITS Model
############################
options(scipen=999)

plot(Sum_Usage_LoginSessions_Login_Sessions_6_21$time, Sum_Usage_LoginSessions_Login_Sessions_6_21$login_count,
     ylim=c(0,17000000),
     ylab="Login Count",
     xlab="Month",
     main="Login Session Count ITS",
     col="red",
     xaxt="n") 

# Add x-axis year labels #1=xaxis
axis(1, at=1:29, labels=Sum_Usage_LoginSessions_Login_Sessions_6_21$month)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty="dotted")


#For observed lines use fitted()
# Plot the first line segment

#first segment: line for pre-period
lines(Sum_Usage_LoginSessions_Login_Sessions_6_21$time[1:15], fitted(model_logincount)[1:15], col="red", lwd=2)

#second segment: lines for post-period
lines(Sum_Usage_LoginSessions_Login_Sessions_6_21$time[16:29], fitted(model_logincount)[16:29], col="red", lwd=2)

#Plot Counterfactual 


segments(1,
         model_logincount$coef[1]+model_logincount$coef[2],
         29,
         model_logincount$coef[1]+model_logincount$coef[2]*29,
         lty=2,
         lwd=2,
         col='red')

#Pred value 12 mo after 1st covid lockdown= time point 28
pred <- fitted(model_logincount)[28]

#Estimate CF at same time point
cfac <- model_logincount$coef[1] + model_logincount$coef[2]*28

#Absolute change at final time point
pred-cfac
#= 5663224 
 

#Relative change at final time point
(pred-cfac)/cfac
# = 4.41

# 12 months after the first lockdown,there was an increase of an average of
# 5663224 NHS app logins. This represented a 441% increase.