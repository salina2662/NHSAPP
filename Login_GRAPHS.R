#Login Count Graphs

options(scipen=999) #remove exp number format
par(mfrow=c(1,2))
dev.off()
#####################Count###########################
plot(Sum_Usage_LoginSessions_Login_Sessions_6_21$time, Sum_Usage_LoginSessions_Login_Sessions_6_21$login_count,
     ylim=c(0,17000000),
     ylab="Login Sessions",
     xlab="Month",
     bty="o", pch=19, col="blue",
     lwd="2",
     main="NHS App Login Sessions January 2019-May 2021",
     type="l",
     xaxt="n",
     panel.first=grid())

# Add x-axis year labels
axis(1, at=1:29, labels=Sum_Usage_LoginSessions_Login_Sessions_6_21$month)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15,lty=2, col="black")
text( 15, 17000000, "1st lockdown", col="black", cex=1.3, pos=4 )


#############ITS##############################
plot(Sum_Usage_LoginSessions_Login_Sessions_6_21$time, Sum_Usage_LoginSessions_Login_Sessions_6_21$login_count,
     type="n",
     ylim=c(0,17000000),
     ylab="Login Sessions",
     xlab="Month",
     main="NHS App Login Sessions ITS",
     col="red",
     bty="o",
     xaxt="n",
     panel.first=grid())

rect(15.5,0,30,17000000,col=grey(0.9),border=F)


# Add in the points for the figure
points(Sum_Usage_LoginSessions_Login_Sessions_6_21$time, Sum_Usage_LoginSessions_Login_Sessions_6_21$login_count,
       col="red",
       pch=20)


# Add x-axis year labels #1=xaxis
axis(1, at=1:29, labels=Sum_Usage_LoginSessions_Login_Sessions_6_21$month)

# Label 1st lockdown  #1st pt in (time) after lockdown announced: April 1, 2020
abline(v=15.5,lty="dotted", col="black")
text( 15.5, 17000000, "1st lockdown", col="black", cex=1.3, pos=4 )



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


