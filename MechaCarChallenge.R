read.csv("MechaCar_mpg.csv") #read in data on prototype vehicle specifications

#create multiple linear regression model predicting mpg and display results
summary(lm(mpg ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + AWD, data=MechaCar_mpg, y = TRUE))

#store the model in the object fit
fit <- lm(mpg ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + AWD, data=MechaCar_mpg, y = TRUE)

#plot the model diagnostics
plot(fit)

#read in data on suspension coil performance testing
Suspension_Coil <- read.csv("Suspension_Coil.csv")

#summary statistics
psi_table <- Suspension_Coil %>% summarize(mean_psi=mean(PSI),median_psi=median(PSI),variance_psi=sd(PSI)^2,stddev_psi=sd(PSI),psi_residuals=PSI-1500) #add columns to original data frame
 
#one-sample t-test vs the population mean of 1500 psi
t_vs_1500 <- t.test(Suspension_Coil$PSI, mu=1500)
print(t_vs_1500)

#summary statistics by lot
psi_by_lot <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(mean_psi=mean(PSI),median_psi=median(PSI),variance_psi=sd(PSI)^2,stddev_psi=sd(PSI),psi_residuals=mean(PSI)-1500) 

#add a variable creating two groups for t-test, Lot3 and not Lot3
Suspension_Coil <- Suspension_Coil %>% select(VehicleID, Manufacturing_Lot, PSI) %>% mutate(lot3 = Manufacturing_Lot=="Lot3")

# Two-sample t-test, Lot3 and not Lot3
g2_ttest <- t.test(Suspension_Coil$PSI[Suspension_Coil$lot3=="TRUE"],Suspension_Coil$PSI[Suspension_Coil$lot3=="FALSE"])
print(g2_ttest)
