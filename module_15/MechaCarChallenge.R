# Import tidyverse
library(tidyverse)

# Import MechaCar_mpg.csv
car_mpg <- read.csv('/Users/hunter/Desktop/Data_Class/module_15/MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)

# Import Suspension_Coil.csv
suspension <- read.csv('/Users/hunter/Desktop/Data_Class/module_15/Suspension_Coil.csv',check.names = F,stringsAsFactors = F)

# Create a multiple linear regression for car_mpg
model <- lm(mpg ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + `AWD`, data=car_mpg)
summary(model)

# Plot the linear regression model for multiple variables
# Plot length vs MPG
plot(car_mpg$`vehicle length`, car_mpg$mpg, main = 'Regression Car Length vs MPG',
    xlab = 'Vehicle Length', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ `vehicle length`,data = car_mpg), col = 'red')

# Plot Car Clearance vs MPG
plot(car_mpg$`ground clearance`, car_mpg$mpg, main = 'Regression Car Clearance vs MPG',
    xlab = 'Vehicle Clearance', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ `ground clearance`,data = car_mpg), col = 'red')

# Plot AWD vs MPG
plot(car_mpg$AWD, car_mpg$mpg, main = 'AWD vs MPG',xlab = 'AWD', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ AWD,data = car_mpg), col = 'red')

# Plot car weight vs MPG
plot(car_mpg$`vehicle weight`, car_mpg$mpg, main = 'Vehicle Weight vs MPG', 
    xlab = 'Vehicle Weight', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ `vehicle weight`,data = car_mpg), col = 'red')

# Plot spoiler angle vs MPG
plot(car_mpg$`spoiler angle`, car_mpg$mpg,
     main = 'Spoiler Angle vs MPG', xlab = 'Spoiler Angle', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ `spoiler angle`, data = car_mpg), col = 'red')

# Create suspenion summary table
suspension_summary <- suspension %>% summarize(
    Mean=mean(PSI),
    Median=median(PSI),
    Var=var(PSI),
    Standard_Deviation=sd(PSI)
)

# Suspension Coil one sample t-test
coil_sample_table <- suspension %>% sample_n(50)
t.test((coil_sample_table$PSI), mu=mean(suspension$PSI))