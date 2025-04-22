#Triage Try 1


#using file "pre and post data analysis civic and general data.excel".
#create an intervention and control group clean sheet in sheet 2 and sheet 4
#by copy the information and combine them into one column 
#update the date that initially put wrong year for 2023 pretrial (july-oct) 
#delete the period for fire and no physical triage in 27-Oct to 31-Oct 2023 and
#26-Aug to 29-Aug 2024 respectively



#load the dataset 
library(readxl)

# Specify the file path
file_path <- "C:/Users/yutchen/OneDrive - The Ottawa Hospital/Documents/triangle/data1.xlsx"

# Get the sheet names in the Excel file
sheet_names <- excel_sheets(file_path)

# Read data from each sheet into a list
data_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

# Assign names to the list elements
names(data_list) <- sheet_names

# Access individual sheets
intervention_data <- data_list[[2]] 
control_data <- data_list[[4]]  

# Print data from a specific sheet
print(intervention_data)


########################
# 11111control data
########################


########################
# Initial Plot
########################

# Plot the PRE- INTERVENTION
plot(intervention_data$Date[1:440],intervention_data$`LWBS %`[1:440],
     ylab="",
     ylim=c(0,0.4),
     xlab="Time",
     type="l",
     col="red",
     xaxt="n")

# Add in control group 
points(control_data$Date[1:229],control_data$`LWBS %`[1:229],
       type='l',
       col="blue")

# Add x-axis year labels
axis(1, at=1:440, labels=intervention_data$time[1:440])

# Add in the points for the figure
points(intervention_data$Date[1:440],intervention_data$`LWBS %`[1:440],
       col="red",
       pch=20)

points(control_data$Date[1:229],control_data$`LWBS %`[1:229],
       col="blue",
       pch=20)

# Label the weather change
abline(v=27.5,lty=2)

# Add in a legend
legend(x=3, y=1000, legend=c("Nile","Huron"),
       col=c("red","blue"),pch=20)

# A preliminary OLS regression
model_ols<-lm( LWBS_per ~ Date + time, data=intervention_data) 
               
          
summary(model_ols)
confint(model_ols)


# It is also useful to produce summary statistics
summary(intervention_data)
summary(control_data)
#tabulate aces before and after the smoking ban
summary(intervention_data$LWBS_per[intervention_data$intervention_ind==0])
summary(intervention_data$LWBS_per[intervention_data$intervention_ind==1])
summary(control_data$LWBS_per[control_data$intervention_ind==0])
summary(control_data$LWBS_per[control_data$intervention_ind==1])




 
  ################################
# Assessing Autocorrelation
################################

# Durbin-watson test to 12 lags
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(intervention_data$time[1:444],
     residuals(model_ols)[1:444],
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce Plots
acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')
# Note decay in ACF, significant spike at 10 in PACF, model p=10


########################
# Run the final model
########################
library(nlme)

# Ensure the data types are appropriate
intervention_data$Date <- as.Date(intervention_data$Date)
intervention_data$time <- as.numeric(intervention_data$time)

# Fit the GLS model
model_p10 <- gls(
  `LWBS %` ~ Date, 
  data = intervention_data,
  correlation = corARMA(p = 1, q = 0, form = ~ time),  # Adjust p and q as needed
  method = "ML"
)

# Check the model summary
summary(model_p10)




# Fit the GLS regression model
model_p10 <- gls(LWBS_per ~ Date, data=intervention_data,
                 correlation=corARMA(p=1,form=~time|LWBS_per),
                 method="ML")
                 
                 
                 + nile + niletime + level + trend + nilelevel + 
                   niletrend,
                 data=data,
                 correlation=corARMA(p=10,form=~time|nile),
                 method="ML")
summary(model_p10)
confint(model_p10)


########################
# Diagnostic tests
########################

# Likelihood-ratio tests to check whether the parameters of the AR process for the errors are necessary and sufficient
model_p18q19 <- update(model_p10,correlation=corARMA(q=19,p=18,form=~time|`LWBS %`))
anova(model_p10,model_p10q1)

model_p11 <- update(model_p10,correlation=corARMA(p=11,form=~time|nile))
anova(model_p10,model_p11)




########################
# Plot results
#########################

# First plot the raw data points for the Nile
plot(intervention_data$time[1:444],intervention_data$`LWBS %`[1:444],
     ylim=c(0,0.6),
     ylab="lwbs per",
     xlab="date",
     pch=20,
     col="lightblue",
     xaxt="n")

# Add x-axis year labels
?axis
axis(2)
, at=1:440, labels=intervention_data$Date[1:440]
# Label the policy change

abline(v = as.Date("2023-07-10"), lty = 2, col = "red")
abline(v = 337, lty = 2, col = "red")
abline(v = 122, lty = 2, col = "red")
abline(2023-07-10,lty=2)
?abline
# Add in the points for the control
points(control_data$time[1:444],control_data$`LWBS %`[1:444],
       col="pink",
       pch=20)

# Plot the first line segment for the intervention group
lines(data$time[1:27], fitted(model_p10)[1:27], col="blue",lwd=2)

# Add the second line segment for the intervention group
lines(data$time[28:60], fitted(model_p10)[28:60], col="blue",lwd=2)

# Add the counterfactual for the intervention group
segments(28, model_p10$coef[1] + model_p10$coef[2]*28 + model_p10$coef[3]+model_p10$coef[4]*28 + 
           model_p10$coef[5] + model_p10$coef[6],
         60, model_p10$coef[1] + model_p10$coef[2]*60 + model_p10$coef[3]+model_p10$coef[4]*60 + 
           model_p10$coef[5] + model_p10$coef[6]*33,
         lty=2,col='blue',lwd=2)

# Plot the first line segment for the control group
lines(data$time[61:87], fitted(model_p10)[61:87], col="red",lwd=2)

# Add the second line segment for the control
lines(data$time[88:120], fitted(model_p10)[88:120], col="red",lwd=2)

# Add the counterfactual for the control group
segments(1, model_p10$coef[1]+model_p10$coef[2],
         60,model_p10$coef[1]+model_p10$coef[2]*60,
         lty=2,col='red',lwd=2)

# Add in a legend
legend(x=3, y=1000, legend=c("Nile","Huron"), col=c("blue","red"),pch=20)


##############################################
# Predict absolute and relative changes
##############################################

# Predicted value at 25 years after the weather change
pred <- fitted(model_p10)[52]

# Estimate the counterfactual at the same time point
cfac <- model_p10$coef[1] + model_p10$coef[2]*52 +
  model_p10$coef[3] + model_p10$coef[4]*52 +
  model_p10$coef[5] + model_p10$coef[6]*25

# Absolute change at 25 years
pred - cfac
# Relative change at 25 years
(pred - cfac) / cfac





wild pttttttt

# Load the necessary libraries
###################################

library(nlme)
library(car)


########################
# Read in the dataset
########################

# Two options: either use "import dataset" in RStudio, or use read.csv
data <- read.csv("/Users/michaellaw/Dropbox/edX/Course Materials/Course Datasets/nile_week_4.csv",header=T)

# Alternative method to create a new variable indicating the wild point
data$drought <- rep(0,60)
data$drought[43] <- 1


########################
# Preliminary Analysis
########################

# Fit the OLS regression model
model_ols <- lm(flow ~ time + level + trend + drought, data=data)
summary(model_ols)


########################
# Modeling
########################

# Fit the GLS regression model with p=10 as in Week 2
model_p10 <- gls(flow ~ time + level + trend + drought,
                 data=data,
                 correlation=corARMA(p=10,form=~time),
                 method="ML")
summary(model_p10)


########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(data$time,data$flow,
     ylim=c(0,4500),
     ylab="Water Flow",
     xlab="Year",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:60, labels=data$year)

# Add line indicating weather pattern change
abline(v=27.5,lty="dotted")

# Plot the first line segment
lines(data$time[1:27], fitted(model_p10)[1:27], col="red",lwd=2)

# Plot the second line segment - Note what happens!
lines(data$time[28:60], fitted(model_p10)[28:60], col="red",lwd=2)

# An alternative using model coefficients
segments(28,
         model_p10$coef[1] + model_p10$coef[2]*28 +
           model_p10$coef[3] + model_p10$coef[4],
         60,
         model_p10$coef[1] + model_p10$coef[2]*60 +
           model_p10$coef[3] + model_p10$coef[4]*33,
         lty=1,
         lwd=2,
         col='red')

# And the counterfactual
segments(1,
         model_p10$coef[1]+model_p10$coef[2],
         60,
         model_p10$coef[1]+model_p10$coef[2]*60,
         lty=2,
         lwd=2,
         col='red')



seasonal changeeeee
?lines

#lines###############################################
# ITSx: Week 4: Seasonality
# Michael Law (michael.law@ubc.ca)
# October 2015
################################################

###################################
# Load the necessary libraries
###################################

library(nlme)
library(car)


########################
# Read in the dataset
########################

# Two options: either use "import dataset" in RStudio, or use read.csv
data <- read.csv("/Users/michaellaw/Dropbox/edX/Course Materials/Course Datasets/nile_week_4.csv",header=T)

# Alternative method to create a new variable indicating the wild point
data$drought <- rep(0,60)
data$drought[43] <- 1


########################
# Modeling
########################

# Fit the GLS regression model with p=10 as in Week 2
model_p10 <- gls(flow ~ time + level + trend + drought + elnino,
                 data=data,
                 correlation=corARMA(p=10,form=~time),
                 method="ML")
summary(model_p10)


###############################
# Plot results - Approach 1
###############################

# Produce the plot, first plotting the raw data points
plot(data$time,data$flow,
     ylim=c(0,4500),
     ylab="Water Flow",
     xlab="Year",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:60, labels=data$year)

# Add line indicating weather pattern change
abline(v=27.5,lty="dotted")

# Plot the first line segment
lines(data$time[1:27], fitted(model_p10)[1:27], col="red",lwd=2)

# Plot the second line segment
lines(data$time[28:60], fitted(model_p10)[28:60], col="red",lwd=2)


###############################
# Plot results - Approach 2
###############################

# Produce the plot, first plotting the raw data points
plot(data$time,data$flow,
     ylim=c(0,4500),
     ylab="Water Flow",
     xlab="Year",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:60, labels=data$year)

# Add line indicating weather pattern change
abline(v=27.5,lty="dotted")

# Calculate the offset due to El Nino events
offset <- mean(data$elnino) * model_p10$coef[6]

# Plot the first line segment
segments(1,
         model_p10$coef[1] + model_p10$coef[2] + offset,
         27,
         model_p10$coef[1] + model_p10$coef[2]*27 + offset,
         lty=1, lwd=2, col='red')

# Plot the second line segment
segments(28,
         model_p10$coef[1] + model_p10$coef[2]*28 + 
           model_p10$coef[3] + model_p10$coef[4] + offset,
         60,
         model_p10$coef[1] + model_p10$coef[2]*60 + 
           model_p10$coef[3] + model_p10$coef[4]*33 + offset,
         lty=1, lwd=2, col='red')

# Plot the counterfactual
segments(1,
         model_p10$coef[1]+model_p10$coef[2] + offset,
         60,
         model_p10$coef[1]+model_p10$coef[2]*60 + offset,
         lty=2, lwd=2, col='red')

# END



phasea innnnnnnn
################################################
# ITSx: Week 4: Phase-in Periods
# Michael Law (michael.law@ubc.ca)
# October 2015
################################################

###################################
# Load the necessary libraries
###################################

library(nlme)
library(car)


########################
# Read in the dataset
########################

# Two options: either use "import dataset" in RStudio, or use read.csv
data <- read.csv("/Users/michaellaw/Dropbox/edX/Course Materials/Course Datasets/nile_phase_in.csv",header=T)


########################
# Initial Plot
########################

# Plot outcome variable versus time
plot(data$time,data$flow,
     ylab="Water Flow of the Nile",
     ylim=c(0,4500),
     xlab="Year",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:35, labels=data$year)

# Add in the points for the figure
points(data$time,data$flow,
       col="red",
       pch=20)

# Label the upstream dam build
abline(v=14.5,lty=2)


#########################
# Create New Dataset
#########################

# Make a vector of the rows we want to include
include <- c(1:14,19:35)

# Duplicate these rows into a new dataset
data_pi <- data[include,]

# Correct the trend variable in the new dataset
data_pi$trend[15:31] <- data_pi$trend[15:31] - 4


########################
# Modeling
########################

# A preliminary OLS regression
model_ols <- lm(flow ~ time + level + trend, data=data_pi)
summary(model_ols)


################################
# Assessing Autocorrelation
################################

# Durbin-watson test, 12 time periods
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(data_pi$time,
     residuals(model_ols),
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')
# Note decay in ACF, significant spike at 4 in PACF, model p=4

# Reset the plot
par(mfrow=c(1,1))


########################
# Modeling
########################

# Fit the GLS regression model
model_p4 <- gls(flow ~ time + level + trend,
                data=data_pi,
                correlation=corARMA(p=4,form=~time),
                method="ML")
summary(model_p4)


########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(data$time,data$flow,
     ylim=c(0,4500),
     ylab="Water Flow",
     xlab="Year",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:35, labels=data$year)

# Add line indicating upstream dam
abline(v=14.5,lty=2)

# Plot the first line segment
lines(data$time[1:14], fitted(model_p4)[1:14], col="red",lwd=2)

# Plot the second line segment
lines(data$time[19:39], fitted(model_p4)[15:35], col="red",lwd=2)

# And the counterfactual
segments(19, model_p4$coef[1]+model_p4$coef[2]*19,
         35, model_p4$coef[1]+model_p4$coef[2]*35,
         lty=2, lwd=2, col='red')

# Add a box to show phase-in period
rect(14.5,-500,18.5,5000 , border = NA, col= '#00000011')

# END



2 interventionnnnnn


###########################################
# ITSx: Week 4 Multiple Interventions
# Michael Law (michael.law@ubc.ca)
# October 2015
###########################################

###################################
# Load the necessary libraries
###################################

library(nlme)
library(car)


########################
# Read in the dataset
########################

# Two options: either use "import dataset" in RStudio, or use read.csv
data <- read.csv("/Users/michaellaw/Dropbox/edX/Course Materials/Course Datasets/teststrips_multiple.csv",header=T)


########################
# Initial Plot
########################

# Plot outcome variable versus time
plot(data$time,data$strips_pt,
     ylab="Test Strips per 1,000 people",
     ylim=c(0,300),
     xlab="Month",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:72, labels=data$yearmonth)

# Add in the points for the figure
points(data$time,data$strips_pt,
       col="red",
       pch=20)

# Label the policy changes
abline(v=30.5,lty=2)
abline(v=56.5,lty=2)


########################
# Modeling
########################

# A preliminary OLS regression
model_ols <- lm(strips_pt ~ time + cerc + cerc_trend + cda + cda_trend, data=data)
summary(model_ols)


################################
# Assessing Autocorrelation
################################

# Durbin-watson test, 12 time periods
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(residuals(model_ols),
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')
# Spike in PACF at 4, exp decay in ACF, model with p=4

# Reset plot window
par(mfrow=c(1,1))

########################
# Run the final model
########################

# Fit the GLS regression model
model_p4 <- gls(strips_pt ~ time + cerc + cerc_trend + cda + cda_trend,
                data=data,
                correlation=corARMA(p=4,form=~time),
                method="ML")
summary(model_p4)


########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(data$time,data$strips_pt,
     ylab="Test Strips per 1,000 people",
     ylim=c(0,300),
     xlab="Month",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:72, labels=data$yearmonth)

# Add line indicating the policy changes
abline(v=30.5,lty=2)
abline(v=56.5,lty=2)

# Plot the first line segment
lines(data$time[1:30], fitted(model_p4)[1:30], col="red",lwd=2)

# Plot the second line segment
lines(data$time[31:56], fitted(model_p4)[31:56], col="red",lwd=2)

# Plot the third line segment
lines(data$time[57:72], fitted(model_p4)[57:72], col="red",lwd=2)

# And the first counterfactual
segments(31, model_p4$coef[1]+model_p4$coef[2]*31,
         56, model_p4$coef[1]+model_p4$coef[2]*56,
         lty=2, lwd=2, col='red')

# And the second counterfactual
segments(57, model_p4$coef[1] + model_p4$coef[2]*57 +
           model_p4$coef[3] + model_p4$coef[4]*27,
         72, model_p4$coef[1] + model_p4$coef[2]*72 +
           model_p4$coef[3] + model_p4$coef[4]*42,
         lty=2, lwd=2, col='red')

# END




quadritic trenddddd


################################################
# ITSx: Week 4: Non-linear Trends
# Michael Law (michael.law@ubc.ca)
# October 2015
################################################

###################################
# Load the necessary libraries
###################################

library(nlme)
library(car)


########################
# Read in the dataset
########################

# Two options: either use "import dataset" in RStudio, or use read.csv
data <- read.csv("/Users/michaellaw/Dropbox/edX/Course Materials/Course Datasets/thailand.csv",header=T)


########################
# Initial Plot
########################

# Plot outcome variable versus time
plot(data$time,data$stdunits,
     ylab="Standard Units of Insulin per 1,000 population",
     ylim=c(0,14),
     xlab="Quarter",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:34, labels=data$yearqtr)

# Add in the points for the figure
points(data$time,data$stdunits,
       col="red",
       pch=20)

# Label the policy change
abline(v=12.5,lty=2)

# Plot the phase-in period
rect(12.5,-5,15.5,20 , border = NA, col= '#00000011')


###########################################
# Create New Dataset to include phase-in
###########################################

# Make a vector of the rows we want to include
include <- c(1:12,16:34)

# Duplicate these rows into a new dataset
data_pi <- data[include,]

# Correct the trend and square variable in the new dataset
data_pi$trend[13:31] <- data_pi$trend[13:31] - 3
data_pi$trendsq <- data_pi$trend^2


#############################
# Modeling - with square term
#############################

# A preliminary OLS regression
model_ols <- lm(stdunits ~ time + level + trend + trendsq, data=data_pi)
summary(model_ols)


################################
# Assessing Autocorrelation
################################

# Durbin-watson test, 12 time periods
dwt(model_ols,max.lag=12,alternative="two.sided")

# Graph the residuals from the OLS regression to check for serially correlated errors
plot(data_pi$time,
     residuals(model_ols),
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Plot ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')
# No significant autocorrelation, model p=0, q=0

# Reset the plot
par(mfrow=c(1,1))


########################
# Modeling
########################

# Fit the GLS regression model with square term
model_sq <- gls(stdunits ~ time + level + trend + trendsq,
                data=data_pi,
                method="ML")
summary(model_sq)


########################
# Plot results
#########################

# Produce the plot, first plotting the raw data points
plot(data$time,data$stdunits,
     ylim=c(0,14),
     ylab="Standard Units of Insulin per 1,000 population",
     xlab="Quarter",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:34, labels=data$yearqtr)

# Add line indicating upstream dam
abline(v=12.5,lty=2)

# Plot the first line segment
lines(data$time[1:12], fitted(model_sq)[1:12], col="red",lwd=2)

# Plot the second line segment
lines(data$time[16:34], fitted(model_sq)[13:31], col="red",lwd=2)

# And the counterfactual
segments(16, model_sq$coef[1] + model_sq$coef[2]*16,
         34, model_sq$coef[1] + model_sq$coef[2]*34,
         lty=2, lwd=2, col='red')

# Add a box to show phase-in period
rect(12.5,-5,15.5,20 , border = NA, col= '#00000011')
