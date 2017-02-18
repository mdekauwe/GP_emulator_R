# Testing a GP emulator for GDAY

library(DiceKriging)
library(ggplot2)

# Train on the first 10 years
yr = 2005
df_met <- read.csv(file="DUKE_met_data_amb_co2.csv", header=TRUE, sep=",")
x1 <- subset(df_met, year<=yr)
keep <- c("tair", "rain","vpd_am","vpd_pm","par_am","par_pm")
x1 <- x1[keep]
x1 <- data.matrix(x1)

# Get the model response
df_mod <- read.csv(file="D1GDAYDUKEAMB.csv", header=TRUE, sep=",")
y1 <- subset(df_mod, YEAR <= yr)$GPP

# Fit the emaulator
gp_model <- km(~., design=data.frame(x=x1), response=y1)

# Use the emulator to make some predictions for the whole timeseries
x2 <- df_met[keep]
x2 <- data.matrix(x2)
p <- predict(gp_model, newdata=data.frame(x=x2), type="SK")

plot(p$mean, type="l", ylim=c(0, 15), xlab="DOY", ylab="GPP")
lines(df_mod$GPP, col = "green")
