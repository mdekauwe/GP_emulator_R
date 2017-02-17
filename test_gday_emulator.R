# Testing a GP emulator for GDAY

library(DiceKriging)
library(ggplot2)


df_met <- read.csv(file="DUKE_met_data_amb_co2.csv", header=TRUE, sep=",")

# Drop the last year, i.e. train on first part, predict the last bit
x1 <- subset(df_met, year < 2007)
x2 <- subset(df_met, year == 2007)

keeps <- c("tair", "rain","vpd_am","vpd_pm","co2","ndep","wind","par_am","par_pm")
x1 <- x1[keeps]
x3 <- x2["doy"]
x2 <- x2[keeps]


x1 <- data.matrix(x1)


df_mod <- read.csv(file="D1GDAYDUKEAMB.csv", header=TRUE, sep=",")

# Drop the last year, i.e. train on first part, predict the last bit
y1 <- subset(df_mod, YEAR < 2007)$GPP
y2 <- subset(df_mod, YEAR == 2007)$GPP

# Fit the emaulator
gp_model <- km(~., design=data.frame(x=x1), response=y1)

# Use the emaulator to make some predictions
t <- seq(0, 365, by=1)
p <- predict(gp_model, newdata=data.frame(x=x2), type="SK")

plot(x3$doy, p$mean, type = "l", xlim = c(0, 365), ylim = c(0, 10),
     xlab = "x", ylab = "y")
lines(x3$doy, y2, col = "green", lty = 2)
#lines(x3$doy, p$lower95, col = "black", lty = 2)
#lines(x3$doy, p$upper95, col = "black", lty = 2)
points(x3$doy, y2, col = "red", pch = 19)
