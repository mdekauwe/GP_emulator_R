# Testing a GP emulator
# Adapted the idea from
# https://dougmcneall.com/2016/12/13/gaussian-process-emulator-example/

library(DiceKriging)

model <- function(s) {
  # https://www.sfu.ca/~ssurjano/hig02.html
  term1 <- sin(2.0 * pi * s / 10.0)
  term2 <- 0.2 * sin(2 * pi * s / 2.5)

  y <- term1 + term2
  return (y)
}

# Fit the emaulator
x <- seq(0, 10, by=0.1)
y <- model(x)
gp_model <- km(~., design=data.frame(x=x), response=y)

# Use the emaulator to make some predictions
t <- seq(0, 10, by=1)
p <- predict(gp_model, newdata=data.frame(x=t), type="SK")

plot(t, p$mean, type = "l", xlim = c(0, 10), ylim = c(-2, 2),
     xlab = "x", ylab = "y")
lines(t, p$lower95, col = "black", lty = 2)
lines(t, p$upper95, col = "black", lty = 2)
points(x, y, col = "red", pch = 19)
