# Title     : TODO
# Objective : TODO
# Created by: Костя
# Created on: 04.12.2020
x1 <- seq(-0.5, 0, 0.1)
x2 <- seq(0, pi, 0.1)
y1 <- acos(x1) - x1
y2 <- sin(2 * x2)
plot(x1, y1, ylim = range(c(y1, y2)), xlim=range(c(x1,x2)), type="l", col="red")
lines(x2, y2, col="blue")
