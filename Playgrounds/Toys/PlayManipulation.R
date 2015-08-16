library(circular)
n <- 500
min <- 0
max <- 900
x <- floor(runif(n=n,min=min, max=max))

theta <- circular(x)
summary(theta)


plot(x, y=rep(1, n))
plot(theta, zero=pi/2, rotation="clock")
plot(theta, zero=pi/2, rotation="clock", stack=T, bins=90)