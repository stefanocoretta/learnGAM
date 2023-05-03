set.seed(8788)

x <- seq(0,100,1)
y <- ((runif(1, 10, 20) * x) / (runif(1, 0, 10) + x)) + rnorm(101, 0, 1)
x_1 <- seq(0, 100, 1)
y_1 <- ((runif(1, 10, 20) * x_1) / (runif(1, 0, 100) + x_1)) + rnorm(101, 0, 1)

sim_dat <- tibble(
  x = c(x, x_1),
  y = c(y, y_1),
  group = rep(c("a", "b"), each = 101)
)

sim_dat_a <- filter(sim_dat, group == "a")

par(mfrow = c(1, 2))

k <- 5

simple <- gam(y ~ s(x, bs = "cr", k = k), data = sim_dat_a)
Knots <- simple$smooth[[1]]$xp
X <- predict(
  simple,
  newdata = data.frame(x = sim_dat_a$x),
  type = "lpmatrix"
)[,2:k]

plot(sim_dat_a$x, X[, 1], type = "n", ylim = c(-2, 3), xlab = "x", ylab = "y")
for (i in 1:ncol(X)) {
  lines(sim_dat_a$x, X[, i], col = i, lw = 1)
}
abline(v= Knots, col= 'red', lty= 2)


X2 <- X %*% diag(coef(simple)[2:k])
plot(
  sim_dat_a$x,
  predict(simple, newdata = data.frame(x = sim_dat_a$x)) -
    coef(simple)[1],
  type = "n",
  ylim = c(-8, 3),
  xlab = "x", ylab = "y"
)
for (i in 1:ncol(X2)) {
  lines(sim_dat_a$x, X2[, i], col = i, lw = 1)
}
lines(
  sim_dat_a$x,
  predict(simple, newdata = data.frame(x = sim_dat_a$x)) -
    coef(simple)[1],
  lw = 3
)
abline(v= Knots, col= 'red', lty= 2)

par(mfrow = c(1, 1))

