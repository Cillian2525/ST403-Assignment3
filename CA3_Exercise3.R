### Exercise 3.1

# 1 Load olive data and prepend "mydf" to its class
library(IMIFA)
class(olive) <- c("mydf", class(olive))

# 2 S3 method for print.mydf()
print.mydf <- function(x, n = 5, ...) {
  
  # warn if n exceeds dimensions
  if (n > nrow(x) | n > ncol(x)) {
    warning("n exceeds data dimensions; printing available rows/columns only.")
  }
  
  # how many rows/cols will print
  nr <- min(n, nrow(x))
  nc <- min(n, ncol(x))
  
  message("Printing first ", nr, " rows and first ", nc, " columns:")
  
  # call print.data.frame() directly to avoid S3 recursion
  print.data.frame(x[1:nr, 1:nc, drop = FALSE], ...)
}

print(olive)

# 3 [rint all but the first row
print(olive[-1, ])

# 4 Generic max_dens() and method for density objects
max_dens <- function(x, ...) {
  UseMethod("max_dens")
}

max_dens.density <- function(x, ...) {
  i <- which.max(x$y)
  c(x = x$x[i], y = x$y[i])
}

# 5 Get peak locations/heights for numeric variables
num_vars <- sapply(olive, is.numeric)

peak_matrix <- vapply(
  olive[, num_vars],
  FUN = function(v) {
    d <- density(v, kernel = "epanechnikov")
    max_dens(d)
  },
  FUN.VALUE = c(x = numeric(1), y = numeric(1))
)

peak_matrix



### Exercise 3.2

# 1 write functions
f <- function(x) x + 3
g <- function(x) x^2 - 4

# 2 Visualise both
curve(f, from = -4, to = 4, ylim = c(-6, 12))
curve(g, from = -4, to = 4, add = TRUE)

# 3 Find intersection points
root1 <- uniroot(function(x) f(x) - g(x), interval = c(-4, 0), tol = 1e-08)$root
root2 <- uniroot(function(x) f(x) - g(x), interval = c(0, 4), tol = 1e-08)$root

# 4 Get area enclosed
area <- integrate(function(x) f(x) - g(x), lower = root1, upper = root2)$value
area



### Exercise 3.3

# 1  regression data
df <- with(mtcars, data.frame(
  y = mpg,
  x1 = disp,
  x2 = hp,
  x3 = wt
))

# 2 Negative log-likelihood
nll_lm <- function(data, par) {
  beta <- par[1:4]
  sigma <- par[5]
  
  X <- as.matrix(cbind(1, data[, c("x1", "x2", "x3")]))
  mu <- drop(X %*% beta)
  
  eps <- data$y - mu
  
  -sum(dnorm(eps, mean = 0, sd = sigma, log = TRUE))
}

# 3 Use optim() with Hessian
init_par <- c(
  mean(df$y),
  0, 0, 0,
  sd(df$y)
)

fit <- optim(
  par = init_par,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = c(-Inf, -Inf, -Inf, -Inf, 1e-6),
  upper = c(Inf, Inf, Inf, Inf, Inf),
  hessian = TRUE
)

mle_par <- fit$par
mle_par

# 5 Matrix beta-hat
X <- as.matrix(cbind(1, df[, c("x1", "x2", "x3")]))
beta_hat_matrix <- solve(t(X) %*% X) %*% t(X) %*% df$y
beta_hat_matrix

# 6 Matrix sigma-hat
res <- df$y - drop(X %*% beta_hat_matrix)
sigma_hat_matrix <- sqrt(mean(res^2))
sigma_hat_matrix

# 7 Standard errors via Hessian
se_coef <- sqrt(diag(solve(fit$hessian)))[1:4]
se_coef