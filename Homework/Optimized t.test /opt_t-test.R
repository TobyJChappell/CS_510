#### Optimizing the t-test ####
# From Wickham's Advanced R, 17.9 (page 371)
library(microbenchmark)

m <- 1000
n <- 50
X <- matrix(rnorm(m*n, mean = 10, sd = 3), nrow = m)
grp <- rep(1:2, each = n/2)

# Using formula
system.time(for(i in 1:m) t.test(X[i, ] ~ grp)$stat)
# Using arguments
system.time(
  for(i in 1:m) t.test(X[i, grp ==1 ], X[i, grp == 2])$stat
)

# Use apply() to store calculations
compT <- function(x, grp){
  t.test(x[grp == 1], x[grp == 2])$stat
}
system.time(
  t1 <- apply(X, 1, compT, grp = grp)
)

# Optimize the t-test! The book walks you through, but it's good to 
# attempt each step on your own.
Wickhamtstat <- function(X, grp){
  t_stat <- function(X) {
    m <- rowMeans(X)
    n <- ncol(X)
    var <- rowSums((X - m) ^ 2) / (n - 1)
    list(m = m, n = n, var = var)
  }
  g1 <- t_stat(X[, grp == 1])
  g2 <- t_stat(X[, grp == 2])
  se_total <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / se_total
}

my_t <- function(X, grp){
  g1 <- X[, grp == 1]
  g1_n <- ncol(g1)
  g1_mu <- rowMeans(g1)
  g2 <- X[, grp == 2]
  g2_n <- ncol(g2)
  g2_mu <- rowMeans(g2)
  (g1_mu - g2_mu)/sqrt(rowSums((g1 - g1_mu)^2)/(g1_n*g1_n - g1_n) + rowSums((g2 - g2_mu)^2)/(g2_n*g2_n - g2_n))
}

microbenchmark(my_t(X,grp), Wickhamtstat(X,grp),  apply(X, 1, compT, grp = grp))
# See source code: 
stats:::t.test.default
