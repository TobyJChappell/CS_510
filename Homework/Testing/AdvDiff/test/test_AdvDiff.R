library(testthat)

context("testing sample")
source("../src/diffuse.R")
source("../src/advect.R")

D <- 1e-8
delta.t <- 1e-5
end.time <- 0.01
start.x <- 0
start.y <- 0
num.dots <- 1000
dotsx <- rep(start.x, num.dots)
dotsy <- rep(start.y, num.dots)
dots <- matrix(c(dotsx, dotsy), num.dots, 2)

x <- as.matrix(read.table("../data/x.csv", header = FALSE, sep = ","))
y <- as.matrix(read.table("../data/y.csv", header = FALSE, sep = ","))
Ux <- as.matrix(read.table("../data/Ux.csv", header = FALSE, sep = ","))
Uy <- as.matrix(read.table("../data/Uy.csv", header = FALSE, sep = ","))
Ux[is.na(Ux)] <- 0
Uy[is.na(Uy)] <- 0

no_diffusion <- diffuse(dots, 0, delta.t)-dots
test_that("matrix", {
  expect_equal(no_diffusion, 
               matrix(no_diffusion[1,],nrow=1000,ncol=2))
})

test_that("integer", {
  expect_equal(sum(advect(dots, x, y, Ux, Uy, 0.5*delta.t)**2),
               4*D*delta.t)
})

