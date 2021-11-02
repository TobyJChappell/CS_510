#### Profiling Examples ####
# 12 - Optimization Lecture 

# Required libraries
library(profvis)
library(microbenchmark)

#### Microbenchmark Package examples ####

# Compare methods of calculating square roots
x <- runif(100)
microbenchmark(sqrt(x), x^0.5)

# Compare method of replacing NA's
y <- sample(1000, 10000, replace = TRUE) # Generates random integers
y[y==10] <- NA # Replaces 10 with NA (to generate sample data with NA's)
microbenchmark(y[is.na(y)] <- NA, # one line replacement
               {a <- is.na(y)     # two line replacement
                y[a] <- NA})

# Comparing byte-code compiled code
# Advanced R 17.8 (page 370)
lapply2 <- function(x, f, ...){
  out <- vector("list", length(x))
  for (i in seq_along(x)){
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

lapply2_c <- compiler::cmpfun(lapply2)

x <- list(1:10, letters, c(F, T), NULL)

microbenchmark(
  lapply2(x, is.null),
  lapply2_c(x, is.null),
  lapply(x, is.null)
)

microbenchmark(
  lapply2(x, is.integer),
  lapply2_c(x, is.integer),
  lapply(x, is.integer)
)

#### Quick example from RStudio's article: ####
# https://support.rstudio.com/hc/en-us/articles/218221837-Profiling-with-RStudio

# Run these lines first 
times <- 4e5
cols <- 150
data <-
  as.data.frame(x = matrix(rnorm(times * cols, mean = 5),
                           ncol = cols))
data <- cbind(id = paste0("g", seq_len(times)), data)

#### First Profiling example #####
profvis({
  # Store in another variable for this run
  data1 <- data
  
  # Get column means
  means <- apply(data1[, names(data1) != "id"], 2, mean)
  
  # Subtract mean from each column
  for (i in seq_along(means)) {
    data1[, names(data1) != "id"][, i] <-
      data1[, names(data1) != "id"][, i] - means[i]
  }
})

#### Second profiling example ####
profvis({
  data1 <- data
  # Four different ways of getting column means
  means <- apply(data1[, names(data1) != "id"], 2, mean)
  means <- colMeans(data1[, names(data1) != "id"])
  means <- lapply(data1[, names(data1) != "id"], mean)
  means <- vapply(data1[, names(data1) != "id"], mean, numeric(1))
})
