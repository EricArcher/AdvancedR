
df <- read.csv("ctd.csv", stringsAsFactors = FALSE)

Rprof(interval = 0.005)

df <- df[df$depth == 1, ]
df.temp <- df$temp
df.st <- df$station

boot.mean <- sapply(1:10, function(i){
  sapply(unique(df.st), function(st) {
    temp <- df.temp[df.st == st]
    temp <- sample(temp, length(temp), replace = TRUE)
    mean(temp)
  })
})
apply(boot.mean, 1, quantile, probs = c(0.025, 0.975))

Rprof(NULL)

summaryRprof()


# Using split

Rprof(interval = 0.005)

df <- df[df$depth == 1, ]
df.temp <- df$temp
df.st <- df$station

temp.split <- split(df.temp, df.st)

boot.mean <- sapply(temp.split, function(orig.temp) {
  sapply(1:100, function(i) {
    boot.temp <- sample(orig.temp, length(orig.temp), replace = TRUE)
    mean(boot.temp)
  })
})
boot.ci <- apply(boot.mean, 2, quantile, probs = c(0.025, 0.975))

Rprof(NULL)

summaryRprof()


# using for instead of sapply

Rprof(interval = 0.0005)

df <- df[df$depth == 1, ]
df.temp <- df$temp
df.st <- df$station

temp.split <- split(df.temp, df.st)

# create container matrix
boot.mean <- matrix(nrow = length(temp.split), ncol = 1000)
rownames(boot.mean) <- names(temp.split)

# nested for loops
for(st in names(temp.split)){
  orig.temp <- temp.split[[st]]
  for(i in 1:ncol(boot.mean)) {
    boot.temp <- sample.int(orig.temp, length(orig.temp), replace = TRUE)
    boot.mean[st, i] <- mean.default(boot.temp)
  }
}

boot.ci <- apply(boot.mean, 2, quantile, probs = c(0.025, 0.975))

Rprof(NULL)

summaryRprof()



# microbenchmark profiling

bootMean1 <- function(nrep) {
  # create container matrix
  boot.mean <- matrix(nrow = length(temp.split), ncol = nrep)
  rownames(boot.mean) <- names(temp.split)

  # nested for loops
  for(st in names(temp.split)){
    orig.temp <- temp.split[[st]]
    for(i in 1:ncol(boot.mean)) {
      boot.temp <- sample.int(orig.temp, length(orig.temp), replace = TRUE)
      boot.mean[st, i] <- mean.default(boot.temp)
    }
  }
  boot.mean
}


bootMean2 <- function(nrep) {
  # create container matrix
  boot.mean <- matrix(nrow = length(temp.split), ncol = nrep)
  rownames(boot.mean) <- names(temp.split)

  # nested for loops
  for(st in names(temp.split)){
    orig.temp <- temp.split[[st]]
    for(i in 1:ncol(boot.mean)) {
      boot.temp <- sample(orig.temp, length(orig.temp), replace = TRUE)
      boot.mean[st, i] <- mean(boot.temp)
    }
  }
  boot.mean
}

library(microbenchmark)
microbenchmark(
  fun1 = bootMean1(5),
  fun2 = bootMean2(5),
  times = 10
)

# Parallel computing
library(parallel)

bootMean2.par <- function(nrep) {
  cl <- makeCluster(3)
  clusterExport(cl, "temp.split")

  boot.mean <- parSapply(cl, 1:nrep, function(i) {
    sapply(temp.split, function(orig.temp) {
      boot.temp <- sample(orig.temp, length(orig.temp), replace = TRUE)
      mean(boot.temp)
    })
  })

  stopCluster(cl)
  boot.mean
}

microbenchmark(
  boot.for = bootMean2(1000),
  boot.par = bootMean2.par(1000),
  times = 10
)
