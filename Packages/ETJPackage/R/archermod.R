

# create data.frame of all combinations of months and years
yr_mn <- expand.grid(month = 1:12, year = 1968:1970)
# create a vector of year_month combinations. this will be used to set
#   factor levels
fac.levels <- paste(yr_mn$year, yr_mn$month, sep = "_")

# create same sort of vector in data.frame
raw <- read.csv("1968TagRecaps_01FEB2018.csv", stringsAsFactors = FALSE)
str (raw)
raw$yr_mn <- paste(raw$Yr, raw$Mo, sep = "_")
head(raw)
# change it to a factor with the proper levels
raw$yr_mn <- factor(raw$yr_mn, levels = fac.levels)
head(raw)


# a table of fish observations in each year_month combination
mytable <- table(Fish = raw$Fish, YrMo = raw$yr_mn)
mytable
my.data <- as.data.frame.matrix(mytable)
my.data
names(my.data)

#create matrix with dummy fish having zero observations for every level
m <- matrix( c(0), nrow=50000, ncol=36, byrow=FALSE,
             dimnames = list(c( ),
                              c("1968_1", "1968_2", "1968_3",  "1968_4",
                                "1968_5",  "1968_6", "1968_7", "1968_8",
                                "1968_9", "1968_10", "1968_11", "1968_12",
                                "1969_1","1969_2", "1969_3", "1969_4",
                                "1969_5", "1969_6", "1969_7", "1969_8",
                                "1969_9", "1969_10", "1969_11","1969_12",
                                "1970_1", "1970_2", "1970_3", "1970_4",
                                "1970_5", "1970_6", "1970_7", "1970_8",
                                "1970_9", "1970_10","1970_11","1970_12")))
m

#combine dummy matrix to table of fish obs
fishaug <- rbind(my.data, m)
tail(fishaug)

#recode zeroes to 2s since 0 not allowed in WinBUGS
fishaug[fishaug==0] <- 2
tail(fishaug)

#analysis

#bundle data

bugs.data <- list(y=fishaug, n.occasions=dim(fishaug) [2],
  M=dim(fishaug) [1])

#initial values

inits <- function() {
  list(mean.phi=runif(1, 0, 1),
  mean.p=runif(1, 0, 1), z=cbind(rep(NA, dim(fishaug)[1]),
  fishaug[,-1]))
}

#parameters monitored
parameters <- c("mean.p", "mean.phi", "b", "Nsuper", "N", "B")

#MCMC settings

ni <- 20000
nt <- 3
nb <- 5000
nc <- 3

#Call WinBUGS from R

js.ms <- bugs(bugs.data, inits, parameters, "js-ms.bug", n.chains = nc,
              n.thin = nt, n.iter = ni, n.burnin = nb, debug = TRUE,
              bugs.directory = bugs.dir, working.directory=getwd())

print (js.ms, digits = 3)




#Winbugs code for achieving similar results above
data.fn <- function(N=100, p=0.5, T=3){
  yfull <- yobs <- array(NA, dim=c(N, T))
  for (j in 1:T){
    yfull[,j] <- rbinom(n=N, size=1, prob=p)
    }
  ever.detected <- apply(yfull, 1, max)
  C <- sum(ever.detected)
  yobs <- yfull[ever.detected == 1,]
  cat(C, "out of", N, "animals present were detected.\n")
  return(list (N = N, p = p, C = C, T = T, yfull = yfull, yobs = yobs))
  }

data <- data.fn()
str(data)
head(data)

nz <- 150
yaug <- rbind(data$yobs, array(0, dim=c(nz, data$T)))
yaug

