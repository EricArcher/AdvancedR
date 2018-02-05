# etmason@ucsd.edu

# original data
df <- data.frame(
  fish = sample(1:3, 100, replace = T),
  month = sample(c(1:10, 12), 100, replace = T),
  year = sample(1990:1999, 100, replace = T)
)

# --- Option 1: use table()

# create factor for months to make sure all levels are present
df$month <- factor(df$month, levels = 1:12)
# create factor for years to make sure all levels are present
df$year <- factor(df$year, levels = 1990:1999)

# create a frequency table then convert it to a data.frame
freq <- table(month = df$month, year = df$year)
freq.df <- as.data.frame(freq)


# --- Option 2: use expand.grid()

# create data.frame of all combinations of months and years
yr_mn <- expand.grid(month = 1:12, year = 1990:1999)
# create a vector of year_month combinations. this will be used to set 
#   factor levels
fac.levels <- paste(yr_mn$year, yr_mn$month, sep = "_")

# create same sort of vector in data.frame
df$yr_mn <- paste(df$year, df$month, sep = "_")
# change it to a factor with the proper levels
df$yr_mn <- factor(df$yr_mn, levels = fac.levels)

# a table of fish observations in each year_month combination
table(date = df$yr_mn, fish = df$fish)

