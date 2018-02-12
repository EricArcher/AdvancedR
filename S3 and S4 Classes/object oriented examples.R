# Example data made into S3 class

df <- list(
  data = iris,
  date.time = Sys.Date()
)
class(df) <- c("irisData", class(df))

# Create plot method for irisData class
plot.irisData <- function(x) {
  plot(x$data$Sepal.Length, x$data$Sepal.Width)
}
summary.irisData <- function(x) summary(x$data)
print.irisData <- function(x) print(summary(x))

# Create nrow generic and method
nrow <- function(x) UseMethod("nrow")
nrow.irisData <- function(x) base::nrow(x$data)
# Have to create default method to ensure original function gets used
nrow.default <- function(x) base::nrow(x)


iris1 <- irisData(iris, Sys.Date())

# constructor

irisData <- function(data, date = Sys.Date()) {
  if(is.character(data)) data <- read.csv(data)
  if(!is.data.frame(data)) stop("error")
  x <- list(
    data = data,
    date = date
  )
  class(x) <- c("irisData", class(x))
  x
}

irisData <- function(data, date) UseMethod("irisData")
irisData.data.frame <- function(data, date = Sys.Date()) {
  if(!"Sepal.Length" %in% colnames(data)) stop("no Sepal.Length column")
  x <- list(
    data = data,
    date.time = date
  )
  class(x) <- c("irisData", class(x))
  x
}
irisData.character <- function(fname, date) {
  data <- read.csv(fname)
  irisData(data, date)
}


# accessors
# gets date
date <- function(x, ...) UseMethod("date")
date.irisData <- function(x) x$date.time
date.default <- function(x) {
  cat("not a 'irisData' type of object\n")
  return(NULL)
}

# sets date
"date<-" <- function(x, ...) UseMethod("date<-")
"date<-.irisData" <- function(x, value) {
  x$date.time <- value
  x
}
"date<-.default" <- function(x, value) {
  cat("not a 'irisData' type of object\n")
  return(NULL)
}

"[.irisData" <- function(x, i, j) {
  x$data[i, j]
}

"[<-.irisData" <- function(x, i, j, value) {
  x$data[i, j] <- value
  x
}

# converters
as.data.frame.irisData <- function(x) {
  x$data
}


# S4 class creation

setClass(
  Class = "irisS4",
  slots = c(
    data = "data.frame",
    date = "POSIXct",
    name = "character"
  )
)

i4 <- new("irisS4", data = iris, date = Sys.time(), name = "EIA")

# Constructor
irisS4 <- function(data, ...) UseMethod("irisS4")
irisS4.data.frame <- function(data, name, date = Sys.time()) {
  new("irisS4", data = data, date = date, name = name)
}
irisS4.character <- function(fname, name, date = Sys.time()) {
  data <- read.csv(fname)
  irisS4(data, name, date)
}

setMethod(
  f = "plot",
  signature = "irisS4",
  definition = function(x) {
    plot(x@data$Sepal.Length, x@data$Sepal.Width)
  }
)

setMethod(
  f = "show",
  signature = "irisS4",
  definition = function(object) {
    cat("Name:", object@name, "\n")
    cat("Date:", format(object@date), "\n")
    print(summary(object@data))
  }
)

setGeneric(
  name = "date",
  def = function(x, ...) standardGeneric("date")
)
setMethod("date", "irisS4", function(x) x@date)


setValidity(
  Class = "irisS4",
  method = function(object) {
    good.cols <- setequal(
      colnames(object@data),
      c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
    )
    good.date <- object@date <= Sys.time()
    if(good.cols & good.date) {
      TRUE
    } else {
      "cols or date bad"
    }
  }
)

i5 <- irisS4(iris, "EIA")

bad.iris <- iris[, -4]
i6 <- irisS4(bad.iris, "EIA")


setClass(
  "irisLocations",
  slots = c(
    name = "character",
    position = "numeric"
  )
)
setGeneric("position", function(x) standardGeneric("position"))
setMethod("position", "irisLocations", function(x) x@position)

p1 <- new("irisLocations", name = "Here", position = c(1, 2))

setClass(
  "irisMeas",
  slots = c(
    data = "data.frame"
  ),
  contains = "irisLocations"
)
im1 <- new("irisMeas", name = "There", position = c(4, 5), data = iris)
