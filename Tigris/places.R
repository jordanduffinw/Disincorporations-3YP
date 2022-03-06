library(tigris)
library(dplyr)
library(usmap)

fipslist <- sort(fips())
dat12 <- list()

for (i in fipslist) {
  print(i)
  newdat <- NULL
  newdat <- places(i, year = 2013)
  dat12 <- c(dat12, newdat)
}

t <- unlist(dat12)
