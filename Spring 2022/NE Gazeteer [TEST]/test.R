setwd("C:/Users/jorda/OneDrive/3YP/NE Gazeteer [TEST]")

dat_2020 <- read.delim("2020_Gaz_place_national.txt")
dat_2010 <- read.delim("2010_Gaz_places_national.txt")

dat_2020$year <- 2020
dat_2010$year <- 2010

### New Places
new_IDs <- setdiff(unique(dat_2020$ANSICODE), unique(dat_2010$ANSICODE))
new_places <- dat_2020[dat_2020$ANSICODE %in% new_IDs,]

drop_IDs <- setdiff(unique(dat_2010$ANSICODE), unique(dat_2020$ANSICODE))
drop_places <- dat_2010[dat_2010$ANSICODE %in% drop_IDs,]

# CDP_20 <- dat_2020[dat_2020$LSAD == "57",]
# CDP_10 <- dat_2010[dat_2010$LSAD == "57",]
# 
# CDP_IDs_20 <- unique(CDP_20$ANSICODE)
# CDP_IDs_10 <- unique(CDP_10$ANSICODE)
# 
# new_CDP_IDS <- setdiff(CDP_IDs_20, CDP_IDs_10)
# new_CDP_dat <- dat_2020[dat_2020$ANSICODE %in% new_CDP_IDS,]
# 
# drop_CDP_IDS <- setdiff(CDP_IDs_10, CDP_IDs_20)
# drop_CDP_dat <- dat_2010[dat_2010$ANSICODE %in% drop_CDP_IDS,]
