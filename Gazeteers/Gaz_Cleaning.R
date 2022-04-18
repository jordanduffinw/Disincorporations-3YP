setwd("C:/Users/jorda/OneDrive/3YP/Gazeteers")

##### LIBRARIES #####
library(lubridate)
library(tidyverse)
library(tis)

##### Loading Gazetteer Data #####
# gaz_1990_raw <- read_table("1990_Gaz_place_national.txt", col_names = FALSE)
# gaz_2000_raw <- read_table("2000_Gaz_place_national.txt", col_names = FALSE)
gaz_2010 <- read.delim("2010_Gaz_place_national.txt")
gaz_2013 <- read.delim("2013_Gaz_place_national.txt")
gaz_2014 <- read.delim("2014_Gaz_place_national.txt")
gaz_2015 <- read.delim("2015_Gaz_place_national.txt")
gaz_2016 <- read.delim("2016_Gaz_place_national.txt")
gaz_2017 <- read.delim("2017_Gaz_place_national.txt")
gaz_2018 <- read.delim("2018_Gaz_place_national.txt")
gaz_2019 <- read.delim("2019_Gaz_place_national.txt")
gaz_2020 <- read.delim("2020_Gaz_place_national.txt")
gaz_2021 <- read.delim("2021_Gaz_place_national.txt")

##### Cleaning 1990 and 2000 Data #####
# column_names <- colnames(gaz_2014)
#  
# gaz_1990 <- data.frame("USPS" = gaz_1990_raw$X5,
#                        "GEOID" = paste0(as.numeric(gaz_1990_raw$X1),
#                                         gaz_1990_raw$X2),
#                        "NAME" = paste(gaz_1990_raw$X3, gaz_1990_raw$X4))
# 
# gaz_2000 <- data.frame("USPS" = gaz_2000_raw$X5,
#                        "GEOID" = paste0(as.numeric(gaz_1990_raw$X1),
#                                         gaz_1990_raw$X2),
#                        "NAME" = paste(gaz_1990_raw$X3, gaz_1990_raw$X4))
# Aborted due to messiness. Maybe come back later.

##### Assigning Year Indicator #####
# gaz_1990$year <- 1990
# gaz_2000$year <- 2000
gaz_2010$year <- 2010
gaz_2013$year <- 2013
gaz_2014$year <- 2014
gaz_2015$year <- 2015
gaz_2016$year <- 2016
gaz_2017$year <- 2017
gaz_2018$year <- 2018
gaz_2019$year <- 2019
gaz_2020$year <- 2020
gaz_2021$year <- 2021

##### Dropping a couple of 2010 Indicators #####
column_names <- colnames(gaz_2014)
gaz_2010 <- subset(gaz_2010, select = column_names)

##### LISTING UNIQUE IDs #####
years <- c(2010, 2013:2021)

IDs_2010 <- unique(gaz_2010$GEOID)
IDs_2013 <- unique(gaz_2013$GEOID)
IDs_2014 <- unique(gaz_2014$GEOID)
IDs_2015 <- unique(gaz_2015$GEOID)
IDs_2016 <- unique(gaz_2016$GEOID)
IDs_2017 <- unique(gaz_2017$GEOID)
IDs_2018 <- unique(gaz_2018$GEOID)
IDs_2019 <- unique(gaz_2019$GEOID)
IDs_2020 <- unique(gaz_2020$GEOID)
IDs_2021 <- unique(gaz_2021$GEOID)

##### MERGING GAZ #####
gaz <- rbind(gaz_2010,
             gaz_2013, gaz_2014,
             gaz_2015, gaz_2016, gaz_2017,
             gaz_2018, gaz_2019, gaz_2020,
             gaz_2021)
gaz <- gaz[order(gaz$GEOID),]
write.csv(gaz, file = "gaz.csv")

##### 3/7 Mapping Changes #####
rm(list = ls())
gaz <- read.csv("gaz.csv")
gaz <- gaz %>% 
  select(X, USPS, GEOID, ANSICODE, NAME, LSAD, year) %>% 
  group_by(GEOID) %>% 
  mutate(before_LSAD = lag(LSAD, order_by = GEOID)) %>% 
  mutate(change = (LSAD != before_LSAD),
         disinc = (LSAD == 57 & before_LSAD != 57))

disincs2010s <- gaz %>% 
  filter(disinc == TRUE)
  
write.csv(disincs2010s, "disincs2010s.csv")
  
##### NEW/DROP CHANGES #####
# 2010-2013
new_IDs_2013 <- setdiff(IDs_2013, IDs_2010)
new_places_2013 <- gaz_2013[gaz_2013$GEOID %in% new_IDs_2013,]

drop_IDs_2013 <- setdiff(IDs_2010, IDs_2013)
drop_places_2013 <- gaz_2010[gaz_2010$GEOID %in% drop_IDs_2013,]

# 2013-2014
new_IDs_2014 <- setdiff(IDs_2014, IDs_2013)
new_places_2014 <- gaz_2014[gaz_2014$GEOID %in% new_IDs_2014,]

drop_IDs_2014 <- setdiff(IDs_2013, IDs_2014)
drop_places_2014 <- gaz_2013[gaz_2013$GEOID %in% drop_IDs_2014,]

# 2014-2015
new_IDs_2015 <- setdiff(IDs_2015, IDs_2014)
new_places_2015 <- gaz_2015[gaz_2015$GEOID %in% new_IDs_2015,]

drop_IDs_2015 <- setdiff(IDs_2014, IDs_2015)
drop_places_2015 <- gaz_2014[gaz_2014$GEOID %in% drop_IDs_2015,]

# 2015-2016
new_IDs_2016 <- setdiff(IDs_2016, IDs_2015)
new_places_2016 <- gaz_2016[gaz_2016$GEOID %in% new_IDs_2016,]

drop_IDs_2016 <- setdiff(IDs_2015, IDs_2016)
drop_places_2016 <- gaz_2015[gaz_2015$GEOID %in% drop_IDs_2016,]

# 2016-2017
new_IDs_2017 <- setdiff(IDs_2017, IDs_2016)
new_places_2017 <- gaz_2017[gaz_2017$GEOID %in% new_IDs_2017,]

drop_IDs_2017 <- setdiff(IDs_2016, IDs_2017)
drop_places_2017 <- gaz_2016[gaz_2016$GEOID %in% drop_IDs_2017,]

# 2017-2018
new_IDs_2018 <- setdiff(IDs_2018, IDs_2017)
new_places_2018 <- gaz_2018[gaz_2018$GEOID %in% new_IDs_2018,]

drop_IDs_2018 <- setdiff(IDs_2017, IDs_2018)
drop_places_2018 <- gaz_2017[gaz_2017$GEOID %in% drop_IDs_2018,]

# 2018-2019
new_IDs_2019 <- setdiff(IDs_2019, IDs_2018)
new_places_2019 <- gaz_2019[gaz_2019$GEOID %in% new_IDs_2019,]

drop_IDs_2019 <- setdiff(IDs_2018, IDs_2019)
drop_places_2019 <- gaz_2018[gaz_2018$GEOID %in% drop_IDs_2019,]

# 2019-2020
new_IDs_2020 <- setdiff(IDs_2020, IDs_2019)
new_places_2020 <- gaz_2020[gaz_2020$GEOID %in% new_IDs_2020,]

drop_IDs_2020 <- setdiff(IDs_2019, IDs_2020)
drop_places_2020 <- gaz_2019[gaz_2019$GEOID %in% drop_IDs_2020,]

# 2020-2021
new_IDs_2021 <- setdiff(IDs_2021, IDs_2020)
new_places_2021 <- gaz_2021[gaz_2021$GEOID %in% new_IDs_2021,]

drop_IDs_2021 <- setdiff(IDs_2020, IDs_2021)
drop_places_2021 <- gaz_2020[gaz_2020$GEOID %in% drop_IDs_2021,]

# ALL NEW/DROPS
places_new <- rbind(new_places_2013, new_places_2014, new_places_2015,
                    new_places_2016, new_places_2017, new_places_2018,
                    new_places_2019, new_places_2020, new_places_2021)

places_drop <- rbind(drop_places_2013, drop_places_2014, drop_places_2015,
                     drop_places_2016, drop_places_2017, drop_places_2018,
                     drop_places_2019, drop_places_2020, drop_places_2021)

# CLEANING ENVIRONMENT
# rm(new_places_2013, new_places_2014, new_places_2015, new_places_2016, new_places_2017,
#    new_places_2018, new_places_2019, new_places_2020, new_places_2021,
#    drop_places_2013, drop_places_2014, drop_places_2015, drop_places_2016, drop_places_2017,
#    drop_places_2018, drop_places_2019, drop_places_2020, drop_places_2021,
#    new_IDs_2013, new_IDs_2014, new_IDs_2015, new_IDs_2016, new_IDs_2017,
#    new_IDs_2018, new_IDs_2019, new_IDs_2020, new_IDs_2021,
#    drop_IDs_2013, drop_IDs_2014, drop_IDs_2015, drop_IDs_2016, drop_IDs_2017,
#    drop_IDs_2018, drop_IDs_2019, drop_IDs_2020, drop_IDs_2021)

##### PLOTTING NEW/DROP #####
# places_plotting <- data.frame("YEAR" = c(2014:2019),
#                                   "NEW" = c(nrow(new_places_2014),
#                                             nrow(new_places_2015),
#                                             nrow(new_places_2016),
#                                             nrow(new_places_2017),
#                                             nrow(new_places_2018),
#                                             nrow(new_places_2019)),
#                                  "DROP" = c(nrow(drop_places_2014),
#                                             nrow(drop_places_2015),
#                                             nrow(drop_places_2016),
#                                             nrow(drop_places_2017),
#                                             nrow(drop_places_2018),
#                                              nrow(drop_places_2019)))
# 
# fig_1 <- ggplot()+
#   geom_line(data = places_plotting, aes(x = YEAR, y = NEW), col = "BLUE")+
#   geom_line(data = places_plotting, aes(x = YEAR, y = DROP), col = "RED")+
#   labs(title = "Figure 1: New and Dropped Places from Gazeteers, 2014-2019")+
#   ylab("CHANGE")+
#   theme_bw()
# fig_1

# CLEANING ENVIRONMENT
rm(new_places_2013, new_places_2014, new_places_2015, new_places_2016, new_places_2017,
   new_places_2018, new_places_2019, new_places_2020, new_places_2021,
   drop_places_2013, drop_places_2014, drop_places_2015, drop_places_2016, drop_places_2017,
   drop_places_2018, drop_places_2019, drop_places_2020, drop_places_2021,
   new_IDs_2013, new_IDs_2014, new_IDs_2015, new_IDs_2016, new_IDs_2017,
   new_IDs_2018, new_IDs_2019, new_IDs_2020, new_IDs_2021,
   drop_IDs_2013, drop_IDs_2014, drop_IDs_2015, drop_IDs_2016, drop_IDs_2017,
   drop_IDs_2018, drop_IDs_2019, drop_IDs_2020, drop_IDs_2021)

##### TIME-SERIES TESTING [SENECA] and [NE] #####
# # Seneca
# gaz_seneca <- gaz[gaz$GEOID == 3144385,]
# 
# gaz_seneca$lag <- lead(gaz_seneca$LSAD)
# 
# for (i in 1:nrow(gaz_seneca)) {
#  if(gaz_seneca$LSAD[i] != 57 & gaz_seneca$lag[i] == 57){
#    gaz_seneca$disinc[i] <- TRUE
#  } else{
#    gaz_seneca$disinc[i] <- FALSE
#  }
# }
# 
# # NE
# gaz_NE <- gaz[gaz$USPS == "NE",]
# 
# gaz_NE <- gaz_NE %>%
#   group_by(GEOID) %>%
#   mutate(before_LSAD = lag(LSAD, order_by = GEOID)) %>%
#   mutate(disinc = (LSAD == 57 & before_LSAD != 57))


##### TIME SERIES #####
gaz <- gaz %>%
  group_by(GEOID) %>%
  mutate(before_LSAD = lag(LSAD, order_by = GEOID)) %>%
  mutate(change = (LSAD != before_LSAD),
         disinc = (LSAD == 57 & before_LSAD != 57))

gaz$disinc_dummy <- as.numeric(gaz$disinc)

disincs <- gaz[gaz$disinc_dummy == 1,]

disincs <- drop_na(disincs)

##### FILE OUTPUT #####
write.csv(disincs, file = "disincs.csv")
write.csv(places_drop, file = "drops.csv")
