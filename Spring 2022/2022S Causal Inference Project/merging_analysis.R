##### LIBRARIES #####
library(tidyverse)
library(stringr)
library(stargazer)

##### RAW DATA #####
# Disincs
raw_disinc <- read.csv("Data/disincs.csv")

# MIT Election data
raw_votes <- read.csv("Data/countypres_2000-2020.csv")

# ACS Population data
raw_county_pop <- read.csv("Data/co-est2020-alldata.csv")

raw_places_2012 <- read.csv("Data/places_2012.csv")
raw_places_2016 <- read.csv("Data/places_2016.csv")
raw_places_2020 <- read.csv("Data/places_2020.csv")

##### CLEANING #####
# Disincs
dat_disinc <- raw_disinc %>% 
  filter(referendum == TRUE,
         year_nextpres > 2008) %>% # dropping places that were dissolved by a non-referendum or after 2009
  mutate(# place_id = as.numeric(place_id ),
         prop_yes = votesyes / (votesyes + voteno),
         margin = prop_yes - threshold,
         disinc = case_when(
           margin > 0 ~ 1,
           margin < 0 ~ 0
         ),
         turnout_disinc = (votesyes + voteno) / pop_dis)
rm(raw_disinc)

# Voting data
dat_votes <- raw_votes %>% 
  filter(year %in% c(2012, 2016, 2020),
         county_fips %in% unique(county_fips),
         candidate == "OTHER") %>% 
  select(year,
         state_po,
         county_name,
         county_fips,
         totalvotes)
rm(raw_votes)

# ACS population data
dat_pop_county <- raw_county_pop %>% 
  summarise(county_fips = (1000 * STATE) + COUNTY,
            state = STNAME,
            county = CTYNAME,
            county_pop_2012 = POPESTIMATE2012,
            county_pop_2016 = POPESTIMATE2016,
            county_pop_2020 = POPESTIMATE2020)
rm(raw_county_pop)

dat_pop_2012 <- raw_places_2012 %>% 
  summarise(place_id = str_sub(GEO_ID, -7, -1),
            name = NAME,
            pop_2012 = S0101_C01_001E) %>% 
  filter(place_id != "id")
rm(raw_places_2012)

dat_pop_2016 <- raw_places_2016 %>% 
  summarise(place_id = str_sub(GEO_ID, -7, -1),
            name = NAME,
            pop_2016 = S0101_C01_001E) %>% 
  filter(place_id != "id")
rm(raw_places_2016)

dat_pop_2020 <- raw_places_2020 %>% 
  summarise(place_id = str_sub(GEO_ID, -7, -1),
            name = NAME,
            pop_2020 = S0101_C01_001E) %>% 
  filter(place_id != "id")
rm(raw_places_2020)

##### MERGING #####
### 2012 ###
dat_disinc_2012 <- dat_disinc %>% 
  filter(year_nextpres == 2012)

dat_votes_2012 <- dat_votes %>% 
  filter(year == 2012)

alldat_2012 <- left_join(dat_disinc_2012,
                         dat_pop_2012,
                         by = c("place_id")) %>% 
  summarise(state = state,
            county_fips = county_fips,
            place_id = place_id,
            name = name.x,
            year_dis = year_dis,
            pop_dis = pop_dis,
            margin = margin,
            threshold = threshold,
            turnout_disinc = turnout_disinc,
            disinc = disinc,
            pop = as.numeric(pop_2012))

alldat_2012 <- left_join(alldat_2012,
                         dat_pop_county,
                         by = c("county_fips")) %>%
  mutate(county_pop = county_pop_2012,
         place_prop = pop / county_pop) %>% 
  select(-county_pop_2012,
         -county_pop_2016,
         -county_pop_2020)


alldat_2012 <- left_join(alldat_2012,
                         dat_votes_2012,
                         by = c("county_fips")) %>% 
  mutate(year_elec = year,
         turnout_pres = totalvotes / county_pop,
         turnout_pres_weighted = turnout_pres * place_prop) %>% 
  select(-year,
         -state_po,
         -county_name,
         -state.y,
         -county)

### 2016 ###
dat_disinc_2016 <- dat_disinc %>% 
  filter(year_nextpres == 2016)

dat_votes_2016 <- dat_votes %>% 
  filter(year == 2016)

alldat_2016 <- left_join(dat_disinc_2016,
                         dat_pop_2016,
                         by = c("place_id")) %>% 
  summarise(state = state,
            county_fips = county_fips,
            place_id = place_id,
            name = name.x,
            year_dis = year_dis,
            pop_dis = pop_dis,
            margin = margin,
            threshold = threshold,
            turnout_disinc = turnout_disinc,
            disinc = disinc,
            pop = as.numeric(pop_2016))

alldat_2016 <- left_join(alldat_2016,
                         dat_pop_county,
                         by = c("county_fips")) %>%
  mutate(county_pop = county_pop_2016,
         place_prop = pop / county_pop) %>% 
  select(-county_pop_2012,
         -county_pop_2016,
         -county_pop_2020)


alldat_2016 <- left_join(alldat_2016,
                         dat_votes_2016,
                         by = c("county_fips")) %>% 
  mutate(year_elec = year,
         turnout_pres = totalvotes / county_pop,
         turnout_pres_weighted = turnout_pres * place_prop) %>% 
  select(-year,
         -state_po,
         -county_name,
         -state.y,
         -county)

### 2020 ###
dat_disinc_2020 <- dat_disinc %>% 
  filter(year_nextpres == 2020)

dat_votes_2020 <- dat_votes %>% 
  filter(year == 2020)

alldat_2020 <- left_join(dat_disinc_2020,
                         dat_pop_2020,
                         by = c("place_id")) %>% 
  summarise(state = state,
            county_fips = county_fips,
            place_id = place_id,
            name = name.x,
            year_dis = year_dis,
            pop_dis = pop_dis,
            margin = margin,
            threshold = threshold,
            turnout_disinc = turnout_disinc,
            disinc = disinc,
            pop = as.numeric(pop_2020))

alldat_2020 <- left_join(alldat_2020,
                         dat_pop_county,
                         by = c("county_fips")) %>%
  mutate(county_pop = county_pop_2020,
         place_prop = pop / county_pop) %>% 
  select(-county_pop_2012,
         -county_pop_2016,
         -county_pop_2020)


alldat_2020 <- left_join(alldat_2020,
                         dat_votes_2020,
                         by = c("county_fips")) %>% 
  mutate(year_elec = year,
         turnout_pres = totalvotes / county_pop,
         turnout_pres_weighted = turnout_pres * place_prop) %>% 
  select(-year,
         -state_po,
         -county_name,
         -state.y,
         -county) %>% 
  distinct()

### Merge All Data ###
alldat <- rbind(alldat_2012,
                alldat_2016,
                alldat_2020) %>% 
  summarise(name = name,
            state = state.x,
            county_fips = county_fips,
            place_id = place_id,
            year_dis = year_dis,
            year_elec = year_elec,
            years_since = year_elec - year_dis,
            pop_dis = pop_dis,
            threshold = threshold,
            margin = margin,
            disinc = disinc,
            turnout_disinc = turnout_disinc,
            pop = pop,
            county_pop = county_pop,
            place_prop = place_prop,
            totalvotes = totalvotes,
            turnout_pres = turnout_pres,
            turnout_pres_weighted = turnout_pres_weighted) %>% 
  drop_na()

rm(list = setdiff(ls(), "alldat"))

##### Bandwidths #####
alldat_10 <- alldat[alldat$margin < 0.10 & alldat$margin > -0.10,]

##### ANALYSIS #####
##### SUMMARY STATISTICS #####
stargazer(alldat[c(8, 10:12, 17)],
          type = "text",
          title = "Disincorporation Attempts since 2010",
          covariate.labels = c(
                               "Pop. at Attempt",
                               "Attempt Margin",
                               "Disincorporated",
                               "Attempt Turnout",
                               "Subsequent General Election Turnout"),
          out = "Tables/table01.tex"
          )

stargazer(as.data.frame(alldat %>% 
                          group_by(disinc) %>% 
                          summarise(n = length(disinc),
                                    pop_dis = mean(pop_dis),
                                    turnout_disinc = mean(turnout_disinc),
                                    )),
          summary = FALSE,
          rownames = FALSE,
          type = "text",
          title = "Balance on 'Treatment'",
          covariate.labels = c("Disincorporated",
                               "N",
                               "Population at Attempt",
                               "Turnout at Attempt"),
          out = "Tables/table02.tex")

ggplot(data = alldat, aes(x = margin,
                       y = turnout_pres))+
  geom_point()+
  geom_vline(aes(xintercept = 0,), linetype = "dashed")+
  geom_smooth(data = alldat[alldat$margin < 0,],
              col = "red", method = "lm")+
  geom_smooth(data = alldat[alldat$margin > 0,], method = "lm")+
  labs(title = "Disincorporation and Voter Turnout",
       x = "Disincorporation Vote Margin",
       y = "Turnout in Subsequent Presidential Election")+
  theme_minimal()
ggsave(filename = "Tables/fig1.png", plot = last_plot())

ggplot(data = alldat_10, aes(x = margin,
                          y = turnout_pres))+
  geom_point()+
  geom_vline(aes(xintercept = 0,), linetype = "dashed")+
  geom_smooth(data = alldat_10[alldat_10$margin < 0,],
              col = "red", method = "lm")+
  geom_smooth(data = alldat_10[alldat_10$margin > 0,], method = "lm")+
  labs(title = "Disincorporation and Voter Turnout",
       subtitle = "Bandwidth 10%",
       x = "Disincorporation Vote Margin",
       y = "Turnout in Subsequent Presidential Election")+
  theme_minimal()
ggsave(filename = "Tables/fig2.png", plot = last_plot())


stargazer(alldat_10[7:16],
          type = "text",
          title = "Disincorporation Attempts since 2010 -- Bandwith 10%",
          covariate.labels = c("Years since Attempt",
                               "Pop. at Attempt",
                               "Attempt Margin",
                               "Disincorporated",
                               "Attempt Turnout",
                               "Pop. General Election",
                               "Pop. County",
                               "Proportion of County Pop.",
                               "County Votes General Election",
                               "General Election Turnout"))

stargazer(as.data.frame(alldat_10 %>% 
                          group_by(disinc) %>% 
                          summarise(n = length(disinc),
                                    pop_dis = mean(pop_dis),
                                    turnout_disinc = mean(turnout_disinc),
                                    place_prop = mean(place_prop)
                          )),
          summary = FALSE,
          rownames = FALSE,
          type = "text",
          title = "Balance on 'Treatment' Bandwidth 10%",
          covariate.labels = c("Disincorporated",
                               "N",
                               "Population at Attempt",
                               "Turnout at Attempt",
                               "Prop. of County Population"))



##### BAD OLS #####
reg01 <- lm(data = alldat,
            turnout_pres ~ disinc)
reg02 <- lm(data = alldat,
            turnout_pres ~ margin)
reg03 <- lm(data = alldat,
            turnout_pres ~ disinc + turnout_disinc)
reg04 <- lm(data = alldat,
            turnout_pres ~ margin + turnout_disinc)

stargazer(reg01, reg02, reg03, reg04,
          type = "text",
          title = "Simple OLS",
          dep.var.labels = c("Turnout in Subsequent Presidential Election"),
          covariate.labels = c("Disinc.",
                               "Disinc. Margin",
                               "Disinc. Turnout"))

##### BANDWIDTH OLS #####
# Common Slope
reg05 <- lm(data = alldat_10,
            turnout_pres ~ disinc + margin)

# Different Slope
reg06 <- lm(data = alldat_10,
            turnout_pres ~ disinc + margin + I(disinc * margin))

# Non-linear Slope
reg07 <- lm(data = alldat_10,
            turnout_pres ~ disinc + margin + I(margin^2) + I(disinc * margin) + I(disinc * margin^2))

# Table
stargazer(reg05, reg06, reg07,
          type = "text",
          dep.var.caption = "Subsequent Turnout",
          dep.var.labels = c(""),
          column.labels = c("Common Slope", "Different Slopes", "Non-linear Slopes"),
          covariate.labels = c("Disincorporated",
                               "Margin",
                               "Margin-squared",
                               "Disinc * Margin",
                               "Disinc * Margin-squared"),
          out = "Tables/table03.tex")

# Non-linear figure
ggplot(data = alldat_10, aes(x = margin,
                             y = turnout_pres))+
  geom_point()+
  geom_vline(aes(xintercept = 0,), linetype = "dashed")+
  stat_smooth(data = alldat_10[alldat_10$margin < 0,],
              col = "red", method = "lm", formula = y ~ poly(x, 2))+
  geom_smooth(data = alldat_10[alldat_10$margin > 0,], method = "lm",
              formula = y ~ poly(x, 2))+
  labs(title = "Disincorporation and Voter Turnout -- Non-linear Estimation",
       subtitle = "Bandwidth 10%",
       x = "Disincorporation Vote Margin",
       y = "Turnout in Subsequent Presidential Election")+
  theme_minimal()
ggsave(filename = "Tables/fig3.png", plot = last_plot())
