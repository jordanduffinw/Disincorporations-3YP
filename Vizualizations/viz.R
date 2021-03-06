##### LIBRARIES #####
library(stargazer)
library(tidyverse)
library(usmap)

##### Data #####
disincs <- read_csv("C:/Users/jorda/OneDrive/3YP - Disincorporations/Disincorporations Data/disincs.csv")
disincs <- disincs %>% 
  filter(year_vote >= 2010,
         referendum == TRUE) %>% 
  mutate(prop_yes = votesyes / (votesyes + voteno),
         margin = prop_yes - threshold,
         disinc = case_when(
           margin > 0 ~ 1,
           margin < 0 ~ 0),
         attempt = 1)

statemap <- us_map(regions = c("states"))
colnames(statemap)[8] <- "state"

countymap <- us_map(regions = c("counties"))
countymap$fips <- as.numeric(countymap$fips)
colnames(countymap)[7] <- "county_fips"


attempt_freq <- disincs %>%
  group_by(state) %>%
  mutate(success = sum(disinc)) %>% 
  group_by(state) %>% 
  count(success) %>% 
  mutate(fail = n - success,
         rate = round((success / n), digits = 2)) %>% 
  select(state, success, fail, n, rate)

attempt_freq_county <- disincs %>% 
  group_by(state, county_fips) %>% 
  mutate(success = sum(disinc)) %>% 
  group_by(county_fips) %>% 
  count(success) %>% 
  mutate(fail = n - success,
         rate = round((success / n), digits = 2))


##### Maps #####
# Frequency and location of attempts
mapdat <- left_join(attempt_freq, statemap, by = "state")

mapdat2 <- full_join(attempt_freq_county, countymap, by = "county_fips")
mapdat2$n[is.na(mapdat2$n)] <- 0

map01 <- ggplot()+
  geom_polygon(data = statemap, aes(x = x, y = y, group = group),
               color = "black",
               fill = "white")+
  geom_polygon(data = mapdat, aes(x = x, y = y, group = group, fill = n),
               color = "black")+
  scale_fill_gradient(low = "light blue", high = "dark blue")+
  labs(title = "Frequency of Voluntary Dissolution Attempts since 2010",
       subtitle = "White = 0")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  guides(fill = guide_legend(title = "Count"))+
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())
map01
ggsave(filename = "map01.png", plot = map01)

map02 <- ggplot()+
  geom_polygon(data = statemap, aes(x = x, y = y, group = group),
               color = "black",
               fill = "white")+
  geom_polygon(data = mapdat, aes(x = x, y = y, group = group, fill = rate),
               color = "black")+
  scale_fill_gradient(low = "light blue", high = "dark blue")+
  labs(title = "Success Rate of Voluntary Dissolution Attempts since 2010")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  guides(fill = guide_legend(title = "Rate"))+
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())
map02
ggsave(filename = "map02.png", plot = map02)

# Where specifically are these?
map03 <- ggplot()+
  # geom_polygon(data = countymap, aes(x = x, y = y, group = group),
  #              color = "light grey",
  #              fill = NA)+
  geom_polygon(data = mapdat2[mapdat2$n > 0,], aes(x = x, y = y, group = group, fill = n), color = "black")+
  scale_fill_gradient(low = "light blue", high = "dark blue")+
  geom_polygon(data = statemap, aes(x = x, y = y, group = group),
               color = "black",
               fill = NA)+
  labs(title = "Locations of Voluntary Dissolution Attempts since 2010")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  guides(fill = guide_legend(title = "Attempts"))+
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())
map03
ggsave(filename = "map03.png", plot = map03)

map04 <- ggplot()+
  # geom_polygon(data = countymap, aes(x = x, y = y, group = group),
  #              color = "light grey",
  #              fill = NA)+
  geom_polygon(data = mapdat2[mapdat2$n > 0,], aes(x = x, y = y, group = group, fill = rate), color = "black")+
  scale_fill_gradient(low = "light blue", high = "dark blue")+
  geom_polygon(data = statemap, aes(x = x, y = y, group = group),
               color = "black",
               fill = NA)+
  labs(title = "Success Rate of Voluntary Dissolution Attempts since 2010")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  guides(fill = guide_legend(title = "Rate"))+
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())
map04
ggsave(filename = "map04.png", plot = map04)

##### Tables #####
# Frequency of attempts
stargazer(attempt_freq,
          type = "text",
          summary = FALSE,
          rownames = FALSE,
          title = "Frequency of Voluntary Dissolution Attempts since 2010",
          covariate.labels = c("State", "Success", "Fail", "Total", "Rate"))

##### Figures #####
