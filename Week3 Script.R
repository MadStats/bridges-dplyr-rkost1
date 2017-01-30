library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)

data = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/WI16.txt"
temp = fread(data)
classes = sapply(temp, class)
temp2 = fread(data, colClasses = classes) 

T = temp2

keep = select(T, STATE_CODE_001, COUNTY_CODE_003, 
              STRUCTURE_NUMBER_008, YEAR_BUILT_027, TRAFFIC_LANES_ON_028A, 
              ADT_029)

keep = mutate(keep, FIPS = ((STATE_CODE_001 * 1000) + COUNTY_CODE_003))

plot(x = keep$TRAFFIC_LANES_ON_028A, y = keep$ADT_029)

#trend with average daily traffic and traffic lanes
keep %>% group_by(TRAFFIC_LANES_ON_028A) %>%
  summarize(avg.ADT = mean(ADT_029)) %>%
  ggplot(mapping = aes(x=TRAFFIC_LANES_ON_028A, y = avg.ADT)) + 
  geom_point()

keep %>% group_by(TRAFFIC_LANES_ON_028A) %>%
  summarize(avg.ADT = mean(ADT_029)) %>%
  ggplot(mapping = aes(x=TRAFFIC_LANES_ON_028A, y = avg.ADT)) + 
  geom_point() + geom_smooth()

#looking at average daily traffic by county
dat2 = keep %>% group_by(FIPS) %>% summarize(avg.ADT = mean(ADT_029))

dat2 %>% transmute(region = FIPS, value = avg.ADT) %>% county_choropleth(state_zoom = "wisconsin")

#looking at average lanes on bridges by county, looks very similar to previous map
dat3 = keep %>% group_by(FIPS) %>% summarize(avg.lanes = mean(TRAFFIC_LANES_ON_028A))

dat3 %>% transmute(region = FIPS, value = avg.lanes) %>% county_choropleth(state_zoom = "wisconsin")
