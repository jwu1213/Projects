######################## IN-PROGRESS #########################

library(tidyverse)
library(lubridate)
#######read in data
storms<-read_csv("StormEvents",col_types = cols(EPISODE_NARRATIVE="c",EVENT_NARRATIVE="c",WFO='c',EPISODE_ID='c',TOR_OTHER_CZ_STATE='c',SOURCE='c',BEGIN_LOCATION='c',END_LOCATION='c',TOR_OTHER_CZ_FIPS='c',TOR_OTHER_CZ_NAME='c',BEGIN_AZIMUTH='c',END_AZIMUTH='c',DAMAGE_PROPERTY="c",DAMAGE_CROPS='c',MAGNITUDE_TYPE='c',FLOOD_CAUSE='c',TOR_OTHER_WFO='c',CATEGORY='i'))
View(storms)

######Aggregate
x<-storms %>% group_by(YEAR,EVENT_TYPE) %>% summarise(count=n())

#look at it:
View(x)

#########Real time stamp
# look at a few:
head(storms$BEGIN_DATE_TIME)

# Look like: "13-MAR-58 19:15:00"

ts<-dmy_hms(storms$BEGIN_DATE_TIME)

# Look at the result
head(ts)

#Notice the problem: "2058-06-18 14:00:00 UTC"
#Fix the year using the year column
year(ts)<-storms$YEAR
head(ts)

#Looks good, add it back to the data
storms$begin_ts<-ts

##############Calculate Property Damage
storms <- storms %>% 
  # Do this all in one mutate command
  mutate(
    # Replace all K,M,B with empty string and assign result to new column dam_num
    dam_num=as.numeric(gsub("[KMB]","",DAMAGE_PROPERTY)),
    # Replace all digits / decimal point with empty string and assign result to new column dam_let
    dam_let=gsub("[0-9.]+","",DAMAGE_PROPERTY),
    # Use case_when to get the multiplier based on the letter and assign to colun dam_mult
    dam_mult=case_when(
      dam_let=="K" ~ 1000,
      dam_let=="M" ~ 10^6,
      dam_let=="B" ~ 10^9,
      # This last bit tells us to return a '1' if we haven't matched anything above
      TRUE ~ 1),
    # Calculate actual damage number and assign the result to prop damage
    prop_damage = dam_num*dam_mult)

#Eyeball the result
View(storms %>% select(DAMAGE_PROPERTY,prop_damage))
unique(storms$STATE)
###############
#Plot1: Top Storms by State
#Map
#Dim1: x
#Dim2: y 
#Dim3: Color: Which storm
`%notin%` <- Negate(`%in%`)
nomap<-c("HAWAII","ALASKA","VIRGIN ISLANDS","PUERTO RICO", "RHODE ISLAND"       ,"GUAM","AMERICAN SAMOA","ATLANTIC SOUTH","LAKE MICHIGAN" ,"GULF OF MEXICO"     ,"ATLANTIC NORTH" ,"LAKE SUPERIOR","LAKE ST CLAIR","E PACIFIC","LAKE HURON"          ,"LAKE ERIE" ,"LAKE ONTARIO" ,"GULF OF ALASKA" ,"HAWAII WATERS","ST LAWRENCE R" )
top <- storms %>% 
  filter(STATE %notin% nomap)%>%
  group_by(STATE,EVENT_TYPE) %>%
  summarise(Total = n())
top$STATE<-tolower(top$STATE)
  
top<-top %>%
  group_by(STATE) %>%
  slice_max(order_by = Total)
View(top)
us<-map_data("state")
View(us)

plot_data <- us %>% 
  left_join(storms, by=c("region"="STATE"))####BAD####
plot_data
###############
#Plot2: Most Frequent Types of Storm + Most Destructive
#Bar Chart
#Dim1: x: type of storm
#Dim2: y: Frequency
#Dim3: Color: Damage



###############
#Plot3: When is storm season?
#Heatmap
#Dim1: x: Month
#Dim2: y: type of storm
#Dim3: color: Average of each storm per month



###############
#Plot4: Duration of storm
#Violin
#Dim1: x: type of storm
#Dim2: y: How long each storm lasts
#Dim3: Color: total amount of each storm
#Dim4: Width: Frequency of each time
