
storms<-read_csv("StormEvents",col_types = cols(EPISODE_NARRATIVE="c",EVENT_NARRATIVE="c",WFO='c',EPISODE_ID='c',TOR_OTHER_CZ_STATE='c',SOURCE='c',BEGIN_LOCATION='c',END_LOCATION='c',TOR_OTHER_CZ_FIPS='c',TOR_OTHER_CZ_NAME='c',BEGIN_AZIMUTH='c',END_AZIMUTH='c',DAMAGE_PROPERTY="c",DAMAGE_CROPS='c',MAGNITUDE_TYPE='c',FLOOD_CAUSE='c',TOR_OTHER_WFO='c',CATEGORY='i'))
View(storms)

######Aggregate
x<-storms %>% 
  group_by(YEAR,EVENT_TYPE) %>% 
  summarise(count=n())

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
View(storms)
###############
#Plot1: Top Storms by State
#Map
#Dim1: x
#Dim2: y 
#Dim3: Color: Which storm
`%notin%` <- Negate(`%in%`)
nomap<-c("HAWAII","ALASKA","VIRGIN ISLANDS","PUERTO RICO", 
         "DISTRICT OF COLUMBIA","GUAM","AMERICAN SAMOA","ATLANTIC SOUTH",
         "LAKE MICHIGAN", "GULF OF MEXICO","ATLANTIC NORTH",
         "LAKE SUPERIOR", "LAKE ST CLAIR","E PACIFIC",
         "LAKE HURON","LAKE ERIE", "LAKE ONTARIO" ,
         "GULF OF ALASKA" ,"HAWAII WATERS","ST LAWRENCE R")

top <- storms %>% 
  filter(YEAR >= 2007)%>%
  filter(STATE %notin% nomap)%>%
  group_by(STATE,EVENT_TYPE) %>%
  summarise(Total = n())
top$STATE<-tolower(top$STATE)
unique(top$STATE)

top<-top %>%
  group_by(STATE) %>%
  slice_max(order_by = Total)

us<-map_data("state")
View(us)
View(top)
?map_data("state")

map_data <- us %>% 
  inner_join(top, by=c("region"="STATE"))
View(map_data)
unique(us$group)

ggplot(map_data)+
  geom_polygon(aes(long,lat,
                   fill=EVENT_TYPE, group=group), 
               color = "black")+
  expand_limits(x=map_data$long, y=map_data$lat)+
  ggtitle("Most Frequently Reported Storm Events per State (2007-2018)")
  

###############
#Plot2: Most Frequent Types of Storm Per Season
#Bar Chart
s <- storms %>% select(YEAR,MONTH_NAME, EVENT_TYPE) %>% 
  filter(YEAR > 2007)
s
s <- s %>% 
  filter(EVENT_TYPE=="Heavy Snow"|
           EVENT_TYPE=="High Wind"|
           EVENT_TYPE=="Thunderstorm Wind" | 
           EVENT_TYPE == "Hail")
s
spring <- c("March", "April", "May")
summer <- c( "June", "July", "August")
fall <- c("September","October", "November")
winter <- c("December", "January", "Februrary")

s$season <- s %>%
  mutate(
    season=case_when(
      MONTH_NAME %in% spring ~ "Spring",
      MONTH_NAME %in% summer ~ "Summer",
      MONTH_NAME %in% fall ~ "Fall",
      TRUE ~ "Winter"
    )
  )
s$season

ggplot(s$season) + 
  geom_bar(aes(EVENT_TYPE, fill = season))+ 
  theme(axis.text.x = element_text(hjust=.5))

s

str(s$season)

###############
#Plot3:
#Heatmap
unique(storms$EVENT_TYPE)
west <- c("california", "oregon", "washington", "montana", 
         "wyoming", "colorado", "utah", "nevada", "idaho")
southWest <- c( "arizona", "new mexico", "texas", "oklahoma")
midWest <- c( "north dakota", "south dakota", "nebraska", 
              "minnesota", "iowa", "missouri", "wisconsin", "michigan",
              "illinois", "indiana", "ohio")
southEast <- c("arkansas", "louisiana", "mississippi", "tennessee",
               "alabama","kentucky", "west virginia", "maryland",
               "virginia", "north carolina", "south carolina", "georgia",
               "florida")
northEast <- c("maine", "vermont", "new hampshire", "massachusetts", 
               "new york", "rhode island", "connecticut", "new jersey",
               "delaware", "pennsylvania")
region.list <- list(West = west,
                    `South West` = southWest,
                    `Mid West` = midWest,
                    `North East` = northEast,
                    `South East` = southEast)
winterEvents <- c("Ice Storm","Winter Weather","Heavy Snow","Winter Storm",
                  "Cold/Wind Chill","Lake-Effect Snow","Sleet","Blizzard",
                  "Avalanche","Frost/Freeze","Extreme Cold/Wind Chill",
                  "Freezing Fog")
region.list
unique(season$EVENT_TYPE)
#Setting up data
#Filter states, weather events, and set to lowercase
season <- storms %>%
  select(STATE:EVENT_TYPE)%>%
  filter(STATE %notin% nomap)%>%
  filter(YEAR >= 2007)%>%
  filter(EVENT_TYPE %in% winterEvents)
season$STATE<-tolower(season$STATE)
View(season)
unique(season$EVENT_TYPE)

#create region attribute
season$regions <- sapply(season$STATE, 
                         function(x) names(region.list)[grep(x,region.list)])
str(season)
season

#group winter events by region and count how many
heatmapData<-season %>% 
  group_by(regions,MONTH_NAME) %>%
  summarise(count=n())

#as_date(heatmapData$MONTH_NAME)
str(heatmapData)

heatmapData$MONTH_NAME<-factor(heatmapData$MONTH_NAME,month.name)
ggplot(heatmapData, 
       aes(x=MONTH_NAME, y=regions, fill=count))+ 
  geom_bin2d()#+
  #scale_fill_viridis_c()

View(heatmapData)
###############
#Plot4:
#lines
lg <- storms %>% 
  group_by(YEAR) %>% 
  distinct() %>% 
  count(EVENT_TYPE) %>% 
  filter(EVENT_TYPE %in% c("Heavy Snow", "High Wind",
                           "Thunderstorm Wind", "Hail")) %>% 
  filter(YEAR > 2007)

ggplot(lg, aes(YEAR, n, 
               group = EVENT_TYPE, color = EVENT_TYPE))+
  geom_line()
