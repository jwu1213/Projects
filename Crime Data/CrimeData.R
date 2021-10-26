
library(tidyverse)

#Building a chloropleth of county level data for all crimes, all ages in 2009
#Reading the initial data
data<-read_delim("./ICPSR_30763/DS0001/30763-0001-Data.tsv",delim = "\t")
unique(data$STUDYNO)
unique(data$FIPS_ST)
view(data)

# Read in the FIPS table
fips <- read_csv("county_fips_master.csv")
View(fips)

# Key Generation
data <- data %>%
  mutate(fips = FIPS_ST*1000 + FIPS_CTY)
View(data)

# All non matches
View(fips %>% 
       select(fips) %>% 
       full_join(data %>% select(fips),keep=T) %>% 
       filter(is.na(fips.x+fips.y)))

View(data %>% filter(FIPS_CTY==777))

# Joining fips data with crime data
data<- data %>% inner_join(fips)
View(data)

# Read in u.s. map data
counties<-map_data("county")

# The names aren't quite right. ie odd spacing and some parts of the towns are removed. 
View(fips %>% filter(state_name== "Maryland"))
View(counties %>% group_by(region,subregion) %>% summarise %>% filter(region=="maryland"))
View(counties %>% group_by(region,subregion) %>% summarise %>% filter(region=="louisiana"))
unique(data$county_name)

#Extracting names into a new column so they are more inline with the other data to merge later 
data <- data %>% 
  extract(county_name,c("county_name_simplified"),"(.*)\\s[^\\s]+$",remove = F) %>%
  mutate(county_name_simplified = tolower(str_remove_all(county_name_simplified,"['.,]")),
         state_name = tolower(state_name))
View(data)

#Better naming
just_regions <- counties %>% 
  group_by(region,subregion) %>% 
  summarise()

View(data %>% 
       select(state_name,county_name_simplified) %>% 
       full_join(just_regions,by=c("state_name"="region",
                                   "county_name_simplified"="subregion"),keep=T) %>% 
       filter(!complete.cases(.)))

#VA is missing counties? Turns out it's not because places like Winchester are classified as cities instead of counties. 
View(just_regions %>% filter(region=="virginia"))
View(just_regions %>% filter(subregion=="winchester"))

#Putting all the information together. This is why I changed the names to lowercase. 
plot_data <- counties %>% 
  left_join(data, by=c("region"="state_name",
                       "subregion"="county_name_simplified"))

#Terrible color scheme. Can barely see anything
ggplot(plot_data)+ 
  geom_polygon(aes(long,lat,fill=GRNDTOT,group=group))+
  scale_fill_viridis_c()

#Much better, adjusting the scale
options(scipen=999)
ggplot(plot_data) + 
  geom_polygon(aes(long,lat,fill=GRNDTOT,group=group))+
  scale_fill_viridis_c(trans="log10")+
  expand_limits(x=plot_data$long, y=plot_data$lat)+
  ggtitle("Map of Crime Totals by County (2009)")+
  theme(text = element_text(size=8))

View(plot_data)
View(data %>% filter(state_name=="florida")) #no Florida data

############## NOT WORKING ################ Plot Juvenile Data
view(data)
View(plot_data)

adult_data <- data %>% mutate(type="adult")
jv_data <- data %>% mutate(type="jv")

str(data)
plot_data <- bind_rows(adult_data, jv_data)
View(plot_data)

ggplot(plot_data) +
  geom_polygon(aes(long,lat,fill=GRNDTOT, group=group))+
  scale_fill_viridis_c(trans="log10")+ 
  facet_wrap(~type)


###############################################################
# Relationship between Murder, Arson, Drug possession, and DUI?
View(data %>% 
       select(MURDER:RUNAWAY) %>% 
       summarise_all(sum) %>% 
       pivot_longer(everything(),names_to="type",values_to="count")%>% 
       arrange(-count))


#Pivot longer here so that I can facet_wrap the resulting plots
sub_data <- data %>% 
  select(state_name,county_name_simplified,MURDER,ARSON,DRGPOSS,DUI) %>% 
  pivot_longer(MURDER:DUI,names_to="type",values_to="count")

#Joining new sub_data with map data
plot_data <- counties %>% 
  left_join(sub_data, by=c("region"="state_name","subregion"="county_name_simplified"))

# Data is missing from a lot of areas, but we can kind of see some correlation between the 4 crimes. 
ggplot(plot_data %>% filter(!is.na(type))) + 
  geom_polygon(aes(long,lat,fill=count,group=group))+
  scale_fill_viridis_c(trans="log10")+
  facet_wrap(~type)+
  ggtitle("Maps of Arson, Drug Possession, DUI, and Murder by Count in the U.S. (2009)")+
  theme(text = element_text(size=8))

###############################################################
#Scatter to explore correlations by county with DUI
View(data)

#Creating new subdata for a new search
sub_data <- data %>% 
  select(state_name,county_name_simplified,MURDER,ARSON,DRGPOSS,DUI) %>%
  pivot_longer(MURDER:DRGPOSS,names_to="type",values_to="count")
View(sub_data)
plot_data <- counties %>% left_join(sub_data, by=c("region"="state_name","subregion"="county_name_simplified"))

#Bad Scaling
ggplot(sub_data %>% filter(!is.na(type))) + 
  geom_point(aes(DUI,count))+
  facet_wrap(~type)

#Better, can see correlation but heavy overplotting on the left side
ggplot(sub_data %>% filter(!is.na(type))) + 
  geom_point(aes(DUI,count))+
  facet_wrap(~type,scales="free_y")

#Much better, chart is much more clear and space is better utilized after logging both dimensions. We can see pretty good correlation between the 3 crimes and DUI. 
ggplot(sub_data %>% filter(!is.na(type))) + 
  geom_point(aes(DUI,count),alpha=.3)+
  facet_wrap(~type,scales="free_y")+
  scale_x_log10()+
  scale_y_log10()+
  ggtitle("Relationship between Arson, Drug Possession, and Murder with a baseline of DUI (2009)")+
  theme(text = element_text(size=8))

###############################################################
#Seeing if there's a correlation between Gambling and other crimes related to assault and burglary.
Gamble_data <- data %>% 
  select(state_name, county_name_simplified, ROBBERY, 
         BURGLRY, MVTHEFT, MURDER,AGASSLT, LARCENY, GAMBLE) %>%
  pivot_longer(ROBBERY:LARCENY, names_to = "type", values_to = "count")

#Again, we can see a pretty clear correlation between the two categories of crime with a baseline of gambling
ggplot(Gamble_data %>% filter(!is.na(type))) + 
  geom_point(aes(GAMBLE,count))+
  facet_wrap(~type,scales="free_y")+
  scale_x_log10()+
  scale_y_log10()+
  ggtitle("Relationship between Theft and Violent Crime with a baseline of Gambling Offenses (2009)")+
  theme(text = element_text(size=8))
###############################################################
#Creating a heat map to explore all crime by state
names(data)

#New sub data which includes all the different crime categories
sub_data <- data %>% 
  select(state_name,MURDER:RUNAWAY) %>% 
  group_by(state_name) %>% 
  summarise_all(sum) 
View(sub_data)

#Factoring both state and crime totals so that I can show them in descending order 
state_order <- sub_data %>% 
  pivot_longer(-state_name, names_to="type", values_to = "count") %>% 
  group_by(state_name) %>% 
  summarise(total = sum(count)) %>% 
  arrange(total)
state_order

crime_order <- sub_data %>% 
  pivot_longer(-state_name, names_to="type", values_to = "count")%>% 
  group_by(type)%>% 
  summarise(total = sum(count))%>% 
  arrange(total)
crime_order

# Ok, apply factor levels and plot
sub_data <- sub_data %>% 
  pivot_longer(-state_name, names_to="type", values_to = "count") %>%
  mutate(state_name = factor(state_name,state_order$state_name),
         type = factor(type,crime_order$type))

ggplot(sub_data) + 
  geom_tile(aes(state_name,type,fill = count))+
  scale_fill_viridis_c()

#have to filter out all others and Drug totals because they aren't specific crimes. 
ggplot(sub_data %>% 
         filter(!(type %in% c("ALLOTHR","DRUGTOT"))))+ 
  geom_tile(aes(state_name,type,fill = count))+
  scale_fill_viridis_c(trans="log10")+
  theme(axis.text.x = element_text(angle=45,hjust=1))

#Pretty scrambled, So we have to scale the total number of crimes per state
sub_data <- sub_data %>% 
  filter(!(type %in% c("ALLOTHR","DRUGTOT"))) %>% 
  group_by(state_name) %>% 
  mutate(scaled_count = count / sum(count,na.rm = T))

#Much easier to see trends now that the top crimes are oriented towards the top of the plot. 
ggplot(sub_data %>% 
         filter(!(type %in% c("ALLOTHR","DRUGTOT")))) + 
  geom_tile(aes(state_name,type,fill = scaled_count)) +
  scale_fill_viridis_c(trans="log10")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  ggtitle("Heat map of All Crime by state (2009)")+
  theme(text = element_text(size=8))

###############################################
#Violin chart representing the proportion of the top 10 crimes for each state
View(sub_data)

#Filtering out the top two crimes as they aren't specific enough, so too many fall under that category. Then only keeping the top 10 crimes by total count. 
ec <- sub_data %>%
  filter(type != "ALLOTHR")%>%
  filter(type != "DRUGTOT") %>%
  arrange(desc(type), desc(count))%>%
  slice(1:10)
ec
#Find the total number of crimes within each state
ec <- ec %>%
  group_by(state_name) %>%
  mutate(`Total in State` = sum(count))

#Taking the proportion of each crime within each state
ec<- ec %>%
  mutate(Scaled_count=count/`Total in State`)

#Finding the totals per each type of crime within each state
ec <- ec %>% 
  group_by(type) %>%
  mutate(`Total per Crime`= sum(count))

#Plotting
ggplot(ec, aes(x=type, y=Scaled_count, fill= `Total per Crime`))+ 
  scale_fill_viridis_c()+
  geom_violin(scale="width")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        text = element_text(size=8))+
  ggtitle("Relative Proportion of top 10 adult crimes by state (colored by total count), 2009", )

ec

