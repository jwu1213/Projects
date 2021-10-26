library(tidyverse)
library(ggrepel)
install.packages("wordcloud2")
library(wordcloud2)
links <- read_csv("links.csv") #9,742 rows, 3 columns
movies <- read_csv("movies.csv") #9,742 rows, 3 columns
ratings <- read_csv("ratings.csv") #100,836, 4 columns; 
tags<-read_csv("tags.csv")

averageRating<-ratings %>% 
  group_by(movieId) %>%
  summarise(average_rating = mean(rating))

numRating<-ratings %>% 
  group_by(movieId) %>%
  summarise(length(userId)) %>%
  rename(`Number of Ratings`=`length(userId)`)

ratingsInfo<- left_join(numRating, averageRating, 
                        by= c("movieId"="movieId"))

all <- left_join(movies, ratingsInfo, by = c("movieId"="movieId"))
all <- all %>% left_join(links, by = c("movieId"="movieId")) %>% 
  mutate(year = str_extract(title,"\\([^()]+\\)")) %>%
  mutate(year = str_extract(year,"\\d{4}"))

all$title <- str_replace(all$title,"\\([^()]+\\)","")
all<-left_join(all, tags, by = c("movieId"="movieId"))
str(all)
genres <- all %>% separate_rows(genres) %>% count(genres)
genres
which(is.na(links))
#########################

View(links)
View(movies)
View(ratings)
View(all)
View(ratingsInfo)
View(tags)

#########################
#1
#most tagged movies by genre might help find the most impactful movies
#Word Cloud
impact <- all %>% 
  select(title, year, genres, tag, movieId)%>%
  filter(!is.na(tag)) %>% 
  filter(genres!="")%>%
  filter(genres!="Fi" )%>%
  filter(genres!="no" )%>%
  filter(genres!="genres" )%>%
  filter(genres!="listed")#%>%
  
tagCount <- impact %>%
  separate_rows(tag)%>%
  count(movieId)

impact<-left_join(impact, tagCount, by = c("movieId"="movieId"))
view(impact)
cloud <- impact %>% 
  select(title, n)%>%
  group_by(title)%>%
  slice(1:1)
View(cloud)
str(cloud)

wordcloud2(cloud, size = 1.6)

arrange(desc(cloud$n))

#Top 3 lowest rated movies per year since 2010
#grouped bar chart

low3year <- all %>% 
  filter(year>=2010 & year<=2017)%>%
  filter(`Number of Ratings` >= 5) %>%
  arrange(-desc(average_rating))%>%
  group_by(year)%>%
  slice(1:3)
View(low3year)
ggplot(low3year, aes(fill=title, x=year, y=average_rating))+ 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(y=1,label=title), 
            position=position_dodge(width=0.9), 
            vjust=.5, 
            hjust=.5 )+
  ylim(0,5) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))+
  theme(legend.position = "none")+
  xlab("Year")+ 
  ylab("Average Rating")+
  labs(title = "Worst Three Movies per year from 2000-2017")+
  coord_flip()


#3 
#Most popular genres
genres<-genres %>% 
  filter(genres!="")%>%
  filter(genres!="Fi" )%>%
  filter(genres!="no" )%>%
  filter(genres!="genres" )%>%
  filter(genres!="listed")
genres$genres<-factor(genres$genres,levels=genres$genres)
genres
ggplot(genres, aes(genres, n))+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
  theme(panel.grid = element_blank(),
        panel.border = element_blank())+
  scale_y_continuous(limits = c(0,4400), expand = c(0, 0))+
  xlab("Genres")+
  ylab("Count")+
  labs(title = "Number of Movies per Genre")+
  geom_col()

#4
#Average rating and Top movie of every year from 2010-2018
#bubble
perYear <- all %>% 
  filter(year>=2010)%>% 
  filter()
perYear

topMovie <- perYear %>%
  arrange(desc(`Number of Ratings`))%>%
  group_by(year)%>%
  slice(1:1)

topMovie$genres <- sub("(\\w+).*", "\\1", topMovie$genres)
View(topMovie)

myColors <- c("#FF0000", "#00FF00", "#4699dd", "#FFFF00")

ggplot(topMovie, 
       aes(x=year, 
           y=average_rating, 
           size=`Number of Ratings`, 
           col=genres))+
  ylim(0,5)+
  scale_size(range = c(.9, 15), 
             name="Number of Ratings")+
  ggtitle("Most Popular Movies from 2010-2018")+
  xlab("Year")+
  ylab("Average Rating")+
  scale_fill_discrete(name="Genres", labels = c("Action", "Adventure", "Comedy", "Sci-Fi"))+
  theme(legend.position = "bottom")+
  geom_label_repel(aes(label=title, lineheight=2), 
                   size=4, 
                   color="black", 
                   force = 30, 
                   box.padding = .3)+
  
  scale_color_manual(values=myColors)+
  geom_point()
  


#5
#Number of movies released each year
#Line chart
numMovies <- all %>% 
  filter(!is.na(year))%>%
  separate_rows(year) %>% 
  count(year)

numMovies$year<-as.numeric(numMovies$year)
numMovies

everysecond <- function(x){
  x <- sort(unique(x))
  x[seq(2, length(x), 2)] <- ""
  x
}

ggplot(numMovies, aes(year,n))+
  theme(panel.grid = element_blank(),
        panel.border = element_blank())+
  scale_y_continuous(limits = c(0,300), expand = c(0, 0))+
  theme(axis.text.x = element_text(angle=45, vjust=0, hjust=0))+
  scale_x_discrete(labels=everysecond(numMovies$year))+
  xlab("Year")+
  ylab("Total Number of Movies")+
  labs(title = "Number of Movies per Year")+
  #geom_smooth(formula = y~year1, method = "lm", color="red",size=.5)+
  geom_point()
  
