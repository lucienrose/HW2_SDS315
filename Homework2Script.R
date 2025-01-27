#HOMEWORK 2 FILE

library(ggplot2)
library(tidyverse)

#PROBLEM 1

profs <- read.csv('profs.csv')

#A

ggplot(profs, aes(x=eval))+
  geom_histogram(fill='red3',color='black')+
  theme_classic()+
  labs(
    title='Distribution of Course Evaluation',
    x='Evaluation',
    y='Frequency')

#B

ggplot(profs, aes(x=native, y=eval))+
  geom_boxplot(fill='orange',color='black')+
  theme_classic()+
  labs(
    title='Course Evaluation of Native vs Non-Native English Speakers',
    x='Is the Professor a Native English Speaker?',
    y='Evaluation')

#C

ggplot(profs, aes(x=eval))+
  geom_histogram(fill='gold3',color='black')+
  theme_classic()+
  labs(
    title='Course Evaluation of Male vs Female Professors',
    x='Evaluation',
    y='Frequency')+
  facet_wrap(~factor(gender))

#D

ggplot(profs, aes(x=beauty, y=eval))+
  geom_point()+
  theme_classic()+
  labs(
    title='Professor Attractiveness vs Course Evaluation',
    x='Attractiveness Score',
    y='Evaluation')+
  geom_smooth(se=FALSE, method=lm)

#PROBLEM 2

bikeshare <- read.csv('bikeshare.csv')

#A

bike_a <- bikeshare %>%
  group_by(hr)%>%
  summarize(avg_total=mean(total))

ggplot(bike_a, aes(x=hr, y=avg_total))+
  geom_line(size=1,col='blue')+
  theme_classic()+
  labs(
    title='Average Bike Rentals per Hour',
    x='Hour of Day',
    y='Average Hourly Bike Rentals')

#B

bike_b <- bikeshare %>%
  group_by(hr,workingday)%>%
  summarize(avg_total=mean(total))

bike_b$workingday<-str_replace_all(bike_b$workingday, '0',"Non-Working Day")
bike_b$workingday<-str_replace_all(bike_b$workingday, '1',"Working Day")

ggplot(bike_b, aes(x=hr, y=avg_total))+
  geom_line(size=1,col='blue')+
  theme_classic()+
  labs(
    title='Average Bike Rentals per Hour',
    x='Hour of Day',
    y='Average Hourly Bike Rentals')+
  facet_wrap(~factor(workingday))

#C

bike_c <- bikeshare %>%
  filter(hr=='9')%>%
  group_by(workingday, weathersit)%>%
  summarize(avg_total=mean(total))

bike_c$workingday<-str_replace_all(bike_c$workingday, '0',"Non-Working Day")
bike_c$workingday<-str_replace_all(bike_c$workingday, '1',"Working Day")

ggplot(bike_c, aes(x=weathersit, weight=avg_total))+
  geom_bar(fill='green3',color='black')+
  theme_classic()+
  labs(
    title='Average Bike Rentals in the 9th Hour by Weather Code',
    x='Weather Code',
    y='Average Hourly Bike Rentals')+
  facet_wrap(~factor(workingday))

#PROBLEM 3

capmetro <- read.csv('capmetro_UT.csv')

capmetro <- mutate(capmetro,
                     day_of_week = factor(day_of_week,
                                          levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun")),
                     month = factor(month,
                                    levels=c("Sep", "Oct","Nov")))

#1

capmetro_avg <- capmetro %>%
  group_by(hour_of_day, day_of_week, month) %>%
  summarise(avg_boardings = mean(boarding))

ggplot(capmetro_avg, aes(x = hour_of_day, y = avg_boardings, color = month))+
  geom_line()+
  facet_wrap(~factor(day_of_week))+
  labs(
    x = "Hour of Day", 
    y = "Average Boardings", 
    title = "Average Boardings by Hour, Day of Week, and Month")+
  theme_classic()

#2

capmetro_wknd <- capmetro %>%
  mutate(weekend = ifelse(day_of_week==c("Sat", "Sun"),'Weekend','Weekday'))

ggplot(capmetro_wknd, aes(x = temperature, y = boarding, color = weekend))+
  geom_point()+
  facet_wrap(~hour_of_day, scales='free')+
  labs(
    x = "Temperature", 
    y = "Boardings", 
    title = "Boardings vs. Temperature Faceted by Hour of Day")+
  theme_classic()
  
#PROBLEM 4

billboard <- read.csv('billboard.csv')

#A

billboard_top <- billboard %>%
  group_by(performer, song) %>%
  summarize(count = max(weeks_on_chart)) %>%
  arrange(desc(count))

head(billboard_top, 10)

#B

billboard_sum <- billboard %>%
  subset(!(year=='1958'|year=='2021')) %>%
  group_by(year)%>%
  summarize(total_unique_songs=length(unique(song_id)))

ggplot(billboard_sum, aes(x=year,y=total_unique_songs))+
  geom_line(size=1,col='blue')+
  theme_classic()+
  labs(
    title='Unique Songs on Billboard Top 100 Each Year',
    x='Year',
    y='Unique Songs')

#C

billboard_ten <- billboard %>%
  group_by(performer,song)%>%
  summarize(count=max(weeks_on_chart))%>%
  filter(count>=10)%>%
  group_by(performer)%>%
  summarize(total=length(performer))%>%
  filter(total>=30)%>%
  arrange(desc(total))

ggplot(billboard_ten, aes(y=reorder(performer,total), weight=total))+
  geom_bar(fill='blue3')+
  theme_classic()+
  labs(
    title='Artists with More than 30 "Ten-Week Hits"',
    x='Total "Ten-Week Hits"',
    y='Artist')
