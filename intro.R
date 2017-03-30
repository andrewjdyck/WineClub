
# Wine night

library("googlesheets")
library(dplyr)
library(ggplot2)

# Alternate package to view sheets?
# http://stackoverflow.com/questions/22873602/importing-data-into-r-from-google-spreadsheet
#wn <- gs_title('Wine Night')
wn <- gs_url('https://docs.google.com/spreadsheets/d/1GjNgtt6unKIJW9BceFrk2qq2ulTT5hAONNwT3v7YzZs/edit?usp=sharing')
#wn <- gs_url('https://docs.google.com/spreadsheets/d/1GjNgtt6unKIJW9BceFrk2qq2ulTT5hAONNwT3v7YzZs/pub?output=csv')


ratings <- gs_read(ss=wn, ws='Ratings')
wines <- gs_read(ss=wn, ws='WinesList')
overview <- gs_read(ss=wn, ws='Overview')
# write.csv(ratings, './data/ratings.csv', row.names=FALSE)
# write.csv(wines, './data/wines.csv', row.names=FALSE)
# write.csv(overview, './data/overview.csv', row.names=FALSE)

fulldata <- ratings %>%
  inner_join(overview) %>%
  inner_join(wines)




ratings %>%
  group_by(Person) %>%
  summarise(avg_rating = mean(Rating, na.rm=TRUE), var_rating = sd(Rating, na.rm=TRUE)) %>%
  arrange(desc(avg_rating))

# Highest and lowest ratings
ratings %>%
  arrange(desc(Rating))

ratings %>%
  arrange(Rating)

fulldata %>%
  group_by(RedWhite) %>%
  summarise(avg_rating = mean(Rating))

fulldata %>%
  group_by(RedWhite, Wine) %>%
  summarise(avg_rating = mean(Rating)) %>%
  arrange(desc(avg_rating))


pd1 <- ratings %>%
  group_by(WineNight, WineNumber) %>%
  summarise(avg_rating = mean(Rating))

ggplot(pd1, aes(x=WineNumber, y=avg_rating, colour=WineNight)) + 
  geom_line()

pd2 <- ratings %>%
  group_by(Person, WineNumber) %>%
  summarise(avg_rating = mean(Rating, na.rm=TRUE))

ggplot(pd2, aes(x=WineNumber, y=avg_rating, colour=Person)) + 
  geom_line()

lm(data=pd1, avg_rating ~ WineNumber)

lm(data=ratings, Rating ~ WineNumber)
