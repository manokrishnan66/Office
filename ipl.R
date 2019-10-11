library(tidyverse)
library(dplyr)
library("readxl")
install.packages(gganimate)
library(gganimate)
install.packages("gifski")
library(forcats)

dirpath <- getwd()
filename <- 'iplallseasons_refined.xlsx'
fullpath <- file.path(dirpath,filename)

ipl <- read_excel("D:/Mine/Data Science/IPL VIZ/Data/iplallseasons_refined.xlsx")

head(iplyr$month-date)

ipl %>% str_split(ipl$Match_date, ",")
ipl %>% strsplit(as.character(Match_date),',')

iplyr <- ipl %>%separate(col = Match_date, into = c("monthdate", "year"), sep = ",")

iplyr <- iplyr %>% mutate(year = as.numeric(year))

iplyr <- iplyr %>%separate(col = monthdate, into = c("month", "date"), sep = " ")

iplyr <- iplyr %>%separate(col = Team1_score, into = c("Team1score", "Team1wickets"), sep = "/")

iplyr <- iplyr %>%separate(col = Team2_score, into = c("Team2score", "Team2wickets"), sep = "/")

iplyr <- iplyr %>% mutate(Team1score = as.numeric(Team1score), Team1wickets = as.numeric(Team1wickets),Team2score = as.numeric(Team2score),Team2wickets = as.numeric(Team2wickets))

iplyr <- as.data.frame(iplyr)

iplyr[is.na(iplyr)] <- 10
head(iplyr)

#----End of Wrangling


scatter <- iplyr %>%  group_by(year,Team1) %>%  select(year,Team1score,Team1wickets,Team2score) %>%
  
  summarize(sum_Team1score = sum(Team1score))

scatter %>% filter (year %in% c(2008)) %>% ggplot(aes(Team1,sum_Team1score)) + geom_point() + facet_wrap(~year, scales = "free") 

scatter<- scatter %>% mutate(Team1 = recode(Team1, 
                        'Rising Pune Supergiants'='Rising Pune Supergiants','Rising Pune Supergiant' = 'Rising Pune Supergiants', 'Deccan Chargers' = 'Sunrisers Hyderabad','Pune Warriors'='Rising Pune Supergiants' ))



warning()
anim <- scatter %>% 
  #with(order(sum_Team1score)) %>%
  #mutate(Team1 = fct_reorder(Team1, desc(sum_Team1score))) %>%
  ggplot(aes(reorder(Team1,sum_Team1score),sum_Team1score,frame=year))+
  #ggplot(aes(Team1,sum_Team1score))+
  #ggplot(aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_bar(stat = 'Identity',alpha = 0.7, show.legend = FALSE) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  coord_flip()+
  transition_states(year, transition_length = 2, state_length = 1) +
  enter_fade() + 
  exit_fade()
#animate(anim,renderer = av_renderer())
animate(anim, nframes = 100, end_pause = 10, rewind = FALSE)

#------------------




scatter_formatted <- scatter %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-sum_Team1score),
         Value_rel = sum_Team1score/sum_Team1score[rank==1],
         Value_lbl = paste0(" ",round(sum_Team1score))) %>%
  group_by(Team1) %>% 
  filter(rank <=10) %>%
  ungroup()


staticplot = ggplot(scatter_formatted, aes(rank, group = Team1, 
                                       fill = as.factor(Team1), color = as.factor(Team1))) +
  geom_tile(aes(y = sum_Team1score/2,
                height = sum_Team1score,
                width = 2), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Team1, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=sum_Team1score,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")
        )

anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'IPL score by teamwise : {closest_state}',  
       subtitle  =  "",
       caption  = "Total scores of IPL teams | BCCI")

animate(anim, 200, fps = 20,end_pause = 10,  width = 1900, height = 1900, 
        renderer = gifski_renderer("gganim1.gif"))
