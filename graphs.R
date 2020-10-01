library(dplyr)
library(tidyr)
library(ggplot2)
library(engsoccerdata)

# Data Wrangling
epl.df <- england %>% filter(division==1)
year.home.win = NULL
year.draw = NULL
year.away.win = NULL

iteration <- max(epl.df$Season) - min(epl.df$Season)
for (i in 1:(iteration+1)) {
  temp.epl <- filter(epl.df, Season==i+min(epl.df$Season)-1)
  year.home.win[i] <- tally(temp.epl, result=="H")/nrow(temp.epl)
  year.draw[i] <- tally(temp.epl, result=="D")/nrow(temp.epl)
  year.away.win[i] <- tally(temp.epl, result=="A")/nrow(temp.epl)
}

df <- data.frame(data.frame(unlist(year.home.win)), data.frame(unlist(year.draw)), data.frame(unlist(year.away.win)), min(epl.df$Season):max(epl.df$Season)) 
colnames(df) <- c("home win", "draw", "away win", "season")
df <- df %>% filter(`home win`>0,`away win`>0,draw>0)%>% gather(result, percentage, `home win`:`away win`)

df.EPL <- df
df.EPL$League <- "EPL"

# EPL Results by Seasons
ggplot(df, aes(x=season,y=percentage,col=result)) + 
  geom_point()+
  geom_smooth(method="lm") +
  scale_y_continuous("",breaks = c(0, 0.2, 0.4, 0.6, 0.8), lim = c(0, 0.8), 
                     labels = c("0%", "20%", "40%", "60%", "80%")) +
  ggtitle("EPL Results by Season") +
  xlab("Season") + ylab("Percentage")+
  theme_bw()

# EPL goals by Venues
hgoal.avg <- NULL
vgoal.avg <- NULL

for (i in 1:(iteration+1)) {
  temp.epl <- filter(epl.df, Season==i+min(epl.df$Season)-1)
  hgoal.avg[i] <- sum(temp.epl$hgoal) / nrow(temp.epl)
  vgoal.avg[i] <- sum(temp.epl$vgoal) / nrow(temp.epl)
}

df2 <- data.frame(hgoal.avg, vgoal.avg, min(epl.df$Season):max(epl.df$Season))
colnames(df2) <- c("home", "away", "season")
df2 <- df2 %>% gather(team, avg.goal,home:away)

ggplot(df2) + geom_point(aes(y=avg.goal, x=season, col=team)) +
  geom_smooth(aes(x=season, y=avg.goal,col=team))+
  #facet_grid(. ~ team) +
  ggtitle("EPL Goals by Venues") +
  xlab("Season") + ylab("Average Goal per Game") +
  theme_bw()

# Get Bundesliga data

epl.df <- germany %>% mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)))%>%
  select(home, visitor, Season, result)

year.home.win = NULL
year.draw = NULL
year.away.win = NULL

iteration <- max(epl.df$Season) - min(epl.df$Season)
for (i in 1:(iteration+1)) {
  temp.epl <- filter(epl.df, Season==i+min(epl.df$Season)-1)
  year.home.win[i] <- tally(temp.epl, result==1)/nrow(temp.epl)
  year.draw[i] <- tally(temp.epl, result==0.5)/nrow(temp.epl)
  year.away.win[i] <- tally(temp.epl, result==0)/nrow(temp.epl)
}

df.Bundesliga <- data.frame(data.frame(unlist(year.home.win)), data.frame(unlist(year.draw)), data.frame(unlist(year.away.win)), min(epl.df$Season):max(epl.df$Season), League = "Bundesliga") 
colnames(df.Bundesliga) <- c("home win", "draw", "away win", "season", "League")
df.Bundesliga <- df.Bundesliga %>% filter(`home win`>0,`away win`>0,draw>0)

# Get La Liga data

epl.df <- spain %>% mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)))%>%
  select(home, visitor, Season, result)

year.home.win = NULL
year.draw = NULL
year.away.win = NULL

iteration <- max(epl.df$Season) - min(epl.df$Season)
for (i in 1:(iteration+1)) {
  temp.epl <- filter(epl.df, Season==i+min(epl.df$Season)-1)
  year.home.win[i] <- tally(temp.epl, result==1)/nrow(temp.epl)
  year.draw[i] <- tally(temp.epl, result==0.5)/nrow(temp.epl)
  year.away.win[i] <- tally(temp.epl, result==0)/nrow(temp.epl)
}

df.Laliga <- data.frame(data.frame(unlist(year.home.win)), data.frame(unlist(year.draw)), data.frame(unlist(year.away.win)), min(epl.df$Season):max(epl.df$Season), League = "La Liga") 
colnames(df.Laliga) <- c("home win", "draw", "away win", "season", "League")
df.Laliga <- df.Laliga %>% filter(`home win`>0,`away win`>0,draw>0)

# Get Serie A data

epl.df <- italy %>% mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)))%>%
  select(home, visitor, Season, result)

year.home.win = NULL
year.draw = NULL
year.away.win = NULL

iteration <- max(epl.df$Season) - min(epl.df$Season)
for (i in 1:(iteration+1)) {
  temp.epl <- filter(epl.df, Season==i+min(epl.df$Season)-1)
  year.home.win[i] <- tally(temp.epl, result==1)/nrow(temp.epl)
  year.draw[i] <- tally(temp.epl, result==0.5)/nrow(temp.epl)
  year.away.win[i] <- tally(temp.epl, result==0)/nrow(temp.epl)
}

df.SerieA <- data.frame(data.frame(unlist(year.home.win)), data.frame(unlist(year.draw)), data.frame(unlist(year.away.win)), min(epl.df$Season):max(epl.df$Season), League = "Serie A") 
colnames(df.SerieA) <- c("home win", "draw", "away win", "season", "League")
df.SerieA <- df.SerieA %>% filter(`home win`>0,`away win`>0,draw>0)

epl.df <- england %>% filter(division==1)
year.home.win = NULL
year.draw = NULL
year.away.win = NULL

iteration <- max(epl.df$Season) - min(epl.df$Season)
for (i in 1:(iteration+1)) {
  temp.epl <- filter(epl.df, Season==i+min(epl.df$Season)-1)
  year.home.win[i] <- tally(temp.epl, result=="H")/nrow(temp.epl)
  year.draw[i] <- tally(temp.epl, result=="D")/nrow(temp.epl)
  year.away.win[i] <- tally(temp.epl, result=="A")/nrow(temp.epl)
}

df <- data.frame(data.frame(unlist(year.home.win)), data.frame(unlist(year.draw)), data.frame(unlist(year.away.win)), min(epl.df$Season):max(epl.df$Season)) 
colnames(df) <- c("home win", "draw", "away win", "season")
df <- df %>% filter(`home win`>0,`away win`>0,draw>0)

df.EPL <- df
df.EPL$League <- "EPL"

# Percentage of Home Wins, Away Wins, and Draws for Top 4 European leagues
df <- rbind(df.EPL, df.Bundesliga, df.Laliga, df.SerieA)
df <- df %>% gather("result","percentage", `home win`:`away win`)

ggplot(df, aes(x=df$season, y=df$percentage, col=League)) +
  facet_grid(.~df$result, scales = "free")+
  geom_point(alpha=1/8)+
  geom_smooth(span=0.7)+
  scale_y_continuous("",breaks = c(0, 0.2, 0.4, 0.6, 0.8), lim = c(0, 0.8), 
                     labels = c("0%","20%","40%", "60%", "80%"))+
  scale_x_continuous(lim=c(1928,2016))+
  ggtitle( "Percentage of Home Wins, Away Wins, and Draws for Top 4 European leagues")+
  xlab("Season")+
  theme_bw()

# Champions League
epl.df <- champs %>% mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)))%>%
  select(home, visitor, Season, result)

year.home.win = NULL
year.draw = NULL
year.away.win = NULL

iteration <- max(epl.df$Season) - min(epl.df$Season)
for (i in 1:(iteration+1)) {
  temp.epl <- filter(epl.df, Season==i+min(epl.df$Season)-1)
  year.home.win[i] <- tally(temp.epl, result==1)/nrow(temp.epl)
  year.draw[i] <- tally(temp.epl, result==0.5)/nrow(temp.epl)
  year.away.win[i] <- tally(temp.epl, result==0)/nrow(temp.epl)
}

df.CL <- data.frame(data.frame(unlist(year.home.win)), data.frame(unlist(year.draw)), data.frame(unlist(year.away.win)), min(epl.df$Season):max(epl.df$Season), League = "Champs") 
colnames(df.CL) <- c("home", "draw", "away", "season", "League")
df.CL <- df.CL %>% filter(home>0,away>0,draw>0)%>% gather(result, percentage, home:away)

# Champions League and European Cup Results by Year
ggplot(df.CL, aes(x=df.CL$season, y=df.CL$percentage, col=result)) + 
  geom_point()+
  geom_smooth(span=0.5, alpha=0.2)+
  scale_y_continuous(name="Percent", breaks=c(0.2, 0.4, 0.6), labels = c("20%", "40%", "60%"))+
  ggtitle("Champions League and European Cup Results by Year")+xlab("Season")+
  theme_bw()
