---
  title: "Homework-3"
author: "Baljinder Smagh"
date: "`r Sys.Date()`"
output:
  pdf_document: default
html_document: default
theme: cerulean
---
  
  ```{r}
library(tidyverse)
goalkeeper<-read_csv('goalkeepers-1920.csv')

players<-read_csv('players-1920.csv')

standings<-read_csv('standings-1920.csv')

scorefixtures<-read_csv('scoresfixtures-1920.csv')

team_stats<-read_csv('teamstats-1920.csv')

teamgoalkeeping<-read_csv('teamgoalkeeping-1920.csv')

```

###Q1(a)How many teams are there?
```{r}
no_of_teams<-length(unique(team_stats$Squad))
no_of_teams
```

###Q1(b)How many players are there?

```{r}
no_of_players<-length(unique(goalkeeper$Player))
no_of_players
```



###Q1(c)What is the total number of goals scored?
```{r}
no_of_goals<-sum(players$Gls)

no_of_goals
```

###Q1(d)What is the average attendance (spectators) at the games?

```{r}
avg_attendence<-mean(standings$Attendance)

avg_attendence
```

###Q(2)Identify the primary key(s) (if any) for each table. Verify your answers. Note: ignore the\Rk" variable, as it usually just represents the row number.
```{r}
goalkeeper %>% count(Player) %>% filter(n>1)
is.na(goalkeeper$Player)
standings%>% count(Goalkeeper,`Top Team Scorer`,Attendance) %>% filter(n>1)


standings%>% count(Squad) %>% filter(n>1)

team_stats%>% count(Squad)%>% filter(n>1)
is.na(team_stats$Squad)

team_stats %>% count(xG) %>% filter(n>1)
is.na(team_stats$xG)


team_stats%>% count(xA)%>% filter(n>1)
is.na(team_stats$xG)

teamgoalkeeping%>% count(Squad)%>% filter(n>1)
is.na(teamgoalkeeping$Squad)

teamgoalkeeping%>% count('Saves%')%>% filter(n>1)
is.na(teamgoalkeeping$Squad)

```

###Q4.For the players and goalkeepers data sets, the \Player" variable has two versions of the player name. Separate this out and keep ONLY the rst version of the name. The \Nation" variable also has two abbreviations listed for the nationalities of the players. Separate the values out into two variables (and keep both of them). Assign the resulting dataframes/tibbles to new names dierent from the original ones.

```{r}

players <- players %>% separate(Player,into=c("Player","Player_dummy"),sep = "\\\\") 
players <-players %>% separate(Nation, into = c('Nation','Nation_Cap'),sep = ' ')

players_new<-data.frame(players[,c(1,2,4:30)])

goalkeeper <- goalkeeper %>% separate(Player,into=c("Player","Player_dummy"),sep = "\\\\")
goalkeeper <-goalkeeper %>% separate(Nation, into = c('Nation','Nation_Cap'),sep = ' ')

goalkeeper_new<-data.frame(goalkeeper[,c(1,2,4:26)])

```
###Q5Find, for each team, the mean player age, the age of the youngest player and the age of the oldest player. Find the name of the youngest player for each team. Your output for the latter should only contain the team name, the minimum age, and the name of the player.

```{r}
age_statistics<-players %>% group_by(Squad) %>% summarise(mean=mean(Age,na.rm = TRUE),min=min(Age,na.rm = TRUE),max=max(Age,na.rm = TRUE),youngest_player=Player[which.min(Age)])

```

###Q6Add the team performance numbers (specically, Possession, Assists, Penalty Kicks scored, Save percentage, and Clean sheet percentage) to the standings table. Add also the age statistics found in Q5 above to the standings table. Then make the following plots (A vs B means A is on the y axis):

```{r}
standings<-Reduce(
  function(x, y) merge(x, y,by='Squad'), 
  list(standings,team_stats[,c(1,3,8)], teamgoalkeeping[,c(1,18,10,14)],age_statistics[,c(1,2:4)]))

```
###(a)Points scored vs Possession

```{r}
ggplot(data = standings,mapping = aes(x=Poss,y=Pts))+geom_line()

```

###(b) Points scored vs Save percentage
```{r}
ggplot(data = standings,mapping = aes(x='Save%',y=Pts))+geom_point() 
```

###(c)Points scored vs Goal against

```{r}
ggplot(data = standings,mapping = aes(x=GA,y=Pts)) + geom_line()
```


###(d)Clean sheet percentage vs Save percentage
```{r}
ggplot(data = standings,mapping = aes(x='Save%',y=CS)) + geom_point()
```
###Q7Identify the names of the top 3 referees that refereed the most games. Your R output should only show three referees. Obtain a list of the games they refereed. Your output for this should only contain columns for the week and date the game was played, name of the home and away teams, and the name of the referee.

```{r}
referees<-scorefixtures %>% count(Referee,sort = TRUE) %>% slice(1:3)
top_3_referee<-c(referees[,1])
top_3_referee

scorefixtures %>% group_by(Referee) %>% select(Wk,Date,Home,Away) %>% filter(Referee=="Anthony Taylor" | Referee=="Martin Atkinson" | Referee=="Michael Oliver" )

```
###Q8Obtain a list of the games played by the top 3 scoring teams (i.e. the 3 teams that scored the most goals), showing only week, date, home/away teams and the resu

```{r}
top_3_teams <-players %>% group_by(Squad) %>% summarise(Goals=sum(Gls)) %>% arrange(desc(Goals))

top_scorer_teams<-c(top_3_teams[c(1,2,3),c(1)])
top_scorer_teams

scorefixtures %>% select(Wk,Date,Home,Away,Score) %>% filter((Home=="Manchester City"| Home =="Liverpool" | Home=="Chelsea") & (Away=="Manchester City"| Away =="Liverpool" | Away=="Chelsea"))


```

###Q9 Find the 3 days with the most games played. Your R output should only show three days.Then obtain the list of games that were played on these days (you can keep all the columns).

```{r}
Dates<-scorefixtures %>% group_by(Date) %>% count(Date,sort = TRUE) 

top_3_days<-Dates[c(1:3),c(1)]
top_3_days

list_of_games<-scorefixtures %>% select(everything()) %>% filter(Date==as.Date("2020-07-26") | Date==as.Date("2019-12-26") | Date==as.Date("2020-01-01"))
list_of_games
```
###Q10 Find the 3 days with the most goals scored. Your resulting tibble should only have 3 rows.

```{r}

```






















