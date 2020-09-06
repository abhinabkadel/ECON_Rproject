# load libraries
library(dplyr)
library(ggplot2)
# load data
orig_data <- read.csv(file = "epl2020.csv")
# select only the necessary columns:
data <- select(orig_data, -c(X, xG, xGA, xpts, npxG, npxGA, deep, deep_allowed, 
       npxGD, ppda_cal, allowed_ppda, B365H.x, B365D.x, B365A.x))

# Let's see summary of the data:
summary(data)

tab = table(data$result)
cat ( "win percentage =", as.vector(tab)[3]*100/sum(data$h_a == 'h'), "%" )
cat ( "Most total goals in a match = ", max(data$scored + data$missed) )
message ( "Most goals scored by ", data$teamId[data$scored == max(data$scored) ])

# Chi-squared test - correlation between home/away matches and wins:
round(prop.table(table(data$h_a, data$wins), margin=1), 2)
chisq.test(data$h_a, data$wins)

# Look at the home games only:
data_home <- data[data$h_a == "h", ]
summary(data_home)

# plot multiple variables against goal scored to see if there is a linear 
# relationship:
ggplot(data_home, aes(x= HST.x, y = scored)) + 
  geom_point() + 
  labs(x="Shots on Target (Home team)", y="Goals scored (Home team)") +
  ggtitle ('goals scored vs shots on target')
ggplot(data_home, aes(x= AF.x , y = scored)) + 
  geom_point() +
  labs(x="Fouls committed (Away team)", y="Goals scored (Home team)") +
  ggtitle ('goals scored vs fouls conceded')
ggplot(data_home, aes(x= HC.x , y = scored)) + 
  geom_point() +
  labs(x="Corners won (Home team)", y="Goals scored (Home team)") +
  ggtitle ('goals scored vs corners won')

## Multiple linear regression to predict goals scored
m2 <- lm(scored ~ HST.x + HC.x + AF.x, data = data_home)
summary(m2)
anova(m2)

# Goals scored per team
p <- ggplot(data, aes(x= teamId , y = scored)) + 
  geom_boxplot() +
  labs(x="Team Name", y="Goals scored") +
  ggtitle ('Goals scored in a game for the EPL teams') 
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_y_continuous(breaks=c(0,1,2,3,5,7,9))

## Create Points Table:
pts_table = data[data$round == max(data$round) , 
                 c("teamId", "tot_points", "tot_goal", "tot_con", "round")]
pts_table2 = data[data$round == max(data$round)-1 , 
                  c("teamId", "tot_points", "tot_goal", "tot_con", "round")]
# filter teams that have not played all 29 matches:
teams = subset(pts_table2, !(pts_table2$teamId %in% pts_table$teamId))

## Alternative:
# diff_teams = setdiff(  unique(data$teamId), pts_table$teamId )
# teams = data[ which(data$round==28 &  data$teamId %in% diff_teams ), 
#              c("teamId", "tot_points", "tot_goal", "tot_con", "round") ]

# find the final pts_table by combining the teams with 28 games and 29 games
# respectively:
pts_table = rbind(pts_table, teams)
View(pts_table)