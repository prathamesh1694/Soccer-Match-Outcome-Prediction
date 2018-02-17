library("RSQLite")
library("dplyr")
library(ggplot2)
library(grid)
library(MASS)
library(tidyverse)
library(VGAM)
library(GGally)
library(reshape2)

# connect to the sqlite file
drv<- dbDriver("SQLite")
con = dbConnect(drv, dbname="database.sqlite")

# get a list of all tables
alltables = dbListTables(con)

Teams = dbGetQuery(con,'select team_api_id, team_long_name from Team')
Matches = dbGetQuery(con,'select id, season, home_team_api_id, away_team_api_id, home_team_goal,
                     away_team_goal from Match where league_id=1729 and season="2014/2015"')
Team_attr = dbGetQuery(con,'select id, team_api_id, buildUpPlaySpeed, buildUpPlayDribbling,
                       buildUpPlayPassing, chanceCreationPassing, chanceCreationCrossing,chanceCreationShooting,
                       defencePressure, defenceAggression, defenceTeamWidth from
                       Team_Attributes where date like "2014%"')

dbDisconnect(con)

colnames(Team_attr)[2]<-"home_team_api_id"
m1<-left_join(Matches, Team_attr, by="home_team_api_id")
colnames(m1)[8:16]<-c(paste("home", colnames(m1)[8:16], sep="_"))

colnames(Team_attr)[2]<-"away_team_api_id"
m2<-left_join(m1, Team_attr, by="away_team_api_id")
colnames(m2)[18:26]<-c(paste("away", colnames(m2)[18:26], sep="_"))

m3<-m2[-7]
m4<-m3[-16]

m4$result[m4$home_team_goal > m4$away_team_goal] = 0
m4$result[m4$home_team_goal == m4$away_team_goal] = 1
m4$result[m4$home_team_goal < m4$away_team_goal] = 2
m4$result<-as.factor(m4$result)

m6<-m4
colnames(Teams)[1]<-"home_team_api_id"
m6<-left_join(m6, Teams, by="home_team_api_id")
colnames(m6)[26]<-"home_team_name"

m4$buildUpPlaySpeed_ratio<-m4$home_buildUpPlaySpeed/m4$away_buildUpPlaySpeed
m4$buildUpPlayDribbling_ratio<-m4$home_buildUpPlayDribbling/m4$away_buildUpPlayDribbling
m4$buildUpPlayPassing_ratio<-m4$home_buildUpPlayPassing/m4$away_buildUpPlayPassing
m4$chanceCreationPassing_ratio<-m4$home_chanceCreationPassing/m4$away_chanceCreationPassing
m4$chanceCreationCrossing_ratio<-m4$home_chanceCreationCrossing/m4$away_chanceCreationCrossing
m4$chanceCreationShooting_ratio<-m4$home_chanceCreationShooting/m4$away_chanceCreationShooting
m4$defencePressure_ratio<-m4$home_defencePressure/m4$away_defencePressure
m4$defenceAggression_ratio<-m4$home_defenceAggression/m4$away_defenceAggression
m4$defenceTeamWidth_ratio<-m4$home_defenceTeamWidth/m4$away_defenceTeamWidth

m5<-m4[-5:-24]
colnames(Teams)[1]<-"home_team_api_id"
m5<-left_join(m5, Teams, by="home_team_api_id")
colnames(m5)[15]<-"home_team_name"

colnames(Teams)[1]<-"away_team_api_id"
m5<-left_join(m5, Teams, by="away_team_api_id")
colnames(m5)[16]<-"away_team_name"

team_aggregate = data.frame(c(unique(m5$home_team_api_id)))
colnames(team_aggregate)[1] = "away_team_api_id"
team_aggregate<-left_join(team_aggregate, Teams, by="away_team_api_id")
colnames(team_aggregate)[1]<- "team_id"
team_aggregate$wins = 0
team_aggregate$draws = 0
team_aggregate$loss = 0
for (row in 1:nrow(m5)){
  if(m5$result[row] ==0){
    team_aggregate$wins[which(team_aggregate$team_id == m5$home_team_api_id[row])] = team_aggregate$wins[which(team_aggregate$team_id == m5$home_team_api_id[row])]+1 
    team_aggregate$loss[which(team_aggregate$team_id == m5$away_team_api_id[row])] = team_aggregate$loss[which(team_aggregate$team_id == m5$away_team_api_id[row])]+1
    
  }
  if(m5$result[row] ==1){
    team_aggregate$draws[which(team_aggregate$team_id == m5$home_team_api_id[row])] = team_aggregate$draws[which(team_aggregate$team_id == m5$home_team_api_id[row])]+1
    team_aggregate$draws[which(team_aggregate$team_id == m5$away_team_api_id[row])] = team_aggregate$draws[which(team_aggregate$team_id == m5$away_team_api_id[row])]+1
    
  }
  if(m5$result[row] ==2){
    team_aggregate$loss[which(team_aggregate$team_id == m5$home_team_api_id[row])] = team_aggregate$loss[which(team_aggregate$team_id == m5$home_team_api_id[row])]+1 
    team_aggregate$wins[which(team_aggregate$team_id == m5$away_team_api_id[row])] = team_aggregate$wins[which(team_aggregate$team_id == m5$away_team_api_id[row])]+1
  }
}
team_aggregate_with_points = team_aggregate
team_aggregate_with_points$points = (3*team_aggregate_with_points$wins)+(team_aggregate_with_points$draws)
team_aggregate_with_points = team_aggregate_with_points[order(-team_aggregate_with_points$points),]
team_aggregate = team_aggregate[-1]
team_aggregate = team_aggregate[,c(1,4,3,2)]
team_aggregate.m <- melt(team_aggregate,id.vars = "team_long_name")


stack = ggplot(team_aggregate.m, aes(x = team_long_name, y = value,fill=variable)) +
  geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab(c('Team name'))

team_aggregate = data.frame(c(unique(m5$home_team_api_id)))
colnames(team_aggregate)[1] = "away_team_api_id"
team_aggregate<-left_join(team_aggregate, Teams, by="away_team_api_id")
colnames(team_aggregate)[1]<- "team_id"
team_aggregate$wins = 0
team_aggregate$draws = 0
team_aggregate$loss = 0
for (row in 1:nrow(m5)){
  if(m5$result[row] ==0){
    team_aggregate$wins[which(team_aggregate$team_id == m5$home_team_api_id[row])] = team_aggregate$wins[which(team_aggregate$team_id == m5$home_team_api_id[row])]+1 
    team_aggregate$loss[which(team_aggregate$team_id == m5$away_team_api_id[row])] = team_aggregate$loss[which(team_aggregate$team_id == m5$away_team_api_id[row])]+1
    
  }
  if(m5$result[row] ==1){
    team_aggregate$draws[which(team_aggregate$team_id == m5$home_team_api_id[row])] = team_aggregate$draws[which(team_aggregate$team_id == m5$home_team_api_id[row])]+1
    team_aggregate$draws[which(team_aggregate$team_id == m5$away_team_api_id[row])] = team_aggregate$draws[which(team_aggregate$team_id == m5$away_team_api_id[row])]+1
    
  }
  if(m5$result[row] ==2){
    team_aggregate$loss[which(team_aggregate$team_id == m5$home_team_api_id[row])] = team_aggregate$loss[which(team_aggregate$team_id == m5$home_team_api_id[row])]+1 
    team_aggregate$wins[which(team_aggregate$team_id == m5$away_team_api_id[row])] = team_aggregate$wins[which(team_aggregate$team_id == m5$away_team_api_id[row])]+1
  }
}
team_aggregate_with_points = team_aggregate
team_aggregate_with_points$points = (3*team_aggregate_with_points$wins)+(team_aggregate_with_points$draws)
team_aggregate_with_points = team_aggregate_with_points[order(-team_aggregate_with_points$points),]
team_aggregate_with_points = team_aggregate_with_points[,c(1,6)]

colnames(team_aggregate_with_points)[1]<-"home_team_api_id"
m6<-left_join(m6, team_aggregate_with_points, by="home_team_api_id")

res = m5$result
cor = ggpairs(data.frame(m5[,6:14], res) , aes(color = res)) + labs(title = "Pairs Plot of Attributes")

g1 = ggplot(m6, aes(x=reorder(home_team_name, -points), group=1)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x='Teams') +
  geom_line(aes(y=home_buildUpPlaySpeed, color='violet')) +
  geom_line(aes(y=home_buildUpPlayDribbling, color='indigo')) +
  geom_line(aes(y=home_buildUpPlayPassing, color='blue')) +
  scale_color_discrete(name = "Team attributes",
                       labels = c('Build Up Speed',
                                  'Dribbling',
                                  'Build Up Play Passing'
                                  ))
  
g2 = ggplot(m6, aes(x=reorder(home_team_name, -points), group=1)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x='Teams') +  
  geom_line(aes(y=home_chanceCreationPassing, color='green')) +
  geom_line(aes(y=home_chanceCreationCrossing, color='yellow')) +
  geom_line(aes(y=home_chanceCreationShooting, color='orange')) + 
    scale_color_discrete(name = "",
                         labels = c('Chance Creation Passing',
                                    'Crossing',
                                    'Shooting',
                                    'Defence Pressure',
                                    'Defence Aggression',
                                    'Defence Team Width'))
  
g3 = ggplot(m6, aes(x=reorder(home_team_name, -points), group=1)) + 
    theme(axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x='Teams') +  
  geom_line(aes(y=home_defencePressure, color='red')) +
  geom_line(aes(y=home_defenceAggression, color='pink')) +
  geom_line(aes(y=home_defenceTeamWidth, color='black')) +
  scale_color_discrete(name = "",
                       labels = c('Defence Pressure',
                                  'Defence Aggression',
                                  'Defence Team Width'))

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(g1, vp = vplayout(1, 1))
print(g2, vp = vplayout(2, 1))
print(g3, vp = vplayout(3, 1))


m5.polr = polr(factor(result)~buildUpPlaySpeed_ratio, data = m5)
speed = data.frame(buildUpPlaySpeed_ratio=seq(min(m5$buildUpPlaySpeed_ratio),
                                              max(m5$buildUpPlaySpeed_ratio), .01))
outcome.probs = predict(m5.polr, newdata = data.frame(speed), type="prob")
outcome.probs.df = data.frame(speed, outcome.probs)
names(outcome.probs.df) = c('buildUpPlaySpeed_ratio', 'win', 'draw', 'loss')
outcome.probs.long = outcome.probs.df %>% gather(outcome, probability, 2:4)
outcome.probs.long$outcome = factor(outcome.probs.long$outcome, levels =
                                      c('win', 'draw', 'loss'))
g1 = ggplot(outcome.probs.long, aes(x=buildUpPlaySpeed_ratio, y=probability,
                                    group=outcome,color=outcome)) + geom_line() +
  labs(x='Build Up Play Speed Ratio')


m5.polr = polr(factor(result)~buildUpPlayDribbling_ratio, data = m5)
speed = data.frame(buildUpPlayDribbling_ratio=seq(min(m5$buildUpPlayDribbling_ratio),
                                              max(m5$buildUpPlayDribbling_ratio), .01))
outcome.probs = predict(m5.polr, newdata = data.frame(speed), type="prob")
outcome.probs.df = data.frame(speed, outcome.probs)
names(outcome.probs.df) = c('buildUpPlayDribbling_ratio', 'win', 'draw', 'loss')
outcome.probs.long = outcome.probs.df %>% gather(outcome, probability, 2:4)
outcome.probs.long$outcome = factor(outcome.probs.long$outcome, levels =
                                      c('win', 'draw', 'loss'))
g2 = ggplot(outcome.probs.long, aes(x=buildUpPlayDribbling_ratio, y=probability,
                                    group=outcome, color=outcome)) + geom_line() +
  labs(x='Build Up Play Dribbling Ratio')


m5.polr = polr(factor(result)~buildUpPlayPassing_ratio, data = m5)
speed = data.frame(buildUpPlayPassing_ratio=seq(min(m5$buildUpPlayPassing_ratio),
                                                  max(m5$buildUpPlayPassing_ratio),
                                                .01))
outcome.probs = predict(m5.polr, newdata = data.frame(speed), type="prob")
outcome.probs.df = data.frame(speed, outcome.probs)
names(outcome.probs.df) = c('buildUpPlayPassing_ratio', 'win', 'draw', 'loss')
outcome.probs.long = outcome.probs.df %>% gather(outcome, probability, 2:4)
outcome.probs.long$outcome = factor(outcome.probs.long$outcome, levels =
                                      c('win', 'draw', 'loss'))
g3 = ggplot(outcome.probs.long, aes(x=buildUpPlayPassing_ratio, y=probability,
                                    group=outcome, color=outcome)) + geom_line() +
  labs(x='Build Up Play Passing Ratio')


m5.polr = polr(factor(result)~chanceCreationPassing_ratio, data = m5)
speed = data.frame(chanceCreationPassing_ratio=seq(min(m5$chanceCreationPassing_ratio),
                                                max(m5$chanceCreationPassing_ratio),
                                                .01))
outcome.probs = predict(m5.polr, newdata = data.frame(speed), type="prob")
outcome.probs.df = data.frame(speed, outcome.probs)
names(outcome.probs.df) = c('chanceCreationPassing_ratio', 'win', 'draw', 'loss')
outcome.probs.long = outcome.probs.df %>% gather(outcome, probability, 2:4)
outcome.probs.long$outcome = factor(outcome.probs.long$outcome, levels =
                                      c('win', 'draw', 'loss'))
g4 = ggplot(outcome.probs.long, aes(x=chanceCreationPassing_ratio, y=probability,
                                    group=outcome, color=outcome)) + geom_line() +
  labs(x='Chance Crreation Passing Ratio')


m5.polr = polr(factor(result)~chanceCreationCrossing_ratio, data = m5)
speed = data.frame(chanceCreationCrossing_ratio=seq(min(m5$chanceCreationCrossing_ratio),
                                                   max(m5$chanceCreationCrossing_ratio),
                                                   .01))
outcome.probs = predict(m5.polr, newdata = data.frame(speed), type="prob")
outcome.probs.df = data.frame(speed, outcome.probs)
names(outcome.probs.df) = c('chanceCreationCrossing_ratio', 'win', 'draw', 'loss')
outcome.probs.long = outcome.probs.df %>% gather(outcome, probability, 2:4)
outcome.probs.long$outcome = factor(outcome.probs.long$outcome, levels =
                                      c('win', 'draw', 'loss'))
g5 = ggplot(outcome.probs.long, aes(x=chanceCreationCrossing_ratio, y=probability,
                                    group=outcome, color=outcome)) + geom_line() +
  labs(x='Chance Crreation Crossing Ratio')


m5.polr = polr(factor(result)~chanceCreationShooting_ratio, data = m5)
speed = data.frame(chanceCreationShooting_ratio=seq(min(m5$chanceCreationShooting_ratio),
                                                    max(m5$chanceCreationShooting_ratio),
                                                    .01))
outcome.probs = predict(m5.polr, newdata = data.frame(speed), type="prob")
outcome.probs.df = data.frame(speed, outcome.probs)
names(outcome.probs.df) = c('chanceCreationShooting_ratio', 'win', 'draw', 'loss')
outcome.probs.long = outcome.probs.df %>% gather(outcome, probability, 2:4)
outcome.probs.long$outcome = factor(outcome.probs.long$outcome, levels =
                                      c('win', 'draw', 'loss'))
g6 = ggplot(outcome.probs.long, aes(x=chanceCreationShooting_ratio, y=probability,
                                    group=outcome, color=outcome)) + geom_line() +
  labs(x='Chance Crreation Shooting Ratio')


m5.polr = polr(factor(result)~defencePressure_ratio, data = m5)
speed = data.frame(defencePressure_ratio=seq(min(m5$defencePressure_ratio),
                                                    max(m5$defencePressure_ratio),
                                                    .01))
outcome.probs = predict(m5.polr, newdata = data.frame(speed), type="prob")
outcome.probs.df = data.frame(speed, outcome.probs)
names(outcome.probs.df) = c('defencePressure_ratio', 'win', 'draw', 'loss')
outcome.probs.long = outcome.probs.df %>% gather(outcome, probability, 2:4)
outcome.probs.long$outcome = factor(outcome.probs.long$outcome, levels =
                                      c('win', 'draw', 'loss'))
g7 = ggplot(outcome.probs.long, aes(x=defencePressure_ratio, y=probability,
                                    group=outcome, color=outcome)) + geom_line() +
  labs(x='Defence Pressure Ratio')


m5.polr = polr(factor(result)~defenceAggression_ratio, data = m5)
speed = data.frame(defenceAggression_ratio=seq(min(m5$defenceAggression_ratio),
                                             max(m5$defenceAggression_ratio),
                                             .01))
outcome.probs = predict(m5.polr, newdata = data.frame(speed), type="prob")
outcome.probs.df = data.frame(speed, outcome.probs)
names(outcome.probs.df) = c('defenceAggression_ratio', 'win', 'draw', 'loss')
outcome.probs.long = outcome.probs.df %>% gather(outcome, probability, 2:4)
outcome.probs.long$outcome = factor(outcome.probs.long$outcome, levels =
                                      c('win', 'draw', 'loss'))
g8 = ggplot(outcome.probs.long, aes(x=defenceAggression_ratio, y=probability,
                                    group=outcome, color=outcome)) + geom_line() +
  labs(x='Defence Aggression Ratio')


m5.polr = polr(factor(result)~defenceTeamWidth_ratio, data = m5)
speed = data.frame(defenceTeamWidth_ratio=seq(min(m5$defenceTeamWidth_ratio),
                                               max(m5$defenceTeamWidth_ratio),
                                               .01))
outcome.probs = predict(m5.polr, newdata = data.frame(speed), type="prob")
outcome.probs.df = data.frame(speed, outcome.probs)
names(outcome.probs.df) = c('defenceTeamWidth_ratio', 'win', 'draw', 'loss')
outcome.probs.long = outcome.probs.df %>% gather(outcome, probability, 2:4)
outcome.probs.long$outcome = factor(outcome.probs.long$outcome, levels =
                                      c('win', 'draw', 'loss'))
g9 = ggplot(outcome.probs.long, aes(x=defenceTeamWidth_ratio, y=probability,
                                    group=outcome, color=outcome)) + geom_line() +
  labs(x='Defence Team Width Ratio')

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(g1, vp = vplayout(1, 1))
print(g2, vp = vplayout(1, 2))
print(g3, vp = vplayout(1, 3))

print(g4, vp = vplayout(2, 1))
print(g5, vp = vplayout(2, 2))
print(g6, vp = vplayout(2, 3))

print(g7, vp = vplayout(3, 1))
print(g8, vp = vplayout(3, 2))
print(g9, vp = vplayout(3, 3))


#fitting polr
set.seed(345)
train = sample(1:nrow(m5), .7*nrow(m5))
m5.train = m5[train,]

#model 1 
m5.polr1 <- polr(factor(result) ~ buildUpPlaySpeed_ratio+buildUpPlayDribbling_ratio+
                 buildUpPlayPassing_ratio+chanceCreationPassing_ratio+
                 chanceCreationCrossing_ratio+chanceCreationShooting_ratio+
                 defencePressure_ratio+defenceAggression_ratio
                +defenceTeamWidth_ratio, data = m5.train)

predictions1 = predict(m5.polr1, newdata=m5[-train,], type='probs')
predict.value1 = ifelse(predictions1[,1]>=predictions1[,2] & predictions[,1]>=predictions1[,3],
                       0, ifelse(predictions1[,2]>=predictions1[,3], 1, 2))
confusion_matrix1 = table(m5[-train,]$result, predict.value1)

#model 2
m5.polr2 <- polr(factor(result) ~ buildUpPlaySpeed_ratio+buildUpPlayDribbling_ratio+
                  buildUpPlayPassing_ratio+ chanceCreationCrossing_ratio+
                  chanceCreationShooting_ratio+defencePressure_ratio+
                  defenceAggression_ratio+defenceTeamWidth_ratio, data = m5.train)

predictions2 = predict(m5.polr2, newdata=m5[-train,], type='probs')
predict.value2 = ifelse(predictions2[,1]>=predictions2[,2] & predictions2[,1]>=predictions2[,3],
                       0, ifelse(predictions2[,2]>=predictions2[,3], 1, 2))
confusion_matrix2 = table(m5[-train,]$result, predict.value2)



m5.polr3 <- polr(factor(result) ~ buildUpPlaySpeed_ratio+buildUpPlayDribbling_ratio+
                  buildUpPlayPassing_ratio+chanceCreationPassing_ratio+
                  chanceCreationShooting_ratio+ 
                  defencePressure_ratio+defenceAggression_ratio, data = m5.train)

predictions3 = predict(m5.polr3, newdata=m5[-train,], type='probs')
predict.value3 = ifelse(predictions3[,1]>=predictions3[,2] & predictions3[,1]>=predictions3[,3],
                       0, ifelse(predictions3[,2]>=predictions3[,3], 1, 2))
confusion_matrix3 = table(m5[-train,]$result, predict.value3)

#lda/qda
m6 = m5[,c(6,7,8,9,10,11,12,13,14,5)]
m6.lda= lda(result ~ . ,data = m6)
m6.lda.values <- predict(m6.lda)
m6.ldaplot=data.frame(cbind(m6.lda.values$x,factor(m6$result)))
ggplot(m6.ldaplot,aes(x=LD1,y=LD2,col=factor(m6$result))) +geom_point() + scale_color_discrete(name = 'Result', labels = c('Win', 'Draw', 'Loss'))
n=380
ntr=304
nte=n-ntr
rep=100
# LDA
set.seed(123456)
errlda=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,ntr)
  ## linear discriminant analysis
  m1=lda(result ~ .,m6[train,])
  predict(m1,m6[-train,])$class
  tablda=table(m6$result[-train],predict(m1,m6[-train,])$class)
  errlda[k]=(nte-sum(diag(tablda)))/nte
}
merrlda=mean(errlda)
merrlda

set.seed(123456)
errqda=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,ntr)
  # quadratic discriminant analysis
  m2=qda(result~.,m6[train,])
  predict(m2,m6[-train,])$class
  tabqda=table(m6$result[-train],predict(m2,m6[-train,])$class)
  errqda[k]=(nte-sum(diag(tabqda)))/nte
}
merrqda=mean(errqda)
merrqda