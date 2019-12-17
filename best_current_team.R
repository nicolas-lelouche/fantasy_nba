library(ballr)
library(googleVis)

AdvancedStats <- NBAPerGameAdvStatistics(season = 2020)
NormalStats <- NBAPerGameStatistics(season = 2020)

AdvancedStats <- subset(AdvancedStats, select = c(g,mp,per,tm,tspercent,ws,vorp,usgpercent))
NormalStats <- subset(NormalStats, select = c(player,pos,trb,ast,pts,blk,stl,tov))

AdvancedStats[is.na(AdvancedStats)] <- 0
NormalStats[is.na(NormalStats)] <- 0

TotalStats <- cbind(AdvancedStats, NormalStats)
TotalStats <- subset(TotalStats, g > 15)

test <- NBAStandingsByDate(date_string = "2019-12-11")
names(test$East)[1] <- "tm"
names(test$West)[1] <- "tm"

fullstandings <- rbind(test$East, test$West)

fullstandings[which(grepl("Milwaukee Bucks*", fullstandings$tm)),]$tm <- "MIL"
fullstandings[which(grepl("Miami Heat*", fullstandings$tm)),]$tm <- "MIA"
fullstandings[which(grepl("Philadelphia 76ers*", fullstandings$tm)),]$tm <- "PHI"
fullstandings[which(grepl("Boston Celtics*", fullstandings$tm)),]$tm <- "BOS"
fullstandings[which(grepl("Toronto Raptors*", fullstandings$tm)),]$tm <- "TOR"
fullstandings[which(grepl("Indiana Pacers*", fullstandings$tm)),]$tm <- "IND"
fullstandings[which(grepl("Brooklyn Nets*", fullstandings$tm)),]$tm <- "BRK"
fullstandings[which(grepl("Orlando Magic*", fullstandings$tm)),]$tm <- "ORL"
fullstandings[which(grepl("Charlotte Hornets*", fullstandings$tm)),]$tm <- "CHO"
fullstandings[which(grepl("Detroit Pistons*", fullstandings$tm)),]$tm <- "DET"
fullstandings[which(grepl("Chicago Bulls*", fullstandings$tm)),]$tm <- "CHI"
fullstandings[which(grepl("Washington Wizards*", fullstandings$tm)),]$tm <- "WAS"
fullstandings[which(grepl("Atlanta Hawks*", fullstandings$tm)),]$tm <- "ATL"
fullstandings[which(grepl("Cleveland Cavaliers*", fullstandings$tm)),]$tm <- "CLE"
fullstandings[which(grepl("New York Knicks*", fullstandings$tm)),]$tm <- "NYK"
fullstandings[which(grepl("Los Angeles Lakers*", fullstandings$tm)),]$tm <- "LAL"
fullstandings[which(grepl("Los Angeles Clippers*", fullstandings$tm)),]$tm <- "LAC"
fullstandings[which(grepl("Dallas Mavericks*", fullstandings$tm)),]$tm <- "DAL"
fullstandings[which(grepl("Houston Rockets*", fullstandings$tm)),]$tm <- "HOU"
fullstandings[which(grepl("Denver Nuggets*", fullstandings$tm)),]$tm <- "DEN"
fullstandings[which(grepl("Utah Jazz*", fullstandings$tm)),]$tm <- "UTA"
fullstandings[which(grepl("Oklahoma City Thunder*", fullstandings$tm)),]$tm <- "OKC"
fullstandings[which(grepl("Phoenix Suns*", fullstandings$tm)),]$tm <- "PHO"
fullstandings[which(grepl("Sacramento Kings*", fullstandings$tm)),]$tm <- "SAC"
fullstandings[which(grepl("Minnesota Timberwolves*", fullstandings$tm)),]$tm <- "MIN"
fullstandings[which(grepl("Portland Trail Blazers*", fullstandings$tm)),]$tm <- "POR"
fullstandings[which(grepl("San Antonio Spurs*", fullstandings$tm)),]$tm <- "SAS"
fullstandings[which(grepl("Memphis Grizzlies*", fullstandings$tm)),]$tm <- "MEM"
fullstandings[which(grepl("New Orleans Pelicans*", fullstandings$tm)),]$tm <- "NOP"
fullstandings[which(grepl("Golden State Warriors*", fullstandings$tm)),]$tm <- "GSW"

TotalStats <- merge(TotalStats, fullstandings, all.x = TRUE)

clusters <- kmeans(cbind(TotalStats$pts, TotalStats$trb, TotalStats$ast, TotalStats$tspercent, TotalStats$per, TotalStats$ws, TotalStats$vorp),3, iter.max = 50)

TotalStats$cluster <- clusters$cluster

library(googleVis)
chart1 <- 
  gvisBubbleChart(TotalStats,
                  idvar="player",
                  xvar = "mp", yvar="per",
                  colorvar = "cluster", sizevar = "ws",
                  options = list(
                    width=800, height=600
                  ))

plot(chart1)

goodcluster <- TotalStats$cluster[which(TotalStats$player == "LeBron James")]

GoodPlayers <- TotalStats[which(TotalStats$cluster == as.numeric(goodcluster)),]

chart2 <- 
  gvisBubbleChart(GoodPlayers,
                  idvar="player",
                  xvar = "mp", yvar="per",
                  colorvar = "pos", sizevar = "w_lpercent",
                  options = list(
                    width=800, height=600
                  ))

plot(chart2)

#POINT GUARDS
PGs <- GoodPlayers[which(GoodPlayers$pos == "PG"),]

chart3 <- 
  gvisBubbleChart(PGs,
                  idvar="player",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "ast",
                  options = list(
                    width=800, height=600
                  ))

plot(chart3)


#CENTERS
Cs <- GoodPlayers[which(GoodPlayers$pos == "C"),]
chart4 <- 
  gvisBubbleChart(Cs,
                  idvar="player",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "trb",
                  options = list(
                    width=800, height=600
                  ))

plot(chart4)

#Shooting Guards
SGs <- GoodPlayers[which(GoodPlayers$pos == "SG"),]
chart5 <- 
  gvisBubbleChart(SGs,
                  idvar="player",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "pts",
                  options = list(
                    width=800, height=600
                  ))

plot(chart5)

#Power Forwards
PFs <- GoodPlayers[which(GoodPlayers$pos == "PF"),]
chart6 <- 
  gvisBubbleChart(PFs,
                  idvar="player",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "pts",
                  options = list(
                    width=800, height=600
                  ))

plot(chart6)

#Strong Forwards
SFs <- GoodPlayers[which(GoodPlayers$pos == "SF"),]
chart7 <- 
  gvisBubbleChart(SFs,
                  idvar="player",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "pts",
                  options = list(
                    width=800, height=600
                  ))

plot(chart7)

#_______________________________________________________________________
#Fantasy Analysis

PGs$fantasy <- PGs$pts + 1.2*PGs$ast + 1.5*PGs$trb + 3*PGs$stl + 3*PGs$blk - PGs$tov
Cs$fantasy <- Cs$pts + 1.2*Cs$ast + 1.5*Cs$trb + 3*Cs$stl + 3*Cs$blk - Cs$tov
SGs$fantasy <- SGs$pts + 1.2*SGs$ast + 1.5*SGs$trb + 3*SGs$stl + 3*SGs$blk - SGs$tov
PFs$fantasy <- PFs$pts + 1.2*PFs$ast + 1.5*PFs$trb + 3*PFs$stl + 3*PFs$blk - PFs$tov
SFs$fantasy <- SFs$pts + 1.2*SFs$ast + 1.5*SFs$trb + 3*SFs$stl + 3*SFs$blk - SFs$tov

PGs <- PGs[order(-PGs$fantasy),]

chart11 <-
  gvisColumnChart(PGs,
                  xvar="player",
                  yvar="fantasy")

plot(chart11)

Cs <- Cs[order(-Cs$fantasy),]
chart12 <-
  gvisColumnChart(Cs,
                  xvar="player",
                  yvar="fantasy")

plot(chart12)

SGs <- SGs[order(-SGs$fantasy),]
chart13 <-
  gvisColumnChart(SGs,
                  xvar="player",
                  yvar="fantasy")

plot(chart13)

PFs <- PFs[order(-PFs$fantasy),]
chart14 <-
  gvisColumnChart(PFs,
                  xvar="player",
                  yvar="fantasy")

plot(chart14)

SFs <- SFs[order(-SFs$fantasy),]
chart15 <-
  gvisColumnChart(SFs,
                  xvar="player",
                  yvar="fantasy")

plot(chart15)



bestfantasyplayer <- rbind(PGs,SGs,Cs,SFs,PFs)
bestfantasyplayer <- bestfantasyplayer[order(-bestfantasyplayer$fantasy),]
bestfantasyplayer$fantasy.style = c('blue','red','green','yellow','purple')
chart16 <-
  gvisColumnChart(bestfantasyplayer,
                  xvar="player",
                  yvar="fantasy")

plot(chart16)

bestpossibleteam <- rbind(PGs[which(PGs$fantasy == max(PGs$fantasy)),],
                          SGs[which(SGs$fantasy == max(SGs$fantasy)),],
                          SFs[which(SFs$fantasy == max(SFs$fantasy)),],
                          PFs[which(PFs$fantasy == max(PFs$fantasy)),],
                          Cs[which(Cs$fantasy == max(Cs$fantasy)),])

sum(bestpossibleteam$fantasy)

bestpossibleteam$fantasy.style <- c("blue","red","black","green","grey")

chart17 <-
  gvisColumnChart(bestpossibleteam,
                  xvar = "player", 
                  yvar  = c("fantasy","fantasy.style"),
                  options = list(
                    vAxis = '{minValue:0, maxValue:65}',
                    legend = 'none'
                  ))
plot(chart17)