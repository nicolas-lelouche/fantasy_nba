library(googleVis)
library(ballr)
#All_time rankings
start_year = 1978 #years turnovers started getting counted
end_year = 2020 #current season
TotalStatsAllTime <- data.frame()

for(year in start_year:end_year)
{
  adv_stats <- NBAPerGameAdvStatistics(year)
  adv_stats <- subset(adv_stats, select = c(per, mp, tspercent, ws))
  norm_stats <- NBAPerGameStatistics(year)
  norm_stats <- subset(norm_stats, select = c(-link,-mp))
  
  temp <- cbind(norm_stats,adv_stats)
  if(year < 2000)
  {
    temp$label <- paste(temp$player, as.character(year-1901),"-",as.character(year-1900))
  }else
  {
    if(year > 2000){
      temp$label <- paste(temp$player, as.character(year-2001),"-",as.character(year-2000))
    }
    else {
      temp$label <- paste(temp$player, "99","-", "00")
    }
  }
  max_games <- 0.75 * max(norm_stats$g)
  temp <- subset(temp, g > max_games)
  TotalStatsAllTime <- rbind(TotalStatsAllTime,temp)
  print(paste0(as.character((year - 1978)*100/(43)),"% done"))
  
}

TotalStatsAllTime[is.na(TotalStatsAllTime)] <- 0 #remove NAs

clusters <- kmeans(cbind(TotalStatsAllTime$pts,TotalStatsAllTime$trb,TotalStatsAllTime$ast,TotalStatsAllTime$per, TotalStatsAllTime$tspercent, TotalStatsAllTime$ws),8, iter.max = 50)

TotalStatsAllTime$cluster <- clusters$cluster

chart21 <- 
  gvisBubbleChart(TotalStatsAllTime,
                  xvar = "mp", yvar="per",
                  colorvar = "cluster", sizevar = "ws",
                  options = list(
                    width=800, height=600
                  ))

plot(chart21)

best_cluster_per <- max(clusters$centers[,4])
list_of_clusters <- row.names(clusters$centers)

best_cluster <- names(which(clusters$center[as.numeric(list_of_clusters),4] == best_cluster_per))

AllTimeGreats <- TotalStatsAllTime[which(TotalStatsAllTime$cluster == best_cluster),]

chart22 <- 
  gvisBubbleChart(AllTimeGreats,
                  idvar="label",
                  xvar = "mp", yvar="per",
                  colorvar = "pos", sizevar = "ws",
                  options = list(
                    width=800, height=600
                  ))
plot(chart22)

AllTimeGreats <- subset(AllTimeGreats, mp > 1500)

chart23 <- 
  gvisBubbleChart(AllTimeGreats,
                  idvar="label",
                  xvar = "mp", yvar="per",
                  colorvar = "pos", sizevar = "ws",
                  options = list(
                    width=800, height=1000
                  ))
plot(chart23)


chart24 <- 
  gvisBubbleChart(AllTimeGreats[which(AllTimeGreats$pos == "PG"),],
                  idvar="label",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "ast",
                  options = list(
                    width=800, height=1000
                  ))
plot(chart24)

chart25 <- 
  gvisBubbleChart(AllTimeGreats[which(AllTimeGreats$pos == "SG"),],
                  idvar="label",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "pts",
                  options = list(
                    width=800, height=1000
                  ))
plot(chart25)

chart26 <- 
  gvisBubbleChart(AllTimeGreats[which(AllTimeGreats$pos == "SF"),],
                  idvar="label",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "pts",
                  options = list(
                    width=800, height=1000
                  ))
plot(chart26)

chart27 <- 
  gvisBubbleChart(AllTimeGreats[which(AllTimeGreats$pos == "PF"),],
                  idvar="label",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "pts",
                  options = list(
                    width=800, height=1000
                  ))
plot(chart27)

chart28 <- 
  gvisBubbleChart(AllTimeGreats[which(AllTimeGreats$pos == "C"),],
                  idvar="label",
                  xvar = "mp", yvar="per",
                  colorvar = "tm", sizevar = "trb",
                  options = list(
                    width=800, height=1000,
                  ))
plot(chart28)


#BEST FANTASY TEAM OF ALL TIME
AllTimeGreats$fantasy <- AllTimeGreats$pts + 1.2*AllTimeGreats$ast + 1.5*AllTimeGreats$trb + 3*AllTimeGreats$stl + 3*AllTimeGreats$blk - AllTimeGreats$tov
AllTimeGreats <- AllTimeGreats[order(-AllTimeGreats$fantasy),]
chart35 <-
  gvisColumnChart(AllTimeGreats[1:10,],
                  xvar="label",
                  yvar="fantasy")
plot(chart35)

chart30 <-
  gvisColumnChart(AllTimeGreats[which(AllTimeGreats$pos == "PG"),][1:10,],
                  xvar="label",
                  yvar="fantasy")
plot(chart30)

chart31 <-
  gvisColumnChart(AllTimeGreats[which(AllTimeGreats$pos == "SG"),][1:10,],
                  xvar="label",
                  yvar="fantasy")
plot(chart31)

chart32 <-
  gvisColumnChart(AllTimeGreats[which(AllTimeGreats$pos == "PF"),][1:10,],
                  xvar="label",
                  yvar="fantasy")
plot(chart32)

chart33 <-
  gvisColumnChart(AllTimeGreats[which(AllTimeGreats$pos == "SF"),][1:10,],
                  xvar="label",
                  yvar="fantasy")
plot(chart33)

chart34 <-
  gvisColumnChart(AllTimeGreats[which(AllTimeGreats$pos == "C"),][1:10,],
                  xvar="label",
                  yvar="fantasy")
plot(chart34)

bestteam <- rbind(AllTimeGreats[which(AllTimeGreats$pos == "PG"),][1,],AllTimeGreats[which(AllTimeGreats$pos == "SG"),][1,],
                  AllTimeGreats[which(AllTimeGreats$pos == "SF"),][1,],AllTimeGreats[which(AllTimeGreats$pos == "PF"),][1,],
                  AllTimeGreats[which(AllTimeGreats$pos == "C"),][1,])

bestteam$fantasy.style <- c("blue","red","green","green","black")
sum(bestteam$fantasy)
chart40 <-
  gvisColumnChart(bestteam,
                  xvar = "label", 
                  yvar  = c("fantasy","fantasy.style"),
                  options = list(
                    vAxis = '{minValue:0, maxValue:65}',
                    legend = 'none'
                  ))
plot(chart40)