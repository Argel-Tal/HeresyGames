## libraryload
library(stringr)

## File load
setwd("C:/Users/64223/Documents/GitHub/HeresyGames")
DirSupportTbs <- file.path(getwd(), "support Tables")
files <- list.files(DirSupportTbs)
DirGamelog <- file.path(getwd(), "records/games")
DirPlayerLog <- file.path(getwd(), "records/player matches")
# how many fake games are already logged
ngames <- length(list.files(DirGamelog))

## tables for randomised selection
Factions <- read.table(file = file.path(DirSupportTbs, files[1]), sep = '\t', header = TRUE, quote = "")
ForceOrgs <- read.table(file = file.path(DirSupportTbs, files[2]), sep = '\t', header = TRUE, quote = "")
Maps     <- read.table(file = file.path(DirSupportTbs, files[3]), sep = '\t', header = TRUE, quote = "")
Missions <- read.table(file = file.path(DirSupportTbs, files[4]), sep = '\t', header = TRUE, quote = "")
Subfactions <- read.table(file = file.path(DirSupportTbs, files[5]), sep = '\t', header = TRUE, quote = "")
WarlordTraits <- read.table(file = file.path(DirSupportTbs, files[6]), sep = '\t', header = TRUE, quote = "")
ptSizes <- seq(1000,5000, 50)

gameHeaders <- c("gameSize", "mission", "deployment")
playerHeaders <- c("faction","forceOrg","ROW/Subfaction","alliedFaction"
                   ,"warlordTrait","VPs","warlord","listName"
                  ) # end headers

## Loop params
maxGenGames <- 15
index <- 1
## Loop 
for(index in 1:maxGenGames){
  # game meta
  map_mission <- sample(1:6, 2, replace=T)
  gameSize    <- sample(ptSizes, 1)
  meta <- as.data.frame(t(c(gameSize, map_mission)))
  colnames(meta) <- gameHeaders
  index = index + 1
  write.csv(meta, paste(DirGamelog, "/", str_pad(index+ngames, 4, pad = "0"), ".csv", sep = ""))
  # player 1
  for (playerCount in 1:2)
  
  # player 2
}

MakePlayerMeta <- function(){
  playerInitials <- paste(sample(LETTERS, 2, replace=TRUE), collapse="")
  faction <- Factions[sample(1:dim(Factions)[1], 1),2]
  parentFaction <- Factions[match(faction,Factions[,2]), 1]
  if (parentFaction == "Mechanicum"){
    forceOrg <- ForceOrgs[sample(1:dim(ForceOrgs)[1], 1),1] 
  } else { 
    forceOrg <- ForceOrgs[sample(2:dim(ForceOrgs)[1], 1), 1]
  }
  # pick subfactions they're allowed to choose from
  ROW <- Subfactions[sample(which(Subfactions[,2] == parentFaction), 1), 1]
  # randomly determine if an allied detach is present, and if so which
  if (sample(1:100, 1)%%4 == 0) {
    alliedFaction <- Factions[sample(1:dim(Factions)[1], 1),2]
  } else {
    alliedFaction <- ""
  }
  trait   <- WarlordTraits[sample(1:dim(WarlordTraits)[1], 1),3]
  warlord <- "jimmy space"
  VPs     <- sample(1:15, 1)
  listName    <- ""
  # dataframe stuff
  playerLog <- as.data.frame(t(c(faction, forceOrg, ROW, alliedFaction, trait, VPs, warlord, listName)))
  colnames(playerLog) <- playerHeaders
  return(playerLog)
}
