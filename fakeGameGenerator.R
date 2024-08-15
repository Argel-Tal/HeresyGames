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
Warlords  <- read.table(file = file.path(DirSupportTbs, files[1]), sep = '\t', header = TRUE, quote = "")
Factions  <- read.table(file = file.path(DirSupportTbs, files[2]), sep = '\t', header = TRUE, quote = "")
ForceOrgs <- read.table(file = file.path(DirSupportTbs, files[3]), sep = '\t', header = TRUE, quote = "")
Maps      <- read.table(file = file.path(DirSupportTbs, files[4]), sep = '\t', header = TRUE, quote = "")
Missions  <- read.table(file = file.path(DirSupportTbs, files[5]), sep = '\t', header = TRUE, quote = "")
Subfactions <- read.table(file = file.path(DirSupportTbs, files[6]), sep = '\t', header = TRUE, quote = "")
WarlordTraits <- read.table(file = file.path(DirSupportTbs, files[7]), sep = '\t', header = TRUE, quote = "")
ptSizes <- seq(1000,5000, 50)

gameHeaders <- c("gameSize", "mission", "deployment")
playerHeaders <- c(
  "faction","forceOrg","ROW/Subfaction","alliedFaction"
  ,"warlordTrait","VPs","warlord","listName"
) # end headers

nGames <- 1:450

# Game data functions

## Make the basic game meta data
##  1. Map
##  2. Mission
##  3. Game size 
MakeGameMeta <- function(index){
  # game meta
  map_mission <- sample(1:6, 2, replace=T)
  gameSize    <- sample(ptSizes, 1)
  meta <- as.data.frame(t(c(gameSize, Missions[map_mission[2],1], Maps[map_mission[1],1])))
  colnames(meta) <- gameHeaders
  return(meta)
} # end MakeGameMeta


## Make player information
##  1. faction
##  2. Parent faction (look up what faction the child list belongs to... mechanicum / legion
##    This is a supporting step, not returned in the final meta
##  3. Force Org - uses the parent action to determine alternate force-org availability
##  4. Subfaction / ROW
##  5. Allied faction - random term to ensure this is not always present
##  6. ROW if applicable
##  7. Warlord trait
##  8. Warlord character - doesn't include any special characters atm
##  9. How many VPs the player got
## 10. List name, so it can be matched against a collection of lists
MakePlayerMeta <- function(index){
  faction <- Factions[sample(1:dim(Factions)[1], 1),2]
  parentFaction <- Factions[match(faction,Factions[,2]), 1]
  if (parentFaction == "Mechanicum"){
    forceOrg <- ForceOrgs[sample(1:dim(ForceOrgs)[1], 1),1] 
  } else { 
    forceOrg <- ForceOrgs[sample(2:dim(ForceOrgs)[1], 1), 1]
  }
  listOfSubFactions <- c(Subfactions[,2] == parentFaction)
  # 2 / 3 times there should be a ROW
  if ((sample(3:27, 1) %% 3) != 0) {
    # not all factions have subs, so need to do error handling for the ones which don't
    if (any(listOfSubFactions)) {
      # pick subfactions they're allowed to choose from
      ROW <- Subfactions[sample(which(listOfSubFactions), 1), 1]
    } else {
      ROW <- ""
    }  
  } else {
    ROW <- ""
  }
  # randomly determine if an allied detach is present, and if so which
  if (sample(1:100, 1)%%4 == 0) {
    alliedFaction <- Factions[sample(1:dim(Factions)[1], 1),2]
  } else {
    alliedFaction <- ""
  }
  trait   <- WarlordTraits[sample(1:dim(WarlordTraits)[1], 1),3]
  warlord <- Warlords[sample(which(c(Warlords[,2] == parentFaction)), 1), 1]
  VPs     <- sample(1:15, 1)
  listName    <- ""
  # dataframe stuff
  playerLog <- as.data.frame(t(c(faction, forceOrg, ROW, alliedFaction, trait, VPs, warlord, listName)))
  colnames(playerLog) <- playerHeaders
  return(playerLog)
} # end MakePlayerMeta


## make the game info
gameMetaList <- lapply(nGames, MakeGameMeta)
gameMetaList <- do.call(rbind, gameMetaList)
gameMetaList <- cbind(str_pad(nGames, 4, pad = "0"), gameMetaList)
colnames(gameMetaList)[1] <- "gameID"

## make player info
MakePlayerMetaDF <- function(PlayerMetalist){
  playerMeta <- do.call(rbind, PlayerMetalist)
  playerMeta <- cbind(
    paste(
      str_pad(ngames, 4, pad = "0")
      , "-"
      , paste(sample(LETTERS, 2, replace=TRUE), collapse="") # 2 random initals
    )
    , playerMeta
  )
  colnames(playerMeta)[1] <- "gameID"
  return(playerMeta)
}

### Append 2 sets of player data together
playerMeta <- rbind(MakePlayerMetaDF(lapply(nGames, MakePlayerMeta)), MakePlayerMetaDF(lapply(nGames, MakePlayerMeta)))


## Write out files
### Game files
write.csv(
  gameMetaList
    , paste(DirGamelog, "/manyGames", str_pad(ngames, 4, pad = "0"), ".csv", sep = "")
    , row.names=FALSE
)
### Player files
write.csv(
  playerMeta
  , paste(DirPlayerLog, "/manyplayers", str_pad(ngames, 4, pad = "0"), ".csv", sep = "")
  , row.names=FALSE
)

