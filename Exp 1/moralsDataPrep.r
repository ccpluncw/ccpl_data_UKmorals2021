library(chMorals)
library(chutils)

#set up the new RT variables
fitCol <- "fit.RT"
resCol <- "res.RT"

# read in parameters
params<-ch.readMoralsDBfile("moralsDBfile.txt")

#set up the group and item directories
mainDir <- getwd()
ch.newDir (mainDir, params$gpSubDir)
gpDir <- getwd()
setwd(mainDir)

ch.newDir (mainDir, params$itemSubDir)
itemDir <- getwd()
setwd(mainDir)


### read in data
data.raw <-read.table(params$moralsTaskDataFile, header=T, sep="\t")
data.ovrlp <-read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

######_____REMOVE PRACTICE TRIALS _____######
data.raw <- data.raw[data.raw$trial_type >=1, ]

### do Prep analysis
processedData <- ch.moralsDataPrep(data.raw, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = c("Yes", "No"), item1cols = c("Item1"), item2cols = c("Item2"), overlapItem1cols = c("IA1"), overlapItem2cols = c("IB1"), params )

### Filter data
analysisReadyData <- ch.moralsFilterData(processedData, "sn", "keybRT", "overlapRound", "avgRT", "correct",c(1,0), item1cols = c("Item1"), item2cols = c("Item2"), params)

### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <- ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef",c("Yes", "No"), "correct",c(1,0), params)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData.gp, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0),  params)
write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.gp, "Item1", "Item2", "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Yes", "No"), params, saveFigures = T)
