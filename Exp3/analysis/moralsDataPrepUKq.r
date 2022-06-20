library(chMorals)
library(chutils)

#set up the new RT variables
fitCol <- "fit.RT"
resCol <- "res.RT"
useTwoParameterModel <- FALSE

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

statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))
sink(statsOutputFile, append = F)
  cat("\n***** New Run ****\n\n")
sink(NULL)

### read in data
data.raw <-read.table(params$moralsTaskDataFile, header=T, sep="\t")
data.ovrlp <-read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

######_____REMOVE PRACTICE TRIALS _____######
data.raw <- data.raw[data.raw$trial_type >=1, ]

### do Prep analysis
processedData <- ch.moralsDataPrep(data.raw, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = c("Yes", "No"), item1cols = c("IA.1.org"), item2cols = c("IB.1.org"), overlapItem1cols = c("IA1"), overlapItem2cols = c("IB1"), statsOutputFile = statsOutputFile, params = params )

### Filter data
analysisReadyData <- ch.moralsFilterDataQ(processedData, "sn", "keybRT", "overlapRound", "correct",c(1,0), statsOutputFile = statsOutputFile, params = params)

### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <- ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef",c("Yes", "No"), "correct",c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.gp, file="analysisReadyData.gp.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")
write.table(df.dPrime, file="df.dPrime.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.sn, item1Col = c("IA.1.org"), item2Col = c("IB.1.org"), "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Yes", "No"), params = params, saveFigures = T)

#### For experiments with quantity variable manipulations, do an analysis a quantity analysis
  ### first plot the directional overlap by quantity grouping
  do.filename <- file.path(itemDir,paste0(params$dt.set,"Quantity by Item",".pdf"))
  parOp <- par(mfrow=c(1,1), mai=c(1,1,1,1), omi=c(1,.75,.25,1), las=2, cex=1.25, lwd=2, bty='n', xpd = T)
  overOut <- ch.moralsGetAndPlotQuantDirOverlap(data.ovrlp, "IA1", "IB1", "overlap", "direction", c(1,10,40), filename = do.filename, parOp = parOp, cexLegend = 1, lgndPlacement = c(0,1), cex1 = 1.25)
  par(parOp)
  #Then analyze the effects of quantity
  quantData <- ch.moralsQuantAnalyis(analysisReadyData.sn, "IA.1.org", "IB.1.org", c(1,10,40),"keyDef", respChoiceVal = c("Yes", "No"),"targetPresent", c(TRUE, FALSE), "overlapRound", params = params, showLegend = FALSE)
