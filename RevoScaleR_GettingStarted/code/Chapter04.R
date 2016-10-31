# Preliminaries ########################################################################
library(RevoScaleR)

# 4.1 ##################################################################################

sampleDataDir <- rxGetOption("sampleDataDir")
inputFile <- file.path(sampleDataDir, "AirlineDemoSmall.csv")

startTime <- Sys.time()
airDS <- rxImport(inData = inputFile,
    outFile = "./data/ADS.xdf",
    missingValueString = "M",
    stringsAsFactors = TRUE,
    overwrite = TRUE)
(runTime <- Sys.time() - startTime)
str(airDS)
head(airDS)

#airDS <- rxImport(inData = "./data/ADS.xdf",
    #missingValueString = "M",
    #stringsAsFactors = TRUE)

colInfo <- list(DayOfWeek = list(type = "factor",
            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
startTime <- Sys.time()
airDS <- rxImport(inData = inputFile,
    outFile = "./data/ADS.xdf",
    missingValueString = "M",
    colInfo = colInfo,
    overwrite = TRUE)
(runTime <- Sys.time() - startTime)

#airDS <- rxImport(inData = "./data/ADS.xdf",
    #missingValueString = "M",
    #colInfo = colInfo,
    #stringsAsFactors = TRUE)

# 4.2 ##################################################################################
dim(airDS)
nrow(airDS)
ncol(airDS)
head(airDS)

rxGetVarInfo(airDS)
myData <- rxReadXdf(airDS, numRows = 10, startRow = 100000)
myData
str(myData)

# 4.3 ##################################################################################
adsSummary <- rxSummary( ~ ArrDelay + CRSDepTime + DayOfWeek, data = airDS)
adsSummary

rxSummary( ~ ArrDelay:DayOfWeek, data = airDS)

options("device.ask.default" = T)
rxHistogram( ~ ArrDelay, data = airDS)
rxHistogram( ~ CRSDepTime, data = airDS)
rxHistogram( ~ DayOfWeek, data = airDS)

options("device.ask.default" = FALSE)
myData <- rxDataStep(inData = airDS,
    rowSelection = ArrDelay > 240 & ArrDelay <= 300,
    varsToKeep = c("ArrDelay", "DayOfWeek"))
rxHistogram( ~ ArrDelay, data = myData)

# 4.4 ##################################################################################

# 4.4.1 ################################################################################
arrDelayLm1 <- rxLinMod(formula = ArrDelay ~ DayOfWeek, data = airDS)
summary(arrDelayLm1)
arrDelayLm1

# 4.4.2 ################################################################################
arrDelayLm2 <- rxLinMod(formula = ArrDelay ~ DayOfWeek, data = airDS,
    cube = TRUE)
summary(arrDelayLm2)

countsDF <- rxResultsDF(arrDelayLm2, type = "counts")
countsDF
rxLinePlot(ArrDelay ~ DayOfWeek, data = countsDF, main = "Average Arrival Delay by Day Of Week")

# 4.4.3 ################################################################################
arrDelayLm3 <- rxLinMod(ArrDelay ~ DayOfWeek:F(CRSDepTime),
    data = airDS, cube = TRUE)
arrDelayDT <- rxResultsDF(arrDelayLm3, type = "counts")
head(arrDelayDT, 15)
rxLinePlot(ArrDelay ~ CRSDepTime | DayOfWeek, data = arrDelayDT,
    title = "Average Arrival Delay by Day Of Ween By Departure Hour")

# 4.5 ##################################################################################
