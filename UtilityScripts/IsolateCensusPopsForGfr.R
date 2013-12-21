rm(list=ls(all=TRUE))
require(plyr)
require(testit)
inputPathsCensus1990s <-paste0("./Datasets/CensusIntercensal/STCH-icen199", 0:9, ".txt")
inputPathFips <- "./Datasets/CountyFipsCode.csv"

###################
# Read in the datasets
###################
lstDatasets1990s <- lapply(inputPathsCensus1990s, function(path) dsCensus199x <- read.table(path, header=FALSE, stringsAsFactors=F)) #A list, where each element is a data.frame.
dsCensus199x <- data.frame(do.call(plyr::rbind.fill, lstDatasets1990s), stringsAsFactors=FALSE) #Vertically stack all the data.frames into a single data.frame
dsFips <- read.csv(inputPathFips, stringsAsFactors=F)

###################
# Groom the Census data from the 1990s & Keep only the needed rows
###################
#See the codebook at ./Datasets/CensusIntercensal/STCH-Intercensal_layout.txt.  The State FIPS is missing for some reason
colnames(dsCensus199x) <- c("Year", "Fips", "AgeGroup", "RaceGender", "Latino", "Population")
ageGroupLabels <- c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49","50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
raceGenderLabels <- c("White male", "White female", "Black male", "Black female", "American Indian or Alaska Native male", "American Indian or Alaska Native female", "Asian or Pacific Islander male", "Asian or Pacific Islander female")

dsCensus199x$Year <- as.integer(dsCensus199x$Year + 1900) #Convert to a four digit year
# dsCensus199x$CountyFips #This looks good as it is.  Be careful if you live in a state with a leading zero in the FIPS
dsCensus199x$AgeGroup <- factor(x=dsCensus199x$AgeGroup, levels=0:18, labels=ageGroupLabels, ordered=TRUE)
dsCensus199x$RaceGender <- factor(x=dsCensus199x$RaceGender, levels=1:8, labels=raceGenderLabels)
dsCensus199x$Latino <- as.logical(factor(dsCensus199x$Latino, levels=1:2, labels=c(TRUE, FALSE)))

testit::assert("All years in dsCensus199x should be in the 1990s", all(1990 <= dsCensus199x$Year & dsCensus199x$Year <=1999))
testit::assert("All County FIPS should start with 40 (ie, be in Oklahoma).", all(grepl(pattern="^40\\d{3}$", x=dsCensus199x$CountyFips, perl=TRUE)))
testit::assert("The mean of the Latino values should be 0.5.", mean(dsCensus199x$Latino)==0.5)
sapply(dsCensus199x, class)

#Identify and tsolate the levels need to calculate GFR (ie, females between 15 & 44)
femaleLabels <- c("White female","Black female", "American Indian or Alaska Native female", "Asian or Pacific Islander female")
fertileLabels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")

dsCensus199x$GfrEligible <- (dsCensus199x$AgeGroup %in% fertileLabels & dsCensus199x$RaceGender %in% femaleLabels)
testit::assert("The proportion of GFR eligible rows should be correct.", mean(dsCensus199x$GfrEligible) == (6/19 * 1/2))

dsCensus199x <- merge(x=dsCensus199x, y=dsFips, by="Fips", all.x=TRUE, all.y=FALSE)

dsCensus199x <- dsCensus199x[dsCensus199x$GfrEligible & dsCensus199x$WatsUrban, ]
