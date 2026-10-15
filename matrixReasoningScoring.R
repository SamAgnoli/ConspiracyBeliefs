#set your paths
setwd("") #to the folder where I want this to pull most info from and save to
path <- "" #path to the subfolder where individual participant CSV files are located
file.names <- dir(path, pattern =".csv")


matrix_scored <- matrix(nrow = length(file.names), ncol = 4)
colnames(matrix_scored)[1:4] <- c("ID", "date", "totalScore", "accuracy")


for (i in 1:length(file.names)) {
  
  Participant <- read.csv(paste(path,"/",file.names[i],sep=""),header=TRUE, sep=",", stringsAsFactors=FALSE)
  ID <- Participant[1,1]
  date <- Participant[1,2]
  
  Participant <- subset(Participant, trial != "prac_test" & trial != "inst2")
  
  score <- as.numeric(sum(Participant$correct))
  accuracy <- score/35
  
  matrix_scored[i, 1] <- ID
  matrix_scored[i, 2] <- date
  matrix_scored[i, 3] <- score
  matrix_scored[i, 4] <- accuracy
  
}

matrix_scored <- data.frame(matrix_scored)



IDs <- read.csv("../ESB Phase 1.csv")

finalDF <- IDs %>%
  left_join(matrix_scored, by = "ID")

MissingIDs <- subset(finalDF, is.na(totalScore))$ID

finalDF <- subset(finalDF, !is.na(totalScore))

finalDF <- finalDF %>%
  group_by(ID) %>%
  filter(date == min(date))


# write.csv(finalDF, "matrixScored_20251006.csv", row.names=F)
