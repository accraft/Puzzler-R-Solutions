#This simulates The Puzzle Of The Lonesome King, user should input number of loops and the number of subjects
TimesToRUn <- 5000
NumberOfSubjects <- 300

TrialList <- data.frame(RunNumber = c(1:TimesToRUn), CrowningCheck = NA)
#TrialList$SubjectCountRun[1] = list(c(1:4))

PlayGame <- function(RunNumber) {
  SubjectsRemaining = NumberOfSubjects
  SubjectsTracking <- SubjectsRemaining
  while(SubjectsRemaining > 2) {
    SubjectsRemainingVector <- c(1:SubjectsRemaining)
    SubjectPicks <- unlist(lapply(SubjectsRemainingVector,function(x) sample(c(1:(x-1),(x+1):SubjectsRemaining),size=1)))
    
    #special logic to deal with first and last person to make sure they don't pick themselves
    SubjectPicks[1] <- sample(2:SubjectsRemaining,size=1)
    SubjectPicks[length(SubjectPicks)] <- sample(1:(length(SubjectPicks)-1),size=1)
    
    #number of individuals eliminated equals the number of unique picks, this saves up from having to remove select individuals
    SubjectsRemaining = SubjectsRemaining - length(unique(SubjectPicks))
    SubjectsTracking <- c(SubjectsTracking,SubjectsRemaining)
  }
  TrialList$CrowningCheck[RunNumber] <<- SubjectsRemaining == 1
  TrialList$SubjectsTracking[RunNumber] <<- list(SubjectsTracking)
}

invisible(lapply(1:TimesToRUn, function(i) PlayGame(i)))

mean(TrialList$CrowningCheck)
