#install.packages('dplyr')
require(dplyr)

##first generate a 100,000 fishpairs
Fish_List <- matrix(round(runif(300000,min = 0, max = 1),5),nrow=100000,ncol=3)

##now add 3 columns of binary variables: if 2nd > 1st, if 3rd > 2nd and if 3rd > max(1st,2nd)
Fish_List <- cbind(Fish_List,Fish_List[,1] < Fish_List[,2])
Fish_List <- cbind(Fish_List,Fish_List[,2] < Fish_List[,3])
Fish_List <- cbind(Fish_List,Fish_List[,1]< Fish_List[,3] & Fish_List[,2]< Fish_List[,3])



FirstFish_results <- data.frame(Fish_List[,1]
                                ,Fish_List[,1]+Fish_List[,2]*Fish_List[,4]+Fish_List[,3]*Fish_List[,6]
                                ,Fish_List[,2]+Fish_List[,3]*Fish_List[,5])
colnames(FirstFish_results)[1] <- "Fish_wght"
colnames(FirstFish_results)[2] <- "Take1st_wgt"
colnames(FirstFish_results)[3] <- "Skip1st_wgt"

FirstFish_results$Skip1st_bigger <- as.numeric(FirstFish_results$Take1st_wgt < FirstFish_results$Skip1st_wgt)
FirstFish_results$orig_row <- c(1:10000)

FirstFish_results <- FirstFish_results[order(FirstFish_results$Fish_wght),]

#now to split first fish weights into sets of .01 by rounding down;
FirstFish_results$wgt_group <- round(FirstFish_results$Fish_wght-.00499999,2)
FirstFish_results_bygroup <- group_by(FirstFish_results,wgt_group)


To_Graph <- summarize(FirstFish_results_bygroup
                      ,Total_fish = n()
                      ,Total_Skip1st_Bigger = sum(Skip1st_bigger)
                      ,Total_Take1st_Bigger = Total_fish - Total_Skip1st_Bigger
                      ,Percent_Skip1st_Bigger = Total_Skip1st_Bigger/Total_fish
)

plot(To_Graph$wgt_group,To_Graph$Percent_Skip1st_Bigger)
