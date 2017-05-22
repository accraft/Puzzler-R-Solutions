

Soldier_Matrix <- matrix(data=sample(1:55,10000*100,replace=1),nrow=10000,ncol=100)
Soldier_Matrix[] <- findInterval(Soldier_Matrix,c(0,1,3,6,10,15,21,28,36,45,55),left.open = 1)
                  
Deploy_Matrix <- matrix(nrow=10000,ncol=10)

i=1
while(i<=10) {
  Deploy_Matrix[,i] <- apply(Soldier_Matrix[,],1,function(x) sum(x==i))
  i <- i + 1
}

Deploy_Matrix_sample <- Deploy_Matrix[1:30,]


calculate_winner_function <- function(base,compare) {
  i=1
  y=0
  while(i<=10) {
    if (Deploy_Matrix_sample[1,i] == Deploy_Matrix_sample[2,i]) {y=y+i/2}  
      else if (Deploy_Matrix_sample[1,i] > Deploy_Matrix_sample[2,i]) {y=y+i}
      else if (Deploy_Matrix_sample[1,i] < Deploy_Matrix_sample[2,i]) {y=y-i}
    i = i+1
  }
}

