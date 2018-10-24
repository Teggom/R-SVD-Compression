#SVD
setwd("~")
library("png")
Name <- "Dog"
pic <- readPNG(paste(Name, ".PNG", sep = ""))
Normalization <- function(x){x/sqrt(sum(x^2))}

#Num_Kept <- 20
#iterations <- 20
#for(Iter in 1:iterations){
#  for(each in 1:3){
#    Current <- pic[,,each]
#    Broken <- svd(Current)
#    Broken$d[(Num_Kept+1):length(Broken$d)] <- 0
#    updated <- Broken$u %*% diag(Broken$d) %*% t(Broken$v)
#    updated[updated<0] <-0
#    updated[updated>1] <-1
#    pic[,,each] <- updated
#    print(paste("Done", each, " of 3 | SVD Value ", Iter, " of ", Num_Kept))
#  }
#  writePNG(image = pic, target = paste(Name, "_Recc_Compd_", Iter, ".png", sep = ""))
#}



ppic <- readPNG("BW_Pic1.png")
number_kept <- c(1, 2, 3, 4, 5, 10, 20, 40, 80, 100)
round = F
for(Kept in number_kept){
  pic <- ppic
  Broken <- svd(pic)
  Saved <- Broken$d
  Broken$d[(Kept+1):length(Broken$d)] <- 0
  updated <- Broken$u %*% diag(Broken$d) %*% t(Broken$v)
  if(round == T){
    updated[updated<0] <-0
    updated[updated>1] <-1
  }
  writePNG(image = updated, target = paste("Girl_Bed_Replace_Mess", Kept, "_", round, ".png"))
  Broken <- svd(updated)
  Broken$d <- Saved
  updated <- Broken$u %*% diag(Broken$d) %*% t(Broken$v)
  if(round == T){
    updated[updated<0] <-0
    updated[updated>1] <-1
  }
  writePNG(image = updated, target = paste("Girl_Bed_Replace_FitBack", Kept, "_", round, ".png"))
  
}






