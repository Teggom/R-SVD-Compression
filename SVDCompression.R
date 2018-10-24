#SVD
setwd("~")
library("png")
Name <- "zzz"
Bpic <- readPNG(paste(Name, ".PNG", sep = ""))
Normalization <- function(x){x/sqrt(sum(x^2))}

Keep_Vectors <- c(100, 75, 50, 30, 20, 15, 10, 5, 4, 3, 2, 1)
Removal_Vector <- c(1, 2, 3, 4, 5, 10, 20, 50, 75, 100, 150, 200)

for(Num_To_Keep in Keep_Vectors){
  pic <- Bpic
  random_thought <- Bpic
  random_thought_2 <- Bpic
  for(each in 1:3){
    Current <- pic[,,each]
    Broken <- svd(Current)
    Broken$d[(Num_To_Keep+1):length(Broken$d)] <- 0
    updated <- Broken$u %*% diag(Broken$d) %*% t(Broken$v)
    updated[updated<0] <-0
    updated[updated>1] <-1
    pic[,,each] <- updated
    #random_thought[,,each] <- Broken$u
    #random_thought_2[,,each] <- (Broken$v-min(Broken$v))/max(Broken$v)
    print(paste("Done", each, " of 3 | SVD Value ", Num_To_Keep))
  }
  writePNG(image = pic, target = paste(Name, "_Compressed_", Num_To_Keep, ".png", sep = ""))
 # writePNG(image = random_thought, target = paste("U Picture --", Num_To_Keep, ".png", sep = ""))
}
#writePNG(image = random_thought_2, target = paste(Name, "_V_Matrix_Picture.png", sep = ""))

for(Top_To_Remove in Removal_Vector){
  pic<-Bpic
  gpic <- Bpic
  for(each in 1:3){
    Current <- pic[,,each]
    Broken <- svd(Current)
    Broken$d[1:Top_To_Remove] <- 0
    updated <- Broken$u %*% diag(Broken$d) %*% t(Broken$v)
    updated[updated<0] <-0
    updated[updated>1] <-1
    scale <- max(updated)
    updated <- updated*(1/scale)
    pic[,,each] <- updated
    gpic[,,each] <- -(pic[,,each]-1)
    print(paste("Done", each, " of 3 | Removal of LARGEST", Top_To_Remove))
  }
  writePNG(image = pic, target = paste(Name, "_Compressed_Removal_Top_UnNeg", Top_To_Remove, ".png", sep = ""))
  writePNG(image = gpic, target = paste(Name, "_Compressed_Removal_Top_", Top_To_Remove, ".png", sep = ""))
}