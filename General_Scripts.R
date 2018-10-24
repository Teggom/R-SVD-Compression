library('png')
setwd("~/../Desktop/Chett Blackmail Pictures/Compression Step 2/")


## Compare before and After matrix
Picture <- readPNG("BW_Pic1.png")
pic <- Picture
Compression_Level = min(ncol(pic), nrow(pic))

Current <- pic
Broken <- svd(Current)
D_Storage <- Broken$d
#Broken$d[(Compression_Level):length(Broken$d)] <- 0
updated <- Broken$u %*% diag(Broken$d) %*% t(Broken$v)
Without_Rounding <- updated
updated[updated<0] <-0
updated[updated>1] <-1



Base_Ret_Variance <- 100*sum(Broken$d^2)/sum(D_Storage^2)
writePNG(image = Without_Rounding, target = "Not_Rounded.png")
writePNG(image = updated, target = "Rounded_Off.png")

Difference_Between <- Without_Rounding - updated
Difference_Between <- abs(Difference_Between)/max(Difference_Between)
writePNG(image = Difference_Between, target = "Difference_Between_Not_Rounded_and_Rounded.png")
writePNG(image = Broken$u, target = "U_Matrix.png")
writePNG(image = Broken$v, target = "V_Matrix.png")