bob <- read.csv("C:\\data\\ProjectsLocal\\Segments\\Hood_Canal_Gradient\\output.txt",header=F)

names(bob) <- c("LineID","Index","xcoor","ycoor","raster")

bob2 <- data.frame(bob, measure=0)

LineIDs <- unique(bob2$LineID)
                                                       
reaches <- split(bob2,bob2$LineID)

test <- reaches[1]

test[[1]]$raster

for (i in 2:length(test[[1]]$raster)){
          test[[1]]$measure[i] <-  test[[1]]$measure[i-1] + dist(test[[1]][c(i-1,i),3:4])
}

         
##Calc Raw measure length
for (j in 1:length(reaches)){
  for (i in 2:length(reaches[[j]]$raster)){
            reaches[[j]]$measure[i] <-  reaches[[j]]$measure[i-1] + dist(reaches[[j]][c(i-1,i),3:4])
  }
}          

plot(reaches[[10]]$xcoor[1:1000],reaches[[10]]$ycoor[1:1000])

#write.csv(cbind(reaches[[10]]$xcoor[1:1000],reaches[[10]]$ycoor[1:1000]),"C:\\data\\ProjectsLocal\\Segments\\Hood_Canal_Gradient\\linetest.csv")      
#write.csv(reaches[[1]],"C:\\data\\ProjectsLocal\\Segments\\Hood_Canal_Gradient\\reaches1.csv")
#write.csv(reaches[[2]],"C:\\data\\ProjectsLocal\\Segments\\Hood_Canal_Gradient\\reaches2.csv") n