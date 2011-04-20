
rnum2 <- 8179
rnum <- rnum2 +1
reaches[[rnum]]
plot(reaches[[rnum]]$measure, reaches[[rnum]]$raster)



write.csv(test[-dim(test)[1],], "C:\\data\\ProjectsLocal\\Segments\\WRIA18Gradient\\test2.csv",   row.names=FALSE)

num_Reach <- length(reaches)

reach_stats <- data.frame(matrix(0, num_Reach, 5))

names(reach_stats) <- c("FID", "skew", "r_squared", "max_resid", "r.sq.poly")

for(i in 1:num_Reach){
test <- reaches[[i]]
testLM <- lm(test$raster ~ test$measure)

bob2 <- summary(testLM)
testLMsq <- lm(test$raster ~ test$measure + I(test$measure^2))
bobsq <- summary(testLMsq)

reach_stats[i,1] <- test[1,]$LineID
reach_stats[i,2] <- skew(testLM$residuals)
reach_stats[i,3] <- bob2$r.squared
reach_stats[i,2] <- max(abs(testLM$residuals))
reach_stats[i,5] <- bobsq$r.squared
}


write.csv(reach_stats, "C:\\data\\ProjectsLocal\\Segments\\WRIA18Gradient\\stats2.csv",   row.names=FALSE)


hist(reach_stats[,3],breaks=c(0,.75,.9,.98,.99,.995, 1), plot=F)
