test <- data.frame(test,elev = test$raster, upmeasure = max(test$measure)-test$measure)/3.28
plot(test$upmeasure, test$elev,xlab= "stream meters",ylab = "elevation (m)")
testLM <- lm(test$elev ~ test$upmeasure)
testLMsq <- lm(test$elev ~ test$upmeasure + I(test$upmeasure^2))
bob<- anova(testLM)
bob2 <- summary(testLM)
bobsq <- summary(testLMsq)
abline(testLM)

numPoints <- length(test$elev)
myModels <- list()
j <- 1
myThreshhold <- .995
minGrain <- 4
mstart <- 1
mend <- mstart-1 + minGrain
for(i in 1:(numPoints-minGrain)){ 
        testLMsub  <- lm(test$elev[mstart:mend] ~ test$upmeasure[mstart:mend])
        subSummary <- summary(testLMsub)
        rStat <- subSummary$r.squared            
        if(rStat > myThreshhold)  {
          curModel <- testLMsub
          rSQ<- rStat
          myModels[[j]] <- c(curModel, mstart=mstart, mend=mend, rsquare= rSQ)
          mend <- min(mend + 1, numPoints)
          }  else{
                  testLMsqsub <- lm(test$elev[mstart:mend] ~ test$upmeasure[mstart:mend] + I(test$upmeasure[mstart:mend]^2))
                  subSummarysq <- summary(testLMsqsub)
                  rStatsq <- subSummarysq$r.squared
                  if( rStatsq > myThreshhold)  {
                      curModel <- testLMsqsub
                      rSQ<- rStatsq
                      myModels[[j]] <- c(curModel, mstart=mstart, mend=mend, rsquare= rSQ)
                      mend <- min(mend + 1, numPoints)
                      
                      } else{      
                        mstart <- mend -1
                        mend <- min(mstart + minGrain, numPoints)
                        j <- j +1 }
                
        }
            
       
}


plot(test$upmeasure, test$elev,xlab= "stream meters",ylab = "elevation (m)")
colr=1
for(i in 1:length(myModels)){
try( lines(test$upmeasure[myModels[[i]]$mstart:myModels[[i]]$mend],myModels[[i]]$fitted.values, col=i, lwd=2))
}

for(i in 1:length(myModels)){
paste("model ", i, " is ",myModels[[i]]$mstart)
}


temp <- c(1,1,1)
for(i in 1:length(myModels)){
temp <- rbind(temp,c( i, myModels[[i]]$mstart, myModels[[i]]$mstart-i))
}
temp

temp2 <- cbind(temp[,1],temp[,2]-temp[,1])
temp2

test3 <- temp2[temp2[,2] > 0,]
test3

# code to get start, stop measures and start stop elevationlength(reaches)
myModels[[284]]$mstart
myModels[[284]]$mend
myModels[[284]]$model[[1]][1]
myModels[[284]]$model[[1]][length(myModels[[284]]$model[[1]])]

#length(myModels)
#getSlot(myModels, "mstart")
#getSlot(myModels, "mend")