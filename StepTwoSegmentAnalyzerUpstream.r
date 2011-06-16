#test <- data.frame(test,elev = test$raster, measure = (max(test$measure)-test$measure)/3.28)
plot(test$measure, test$elev,xlab= "stream meters",ylab = "elevation (m)")
testLM <- lm(test$elev ~ test$measure)
testLMsq <- lm(test$elev ~ test$measure + I(test$measure^2))
bob<- anova(testLM)
bob2 <- summary(testLM)
bobsq <- summary(testLMsq)
abline(testLM)

maxMeasure <- max(test$measure)
maxElevation <- max(test$elev)

numPoints <- length(test$elev)
myModels <- list()
j <- 1
myThreshhold <- .90
minGrain <- 4
mstart <- 1
mend <- mstart-1 + minGrain
curModel <- 1 #initiates model state to numeric, acts as a flag for unassigned model
for(i in 1:(numPoints-minGrain))
	{ 
        testLMsub  <- lm(test$elev[mstart:mend] ~ test$measure[mstart:mend])
        subSummary <- summary(testLMsub)
        rStat <- subSummary$r.squared            
        if(rStat > myThreshhold)  
		{
		  curModel <- testLMsub
		  rSQ<- rStat
		  myModels[[j]] <- c(curModel, mstart=mstart, mend=mend, rsquare= rSQ)
		  mend <- min(mend + 1, numPoints)
		}  else{
			  testLMsqsub <- lm(test$elev[mstart:mend] ~ test$measure[mstart:mend] + I(test$measure[mstart:mend]^2))
			  subSummarysq <- summary(testLMsqsub)
			  rStatsq <- subSummarysq$r.squared
			  if( rStatsq > myThreshhold)  
				{
				  curModel <- testLMsqsub
				  rSQ<- rStatsq
				  myModels[[j]] <- c(curModel, mstart=mstart, mend=mend, rsquare= rSQ)
				  mend <- min(mend + 1, numPoints)                     
				} else{ 
					if (is.numeric(curModel)) 
						{
						  mstart <- mstart + 1
						  mend <- mend +1
						}	else{      
								curModel <- 1 #initiates model state to numeric, acts as a flag for unassigned model
								mstart <- mend -1
								mend <- min(mstart + minGrain, numPoints)
								j <- j +1 
								}
						}
				}		
	}


plot(test$measure, test$elev,xlab= "stream meters",ylab = "elevation (m)", cex=0.5)
colr=1
for(i in 1:length(myModels)){
try( lines(test$measure[myModels[[i]]$mstart:myModels[[i]]$mend],myModels[[i]]$fitted.values, col=i, lwd=2))
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

ModelsNonNull <- temp2[temp2[,2] > 0,]


# code to get start, stop measures and start stop elevationlength(reaches)
myModels[[mod]]$mstart
myModels[[mod]]$mend

modelSummaries <-data.frame(matrix(0, dim(ModelsNonNull)[1],10))

eventTable <- c("LineID", "startMeasure", "startElevation","endMeasure",  "endElevation", "Intercept", "Linear", "Quadratic", "rSq", "segLength")

names(modelSummaries) <- eventTable
eventIndex <- 1
for (mod in ModelsNonNull[,1]) {

#mod <- 21
tempLineID <- test[myModels[[mod]]$mstart, "LineID"]
tempMstart <- test[myModels[[mod]]$mstart, "measure"]
tempMstartElev <- test[myModels[[mod]]$mstart,"elev"]
tempMend <- test[myModels[[mod]]$mend, "measure"]
tempMendElev <- test[myModels[[mod]]$mend,"elev"]
tIntercept <- myModels[[mod]]$coefficients["(Intercept)"]
tLinear <- myModels[[mod]]$coefficients[2][1]
tQuadratic <- myModels[[mod]]$coefficients[3][1]


tCoefficients <- as.vector(myModels[[mod]]$coefficients)

if (length(tCoefficients) == 2) tCoefficients <- c(tCoefficients, 0)

tRsq <- myModels[[mod]]$rsquare

tempLineID
tempMstart
tempMstartElev
tempMend
tempMendElev
tCoefficients
tRsq


tempSummary <- c(tempLineID, tempMstart, tempMstartElev, tempMend, tempMendElev, tCoefficients, tRsq, tempMend-tempMstart )
tempSummary
modelSummaries[eventIndex,] <- tempSummary
eventIndex = eventIndex + 1
}

sum(modelSummaries[,"segLength"])

