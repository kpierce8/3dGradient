#RUN AFTER LineCalculator.r

library(SiZer)
par(mfrow=c(2,1))
##FUNCTIONS
findInflection <- function(x,y){
numPoints <- length(x)
inflections <- rep(0,numPoints)
inflections[1] <- 0
inflections[numPoints] <- 0
  for(i in 2:(numPoints-1)){
  inflections[i] <-  ((y[i]-y[i-1])/(x[i]-x[i-1])) -  ((y[i+1]-y[i])/(x[i+1]-x[i]))
  ((y[i]-y[i-1])/(x[i]-x[i-1]))
  ((y[i+1]-y[i])/(x[i+1]-x[i]))
  }
  return(inflections)
}

findGradients   <- function(x,y){
numPoints <- length(x)
inflections <- rep(0,numPoints)
inflections[1] <-  ((y[2]-y[1])/(x[2]-x[1]))
inflections[numPoints] <- ((y[numPoints]-y[numPoints-1])/(x[numPoints]-x[numPoints-1]))


  for(i in 2:(numPoints-1)){
  inflections[i] <-  (((y[i]-y[i-1])/(x[i]-x[i-1])) +  ((y[i+1]-y[i])/(x[i+1]-x[i]))) /2
  ((y[i]-y[i-1])/(x[i]-x[i-1]))
  ((y[i+1]-y[i])/(x[i+1]-x[i]))
  }
  return(inflections)
}


#######################################

# rnum <- sample(seq(1:length(reaches)),1)  +1
# rnum<- 9001
#rnum <- 4679 #single model
#rnum <- rnum2 +1
#reaches[[rnum]]


test <- na.omit(reaches[[rnum]])

test <- data.frame(test,elev = test$raster, upmeasure = (max(test$measure)-test$measure)/3.28)
plot(test$upmeasure, test$elev,xlab= "stream meters",ylab = "elevation (m)")
testLM <- lm(test$elev ~ test$upmeasure)
testLMsq <- lm(test$elev ~ test$upmeasure + I(test$upmeasure^2))
bob<- anova(testLM)
bob2 <- summary(testLM)
bobsq <- summary(testLMsq)
abline(testLM)
yRange <-   (max(test$elev)-min(test$elev))
yMin <- min(test$elev)
xMax <- max(test$upmeasure)
exagLine <- (yRange/xMax*yRange) + yMin
text(.15*(max(test$upmeasure)),(.9*(max(test$elev)-min(test$elev)))+min(test$elev),paste("reach id = ", rnum))
skew(testLM$residuals)
bob2$r.squared
numPoints <- length(test$elev)
aveGrad =   ((test$elev[numPoints]-test$elev[1])/(test$upmeasure[numPoints]-test$upmeasure[1]))
text(.15*(max(test$upmeasure)),(.8*(max(test$elev)-min(test$elev)))+min(test$elev),paste("linear r2 = ", sig3(bob2$r.squared)))
text(.15*(max(test$upmeasure)),(.7*(max(test$elev)-min(test$elev)))+min(test$elev),paste("2nd order r2 = ", sig3(bobsq$r.squared)))
text(.15*(max(test$upmeasure)),(.6*(max(test$elev)-min(test$elev)))+min(test$elev),paste("reach grad = ", sig3(aveGrad)))

abline(h=exagLine, lty=4, col=6)


max(abs(testLM$residuals))/length(test$upmeasure)
max(abs(testLM$residuals))/mean(abs(testLM$residuals))
max(abs(testLMsq$residuals))/mean(abs(testLMsq$residuals))
max(abs(testLM$residuals))
max(abs(testLMsq$residuals))
max(abs(testLMsq$residuals))/max(abs(testLM$residuals))
rnum
pl<-piecewise.linear(test$upmeasure, test$elev, middle = 1)
#abline(v=pl$change.point)
lines(test$upmeasure,testLMsq$fitted.values, col=3, lty=2, lwd=2)
#lines(test$upmeasure,smooth(test$elev))

gradients <- findGradients(test$upmeasure,test$elev)
plot(test$upmeasure,gradients,xlab= "stream meters")




# inflections <- findInflection(test$upmeasure,test$elev)
# plot(test$upmeasure,inflections)
# abline(h=0)
# abline(h=  sd(inflections),lty=2)
# abline(h=-sd(inflections),lty=2)

# infl2<-findInflection(test$upmeasure,inflections)

# plot(test$upmeasure,infl2)
# abline(h=0)
# abline(h=  sd(infl2),lty=2)
# abline(h=-sd(infl2),lty=2)

savePlot(paste("D:\\work\\streamAnalysis\\CheckOneSegmentSlope\\stream", rnum, ".pdf",sep=""),type="pdf")
