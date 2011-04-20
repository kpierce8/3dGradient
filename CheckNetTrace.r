#net25 <- read.csv("C:\\data\\ProjectsLocal\\Segments\\WRIA25test\\NetTraceTable.txt")
makePlotDir("CheckNetTrace")
samp<- read.table("C:\\data\\ProjectsLocal\\Segments\\WRIA25test\\NetTraceLLID.txt", header=T)
par(mfrow=c(2,1))

i<- 3

#for(i in 1:dim(samp)[1]){
  test <- subset(net25, LLID == as.numeric(samp[i,3]))
  numPoints <- length(test$ELEV_M)
  plot(test$FROM_DIST,test$ELEV_M)
  text(.15*(max(test$FROM_DIST)),(.9*(max(test$ELEV_M)-min(test$ELEV_M)))+min(test$ELEV_M),paste("reach llid = ", samp[i,3]))
  text(.15*(max(test$FROM_DIST)),(.8*(max(test$ELEV_M)-min(test$ELEV_M)))+min(test$ELEV_M),paste("reach id = ", samp[i,1]))
  aveGrad <- ((test$ELEV_M[numPoints]-test$ELEV_M[1])/(test$FROM_DIST[numPoints]-test$FROM_DIST[1]))
  text(.15*(max(test$FROM_DIST)),(.7*(max(test$ELEV_M)-min(test$ELEV_M)))+min(test$ELEV_M),paste("ave grad = ", sig3(aveGrad)))
  plot(test$FROM_DIST,test$MEAN_GRAD)
  #savePlot(paste(pldir,"\\CheckNetTrace\\stream", samp[i,3], ".pdf",sep=""),type="pdf")
#}

