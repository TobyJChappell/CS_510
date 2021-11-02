## Advection component
library(foreach)
advect.serial<-function(dots,x,y,Ux,Uy,delta.t){
  idx<-find.spot.serial(dots[,1],x)
  idy<-find.spot.serial(dots[,2],y)
  dots[,1]<-dots[,1]+Ux[idx]*delta.t
  dots[,2]<-dots[,2]+Uy[idy]*delta.t
  return(dots)
}

advect.parallel<-function(dots,x,y,Ux,Uy,delta.t){
  idx<-find.spot.parallel(dots[,1],x)
  idy<-find.spot.parallel(dots[,2],y)
  dots[,1]<-dots[,1]+Ux[idx]*delta.t
  dots[,2]<-dots[,2]+Uy[idy]*delta.t
  return(dots)
}

find.spot.serial<-function(points,field) {
  idx<-rep(NA,length=length(points))
  for (j in 1:length(points)) idx[j] <- which.min((field - points[j])^2)
  return(idx)
}

find.spot.parallel<-function(points,field) {
  idx <- foreach(j = 1:length(points), .combine='c') %dopar% {which.min((field - points[j])^2)}
  return(idx)
}