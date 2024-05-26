# Newton’s Backward Interpolation Formula
NBF<-function(x,y,xn){ #xn for which, y-value (yx) will be interpolated
  l<-length(x)
  h<-x[2]-x[1]
  u<-(xn-x[l])/h
  sum<-0
  for(i in 1:(l-1)){
    del<-diff.default(y, lag=1, differences=i)
    sum<-sum+choose(u+i-1, i)*del[l-i]	
  }
  return(y[l]+sum)
}
# An example
x<-seq(320, 325, 1)
y<-c(2.505150, 2.506505, 2.507856, 2.509203, 2.510545, 2.511883)
options(digits = 10)
NBF(x, y, 324.5)	#interpolation (ending)
# R output
[1] 	2.511214277
NBF(x,y,325.2)	#extrapoltion (ending)
# R output
[1] 	2.512150526
