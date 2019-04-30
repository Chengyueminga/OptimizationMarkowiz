###4/30/2019
install.packages("installr")
library(installr)
updateR()

install.packages("quadprog")
library(quadprog)
install.packages("qrmdata")
library(qrmdata)

rm(list=ls())
#Get the price 
data("EUR_USD")
data("GBP_USD")
data("OIL_Brent")
data1 <- na.omit(merge(EUR_USD,GBP_USD,OIL_Brent))

##return
ret <- na.omit(diff(log(data1)))*100
meanRet <- apply(ret,2,mean)
#summary(ret)

##Covariance Matrix
covRet <- cov(ret)

##standard deviation for three equities
SDRet <- sqrt(diag(covRet))


###Quadratic 
###Short is allowed
###mean return * weights we get the total return
Amat = cbind(rep(1,3),meanRet)

###Get the possible target returns in this portfolio 
mu.p <-seq(min(meanRet) + 1e-04,max(meanRet) - 1e-04,length = 300)

###bvec we firstly calculate it in mu.p[1] and finish it in 300
bvec <- cbind(1,mu.p[1])

###loop
weights <- matrix(NA,300,3)
sigma <- numeric(300)
for (i in 1:300){
  result <- solve.QP(Dmat =2 * covRet, dvec = rep(0,3), Amat = cbind(rep(1,3),meanRet), 
                   bvec = cbind(1,mu.p[i]) ,meq = 2)
  sigma[i] = sqrt(result$value)
  weights[i,] = result$solution
}


###Plot 
muFree = 1.3/253
plot(sigma,mu.p,type = "l",xlim = c(0, max(SDRet) * 1.1), ylim = c(0, max(meanRet) * 1.1), lty = 3, lwd = 3)
points(0, muFree, cex = 1, pch = "+")  


###Potential sharpo Ratio
Ratio <- (mu.p - muFree)/sigma
sharpeRatio <- max(Ratio)
ind <- which(Ratio == sharpeRatio)
##Efficient Frontier
lines(c(0,2),muFree + sharpeRatio * c(0,2),col = "blue")

###Target portfolio
weights[ind,]
sigma[ind]

###Short is not allowed
for (i in 1:300){
  bvec <- c(1,mu.p[i],rep(0,3))
  result <- solve.QP(Dmat = 2 * covRet,
                    dvec = rep(0,3),
                    Amat = cbind(rep(1, 3), meanRet,diag(1,nrow = 3)),
                    bvec = bvec,
                    meq = 2)
  sigma[i] = sqrt(result$value)
  weights[i,] = result$solution
}
plot(sigma,mu.p,type = "l",xlim = c(0,max(SDRet)),ylim = c(0,max(mu.p)))
points(0,muFree, cex = 1, pch = "+", col = "red")

##Find the sharpe Ratio
##And plot the efficient frontier
Ratio <- (mu.p - muFree)/sigma
sharpeRatio <- max(Ratio)
ind <- which(Ratio == sharpeRatio)
lines(c(0,2), muFree + sharpeRatio * c(0,2),col = "red")
weights[ind,]
