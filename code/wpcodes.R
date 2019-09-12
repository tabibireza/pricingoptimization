# Reading Data
priceData <- read.csv("wtpData.csv")
summary(priceData)
sd(priceData$Wtp) # Standard Deviation
dens <-density(priceData$Wtp)
plot(dens)

# producing better chart with ggplot
library(ggplot2)
base <- ggplot(priceData, aes(x=Wtp))+
  geom_density(fill="yellow", alpha =.3) +
  labs(x="Willingness to pay",y="% per WTP" )
base

# Theoritical distribution 
m <- mean(priceData$Wtp)
S <- sd(priceData$Wtp)
base+stat_function(fun=dnorm, args = list(mean = m, sd=s), color="black")




# visually assess the fit of the distribution
# the method of moments (MOM)
install.packages("fitdistrplus")
library(fitdistrplus)
fitn <-fitdist(priceData$Wtp,"norm") 
summary(fitn)


# fit the logistic to the data, using the method of maximum likelihood
fitw <-fitdist(priceData$Wtp,"logis") 
summary(fitw)
plot(fitw)
# logistic has been compared with norm 
# based on Likelihood or BIC log is slightly better 
# likelihood more and BIC is less for better. 


# computes the demand function based on the logistic distribution
# demand at a given price p is given by (1 - cdf(p))
demand <- function(x,m,s, mktSize){ 
  mktSize*(1-plogis(x, location = m, scale = s)) }

# plotting the demand 
mktSize <- 10000
demandcurve = ggplot(priceData, aes(x=Wtp))+stat_function(fun = demand, args = list(m,s,mktsize),
                                                          color="Black")+theme_minimal()+ylab("Demand")+xlab("price")
demandcurve

# having the demand curve if you know the marginal cost you can estimate profit
profit <- function(price,cost, m,s,mktSize){(price-cost)*demand(price, m, s, mktSize)}

#computing the profit for different prices and marginal cost 
profit(20,15,m,s,10000)
profit(35,15,m,s,10000)
# it is better to set a slicer book2!!!

# the optimal price for maximization our profit
# optim command minimizes
# we use the negative profit
res <- optim(22, profit,cost=15,m=m, s=s, mktSize=10000, method = "BFGS", 
             control = list(fnscale=-1))
optPrice <- res$par
optProfit <- (res$value)
optPrice
optProfit

# Uncertainty and Bootstrapping Demand Curves
# It is not possible to measure wtp for every one
# So we come up with statistics 
# but we must measure the uncertainty 
# bootstrap = given total population is equal to our sample then
# randomely select n number of that as a new sample for test
# repeat this test as many as it is sutible for instance 1000 times
# bootstapping to construct 1000 bootstrap samples
n <-60 
bootProfit <- rep(0.0, 1000) 
cost<-15 
for(i in 1:1000)
{bsample <- sample(priceData$Wtp, n, replace=TRUE) 
mb<-mean(bsample) 
sb<-sqrt(var(bsample)*3/pi^2) 
bootProfit[i]<- (profit(optPrice, cost, mb, sb,10000))}
# compute a bootstrap interval for the range of profits
quantile(bootProfit, probs=c(0.025, 0.975))


 



