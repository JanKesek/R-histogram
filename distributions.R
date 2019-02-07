#ROZKLADY:
#ZAD: mozna przegrac 4 z p 0.32, lub wygrac 0,1,8 z p 0.49,0.15,0.05
X.outcomes <- c(-4,0,1,8)
X.prob <- c(0.32, 0.48, 0.15, 0.05)
barplot(X.prob, ylim = c(0,0.5), names.arg = X.outcomes, space = 0)
#cumulative prob:
cumsum(X.prob)
barplot(cumsum(X.prob))

#PROB of Continuous Variables
w<- seq(35,95,by=5)
lower.w <- w>=40 & w<=65
upper.w <- w>65 & w<=90
#there is 18.5% chance that max(Temperature) will be < or == to 55.2 F
fw.specific<- (55.2-40)/625
fw.specific.area<- 0.5*15.2*fw.specific
fw.specific.vertices<- rbind(c(40,0),c(55.2,0), c(55.2,fw.specific))
plot(w,fw,type = "l", ylab = "f(w)")
abline(h=0, col="gray", lty=2)
polygon(fw.specific.vertices, col = "gray", border = NA)
abline(v=55.2, lty=3)
text(50,0.005,labels = fw.specific.area)


#BIMODAL/BERNOULLI - prob of getting five 4 in a row in 8x dice roll
dbinom(x=5, size = 8, prob = 1/6)
round(dbinom(x=0:8, size = 8, prob = 1/6), 3)
#sd = n*p*(1-p)
8*(1/6)*(5/6)
#cumulative sum of prob of getting three or fewer 4s in 8x diceroll
sum(dbinom(x=0:3, size = 8, prob = 1/6))

#A forested nature reserve has 13 bird-viewing platforms scattered
#throughout a large block of land. The naturalists claim that atany 
#point in time, there is a 75 percent chance of seeing birds ateach 
#platform.Suppose you walk through the reserve and visitevery platform. 
sum(dbinom(x=0:10,size = 13, prob = 0.75 ))

#POISSON DISTRIBUTION
f(x)=lambda^x*exp(-lambda)/factorial(x)
czyli:
  (3.22^3*exp(-3.22))/prod(3:1)
round(dpois(0:10, 3.22), 3)
barplot(dpois(x=0:10, lambda = 3.22), ylim = c(0,0.25), space=0, names.arg = 0:10)
#NORMAL DISTRIBUTION
xvals<-seq(-4,4,length = 50)
fxvals<- dnorm(xvals, mean = 0, sd=1)
plot(xvals,fxvals, type = "l")

xvals2<-seq(-5,-2, length=300)
fxvals2<-dnorm(xvals2, mean = -3.42, sd = 0.2)
plot(xvals2, fxvals2, type = "l", xlim = c(-4.4, -2.5),
     main = "N(-3.42,0.2) norm distr", xlab = "x", ylab = "f(x)")
abline(h=0, col="gray")
abline(v=c((-3.42+0.2),(-3.42-0.2)), lty=3:2)
xvals2.sub <- xvals2[xvals2>=(-3.42-0.2) & xvals2<=(-3.42+0.2)]
fxvals2.sub <- fxvals2[xvals2>=(-3.42-0.2) & xvals2<=(-3.42+0.2)]
polygon(rbind(c((-3.42-0.2),0), cbind(xvals2.sub, fxvals2.sub)
        ,c((-3.42+0.2),0)), border = NA, col = "gray")
#Let’s finish this section with one more working problem. 
Assume the man-ufacturer of a certain type of snack knows that the total net 
weight of thesnacks in its 80-gram advertised package,X, is normally distributed
with amean of 80.2 grams and a standard deviation of 1.1 grams. The manufac-turer 
weighs the contents of randomly selected individual packets. The prob-ability 
a randomly selected packet is less than 78 grams (that is, Pr(X<78))is as follows:
  pnorm(78,80.2,1.1)

The probability a packet is found to weigh between 80.5 and 81.5
gramsis as follows: pnorm(81.5,80.2,1.1)-pnorm(80.5,80.2,1.1)

The weight below which the lightest 20 percent of packets lie is asfollows:
  qnorm(0.2,80.2,1.1)

A simulation of five randomly selected packets can be found with thefollowing:
  round(rnorm(5,80.2,1.1),1)