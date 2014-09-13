##set seed for predictable random numbers (!)
set.seed(23)

##satisfy project recommendation
sims=1000
lambda=0.2
obs=40

dat1000 <- as.data.frame(replicate(sims, mean(rexp(obs, lambda))))
names(dat) <- c("Means")
g <- ggplot(dat, aes(x=Means)) + geom_histogram(aes(y=..density..), binwidth=.25,  colour="black", fill="white")
g <- g + ggtitle("Average of Distributions")
g <- g + xlab("Mean") + ylab("Density")
g <- g + geom_vline(aes(xintercept=mean(dat$Means)), color="blue", linetype="dashed", size=1)
##plot the normal distribution on top, we know the mean is 5 from 1/lambda 
##standard deviation is 1 for a standard normal distribution (source: wikipedia)

g <- g + stat_function(fun = dnorm, colour = "red", arg = list(mean=5, sd =1))
g

##satisfy curiosity - compare this and 10000 to see if CLT does start to hold the larger the number
sims=15
lambda=0.2
obs=40

dat15 <- as.data.frame(replicate(sims, mean(rexp(obs, lambda))))
names(dat) <- c("Means")
g <- ggplot(dat, aes(x=Means)) + geom_histogram(aes(y=..density..), binwidth=.25,  colour="black", fill="white")
g <- g + ggtitle("Average of Distributions")
g <- g + xlab("Mean") + ylab("Density")
g <- g + geom_vline(aes(xintercept=mean(dat$Means)), color="blue", linetype="dashed", size=1)
g <- g + stat_function(fun = dnorm, colour = "red", arg = list(mean=5, sd =1))
g

##satisfy curiosity
sims=100000
lambda=0.2
obs=40

dat10000 <- as.data.frame(replicate(sims, mean(rexp(obs, lambda))))
names(dat) <- c("Means")
g <- ggplot(dat, aes(x=Means)) + geom_histogram(aes(y=..density..), binwidth=.25,  colour="black", fill="white")
g <- g + ggtitle("Average of Distributions")
g <- g + xlab("Mean") + ylab("Density")
g <- g + geom_vline(aes(xintercept=mean(dat$Means)), color="blue", linetype="dashed", size=1)
g <- g + stat_function(fun = dnorm, colour = "red", arg = list(mean=5, sd =1))
g


##look at the variance
var(dat$Means)

##standard deviation is the square root of the variance
sqrt(var(dat$Means))

##Create a 95% confidence interval


##Part 2
##Q1 

tg <- data(ToothGrowth)
str(tg)
summary(tg)
names(tg)
head(tg)
unique(tg$dose)

library(sqldf)

sqldf("select count(*) from tg where supp='VC'")
sqldf("select count(*) from tg where supp='OJ'")
##useful, same sample size, can use t.test and replicate the sleep study

qplot(ToothGrowth, y=ToothGrowth$len, x=ToothGrowth$dose)
hist(ToothGrowth$len)

##facets for dose

g <- ggplot(data=tg, aes(x=len)) + geom_bar(colour="black", stat="bin", fill="#DD8888", binwidth=0.5) + facet_grid(dose ~ .) + xlab("Length") + ylab("Count")
g <- g + ggtitle("Breakdown of length of toothgrowth by Dose")
g

##facets for supplement

g <- ggplot(data=tg, aes(x=len)) + geom_bar(colour="black", stat="bin", fill="#DD8888", binwidth=0.5) + facet_grid(supp ~ .) + xlab("Length") + ylab("Count")
g <- g + ggtitle("Breakdown of length of toothgrowth by Supplement")
g

##Q2: basic summary
summary(tg)
str(tg)

##shows us the distribution is roughly symmetric and mound shaped, so we can use the t interval (Assumption)
##if we have the same sample sizes we could use the sleep study?




##Q3:  hypothesis testing
##H0: the dose has no influence on toothgrowth
t.test(tg$len, tg$dose)

--> do not reject the null hypothesis


##H0: the supplement has no influence on toothgrowth
t.test(len ~ supp, data=tg)

--> reject the null hypothesis!

##State your conclusions and the assumptions needed for your conclusions. 
