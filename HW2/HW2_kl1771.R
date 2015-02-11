

##1
#a.  Using R or Python, write code to draw at random 10 observations from a N(0,1) random variable.  
#Instruct the machine to calculate the mean, variance and standard deviation of your draws.

x <- round(rnorm(10),2)

x1 <- mean(x)
x2 <- var(x)
x3 <- sd(x)

#b.  Repeat this exercise using 10,000 draws from a N(0,1), instructing again the machine to calculate 
#the mean, variance and standard deviation of your draws.

y <- rnorm(10000)

y1 <- mean(y)
y2 <- var(y)
y3 <- sd(y)

#c.  Repeat this exercise with 1,000,000 draws from a N(0,1), instructing again the machine to calculate 
#the mean, variance and standard deviation of your draws.  

z <- rnorm(1000000)

z1 <- mean(z)
z2 <- var(z)
z3 <- sd(z)

#d.  What conclusions, if any, do you draw from increasing the sample size?

##2 

#a.  Go to http://www.random.org/integers/ and generate two series of 1,000 random integers 
#with values between 0 and 9.  Call one series y and the other x

x <- read.table("/Users/krluna/Desktop/num.txt", sep="\n")
x <- as.vector(x$V1)

y <- read.table("/Users/krluna/Desktop/numy.txt", sep="\n")
y <- as.vector(y$V1)

#b.  Using Python or R, fit the bivariate linear regression model.

model <- lm(y ~ x)
summary(model)

#c.  Examine your t-statistic to evaluate whether it is greater than two in absolute value.  
#Would you reject or fail to reject that there is any relationship between these two series?  

## http://www.answers.com/Q/What_does_a_high_t_statistic_mean

##3 

#a.  Using R or Python, calculate the log returns of each series as the natural log of the ratio of 
#(price today/price yesterday).  Use the reported closing price for this exercise.

AMEX = Quandl("GOOG/NYSE_AXP", 
              start_date="2004-09-18", end_date="2014-09-18")

AMEX <- AMEX[order(AMEX$Date),]

amex <- as.vector(AMEX$Close)

DJ = Quandl("YAHOO/INDEX_NYA", 
            start_date="2004-09-18", end_date="2014-09-18")

DJ <- DJ[order(DJ$Date),]

dj <- as.vector(DJ$Close)

log_ret_amex <- diff(log(amex), lag=1)
log_ret_dj <- diff(log(dj), lag=1)


#b.  Using R or Python, generate a histogram of log returns of the stock of your choice.
hist(log_ret_amex)
hist(diff(log(table$amex)))


#c.  Using R or Python, generate a scatterplot that relates the log returns of your stock 
#of choice to the log returns of the exchange on which it is traded.  

AMEX_table <- AMEX[c(1,5)]
DJ_table <- DJ[c(1,5)]
table <- merge(AMEX_table, DJ_table, c("Date"))

fix(table)

amexlog <- as.vector(table$amex)
amexlog <- diff(log(amexlog))

djlog <- as.vector(table$dj)
djlog <- diff(log(djlog))

new_data <- data.frame(amexlog, djlog)

summary(lm(amexlog~djlog))
fit(plot(djlog, amexlog))

lm(amexlog~djlog)

test1 <- as.vector(table$amex)
test2 <- as.vector(table$dj)

summary(lm(log(test1)~log(test2)))


##d.  Finally, using R or Python, fit a linear model to obtain estimates of what 
# finance folks call the “alpha” and the “beta”.  Is “alpha” significantly different 
#than zero at a 95% level?  Does a 95% confidence level for “beta” include one?  

b <- lm(djlog ~ amexlog)

summary(b)


##4 

#a.  Read this dataset into R or Python.  (For R, you may find the “foreign” library 
#of use.  For Python, check out Pandas.  The goal here is to get you familiar with 
#reading datasets with alternative formats.)  

install.packages("foreign")
library(foreign)
train <- read.dta("/Users/krluna/Desktop/train.dta")

#b.  Generate summary statistics for the following variables in the data:
  #•	d, which is an indicator for whether a particular email is spam
summary(train$d)  

  #•	x1, which is an attribute of the email
round(summary(train$x1),3)


#c.  Using least squares, regress d on x1.  (For R, check out lm.  For Python, 
#check out StatsModels.)  Congratulations, you have created a support vector machine 
#that you will use to forecast whether an incoming email with a different attribute is 
#spam.

trainmodel <- lm(d ~ x1, train)
summary(trainmodel)

#d.  Suppose you set the threshold that an email is spam if the predicted value 
#exceeds 1.   I give you a new email with an attribute value 0.65.  Would you classify 
#it as spam or not spam?

spam <- -0.01795 + 1.01554 * .65
spam = .642151
# No the email is not spam.  

#e.  I give you another new email with an attribute value of 0.99.  Would you classify 
#it as spam or not spam?

spam <- -0.01795 + 1.01554 * .99
spam = .9874346

# No the email is not spam. 

##5 

#a. Suppose your DGP is y_i=1+2x_i+ϵ_i, where x~N(0,1)  and ϵ~N(0,1).  

#b. Using R or Python, write code to generate 1,000 draws for x and ϵ.  
#Use these draws to generate y in accordance with the DGP in a.  

X <- rnorm(1000)
ei <- rnorm(1000)

result.a <-function(x,y) {
  result <- 1 + 2 * x + y
}

output.a <- result.a(X, ei)

#c. Using R or Python, write code to estimate the bivariate model, 
#y_i=β_0+β_1 x_i and summarize the findings.  

result.b <-function(x) {
  result <- 1 + 2 * x
}

output.b <- result.b(X)

output.b_model <- lm(output.b ~ X)
summary(output.b_model)

#d.  Repeat b. and c. above five times for a new set of random draws for 
#each replication.  (This effort is called Monte Carlo simulation.)  

output.a5 <- replicate(5, {
  X <- rnorm(1000)
  ei <- rnorm(1000)
  output <- result.a(X, ei)
})

output.b5 <- replicate(5, {
  Bi <- rnorm(1000)
  ei <- rnorm(1000)
  output <- result.b(X)
})

#e.  Given what you’ve done in d., Suppose you wrote code to repeat b. and c. 
#above 1,000 times, each time recording the estimated value of β_1.  What do you 
#think a histogram of these 1,000 replications of the estimate value of β_1 would show?  

for (i in 1:1000){
  Bi <- rnorm(1000)
  ei <- rnorm(1000)
  output <- result.a(X, ei)
  model <- lm(output ~ X)
  vec <- append(vec, summary(model)$coefficient[2,1])
}


for (i in 1:1000){
  Bi <- rnorm(1000)
  ei <- rnorm(1000)
  output <- result(X, ei)
  model <- lm(output ~ X)
  vec <- append(vec, summary(model)$coefficient[2,1])
}


#e. Suppose that you were not interested in the estimate of β_1 per se, 
#but instead in some functional transformation, such as the estimate of 
#expa(β_1).  What might you do with your 1,000 replications from e. above
#to inform you about the distribution of the estimate of expa(β_1)?  

e <- exp(2)

#f. Submit code and results.




