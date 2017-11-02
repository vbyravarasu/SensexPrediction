# Project Title:  The Influence of daily share price movements of equity stocks of Infosys,TCS, Tata motors,Sun Pharma,Axis bank,ICICI bank,ITC and AsainPaints, on daily movements of  Bombay Stock Exchange -SENSEX.
# NAME: Sambasiva Rao Kambala
# EMAIL: shiva.kambala2016@gmail.com
# COLLEGE / COMPANY: Andhra Loyola College/Self employed 

### Reading dataset into R
sensexmovements.df <- read.csv("DailySensexmovement.csv")
View(sensexmovements.df)

### Summarizing the data frame
str(sensexmovements.df)
summary(sensexmovements.df)


### BOX plot for the independent varaible TCS.Bo
boxplot(sensexmovements.df$TCS.BO,horizontal = TRUE,col = "red",xlab = "Price of TCS share(INR)",main = "Price of TCS share during the period 2014-2016")


### BOX plot for the independent varaible ICICIBANK.Bo
boxplot(sensexmovements.df$ICICIBANK.BO,horizontal = TRUE,col = "green",xlab = "Price of ICICIBANK share(INR)",main = "Price of ICICI BANK share during the period 2014-2016")



### BOX plot for the independent varaible ASIANPAINTS.Bo
boxplot(sensexmovements.df$ASIANPAINTS.BO,horizontal = TRUE,col = "blue",xlab = "Price of ASIANPAINTS share(INR)",main = "Price of ASIANPAINTS share during the period 2014-2016")

### Scatter plot for SENSEX.BO versus TCS.BO
plot(y = sensexmovements.df$SENSEX.BO, x =sensexmovements.df$TCS.BO,col= "red",xlab = "TCS daily price movement during 2014-2016", ylab  ="Sensex daily movement during 2014-2016",main =" TCS VS SENSEX",xlim= c(1000,3000),ylim = c(5000,33000))
abline(h=mean(sensexmovements.df$SENSEX.BO),col = "red")
abline(v=mean(sensexmovements.df$TCS.BO),col = "blue")
abline(lm(sensexmovements.df$SENSEX.BO ~ sensexmovements.df$TCS.BO))




### Scatter plot for SENSEX.BO versus ICICIBANK.BO
plot(y = sensexmovements.df$SENSEX.BO, x =sensexmovements.df$ICICIBANK.BO,col =  "blue", xlab = "ICICI BANK daily price movement during 2014-2016", ylab  = "Sensex daily movement during 2014-2016",main ="ICICI BANK VS SENSEX", xlim= c(500,2500), ylim = c(5000,33000))
abline(lm(sensexmovements.df$SENSEX.BO ~ sensexmovements.df$ICICIBANK.BO))
abline(h=mean(sensexmovements.df$SENSEX.BO),col = "red")
abline(v=mean(sensexmovements.df$ICICIBANK.BO),col = "blue")



### Scatter plot for SENSEX.BO versus ASIANPAINTS.BO
plot(y = sensexmovements.df$SENSEX.BO, x =sensexmovements.df$ICICIBANK.BO,col =  "blue", xlab = "ICICI BANK daily price movement during 2014-2016", ylab  = "Sensex daily movement during 2014-2016",main ="ICICI BANK VS SENSEX", xlim= c(500,2500), ylim = c(5000,33000))
abline(lm(sensexmovements.df$SENSEX.BO ~ sensexmovements.df$ASIANPAINTS.BO))
abline(v=mean(sensexmovements.df$ASIANPAINTS.BO),col = "blue")
abline(h=mean(sensexmovements.df$SENSEX.BO),col = "red")


### Drawing Corrgram for important variables
library(corrgram)
corrgram(sensexsub.df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt,main="Corrgram of Sensex and other share prices intercorrelations")


### Drawing variance-Covariance matrix forimportant variables
sensexsub.df<-sensexmovements.df[,c(2,3,8,10)]
cov(sensexsub.df)


### AArticulating  a Hypothesis that could be tested usisng t-test.
### Null Hypothesis Ho:Thre is no change in the average sensenx points with the change in the average price change in TCS share.

### Alternate Hypothesis Ha:Thre is a siginificant change in the average sensenx points with the change in the average price change in TCS share.


### Running t-tests for testing the above hypothesis
t.test(sensexmovements.df$SENSEX.BO - sensexmovements.df$TCS.BO)




### Null Hypothesis Ho:Thre is no change in the average sensenx points with the change in the average price change in ICICIBANK share.

### Alternate Hypothesis Ha:Thre is a siginificant change in the average sensenx points with the change in the average price change in ICICIBANK share.



### Running t-tests for testing the hypothesis

t.test(sensexmovements.df$SENSEX.BO - sensexmovements.df$ICICIBANK.BO)


### Null Hypothesis Ho:Thre is no change in the average sensenx points with the change in the average price change in ASIANPAINTS share.

### Alternate Hypothesis Ha:Thre is a siginificant change in the average sensenx points with the change in the average price change in ASIANPAINTS share.



### Running t-tests for testing the hypothesis

t.test(sensexmovements.df$SENSEX.BO - sensexmovements.df$ASIANPAINTS.BO)


### Linear Regression Models using lm()
fit <- lm(SENSEX.BO ~ TCS.BO + ICICIBANK.BO + ASIANPAINTS.BO, data=sensexmovements.df)
fit
summary(fit)



