sales <-read.csv(choose.files())
View(sales) 
summary(sales)

# we need to transfer character to numeric data 
#install.packages(plyr)
library(plyr)
cdata <- sales
cdata$cd <-as.numeric(revalue(cdata$cd,c("yes"=1,"no"=0)))
cdata$multi <-as.numeric(revalue(cdata$multi,c("yes"=1,"no"=0)))
cdata$premium <-as.numeric(revalue(cdata$premium,c("yes"=1,"no"=0)))
View(cdata)
class(cdata)
attach(cdata)
summary(cdata)

# scatter plots
plot(speed,price)
plot(hd,price)
plot(ram,price)
plot(screen,price)
plot(cd,price)
plot(multi,price)
plot(premium,price)
plot(ads,price)
plot(trend,price)
#relation between correlation variable
pairs(cdata)

# correlation coefficient matrix
cor(cdata)
#barplot
barplot(height=cdata$price, names=cdata$price)

## correlation
cor(cdata$price,cdata$X)
cor(cdata$price,cdata$speed)
cor(cdata$price,cdata$hd)
cor(cdata$price,cdata$screen)
cor(cdata$price,cdata$cd)
cor(cdata$price,cdata$multi)
cor(cdata$price,cdata$premium)
cor(cdata$price,cdata$ads)
cor(cdata$price,cdata$trend)

## Build linear model
model1 <- lm(price~.,data=cdata) ##R-squared:  0.7778,	Adjusted R-squared:  0.7775 
summary(model1)
plot(model1)

library(car)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model1,id.n=2,id.cex=0.7)

library(mvinfluence)
library(car)
influenceIndexPlot(model1,id.n=3)
influencePlot(model1,id.n=3)


predict(model1,interval = 'predict')

# Final Model
FinalModel<-lm(price~speed+I(speed^2)+I(speed^3)+
                 hd+I(hd^2)+I(hd^3)+
                 ram+I(ram^2)+I(ram^3)+
                 screen+I(screen^2)+I(screen^3)+
                 cd+I(cd^2)+I(cd^3)+
                 multi+I(multi^2)+I(multi^3)+
                 premium+I(premium^2)+I(premium^3)+
                 ads+I(ads^2)+I(ads^3)+
                 trend+I(trend^2)+I(trend^3))
View(FinalModel)
summary(FinalModel)

#Prediction
Profpred <- predict(FinalModel)
View(Profpred)

finalplot <-cdata[-c(1441,1701),]
View(finalplot)

Final <- cbind(speed,hd,ram,screen,cd,multi,premium,ads,trend,price,Profpred)
View(Final)