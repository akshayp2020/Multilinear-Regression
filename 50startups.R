startup = read.csv(choose.files())
View(startup)
attach(startup)

startup$State<- as.numeric(State,c("New York"=1,"California" = 2,"Florida" =3))
View(startup)
summary(startup)

plot(R.D.Spend,Profit)
plot(Administration,Profit)
plot(Marketing.Spend,Profit)

pairs(startup)
cor(startup)

library(corpcor)
cor2pcor(cor(startup))


## Building a Model
model<- lm(Profit~., data = startup[,-4])
summary(model)
# R-squared:  0.9507,	Adjusted R-squared:  0.9475
plot(model)

library(car)
avPlots(model)
vif(model)

influence.measures(model)
influenceIndexPlot(model,id.n = 3)
influencePlot(model,id.n= 3) ##50th observtn is influential point

## regression model after deleting 50th observtn
model_2<- lm(Profit~., data = startup[-50,-4])
summary(model_2)
##R-squared:  0.9613,	Adjusted R-squared:  0.9587

influence.measures(model_2)
influenceIndexPlot(model_2,id.n=3)
influencePlot(model_2,id.n = 3)

## regression Model after deleting 49th n 50th observtn
model_3<- lm(Profit~., data = startup[-c(49,50),-4])
summary(model_3)
##R-squared:  0.9627,	Adjusted R-squared:  0.9601

avPlots(model_3)
vif(model_3)

## Final Model is model_3
plot(model_3)
hist(residuals(model_3))  ##close to normal distribution

predict_model<- predict(model_3)
predict_model

