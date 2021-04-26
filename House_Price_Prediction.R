house_price<-read.csv("D:/R studio/NOTES/Assignments/Test/House Price Data.csv")
summary(house_price)


library(ggpubr)
ggqqplot(house_price$Price)
#as from the ggplot the points fall majourly on the refrence line so we can say that the data is normally distributed

house_price$X.1<-NULL
house_price$X<- NULL

house1To90<-house_price[1:90,]
#View(house1To90)
summary(house1To90)

#droping insignifficant variables
house1To90$Houseid<-NULL

#convert school var into factor
house1To90$Schools<-factor(house1To90$Schools)

#performing Reg analysis using LM

house_Lm_Model<-lm(formula = Price ~ Area + Distance + Schools, data = house1To90)
summary(house_Lm_Model)

#stepwise for feature selection
null_house_Model<-lm(Price~1, data = house1To90,)
step(null_house_Model, direction = "forward", scope = list(lower=null_house_Model, upper=house_Lm_Model))



#R-squared:  0.8355, as the value of Rsq is greater than 0.6 the model tends for good fit

##to predict sales from id 91-99
house91To99<-house_price[91:99,]
View(house91To99)

#droping insignificant factors
house91To99$Houseid<-NULL

house91To99$Schools<-factor(house91To99$Schools)
str(house91To99)

#model
Model91to99<-lm(Price~., data = house91To99)
summary(Model91to99)

#predict

house91To99$pred_prob<-predict(Model91to99,house91To99,type = "response")
head(house91To99)

difference<-house91To99$pred_prob - house91To99$Price

RMSE1<-sqrt(mean(difference^2))
RMSE1

#double sure using RMSE function
library(Metrics)
rmse(house91To99$Price,house91To99$pred_prob)
#RMSE: 2.221573

#Q3
shapiro.test(difference)
ggqqplot(difference)

#as from the shaprio test and qq plot the error is seen to be homoscedastic and normally distributed

#Q5
house_price_Q4<-house_price
house_price_Q4$Houseid<-NULL

median(house_price_Q4$Price) #25.03

house_price_Q4$price_bin<-ifelse(house_price_Q4$Price>25.03,"1","0")
head(house_price_Q4$price_bin, 15)
house_price_Q4$bin<-NULL
house_price_Q4$Price<-NULL

#as school variable has p: 0.904458 remove it

house_price_Q4$Schools<-NULL

#modeling price_bin
house_price_Q4$price_bin<-factor(house_price_Q4$price_bin)
str(house_price_Q4)
price_bin_glm_model<-glm(price_bin~., data = house_price_Q4, family = "binomial")
summary(price_bin_glm_model)



#ROC Curve
library(ROCR)

house_price_Q4$predict<-predict(price_bin_glm_model, house_price_Q4, type = "response")

pred1<-prediction(house_price_Q4$predict, house_price_Q4$price_bin)
perf<-performance(pred1,"tpr","tnr")
plot(perf,colorize=T,print.cutoffs.at=seq(0.1,1.0,0.1))

#according to ROCR curve 0.5 looks as a good threshold

house_price_Q4$predict<-ifelse(house_price_Q4$predict>0.5,"1","0")
head(house_price_Q4$predict)

#Q6

house_price_Q6<-house_price

#using CUT Function
str(house_price_Q6)

house_price_Q6$Price_cat<-cut(house_price_Q6$Price, breaks = c(1,20,30,Inf), labels = c("1", "2", "3"))


house_price_Q6$Price_cat<-factor(house_price_Q6$Price_cat) 
house_price_Q6$Schools<-factor(house_price_Q6$Schools)
library(randomForest)
house_price_Q6_RF<-randomForest(Price_cat~Area+Distance+Schools,data = house_price_Q6, importance=TRUE)
house_price_Q6_RF

#To measure Accuracy
house_price_Q6$pred_cat<-predict(house_price_Q6_RF,house_price_Q6)
library(caret)
accuracy(house_price_Q6$Price_cat,house_price_Q6$pred_cat)
# using accuracy function Accuracy comes out to be: 0.959596 ie 95%