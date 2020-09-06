############# Packages Required #################

library(forecast)
library(rmarkdown)
library(smooth)
library(fpp)
library(readxl)



########### Reading and  understanding the data ############

Airlines_Data<-read.csv(file.choose())
View(Airlines_Data)
nrow(Airlines_Data)#[1] 2
ncol(Airlines_Data)#[1] 96
names(Airlines_Data)
Airlines_Data


windows()
plot(Airlines_Data$Passengers,type = "o")

##### Creating Dummy  variables any no. of dummy variables we want to create

X<-data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)#so craeting 12 dummy variables
###creating dummies for 12 months

View(X)
colnames(X)<-month.abb  # Assigning month names 

Airlines_Data1<-cbind(Airlines_Data,X)
View(Airlines_Data1)
colnames(Airlines_Data1)



########### Splitting the data into training and test data ######


Airlines_Data1["t"]<- 1:96
View(Airlines_Data1)
Airlines_Data1["log_Passenger"]<-log(Airlines_Data1["Passengers"])
Airlines_Data1["t_square"]<-Airlines_Data1["t"]*Airlines_Data1["t"]
attach(Airlines_Data1)

train<-Airlines_Data1[1:84,]
View(train)
nrow(train)#[1] 84

test<-Airlines_Data1[85:96,]
View(test)
nrow(test)#[1] 12

########################### Linear model #############################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)#Multiple R-squared:  0.7923,	Adjusted R-squared:  0.7898 

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.19924






########************* Exponential model  *****************#######

expo_model<-lm(log_Passenger~t,data=train)
summary(expo_model)#Multiple R-squared:  0.8239,	Adjusted R-squared:  0.8218 

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.05736  



######################### Quadratic model ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)#Multiple R-squared:  0.7963,	Adjusted R-squared:  0.7912 


Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 
## [1] 48.05189




######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)#Multiple R-squared:  0.1674,	Adjusted R-squared:  0.04015

sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 
## [1] 132.8198






######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)#Multiple R-squared:  0.9551,	Adjusted R-squared:  0.9475

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 
## [1] 35.34896



######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)#Multiple R-squared:  0.9598,	Adjusted R-squared:  0.9524

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36082 







######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)###Multiple R-squared:  0.1548,	Adjusted R-squared:  0.02568 

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.0632







######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) ####Multiple R-squared:  0.9763,	Adjusted R-squared:  0.9723 

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 10.51917 and Adjusted R2 - 97.23%






#######Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)






# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = Airlines_Data1)
new_model_pred<-data.frame(predict(new_model,newdata=Airlines_Data1,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(Airlines_Data$Month)

Final <- as.data.frame(cbind(Month,Airlines_Data1$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)




