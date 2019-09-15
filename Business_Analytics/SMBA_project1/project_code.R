#uploading Dataset into R-studio
data<-read.csv("C:/Users/DANISH/Desktop/project_sem2/SMBA_project1/project1_data.csv")


#checking for any missing values
sum(is.na(data))

#Data summary
summary(data)
str(data)


#data visualization
new.data<-data.frame(data)
data.scatter<new.data
plot(data.scatter)

#Multicollinearity check

plot(data.scatter$X3.distance.to.the.nearest.MRT.station,data.scatter$X6.longitude)
corvalue<-cor(data.scatter)
round(corvalue,2) 
#There is no perfect multicollinearity


data.long<-data.scatter





data.model<-data.long



#checking for outlier
plot(data.model) #last row shows there is possibility of existing of an outlier
boxplot(data.model$X2.house.age ,
          col = "blue")
boxplot(data.model$X3.distance.to.the.nearest.MRT.station ,
          col = "red") #large potential outliers exist
boxplot(data.model$X5.latitude ,
          col = "green")
boxplot(data.model$X4.number.of.convenience.stores ,
          col = "grey")




data.final<-data.model



plot(as.factor(data.final$X4.number.of.convenience.stores),data.final$Y.house.price.of.unit.area,
     col="blue",
     xlab="stores", ylab="HousePrice")

attach(data.final)

model4<-lm(Y.house.price.of.unit.area~X4.number.of.convenience.stores)
summary(model4)

plot(model4)

library(jtools)
library(kableExtra)
library(huxtable)
summ(model4)

effect_plot(model4, pred = X4.number.of.convenience.stores, interval = TRUE, plot.points = TRUE,
            colors="blue")
export_summs(model4, scale = TRUE)




model1=lm(data=train,hprice~poly(as.numeric(td),3))
summary(model1)
plot(model1)


model2=lm(data=train, hprice~ poly(ha,3))
summary(model2)
plot(model2)

model3=lm(data=train, hprice~ log(dtmrt))
summary(model3)
plot(model3)