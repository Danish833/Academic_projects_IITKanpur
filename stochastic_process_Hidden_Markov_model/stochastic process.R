#Loading data
train <- read.csv("C:/Users/DANISH/Desktop/credit1.csv")

test <- read.csv("C:/Users/DANISH/Desktop/test.csv")


# training the model
states = c(1,2,3) 
symbols = c(1,2,3)

library(depmixS4) 
library(HMM)
hmmInit = initHMM(states, symbols , c(4/7,2/7,1/7)) 
hmmFit = baumWelch(hmmInit, train$obs) 

#Testing the Model
result<-c()


combined<-rbind(train,test)

hmm_model<-(hmmFit$hmm)

#If % changes in oberving the sequence is greater than 
#specied threshold then it is a fraud tranaction 

for(i in 1:32){
logForwardProbabilities = forward(hmm_model,c(as.character(combined$obs[i:(i+68)])))
probO1<-data.frame(exp(logForwardProbabilities))
##print(exp(logForwardProbabilities))

alpha1<-sum(probO1$X10)

logForwardProbabilities = forward(hmm_model,c(as.character(combined$obs[(i+1):(i+69)])))
probO2<-data.frame(exp(logForwardProbabilities))
##print(exp(logForwardProbabilities))

alpha2<-sum(probO2$X10)


# creating a vector to store results
{
if(((alpha2-alpha1)*100/alpha1)<10)
  result<-c(result,"Legal")
else
  result<-c(result,"Fraud")
}


}
 #final results
final_result1<-cbind(test,result)



