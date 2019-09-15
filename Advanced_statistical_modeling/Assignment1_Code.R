  #Question 1  

  library(mvtnorm)
  #mean of class A
  m_A=c(1,0)
  #mean of class B
  m_B=c(0,1)
  #covariance matrix
  cv=matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  #generating points for multivariate distribution for class A and classs B
  Cluster_means_A=rmvnorm(10, mean = m_A, sigma = cv)
  Cluster_means_B=rmvnorm(10, mean = m_B, sigma = cv)
  #Generating train data using  above means
  Data_A=data.frame()
  for (i in 1:100) {
    p=sample(1:10,1, replace = TRUE)
    temp_mean=as.vector(Cluster_means_A[p,])
    new_point=as.data.frame(rmvnorm(1, mean = temp_mean, sigma = cv/5))
    Data_A=rbind(Data_A, new_point)
  }
  Data_B=data.frame()
  for (i in 1:100) {
    p=sample(1:10,1, replace = TRUE)
    temp_mean=as.vector(Cluster_means_B[p,])
    new_point=as.data.frame(rmvnorm(1, mean = temp_mean, sigma = cv/5))
    Data_B=rbind(Data_B, new_point)
  }
  
  #labeling the data
  Y=rep(1,100)
  Data_A=cbind(Data_A,Y)    
  Y=rep(2,100)
  Data_B=cbind(Data_B,Y)
  #binding A and B classes for training purpose
  train_data=rbind(Data_A,Data_B)
  #shuffling the data
  train_data<- train_data[sample(nrow(train_data)),]
  
  
  
  
  #Question 2
  test_Data_A=data.frame()
  for (i in 1:5000) {
    p=sample(1:10,1, replace = TRUE)
    temp_mean=as.vector(Cluster_means_A[p,])
    new_point=as.data.frame(rmvnorm(1, mean = temp_mean, sigma = cv/5))
    test_Data_A=rbind(test_Data_A, new_point)
  }
  
  
  test_Data_B=data.frame()
  for (i in 1:5000) {
    p=sample(1:10,1, replace = TRUE)
    temp_mean=as.vector(Cluster_means_B[p,])
    new_point=as.data.frame(rmvnorm(1, mean = temp_mean, sigma = cv/5))
    test_Data_B=rbind(test_Data_B, new_point)
  }
  
  
  Y=rep(1,5000)
  test_Data_A=cbind(test_Data_A,Y)    
  Y=rep(2,5000)
  test_Data_B=cbind(test_Data_B,Y)
  #binding A and B classes for training purpose
  test_data=rbind(test_Data_A,test_Data_B)
  #shuffling the data
  test_data<- test_data[sample(nrow(test_data)),]
  
  





  #Question 3
    
  #Knn Model
  library(class)
  
  #number of neighbours to try
  neighbors=c(1,5,10,15,20,25,30,35,40,45,50)
  test_missclassification_error=c()
  train_missclassification_error=c()#(in percentage)
  accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
  for (i in neighbors) {
    prd <- knn(train=train_data[,c("V1","V2")],test=test_data[,c("V1","V2")],cl=train_data[,c("Y")],k=i)
    tab <- table(prd,test_data[,"Y"])
    test_missclassification_error=append(test_missclassification_error,c(100-accuracy(tab)))
    prd <- knn(train=train_data[,c("V1","V2")],test=train_data[,c("V1","V2")],cl=train_data[,c("Y")],k=i)
    tab <- table(prd,train_data[,"Y"])
    train_missclassification_error=append(train_missclassification_error,c(100-accuracy(tab)))
  }
  
  
  
  #Linear Model
  linear_model=lm(data=train_data,Y~V1+V2)
  summary(linear_model)
  labeling_data <-function(x) {   
    k=ifelse( x> 1.5, 2, 1)
    return(k) }
  
  test_predict <- predict(linear_model, newdata = test_data[,c("V1","V2")]) 
  test_predict=sapply(test_predict, labeling_data)
  tab <- table(test_predict,test_data[,"Y"])
  linear_test_accuracy=accuracy(tab)
  
  train_predict <- predict(linear_model, newdata = train_data[,c("V1","V2")]) 
  train_predict=sapply(train_predict, labeling_data)
  tab <- table(train_predict,train_data[,"Y"])
  linear_train_accuracy=accuracy(tab)
  
  
  #Question 4
  #bayes error
  # Approximating using grid approximation here small grid value is taken as 0.1 squares
  # taking density fucntion from range -4 to +5 (10 values with 10/100=0.1 squares)
  set.seed(500)
  A <- matrix(rep(0,100*100),100)
  B <- matrix(rep(0,100*100),100)
  Range_X <- seq(-4,5,length.out=100)
  Range_Y<- seq(-4,5,length.out=100)
  
  for (i in 1:100) {
    for (j in 1:100) {
      for (k in 1:10)
      {
        A[i,j] = A[i,j]+0.1*dmvnorm(c(Range_X[i],Range_Y[j]),Cluster_means_A[k,],cv/5)
        B[i,j] = B[i,j]+0.1*dmvnorm(c(Range_X[i],Range_Y[j]),Cluster_means_B[k,],cv/5)
      }
    }
  }
  
  # doing the integration to obtain the bayes error rate
  dx=Range_X[2]-Range_X[1]
  dy=Range_Y[2]-Range_Y[1]
  prob=0
  
  for (i in 1:100) {
    for (j in 1:100) {
      prob = prob + 0.5*max(A[i,j],B[i,j])*dx*dy
    }
    
  }
  bayes_error=(1-prob)*100
  #plotting misclassification Error vs Number of Neighbors  
  library(ggplot2)
  ggplot() + geom_line(aes(x=neighbors, y=train_missclassification_error, color="Train_error")) + geom_point(aes(x=neighbors, y=train_missclassification_error))+
    scale_x_continuous(name = "Number of Neighbors", limits = c(0, 70)) +scale_y_continuous(name = "Misclassification Error(%)", limits = c(0, 28))+
    geom_line(aes(x=neighbors, y=test_missclassification_error, color="Test_error"))+ geom_point(aes(x=neighbors, y=test_missclassification_error)) +
    geom_point(aes(x=20, y=100-linear_train_accuracy, color="linear_test_error")) + geom_point(aes(x=20, y=100-linear_test_accuracy, color="linear_train_error"))+
    geom_hline(aes(yintercept=bayes_error  ,colour="Bayes Error"), linetype="dashed")
  
