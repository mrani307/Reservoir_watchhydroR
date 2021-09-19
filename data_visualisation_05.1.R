
#'data visualisation : model training and validation

training<-data.frame()
validation<-data.frame()
run<-data.frame()
NSE<-data.frame()

 
for(month in 1:12)
{
  input<-read.csv(paste0("month_wise_dataset_prepared/dataset",month,".csv"))
  output<-read.csv(paste0("month_wise_model_output/output_month",month,".csv"))
  
  if(all(input[,1] == output[,1])==FALSE)
    stop(paste0("Check the files for month",month))
  
  renorm<-function(data)
  {
    m1<-max(data[,3])
    m2<-min(data[,3])
    re_norm<-(m1-m2)*data[,4]+m2
    return(re_norm)
  }
  
  for(i in 1:nrow(output))
  {
    if((format(as.Date(output[i,1]),"%Y")<=2015)==FALSE)
      break
  }
  
  for(j in i:nrow(output))
  {
    if((format(as.Date(output[j,1]),"%Y")>=2016 & format(as.Date(output[j,1]),"%Y")<=2019)==FALSE)
      break
  }
  
  c_name<-c("Dates","RL","RL30","Model_Output","F_RL30")
  
  t<-cbind(input[1:i-1,1:3],output[1:i-1,2])
  t<-cbind(t,renorm(data=t))
  
  v<-cbind(input[i:j-1,1:3],output[i:j-1,2])
  v<-v[2:nrow(v),]
  v<-cbind(v,renorm(data=v))
  
  r<-cbind(input[j:nrow(output),1:3],output[j:nrow(output),2])
  r<-cbind(r,renorm(data=r))
  
  if(nrow(t)+nrow(v)+nrow(r)!=nrow(input))
    stop("Error")
  
  colnames(t)<-c_name
  colnames(v)<-c_name
  colnames(r)<-c_name
  
  NSE[month,1]<-month
  NSE[month,2]<-dynatopmodel::NSE( t[,3],t[,5] , digits=2)
  NSE[month,3]<-dynatopmodel::NSE( v[,3],v[,5] , digits=2)
  NSE[month,4]<-dynatopmodel::NSE( r[,3],r[,5] , digits=2)
  
  
  
  print(paste0("Month : ",month))
  print(paste0("Training   NSE:",dynatopmodel::NSE( t[,3],t[,5] , digits=2)))
  print(paste0("Validation NSE:",dynatopmodel::NSE( v[,3],v[,5] , digits=2)))

  if(month == 1)
  {
    training<-t
    validation<-v
  }
  else
  {
    training<-rbind(training,t)
    validation<-rbind(validation,v)
  }
  
  
}   

colnames(NSE)<-c("month","training","validation","run")
 

training<-training[order(as.Date(training[,1])),]  
validation<-validation[order(as.Date(validation[,1])),]  

train_NSE<-dynatopmodel::NSE( training[,3],training[,5] , digits=2)
validation_NSE<-dynatopmodel::NSE( validation[,3],validation[,5] , digits=2)

print(paste0("Training   NSE:",dynatopmodel::NSE( training[,3],training[,5] , digits=3)))
print(paste0("Validation NSE:",dynatopmodel::NSE( validation[,3],validation[,5] , digits=3)))

plot(as.Date(training[,1]),training[,5],type = "l",col="red", ylab = "RL [in metres]", xlab = "Date", ylim = c(730,755))
lines(as.Date(training[,1]),training[,3],col="blue")
title(paste0("Training:2001-2015    NSE:",train_NSE))
legend(x = "bottomright", 
       lty = c(1,1),
       col = c("blue","red"),
       title="Legend",
       title.col = "black",
       text.col = c("blue","red"),
       legend=c("Observed", "Forecasted"))

plot(as.Date(validation[,1]),validation[,5],type = "l",col="red", ylab = "RL [in metres]", xlab = "Date")
lines(as.Date(validation[,1]),validation[,3],col="blue")
title(paste0("Validation:2016-2019  NSE:",validation_NSE))
legend(x="topleft",
       lty = c(1,1),
       col = c("blue","red"),
       text.font = 1,
       title="Legend",
       title.col = "black",
       text.col = c("blue","red"),
       legend=c("Observed", "Forecasted"))

 



