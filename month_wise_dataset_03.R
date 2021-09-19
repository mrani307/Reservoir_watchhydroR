dir.create(file.path(getwd(), "month_wise_dataset_prepared"), showWarnings = FALSE)

twovariable_month_wise_dataset<-function(month)
{
    data<-read.csv(paste0("month_wise_data/Month",month,".csv"))
    d<-dplyr::select(data,-c(Dates))
    correl<-cor(d)
    correl<-as.data.frame(correl)
    correl_threshold<-max(correl[3:nrow(correl),2])
    #write.csv(correl,paste0("month_wise_dataset_prepared/correlation_values_month",month,".csv"))
    
    #checking  and saving the best correlation months
    c_month<-data.frame()
    c<-3
    for(row in 2:nrow(correl))
    {
      if(correl[row,2]==correl_threshold)
      {
        c_month[c,1]<-colnames(correl)[row]
        c_month[c,2]<-correl[row,2]
        c<-c+1
      }
    }
    c_month[1,1]<-colnames(correl)[1]
    c_month[1,2]<-correl[1,2]
    c_month[2,1]<-colnames(correl)[2]
    c_month[2,2]<-1
    write.csv(c_month,paste0("month_wise_dataset_prepared/corelleated_variables_month",month,".csv"),row.names = FALSE)
    
    #preparing the dataset for each month
    month_model_input<-dplyr::select(data,c_month$V1)
    month_model_input<-cbind(data$Dates,month_model_input)    
    write.csv(month_model_input,paste0("month_wise_dataset_prepared/dataset",month,".csv"),row.names = FALSE)
  
}

multivariable_month_wise_dataset<-function(month,correl_threshold)
{
  
  data<-read.csv(paste0("month_wise_data/Month",month,".csv"))
  d<-dplyr::select(data,-c(Dates))
  correl<-cor(d)
  correl<-as.data.frame(correl)
  #write.csv(correl,paste0("month_wise_dataset_prepared/correlation_values_month",month,".csv"))
  
  #checking  and saving the best correlation months
  c_month<-data.frame()
  c<-2
  for(row in 2:nrow(correl))
  {
    if(correl[row,2]>correl_threshold)
    {
      c_month[c,1]<-colnames(correl)[row]
      c_month[c,2]<-correl[row,2]
      c<-c+1
    }
  }
  c_month[1,1]<-colnames(correl)[1]
  c_month[1,2]<-correl[1,2]
  write.csv(c_month,paste0("month_wise_dataset_prepared/corelleated_variables_month",month,".csv"),row.names = FALSE)
  
  #preparing the dataset for each month
  month_model_input<-dplyr::select(data,c_month$V1)
  month_model_input<-cbind(data$Dates,month_model_input)    
  write.csv(month_model_input,paste0("month_wise_dataset_prepared/dataset",month,".csv"),row.names = FALSE)
}


      #prepare dataset for model input
      for(month in 1:12)
      {
        twovariable_month_wise_dataset(month = month) 
      }
      

       
