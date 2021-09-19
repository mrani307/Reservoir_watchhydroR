   
  split_data<-function(data,start_year,end_year,with.dates)
  {
    month<-matrix(nrow = 31*(end_year-start_year+1), #no of dates of month*no of years on rec.
                  ncol=ncol(data))
    d<-data.frame()
    r<-1
    for (row in 1:nrow(data)) 
    {
      if(as.numeric(format(as.Date(data[row,1]),"%Y"))>= start_year & 
         as.numeric(format(as.Date(data[row,1]),"%Y"))<= end_year)
      {
        month[r,]<-unlist(data[row,])
        r<-r+1
      }
    }
    colnames(month)<-colnames(data)
    month<-as.data.frame(month) 
    month<-na.omit(month)
    if(with.dates==TRUE)
      return(month)
    else{
      month<-(dplyr::select(month,-data.Dates))
      for (i in 1:ncol(month)) {
        month[,i]<-as.numeric(month[,i])
      }
      return(month)
    }
  }
  
  data2020<-data.frame()
  for(month in 1:12)
  {
      nn<-readRDS(paste0("month_wise_model_output/model_month",month,".RDS"))
      data<-read.csv(paste0("month_wise_dataset_prepared/dataset",month,".csv"))
      max_min<-readxl::read_excel(paste0("maxmin/month",month,".xlsx"))
      
       
      r<-split_data(data = data,start_year = 2020 ,end_year = 2020, with.dates = F)
      r<-BBmisc::normalize(x= r,method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")
      r1<-dplyr::select(r,-RL30)
      model_run<-neuralnet::compute(nn,r1)
      err<-dynatopmodel::NSE(model_run$net.result,r$RL30, digits = 3)
      
      renorm<-function(model_run,max_min)
      {
        m1<-max_min$Max[3]
        m2<-max_min$Min[3]
        re_norm<-(m1-m2)*model_run$net.result+m2
        return(re_norm)
      }
      run_input<-split_data(data = data,start_year = 2020 ,end_year = 2020, with.dates = T)
      run_output<-renorm(model_run = model_run, max_min = max_min)  
      run_output<-cbind(run_input[,1],run_input[,3],run_output)
      run_output<-as.data.frame(run_output)
      run_output[,1]<-as.Date(run_output[,1])
      run_output[,2]<-as.numeric(run_output[,2])
      run_output[,3]<-as.numeric(run_output[,3])
      err_denorm<-dynatopmodel::NSE(run_output[,3],run_output[,2], digits = 2)
      write.csv(run_output,paste0("month_wise_model_output/ModelRun",month,"_",err_denorm,".csv"))
      if(month == 1)
      {
         data2020<-run_output
      }
      else
      { 
        data2020<-rbind(data2020,run_output)
      }
       
  }
  
  run_err<-dynatopmodel::NSE(data2020[,3],data2020[,2], digits = 2)
  write.csv(data2020,paste0("month_wise_model_output/ModelRun2020_",run_err,".csv"))
  plot(data2020[,1],data2020[,3],col="red",type = "l", ylab = "RL [in metres]", xlab = "Date")  
  lines(data2020[,1],data2020[,2],col="blue")
  title(paste0("Model Run: 2020   NSE :",run_err))
  legend(x = "bottomright", 
         lty = c(1,1),
         col = c("blue","red"),
         title="Legend",
         title.col = "black",
         text.col = c("blue","red"),
         legend=c("Observed", "Forecasted"))
  