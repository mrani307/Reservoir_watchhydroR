dir.create(file.path(getwd(), "month_wise_model_output"), showWarnings = FALSE)

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
model_run<-function(month,c,epoch,threshold,is.check)
{
        data<-read.csv(paste0("month_wise_dataset_prepared/dataset",month,".csv"))
        
        if((any(colnames(data)=="RL"))==TRUE)
        {
          if(length(colnames(data))<=3)
            stop("Check the inputs")
        }
        else
          stop("Check the inputs")
          
        t<-split_data(data = data,start_year = 2001 ,end_year = 2015, with.dates = F)
        v<-split_data(data = data,start_year = 2016 ,end_year = 2019, with.dates = F)
        r<-split_data(data = data,start_year = 2020 ,end_year = 2020, with.dates = F)
        # t,v,r must not have date column i.e. with.dates=F
        t<-BBmisc::normalize(x= t,method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")
        v<-BBmisc::normalize(x= v,method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")
        r<-BBmisc::normalize(x= r,method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")
        #prepering data for validation and run
        v1<-dplyr::select(v,-RL30)
        r1<-dplyr::select(r,-RL30)
        
        #modelrun
        #c<-c #c(3,2)
        #epoch=50
        rsq <- function (x, y) cor(x, y) ^ 2
        err<-matrix(nrow = epoch, ncol = 5)
        
          for (k in 1:epoch) 
          {
            nn<-neuralnet::neuralnet(formula = RL30 ~ .,
                                     data = t,
                                     hidden = c,
                                     linear.output = FALSE,
                                     threshold=0.01,
                                     stepmax = 1e+07,
                                     err.fct = "sse")
            nn_v<-neuralnet::compute(nn,v1)
            model_run<-neuralnet::compute(nn,r1)
             
            err[k,1]<-dynatopmodel::NSE(nn$net.result[[1]], t$RL30, digits=3)
            err[k,2]<-rsq(nn$net.result[[1]], t$RL30)
            err[k,3]<-dynatopmodel::NSE(nn_v$net.result, v$RL30, digits=3)
            err[k,4]<-rsq(nn_v$net.result, v$RL30)
            err[k,5]<-dynatopmodel::NSE(model_run$net.result,r$RL30, digits = 3)
            
            if(err[k,3]>=threshold)
            {
              print(paste0(k,"-",c,"-",err[k,3]))
              output<-rbind(nn$net.result[[1]],nn_v$net.result,model_run$net.result)
              output<-as.data.frame(output)
              output<-cbind(data$data.Dates,output)
              saveRDS(nn,paste0("month_wise_model_output/model_month",month,".RDS"))
              write.csv(output,paste0("month_wise_model_output/output_month",month,".csv"),row.names = FALSE)
              break
            }
          }
        if(is.check==FALSE)
          print(max(err[,3]))
        else
          return(max(err[,3]))
}

{
month<-7  
  #run month_wise_dataset for monson months
  monsoon_month_wise_dataset(month = month, correl_threshold = 0.55) #0.65
  
        #check for 1 hidden layer
        a<-c()
        for (i in 1:10) 
        {
          a[i]<-model_run(month = month, c=i , epoch =5 , threshold = 0.999, is.check = TRUE)  
        }
        a
    
        #check for two hidden layers
        
        b<-c()
        c<-1
        for (i in 1:10) 
        {
          for(j in 1:10)
          {
            b[c]<-model_run(month = month, c=c(i,j) , epoch = 2, threshold = 0.999, is.check = TRUE)
            print(paste0(i,",",j,"-",b[c]))
            c<-c+1
          }
        }
} #checks

#model_run
model_run(month = 7 , c=1, epoch = 500, threshold = 0.54, is.check = FALSE)
        
 
#'0.993-Jan-(2,2)
#'
#'0.977-Feb-20
#'
#'0.981-Mar-2
#'
#'0.927-Apr-7
#'
#'0.868-May- (10,2)
#'
#' 0.764-june-(10,1)
#' 
#'0.54-july-1
#'
#'0.916-august-(3,5)
#'
#'0.82-sept-6
#'
#'0.891-oct-19
#'
#'0.96-nov-4
#'
#'0.987-dec-c(3,3)
#'
    







 