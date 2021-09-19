  data<-read.csv("Rainfall_acc.csv")                        #must not have dates
  dates<-seq(as.Date("2001-01-31"),as.Date("2020-12-31"),1) #prepare the dates as per the observation
  data<-cbind(dates,data)                                   #add dates
  
  dir.create(file.path(getwd(), "month_wise_data"), showWarnings = FALSE) #folder for storing the monthly data
  
  for(m in 1:12)
  {
    month<-matrix(nrow = 31*21, #no of dates of month*no of years on rec.
                  ncol=ncol(data))
    d<-data.frame()
    r<-1
    for (row in 1:nrow(data)) 
    {
      if(as.numeric(format(data[row,1],"%m"))==m)
      {
        month[r,]<-unlist(data[row,])
        d[r,1]<-as.Date(data[row,1])
        r<-r+1
      }
    }
    colnames(month)<-colnames(data)
    month<-as.data.frame(month) 
    month<-na.omit(month)
    d<-na.omit(d)
    month<-cbind(d,month)
    month<-dplyr::select(month,-dates)  
    colnames(month)[1]<-"Dates"
    write.csv(month,paste0("month_wise_data/Month",m,".csv"),row.names=FALSE)
  }







