dir.create(file.path(getwd(), "max_min_for_denormalisation"), showWarnings = FALSE)
#non-monsoon months
 for(month in 1:12)
 {
    c<-0
     
    max_min<-matrix(nrow = 20, ncol = 2) 
    colnames(max_min)<-c("Maximum","Minimum")
    n_data<-read.csv(paste0("month_wise_data/Month",month,".csv"))
    n_data$Dates<-as.Date(n_data$Dates)
    
          year_data_sum<-matrix(ncol = ncol(n_data), nrow = 20)
          year_data_avg<-matrix(ncol = ncol(n_data), nrow = 20)
          year_data_min<-matrix(ncol = ncol(n_data), nrow = 20)
          year_data_max<-matrix(ncol = ncol(n_data), nrow = 20)
          RL<-matrix(ncol = 2,nrow = 20)
          RL30<-matrix(ncol = 2,nrow = 20)
          colnames(RL30)<-c("Maximum","Minimum")
          for(year in 2001:2020)
          {
          
            y_data<-matrix(ncol=ncol(n_data),nrow = 31*20)
            y_data<-as.data.frame(y_data)
            date<-data.frame()
            r<-1
            for(i in 1:nrow(n_data))
            {
              if(as.numeric(format(n_data[i,1],"%Y"))==year)
              {
                y_data[r,]<-n_data[i,]
                date[r,1]<-n_data[i,1]
                r<-r+1
              }
            }
            y_data<-na.omit(y_data)
            y_data<-y_data[,-1]
            y_data<-cbind(date,y_data)
            
            s_data<-c()
            for(i in 4:ncol(n_data))
              s_data[i]<-sum(y_data[,i])
            
            a_data<-c()
            for(i in 4:ncol(n_data))
              a_data[i]<-mean(y_data[,i])
            
            max_data<-c()
            for(i in 4:ncol(n_data))
              max_data[i]<-max(y_data[,i])
            
            min_data<-c()
            for(i in 4:ncol(n_data))
              min_data[i]<-min(y_data[,i])
             
            RL[(year-2000),1]<-max(y_data[,2]) 
            RL[(year-2000),2]<-min(y_data[,2]) 
            RL30[(year-2000),1]<-max(y_data[,3]) 
            RL30[(year-2000),2]<-min(y_data[,3]) 
            year_data_sum[(year-2000),]<-s_data
            year_data_avg[(year-2000),]<-a_data
            year_data_min[(year-2000),]<-min_data
            year_data_max[(year-2000),]<-max_data
          }
          
      try_max<-matrix(ncol = ncol(n_data), nrow = 20)
      try_min<-matrix(ncol = ncol(n_data), nrow = 20)
      try_min_sum<-c()
      try_max_sum<-c()
        for(st in 4:ncol(n_data))
        {
          try_max[,st]<-abs((RL[1:20,1]-year_data_max[1:20,st]/year_data_sum[1:20,st]*nrow(date)+c)-RL30[1:20,1])
          try_min[,st]<-abs((RL[1:20,2]-year_data_max[1:20,st]/year_data_sum[1:20,st]*nrow(date)+c)-RL30[1:20,2])
          try_max_sum[st]<-sum(try_max[,st])
          try_min_sum[st]<-sum(try_min[,st])
        }
      
      print(month)
      #print(try_max[,which.min(try_max[20,])])
      #print(try_min[,which.min(try_min[20,])])
      
      #print(min(try_max_sum,na.rm = TRUE))
      #print(min(try_min_sum,na.rm = TRUE))
      

      #print(try_max[,which.min(try_max_sum)])
      #print(try_min[,which.min(try_min_sum)])
      
      #max_min[,1]<-(RL[1:20,1]-year_data_max[1:20,which.min(try_max_sum)]/year_data_sum[1:20,which.min(try_max_sum)]*nrow(date)+c)
      #max_min[,2]<-(RL[1:20,2]-year_data_max[1:20,which.min(try_min_sum)]/year_data_sum[1:20,which.min(try_min_sum)]*nrow(date)+c)
      
      #write.csv(max_min,paste0("max_min_for_denormalisation/Month",month,".csv"))
      #print("-----------------------------------------------------------------")
      #print("-----------------------------------------------------------------")
      write.csv(RL30,paste0("max_min_for_denormalisation/RL30_data_Month",month,".csv"))
}
      
 
 #monsoon months
     for(month in 8)
     {
       c_max<-1
       c_min<-1
       pow<-(1/7)
       
       max_min<-matrix(nrow = 20, ncol = 2) 
       colnames(max_min)<-c("Maximum","Minimum")
       n_data<-read.csv(paste0("month_wise_data/Month",month,".csv"))
       n_data$Dates<-as.Date(n_data$Dates)
       
       year_data_sum<-matrix(ncol = ncol(n_data), nrow = 20)
       year_data_avg<-matrix(ncol = ncol(n_data), nrow = 20)
       year_data_min<-matrix(ncol = ncol(n_data), nrow = 20)
       year_data_max<-matrix(ncol = ncol(n_data), nrow = 20)
       year_data_sd<-matrix(ncol = ncol(n_data), nrow = 20)
       
       RL<-matrix(ncol = 2,nrow = 20)
       RL30<-matrix(ncol = 2,nrow = 20)
       for(year in 2001:2020)
       {
         
         y_data<-matrix(ncol=ncol(n_data),nrow = 31*20)
         y_data<-as.data.frame(y_data)
         date<-data.frame()
         r<-1
         for(i in 1:nrow(n_data))
         {
           if(as.numeric(format(n_data[i,1],"%Y"))==year)
           {
             y_data[r,]<-n_data[i,]
             date[r,1]<-n_data[i,1]
             r<-r+1
           }
         }
         y_data<-na.omit(y_data)
         y_data<-y_data[,-1]
         y_data<-cbind(date,y_data)
         
         s_data<-c()
         for(i in 4:ncol(n_data))
           s_data[i]<-sum(y_data[,i])
         
         a_data<-c()
         for(i in 4:ncol(n_data))
           a_data[i]<-mean(y_data[,i])
         
         max_data<-c()
         for(i in 4:ncol(n_data))
           max_data[i]<-max(y_data[,i])
         
         min_data<-c()
         for(i in 4:ncol(n_data))
           min_data[i]<-min(y_data[,i])
         
         sd_data<-c()
         for(i in 4:ncol(n_data))
           sd_data[i]<-sd(y_data[,i])
         
         RL[(year-2000),1]<-max(y_data[,2]) 
         RL[(year-2000),2]<-min(y_data[,2]) 
         RL30[(year-2000),1]<-max(y_data[,3]) 
         RL30[(year-2000),2]<-min(y_data[,3]) 
         year_data_sum[(year-2000),]<-s_data
         year_data_avg[(year-2000),]<-a_data
         year_data_min[(year-2000),]<-min_data
         year_data_max[(year-2000),]<-max_data
         year_data_sd[(year-2000),]<-sd_data
       }
       
       try_max<-matrix(ncol = ncol(n_data), nrow = 20)
       try_min<-matrix(ncol = ncol(n_data), nrow = 20)
       try_min_sum<-c()
       try_max_sum<-c()
       for(st in 4:ncol(n_data))
       {
         try_max[,st]<-abs((RL[1:20,1]+year_data_sum[1:20,st]/(year_data_max[1:20,st]*nrow(date))+(year_data_sd[1:20,st])^(pow)+c_max)-RL30[1:20,1])
         try_min[,st]<-abs((RL[1:20,2]+year_data_sum[1:20,st]/(year_data_avg[1:20,st]*nrow(date))+(year_data_sd[1:20,st])^(pow)+c_min)-RL30[1:20,2])
         try_max_sum[st]<-sum(try_max[,st])
         try_min_sum[st]<-sum(try_min[,st])
       }
       
       print(month)
       #print(pow)
       #print(c)
       #print(try_max[,which.min(try_max[20,])])
       #print(try_min[,which.min(try_min[20,])])
       
       print(min(try_max_sum,na.rm = TRUE))
       print(try_max[,which.min(try_max_sum)])
       
       print(min(try_min_sum,na.rm = TRUE))
       print(try_min[,which.min(try_min_sum)])
       
       max_min[,1]<-(RL[1:20,1]+year_data_sum[1:20,st]/(year_data_max[1:20,st]*nrow(date))+(year_data_sd[1:20,st])^(pow)+c_max)
       max_min[,2]<-(RL[1:20,2]+year_data_sum[1:20,st]/(year_data_avg[1:20,st]*nrow(date))+(year_data_sd[1:20,st])^(pow)+c_min)
       
       #write.csv(max_min,paste0("max_min_for_denormalisation/Month",month,".csv"))
       #print("-----------------------------------------------------------------")
       #print("-----------------------------------------------------------------")
     }
   
 