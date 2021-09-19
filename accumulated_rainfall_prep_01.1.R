      #load raster package
      library(raster)
      library(tidyr)
      library(sf)
      library(dplyr)
      
      #read netcdf as a raster brick
      setwd("C:/Users/merul/Desktop/Hackathon/imd_data/rain")
      start_year<-2000
      end_year<-2020
      rec <- as.Date(paste0(start_year,"-01-01"))
      mat<-matrix()
      
      for (i in start_year:end_year)
      {
        N=365
        if(i%%4==0)
        {
          N=366
        }
        nc.brick<- brick(paste0("_Clim_Pred_LRF_New_RF25_IMD0p25",toString(i),".nc"))#use paste0
        #show dimensions of the raster brick
        #dim(nc.brick)
        nc<- as.data.frame(nc.brick[[1]], xy=T)
        nc<-subset(nc,select(nc,matches(colnames(nc)[3]))!="-999")
        m<-select(nc,matches(colnames(nc)[3]))
        colnames(m)[1]<-format.Date(rec)
        rec<-rec+1
        for (j in 2:N) 
        {
          nc <- as.data.frame(nc.brick[[j]], xy=T)
          nc<-subset(nc,select(nc,matches(colnames(nc)[3]))!="-999")
          am<-as.data.frame(select(nc,matches(colnames(nc)[3])))
          m[j]<-am
          colnames(m)[j]<-format.Date(rec)
          rec<-rec+1
        }
        mat<-cbind(mat,m)
      }
      ncF<-as.data.frame(mat)
      ncF <- subset(ncF, select = -mat)
      ncF<-cbind(nc$x,nc$y,ncF)
      ncF<-subset(ncF,
                  (select(ncF,colnames(ncF)[1])== 75.5   &  select(ncF, colnames(ncF)[2])== 12.25) 
                  |(select(ncF,colnames(ncF)[1])== 76      &  select(ncF, colnames(ncF)[2])== 12.25)
                  |(select(ncF,colnames(ncF)[1])== 75.5      &  select(ncF, colnames(ncF)[2])== 12.5)
                  |(select(ncF,colnames(ncF)[1])== 75.75      &  select(ncF, colnames(ncF)[2])== 12.25)
                  |(select(ncF,colnames(ncF)[1])== 76      &  select(ncF, colnames(ncF)[2])== 12)
                  |(select(ncF,colnames(ncF)[1])== 76.25      &  select(ncF, colnames(ncF)[2])== 12.25)
                  |(select(ncF,colnames(ncF)[1])== 75.75      &  select(ncF, colnames(ncF)[2])== 12.5)
                  |(select(ncF,colnames(ncF)[1])== 76      &  select(ncF, colnames(ncF)[2])== 13)
                  |(select(ncF,colnames(ncF)[1])== 76      &  select(ncF, colnames(ncF)[2])== 12.5)
                  |(select(ncF,colnames(ncF)[1])== 76.25      &  select(ncF, colnames(ncF)[2])== 12.5)
                  |(select(ncF,colnames(ncF)[1])== 76.5      &  select(ncF, colnames(ncF)[2])== 12.75)
                  |(select(ncF,colnames(ncF)[1])== 76.25      &  select(ncF, colnames(ncF)[2])== 12.75)
                  |(select(ncF,colnames(ncF)[1])== 76.5      &  select(ncF, colnames(ncF)[2])== 12.5)
                  |(select(ncF,colnames(ncF)[1])== 76.5      &  select(ncF, colnames(ncF)[2])== 12.25)
                  |(select(ncF,colnames(ncF)[1])== 76      &  select(ncF, colnames(ncF)[2])== 12.75)
                  |(select(ncF,colnames(ncF)[1])== 76.5      &  select(ncF, colnames(ncF)[2])== 13)
                  |(select(ncF,colnames(ncF)[1])== 76.25      &  select(ncF, colnames(ncF)[2])== 13))
      colnames(ncF)[1]<-("Latitude")
      colnames(ncF)[2]<-("Longitude")
      #write the data frame as a csv
      t_ncF<-as.data.frame(t(ncF))
      write.csv(t_ncF,"imd_rain.csv", row.names = F) #,toString(i),".csv"))
      
       
      
       