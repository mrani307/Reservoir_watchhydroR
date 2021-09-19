rain<-read.csv("imd_rain.csv") #1st column is date

d<-matrix(nrow=nrow(rain), ncol=408)
for (row in 367:nrow(rain)) 
{ 
  c<-1
  for (s in 2:ncol(rain)) 
  {
    
    for (m in 1:24) 
    {
      d[row,c]<-sum(rain[(row-m*0.5*30+1):(row), s] )
      c<-c+1
      
    }
    
  }
}

cname<-c()
c<-1
for (i in 1:(ncol(rain)-1))
{
  for (m in 1:24) 
  {
    cname[c]<-paste0("S",i,"M",m*0.5)
    c<-c+1
  }
}

colnames(d)<-cname
d<-na.omit(d)
write.csv(d,"Rainfall_acc.csv")
