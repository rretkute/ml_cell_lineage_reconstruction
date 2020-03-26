
twin.cells<- function(training_data){
  train.tw<-data.frame(not.mut=c(), same=c(), one.mut=c(), both.mut=c(), rf=c())
  for(ii in 1:nrow(training_data)){
    xx = as.character(training_data[ii,]$ground)
    x1<-strsplit(xx,"")[[1]]
    x2<-match(x1,c("_"))
    x2[is.na(x2)]<-0
    for(i in 1:(length(x2)-18)){
      if(x2[i]==1 & x2[i+15]==1) {
        t1<-x1[(i+1):(i+10)]
        t2<-x1[(i+16):(i+26)]
        cnt<-c(0,0,0,0)
        for(j in 1:10){
          if(t1[j]==0 & t2[j]==0) cnt[1]<-cnt[1]+1
          if(t1[j]==1 & t2[j]==1) cnt[2]<-cnt[2]+1
          if(t1[j]==2 & t2[j]==2) cnt[2]<-cnt[2]+1
          if(t1[j]==0 & t2[j]==1) cnt[3]<-cnt[3]+1
          if(t1[j]==1 & t2[j]==0) cnt[3]<-cnt[3]+1
          if(t1[j]==0 & t2[j]==2) cnt[3]<-cnt[3]+1
          if(t1[j]==2 & t2[j]==0) cnt[3]<-cnt[3]+1
          if(t1[j]==1 & t2[j]==2) cnt[4]<-cnt[4]+1
          if(t1[j]==2 & t2[j]==1) cnt[4]<-cnt[4]+1
        }
        train.tw<-rbind(train.tw, data.frame(not.mut=cnt[1], same=cnt[2], one.mut=cnt[3], both.mut=cnt[4], rf=1))
      }
      if(x2[i]==1 & x2[i+16]==1) {
        t1<-x1[(i+1):(i+10)]
        t2<-x1[(i+17):(i+27)]
        cnt<-c(0,0,0,0)
        for(j in 1:10){
          if(t1[j]==0 & t2[j]==0) cnt[1]<-cnt[1]+1
          if(t1[j]==1 & t2[j]==1) cnt[2]<-cnt[2]+1
          if(t1[j]==2 & t2[j]==2) cnt[2]<-cnt[2]+1
          if(t1[j]==0 & t2[j]==1) cnt[3]<-cnt[3]+1
          if(t1[j]==1 & t2[j]==0) cnt[3]<-cnt[3]+1
          if(t1[j]==0 & t2[j]==2) cnt[3]<-cnt[3]+1
          if(t1[j]==2 & t2[j]==0) cnt[3]<-cnt[3]+1
          if(t1[j]==1 & t2[j]==2) cnt[4]<-cnt[4]+1
          if(t1[j]==2 & t2[j]==1) cnt[4]<-cnt[4]+1
        }
        train.tw<-rbind(train.tw, data.frame(not.mut=cnt[1], same=cnt[2], one.mut=cnt[3], both.mut=cnt[4], rf=1))
      }
      if(x2[i]==1 & x2[i+17]==1) {
        t1<-x1[(i+1):(i+10)]
        t2<-x1[(i+18):(i+28)]
        cnt<-c(0,0,0,0)
        for(j in 1:10){
          if(t1[j]==0 & t2[j]==0) cnt[1]<-cnt[1]+1
          if(t1[j]==1 & t2[j]==1) cnt[2]<-cnt[2]+1
          if(t1[j]==2 & t2[j]==2) cnt[2]<-cnt[2]+1
          if(t1[j]==0 & t2[j]==1) cnt[3]<-cnt[3]+1
          if(t1[j]==1 & t2[j]==0) cnt[3]<-cnt[3]+1
          if(t1[j]==0 & t2[j]==2) cnt[3]<-cnt[3]+1
          if(t1[j]==2 & t2[j]==0) cnt[3]<-cnt[3]+1
          if(t1[j]==1 & t2[j]==2) cnt[4]<-cnt[4]+1
          if(t1[j]==2 & t2[j]==1) cnt[4]<-cnt[4]+1
        }
        train.tw<-rbind(train.tw, data.frame(not.mut=cnt[1], same=cnt[2], one.mut=cnt[3], both.mut=cnt[4], rf=1))
      }
    }
  }
return(train.tw)
}

not.twin.cells<-function(train.tw, train){
  uniq.train.tw<-unique(train.tw)
  train.ntw<-data.frame(not.mut=c(), same=c(), one.mut=c(), both.mut=c(), rf=c())
  for(ii in 1:length(train)){
    xx<-train[[ii]]
    for(j1 in 1:nrow(xx)){
      if(j1<nrow(xx)){
        for(j2 in (j1+1):nrow(xx)){
          cnt<-c(0,0,0,0)
          t1<-strsplit(xx[j1,2],"")[[1]]
          t2<-strsplit(xx[j2,2],"")[[1]]
          for(j in 1:10){
            if(t1[j]==0 & t2[j]==0) cnt[1]<-cnt[1]+1
            if(t1[j]==1 & t2[j]==1) cnt[2]<-cnt[2]+1
            if(t1[j]==2 & t2[j]==2) cnt[2]<-cnt[2]+1
            if(t1[j]==0 & t2[j]==1) cnt[3]<-cnt[3]+1
            if(t1[j]==1 & t2[j]==0) cnt[3]<-cnt[3]+1
            if(t1[j]==0 & t2[j]==2) cnt[3]<-cnt[3]+1
            if(t1[j]==2 & t2[j]==0) cnt[3]<-cnt[3]+1
            if(t1[j]==1 & t2[j]==2) cnt[4]<-cnt[4]+1
            if(t1[j]==2 & t2[j]==1) cnt[4]<-cnt[4]+1
          }
          ac.tw<-T
          for(j in 1:nrow(uniq.train.tw)){
            if(uniq.train.tw[j,1]==cnt[1] & uniq.train.tw[j,2]==cnt[2] & uniq.train.tw[j,3]==cnt[3] & uniq.train.tw[j,4]==cnt[4]) ac.tw<-F
          }
          if(ac.tw) {
            train.ntw<-rbind(train.ntw, data.frame(not.mut=cnt[1], same=cnt[2], one.mut=cnt[3], both.mut=cnt[4], rf=0))
          }
        }
      }
    }
  }
  return(train.ntw)
}
