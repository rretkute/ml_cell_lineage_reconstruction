

get.features<-function(t1, t2){
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
  return(cnt)
}


make.lineage<-function(xx, gbm.fit, thrs, n.trees){
 
  test.tw<-data.frame(same=c(), not.mut=c(), one.mut=c(), both.mut=c())
  ids<-data.frame(no=c(), cell1=c(), cell2=c())
  
  cells<-sapply(1:nrow(xx), function(a) paste0(xx[a,1],"_", xx[a,2]))
  for(j1 in 1:nrow(xx)){
    if(j1<nrow(xx)){
      for(j2 in (j1+1):nrow(xx)){
        t1<-strsplit(xx[j1,2],"")[[1]]
        t2<-strsplit(xx[j2,2],"")[[1]]
        cnt<-get.features(t1, t2)
        test.tw<-rbind(test.tw, data.frame(not.mut=cnt[1], same=cnt[2], one.mut=cnt[3], both.mut=cnt[4]))
        ids<-rbind(ids, data.frame(no=nrow(test.tw),  cell1=j1, cell2=j2))
        test.tw<-rbind(test.tw, data.frame(not.mut=cnt[1], same=cnt[2], one.mut=cnt[3], both.mut=cnt[4]))
        ids<-rbind(ids, data.frame(no=nrow(test.tw), cell1=j2, cell2=j1))
      }
    }
  }
  
  pred<-predict(gbm.fit, test.tw, n.trees=n.trees, type='response')
  xy<-cbind(ids, pred)
  xy<-xy[order(-xy$pred),]
  

    GRP<-list()
  SNGL<-list()
  LEVELS<-list()
  RECORD<-list()
  CELLS<-list()
  
  grp<-data.frame(cell1=c(), cell2=c())
  levels<-list()
  for(i in 1:nrow(xy)){
    if(xy$pred[i]>thrs[1]){  # # # # # # # # # #
      if(nrow(grp)==0) {grp<-rbind(grp, data.frame(cell1=xy$cell1[i], cell2=xy$cell2[i]))
      levels[[1]]<-c(xy$cell1[i], xy$cell2[i])
      } else if((!(xy$cell1[i] %in% grp[,1])) & (!(xy$cell1[i] %in% grp[,2])) & (!(xy$cell2[i] %in% grp[,1])) & (!(xy$cell2[i] %in% grp[,2]))) {
        grp<-rbind(grp, data.frame(cell1=xy$cell1[i], cell2=xy$cell2[i]))
        levels[[length(levels)+1]]<-c(xy$cell1[i], xy$cell2[i])
      }
    }
  }
  
  
  sngl<-c()
  for(i in 1:length(cells)){
    if(nrow(grp)>0){
      if(!(i %in% c(grp[,1],grp[,2]))){
        sngl<-c(sngl, i)
        levels[[length(levels)+1]]<-i
      }
    } else
    {
      sngl<-c(sngl, i)
      levels[[length(levels)+1]]<-i
    }
  }
  GRP[[1]]<-grp
  SNGL[[1]]<-sngl
  LEVELS[[1]]<-levels
  CELLS[[1]]<-LEVELS[[1]]
  
  
  record<-list(rep(NA, length(levels)))
  if(length(levels)>0){
    for(i in 1:length(levels)){
      tmp<-levels[[i]]
      if(length(tmp)==2){
        record[[i]]<-paste0('(', as.character(cells[tmp[1]]),',', as.character(cells[tmp[2]]),')')
      } else {
        record[[i]]<-paste0('(', as.character(cells[tmp]),',)')
      }
    }
  }
  RECORD[[1]]<-record
  
  if(length(record)>2 & nrow(grp)>0){
    
    xy.2<-data.frame(cell1=c(), cell2=c(), pred=c())
    for(i in 1:length(levels)){
      t1<-levels[[i]]
      for(j in 1:length(levels)){
        if(!(i==j)){
          t2<- levels[[j]]
          pr<-c()
          for(i1 in 1:length(t1)){
            for(j1 in 1:length(t2)){
              wh<-which(t1[i1]==xy$cell1 & t2[j1]==xy$cell2)
              pr<-c(pr, xy$pred[wh])
            }
          }
          xy.2<-rbind(xy.2, data.frame(cell1=i, cell2=j, pred=max(pr)))
        }
      }
    }
    xy.2<-xy.2[order(-xy.2$pred),]
    
    grp<-data.frame(cell1=c(), cell2=c())
    levels<-list()
    ind.cells<-list()
    for(i in 1:nrow(xy.2)){
      if(xy.2$pred[i]>thrs[2]){  # # # # # # 
        if(nrow(grp)==0) {grp<-rbind(grp, data.frame(cell1=xy.2$cell1[i], cell2=xy.2$cell2[i]))
        levels[[1]]<-c(xy.2$cell1[i], xy.2$cell2[i])
        ind.cells[[1]]<-c(CELLS[[1]][[xy.2$cell1[i]]], CELLS[[1]][[xy.2$cell2[i]]])
        } else if((!(xy.2$cell1[i] %in% grp[,1])) & (!(xy.2$cell1[i] %in% grp[,2])) & (!(xy.2$cell2[i] %in% grp[,1])) & (!(xy.2$cell2[i] %in% grp[,2]))) {
          grp<-rbind(grp, data.frame(cell1=xy.2$cell1[i], cell2=xy.2$cell2[i]))
          levels[[length(levels)+1]]<-c(xy.2$cell1[i], xy.2$cell2[i])
          ind.cells[[length(ind.cells)+1]]<-c(CELLS[[1]][[xy.2$cell1[i]]], CELLS[[1]][[xy.2$cell2[i]]])
        }
      }
    }
    
    sngl<-c()
    for(i in 1:length(LEVELS[[1]])){
      if(nrow(grp)>0){
        if(!(i %in% c(grp[,1], grp[,2]))){
          sngl<-c(sngl, i)
          levels[[length(levels)+1]]<-i
          ind.cells[[length(ind.cells)+1]]<-c(CELLS[[1]][[i]])
        }
      } else {
        sngl<-c(sngl, i)
        levels[[length(levels)+1]]<-i
        ind.cells[[length(ind.cells)+1]]<-c(CELLS[[1]][[i]])
      }
    }
    
    GRP[[2]]<-grp
    SNGL[[2]]<-sngl
    LEVELS[[2]]<-levels
    CELLS[[2]]<-ind.cells
    if(nrow(grp)>0){
      rr<-RECORD[[1]]
      record<-list(rep(NA, length(levels)))
      if(length(levels)>0){
        for(i in 1:length(levels)){
          tmp<-levels[[i]]
          if(length(tmp)==2){
            record[[i]]<-paste0('(', as.character(rr[[tmp[1]]]),',', as.character(rr[[tmp[2]]]),')')
          } else {
            record[[i]]<-paste0('(', as.character(rr[[tmp]]),',)')
          }
        }
      }
      RECORD[[2]]<-record
    }
    
    if(length(record)>2 & nrow(grp)>0){
      xy.2<-data.frame(cell1=c(), cell2=c(), pred=c())
      for(i in 1:length(ind.cells)){
        t1<-ind.cells[[i]]
        for(j in 1:length(ind.cells)){
          if(!(i==j)){
            t2<- ind.cells[[j]]
            pr<-c()
            for(i1 in 1:length(t1)){
              for(j1 in 1:length(t2)){
                wh<-which(t1[i1]==xy$cell1 & t2[j1]==xy$cell2)
                pr<-c(pr, xy$pred[wh])
              }
            }
            xy.2<-rbind(xy.2, data.frame(cell1=i, cell2=j, pred=max(pr)))
          }
        }
      }
      xy.2<-xy.2[order(-xy.2$pred),]
      
      grp<-data.frame(cell1=c(), cell2=c())
      levels<-list()
      ind.cells<-list()
      for(i in 1:nrow(xy.2)){
        if(xy.2$pred[i]>thrs[3]){  # # # # # # # # 
          if(nrow(grp)==0) {grp<-rbind(grp, data.frame(cell1=xy.2$cell1[i], cell2=xy.2$cell2[i]))
          levels[[1]]<-c(xy.2$cell1[i], xy.2$cell2[i])
          ind.cells[[1]]<-c(CELLS[[2]][[xy.2$cell1[i]]], CELLS[[2]][[xy.2$cell2[i]]])
          } else if((!(xy.2$cell1[i] %in% grp[,1])) & (!(xy.2$cell1[i] %in% grp[,2])) & (!(xy.2$cell2[i] %in% grp[,1])) & (!(xy.2$cell2[i] %in% grp[,2]))) {
            grp<-rbind(grp, data.frame(cell1=xy.2$cell1[i], cell2=xy.2$cell2[i]))
            levels[[length(levels)+1]]<-c(xy.2$cell1[i], xy.2$cell2[i])
            ind.cells[[length(ind.cells)+1]]<-c(CELLS[[2]][[xy.2$cell1[i]]], CELLS[[2]][[xy.2$cell2[i]]])
          }
        }
      }
      
      sngl<-c()
      for(i in 1:length(LEVELS[[2]])){
        if(nrow(grp)>0){
          if(!(i %in% c(grp[,1], grp[,2]))){
            sngl<-c(sngl, i)
            levels[[length(levels)+1]]<-i
            ind.cells[[length(ind.cells)+1]]<-c(CELLS[[2]][[i]])
          } 
        } else {
          sngl<-c(sngl, i)
          levels[[length(levels)+1]]<-i
          ind.cells[[length(ind.cells)+1]]<-c(CELLS[[2]][[i]])
          
        }
      }
      
      GRP[[3]]<-grp
      SNGL[[3]]<-sngl
      LEVELS[[3]]<-levels
      CELLS[[3]]<-ind.cells
      
      if(nrow(grp)>0){
        rr<-RECORD[[2]]
        record<-list(rep(NA, length(levels)))
        if(length(levels)>0){
          for(i in 1:length(levels)){
            tmp<-levels[[i]]
            if(length(tmp)==2){
              record[[i]]<-paste0('(', as.character(rr[[tmp[1]]]),',', as.character(rr[[tmp[2]]]),')')
            } else {
              record[[i]]<-paste0('(', as.character(rr[[tmp]]),',)')
            }
          }
        }
        RECORD[[3]]<-record
      }
      
      if(length(record)>2 & nrow(grp)>0){
        xy.2<-data.frame(cell1=c(), cell2=c(), pred=c())
        for(i in 1:length(ind.cells)){
          t1<-ind.cells[[i]]
          for(j in 1:length(ind.cells)){
            if(!(i==j)){
              t2<- ind.cells[[j]]
              pr<-c()
              for(i1 in 1:length(t1)){
                for(j1 in 1:length(t2)){
                  wh<-which(t1[i1]==xy$cell1 & t2[j1]==xy$cell2)
                  pr<-c(pr, xy$pred[wh])
                }
              }
              xy.2<-rbind(xy.2, data.frame(cell1=i, cell2=j, pred=max(pr)))
            }
          }
        }
        xy.2<-xy.2[order(-xy.2$pred),]
        
        grp<-data.frame(cell1=c(), cell2=c())
        levels<-list()
        ind.cells<-list()
        for(i in 1:nrow(xy.2)){
          if(xy.2$pred[i]>thrs[4]){  # # # # # # 
            if(nrow(grp)==0) {grp<-rbind(grp, data.frame(cell1=xy.2$cell1[i], cell2=xy.2$cell2[i]))
            levels[[1]]<-c(xy.2$cell1[i], xy.2$cell2[i])
            ind.cells[[1]]<-c(CELLS[[3]][[xy.2$cell1[i]]], CELLS[[3]][[xy.2$cell2[i]]])
            } else if((!(xy.2$cell1[i] %in% grp[,1])) & (!(xy.2$cell1[i] %in% grp[,2])) & (!(xy.2$cell2[i] %in% grp[,1])) & (!(xy.2$cell2[i] %in% grp[,2]))) {
              grp<-rbind(grp, data.frame(cell1=xy.2$cell1[i], cell2=xy.2$cell2[i]))
              levels[[length(levels)+1]]<-c(xy.2$cell1[i], xy.2$cell2[i])
              ind.cells[[length(ind.cells)+1]]<-c(CELLS[[3]][[xy.2$cell1[i]]], CELLS[[3]][[xy.2$cell2[i]]])
            }
          }
        }
        
        sngl<-c()
        for(i in 1:length(LEVELS[[3]])){
          if(nrow(grp)>0){
            if(!(i %in% c(grp[,1], grp[,2]))){
              sngl<-c(sngl, i)
              levels[[length(levels)+1]]<-i
              ind.cells[[length(ind.cells)+1]]<-c(CELLS[[3]][[i]])
            } 
          } else {
            sngl<-c(sngl, i)
            levels[[length(levels)+1]]<-i
            ind.cells[[length(ind.cells)+1]]<-c(CELLS[[3]][[i]])
            
          }
        }
        
        GRP[[4]]<-grp
        SNGL[[4]]<-sngl
        LEVELS[[4]]<-levels
        CELLS[[4]]<-ind.cells
        
        if(nrow(grp)>0){
          rr<-RECORD[[3]]
          record<-list(rep(NA, length(levels)))
          if(length(levels)>0){
            for(i in 1:length(levels)){
              tmp<-levels[[i]]
              if(length(tmp)==2){
                record[[i]]<-paste0('(', as.character(rr[[tmp[1]]]),',', as.character(rr[[tmp[2]]]),')')
              } else {
                record[[i]]<-paste0('(', as.character(rr[[tmp]]),',)')
              }
            }
          }
          RECORD[[4]]<-record
        }
        
        
        if(length(record)>2 & nrow(grp)>0){
          xy.2<-data.frame(cell1=c(), cell2=c(), pred=c())
          for(i in 1:length(ind.cells)){
            t1<-ind.cells[[i]]
            for(j in 1:length(ind.cells)){
              if(!(i==j)){
                t2<- ind.cells[[j]]
                pr<-c()
                for(i1 in 1:length(t1)){
                  for(j1 in 1:length(t2)){
                    wh<-which(t1[i1]==xy$cell1 & t2[j1]==xy$cell2)
                    pr<-c(pr, xy$pred[wh])
                  }
                }
                xy.2<-rbind(xy.2, data.frame(cell1=i, cell2=j, pred=max(pr)))
              }
            }
          }
          xy.2<-xy.2[order(-xy.2$pred),]
          
          grp<-data.frame(cell1=c(), cell2=c())
          levels<-list()
          ind.cells<-list()
          for(i in 1:nrow(xy.2)){
            if(xy.2$pred[i]>thrs[5]){  # # # # # # # 
              if(nrow(grp)==0) {grp<-rbind(grp, data.frame(cell1=xy.2$cell1[i], cell2=xy.2$cell2[i]))
              levels[[1]]<-c(xy.2$cell1[i], xy.2$cell2[i])
              ind.cells[[1]]<-c(CELLS[[4]][[xy.2$cell1[i]]], CELLS[[4]][[xy.2$cell2[i]]])
              } else if((!(xy.2$cell1[i] %in% grp[,1])) & (!(xy.2$cell1[i] %in% grp[,2])) & (!(xy.2$cell2[i] %in% grp[,1])) & (!(xy.2$cell2[i] %in% grp[,2]))) {
                grp<-rbind(grp, data.frame(cell1=xy.2$cell1[i], cell2=xy.2$cell2[i]))
                levels[[length(levels)+1]]<-c(xy.2$cell1[i], xy.2$cell2[i])
                ind.cells[[length(ind.cells)+1]]<-c(CELLS[[4]][[xy.2$cell1[i]]], CELLS[[4]][[xy.2$cell2[i]]])
              }
            }
          }
          
          sngl<-c()
          for(i in 1:length(LEVELS[[4]])){
            if(nrow(grp)>0){
              if(!(i %in% c(grp[,1], grp[,2]))){
                sngl<-c(sngl, i)
                levels[[length(levels)+1]]<-i
                ind.cells[[length(ind.cells)+1]]<-c(CELLS[[4]][[i]])
              }
            } else {
              sngl<-c(sngl, i)
              levels[[length(levels)+1]]<-i
              ind.cells[[length(ind.cells)+1]]<-c(CELLS[[4]][[i]])
              
            }
          }
          
          GRP[[5]]<-grp
          SNGL[[5]]<-sngl
          LEVELS[[5]]<-levels
          CELLS[[5]]<-ind.cells
          
          if(nrow(grp)>0){
            rr<-RECORD[[4]]
            record<-list(rep(NA, length(levels)))
            if(length(levels)>0){
              for(i in 1:length(levels)){
                tmp<-levels[[i]]
                if(length(tmp)==2){
                  record[[i]]<-paste0('(', as.character(rr[[tmp[1]]]),',', as.character(rr[[tmp[2]]]),')')
                } else {
                  record[[i]]<-paste0('(', as.character(rr[[tmp]]),',)')
                }
              }
            }
            RECORD[[5]]<-record
          }
        }
        
      }
    } 
  }
  
  lng<-paste0("();")
  if(length(record)==2)	lng<-paste0("(", as.character(record[[1]]),',', as.character(record[[2]]),")", "root;")
  
  if(length(record)==3)	lng<-paste0("(", as.character(record[[1]]),',', as.character(record[[2]]),',', as.character(record[[3]]),")", ";")
  
  if(length(record)==4)	lng<-paste0("(", as.character(record[[1]]),',', as.character(record[[2]]),',', as.character(record[[3]]),',', as.character(record[[4]]),")", ";")
  
  if(length(record)==5)	lng<-paste0("(", as.character(record[[1]]),',', as.character(record[[2]]),',', as.character(record[[3]]),',', as.character(record[[4]]),',', as.character(record[[5]]),")", ";")
  
  if(length(record)==6)	lng<-paste0("(", as.character(record[[1]]),',', as.character(record[[2]]),',', as.character(record[[3]]),',', as.character(record[[4]]),',', as.character(record[[5]]),',', as.character(record[[6]]),")", ";")
  
  if(length(record)==7)	lng<-paste0("(", as.character(record[[1]]),',', as.character(record[[2]]),',', as.character(record[[3]]),',', as.character(record[[4]]),',', as.character(record[[5]]),',', as.character(record[[6]]),',', as.character(record[[7]]),")", ";")
  
  if(length(record)==8)	lng<-paste0("(", as.character(record[[1]]),',', as.character(record[[2]]),',', as.character(record[[3]]),',', as.character(record[[4]]),',', as.character(record[[5]]),',', as.character(record[[6]]),',', as.character(record[[7]]),',', as.character(record[[8]]),")", ";")
  
  if(length(record)==9)	lng<-paste0("(", as.character(record[[1]]),',', as.character(record[[2]]),',', as.character(record[[3]]),',', as.character(record[[4]]),',', as.character(record[[5]]),',', as.character(record[[6]]),',', as.character(record[[7]]),',', as.character(record[[8]]),',', as.character(record[[9]]),")", ";")
  
  if(length(record)==10)	lng<-paste0("(", as.character(record[[1]]),',', as.character(record[[2]]),',', as.character(record[[3]]),',', as.character(record[[4]]),',', as.character(record[[5]]),',', as.character(record[[6]]),',', as.character(record[[7]]),',', as.character(record[[8]]),',', as.character(record[[9]]),',', as.character(record[[10]]),")", "root;")
  return(lng)
}
