######################################################################
### Allen Institute Cell Lineage Reconstruction DREAM Challenge
### Renata Retkute, 2020
######################################################################

library(phytools)
library(gbm)
library(ggplot2)

source("train.R")
source("infer.R")

# Thresholds of clustering for each layer in phylogenin tree
thresholds<-c(0.3, 0.1, 0.05, 0.01, 0.005)
n.trees<-100

# File with ground trouth (columns with IDs and nw)
train.set<-"train_setDREAM2019.txt"

# Directory with training data
train.dir<-"train_data"

# Directory with testing data
test.dir<-"test_data"

######################################################################
#  Use fitted model
######################################################################

fit.model<-readRDS("fitted_model_for_DREAM_challenge.rds")
n.trees<-100

######################################################################
#  or train model on data
######################################################################

training_data <- read.table(train.set, header=T, colClasses = "character")

ls.tr<-list.files(train.dir)
n.train<-length(ls.tr)

train<-list("NA", n.train)
for(i in 1:n.train){
  file<-paste0(train.dir,"/sub1_train_",i,".txt")
  tmp<-read.table(file, header=T, colClasses="character")
  train[[i]]<-tmp
}

# Find twin cells
train.tw<-twin.cells(training_data)

# The rest of cells
train.ntw<-not.twin.cells(train.tw, train)

#  All training data
train.all<-rbind(train.tw, train.ntw)

fit.model <- gbm(
  formula = rf ~ .,
  data = train.all,
  distribution = "bernoulli",
  n.trees = n.trees,
  interaction.depth = 1,
  n.minobsinnode = 3,
  cv.folds = 5,
  verbose = FALSE
)  	

######################################################################
#  Plot how good the method predicting training data
######################################################################

pred.tw<-predict(fit.model, train.tw, n.trees=n.trees, type='response')
pred.ntw<-predict(fit.model, train.ntw, n.trees=n.trees, type='response')

pred.tr<-rbind(data.frame(obs="twins", pred=pred.tw), data.frame(obs="not.twins", pred=pred.ntw))
pred.tr$obs<-as.factor(pred.tr$obs)
ggplot(pred.tr, aes(obs, pred)) + geom_boxplot(color="blue") + xlab("Truth") + ylab("Prob. cells are twins")

######################################################################
#  Make predictions 
######################################################################

ls.ts<-list.files(test.dir)
n.test<-length(ls.ts)

ans<- data.frame(dreamID=c(),	 nw=c())
for(i in 1:n.test){
  xx <- read.table(paste0(test.dir, "/sub1_test_",i,".txt"), header=T, colClasses="character")
  lng<-  make.lineage(xx, fit.model, thresholds, n.trees)
  ans<-rbind(ans, data.frame(dreamID=i,	 nw=as.character(lng)))
  plot(read.newick(text = lng), main=paste0('Test: ',i))
}

######################################################################
#  Write results 
######################################################################

write.table(ans, file="Results.txt",sep = "\t", row.names = F)    
