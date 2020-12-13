######################################################################
### Allen Institute Cell Lineage Reconstruction DREAM Challenge
### Renata Retkute, 2020
######################################################################

library(phytools)

library(gbm)
library(ggplot2)
library(treeman)
require(ggplot2)
theme_set(theme_bw())
library(RColorBrewer)
require(gridExtra)
library(cowplot)
library(ggpubr)

source("train.R")
source("thresholds_func.R")


# Thresholds of clustering for each layer in phylogenin tree
thresholds<-c(0.3, 0.1, 0.05, 0.01, 0.005)
n.trees<-100

# File with ground trouth (columns with IDs and nw)
train.set<-"train_setDREAM2019.txt"
training_data <- read.table(train.set, header=T, colClasses = "character")

# Directory with training data
train.dir<-"train_data"

# Directory with testing data
test.dir<-"test_data"

######################################################################
#  Fitted model
######################################################################

fit.model<-readRDS("fitted_model_for_DREAM_challenge.rds")
n.trees<-100



ls.tr<-list.files(train.dir)
n.train<-length(ls.tr)

thresholds<-c(0.3, 0.1, 0.05, 0.01, 0.005)
i<-1
  xx <- read.table(paste0(train.dir, "/sub1_train_",i,".txt"), header=T, colClasses="character")
  lng<-  make.lineage(xx, fit.model, thresholds, n.trees)
observed<-training_data$ground[i]
  
par(mfrow = c(2, 1)) 
plot(read.newick(text = lng), main=paste0('Test: ',i))
plot(read.newick(text = observed))

tree_1<-readTree(text=paste0(observed))
tree_2<-readTree(text = lng)
score<- calcDstRF(tree_1, tree_2, nrmlsd = T)
triplets<-calcDstTrp(tree_1, tree_2, nrmlsd = T, parallel = F, progress="text")
c(score, triplets)

ans<-data.frame(set=c(), score=c(), triplets=c())
for(i in 1:n.train){
  xx <- read.table(paste0(train.dir, "/sub1_train_",i,".txt"), header=T, colClasses="character")
  lng<-  make.lineage(xx, fit.model, thresholds, n.trees)
  observed<-training_data$ground[i]
  par(mfrow = c(2, 1)) 
  plot(read.newick(text = lng), main=paste0('Test: ',i))
  plot(read.newick(text = observed))
  
  tree_1<-readTree(text=paste0(observed))
  tree_2<-readTree(text = lng)
  score<- calcDstRF(tree_1, tree_2, nrmlsd = T)
  if(is.na(score)) score<-1
  triplets<-calcDstTrp(tree_1, tree_2, nrmlsd = T, parallel = F, progress="text")
  if(is.na(triplets)) triplets<-1
  cat(c(i, "", score, "", triplets, "\n"))
  ans<-rbind(ans, data.frame( set=i, score=score, triplets=triplets))
  
}

plot(ans$score, ans$triplets, xlab='Robinson-Foulds distance', ylab='Triplet distance')
c(mean(ans$score), mean(ans$triplets))
