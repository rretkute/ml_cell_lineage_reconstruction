# Using Machine Learning (ML) for cell lineage reconstruction
Renata Retkute (rr614@cam.ac.uk), Alidivinas Prusokas and Augustinas Prusokas

This is team AMbeRland submission to Allen Institute Cell Lineage Reconstruction DREAM Challenge https://www.synapse.org/#!Synapse:syn20692755/wiki/

## Methods
We used  Generalized Boosted Regression (GBR)  model to predict the probabilities that two cells are sisters, and custom clustering algorithm, to partition cells into lineages based on these probabilities.  

As a predictors, we have chosen the following features based on pairwise comparison of each recording array position:
- number of recorded units which have not mutated;
- number of recorded units which have  the same mutation;
- number of recorded units which have  single mutation;
- number of recorded units which have different mutations. 

The status  was set to 1 if two cells were sister cells and to 0 if observed two cells were not sister cells. We included only those pairs with status 0, that have not already been included with status 1.  We assumed only binary trees.

Figure 1. AMbeRland approach for learning from the DREAM Challenge training data. .
![](AMbeRland_Method.png)

## Details
All calculations were performed in R using package gbm https://cran.r-project.org/web/packages/gbm/index.html.
Following options were used to train Generalized Boosted Regression model:
- distribution = "bernoulli",
-  n.trees = 100,
-  interaction.depth = 1,
-  n.minobsinnode = 3,
-  cv.folds = 5.

## Usage
To reproduce AMbeRland's submission for Subchallenge 1:

    Rscript run.R

By default, the code will use fitted model. To fit the model based on training data, change:

      use.trained.model<-FALSE
    
To write results into spreadsheet, change:

      write.results.to.file<-TRUE

    
