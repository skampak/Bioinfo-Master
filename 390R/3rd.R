###  1 ####
#i have already cleaned the file by deleting the proper lines
raw.data <- read.table("GDS2447.soft", sep="\t", header=TRUE)
dim(raw.data)

#The first two columns contain the probe ID and the gene name for each row. Thus, we need to create a file that does not contain these two columns.

data <- raw.data[,-c(1,2)]

## Let's have a look at the head of the file
head(data)
boxplot(data)  ##the data are not normailized therefore we have to use the log scale

data2 <- log2(data)  

##now let's make a boxplot
boxplot(data2, main=expression(paste("Boxplot of the ", log2, " data")))
#Normalization is necessary when we want to perform comparisons between different samples. Thus, we are ready now to proceed with the differential expression analysis, that is to detect the genes that behave differently in different classes of the data.




### 2 ####

#Now lets have a look at the data. In this case we are talking about transcriptional profiling of tobacco smokers versus controls.
#More specifially, the first 6 columns (after we removed the original first two columns) are about tobacco smokers and the remaining 9 are about controls
labels <- c(rep("tob_smokers", 6), rep("control", 9))
s_threshold=0.05

#### 3 ####
my.ttest <- function(v, labels)
{
  levels <- unique(labels)
  classA <- v[ labels == levels[1]]
  classB <- v[ labels == levels[2]]
  pval <- t.test(classA, classB, alternative = "greater", var.equal = F)$p.value
  pval
}

allpvalues1 <- apply(data2, 1, my.ttest, labels)

## let's see which p values are smaller than a threshold
sig.inds1 <- which(allpvalues1 < s_threshold)
length(sig.inds1)  #how many genes
raw.data[sig.inds1,2] #which genes



### 4 ###
my.ttest <- function(v, labels)
{
  levels <- unique(labels)
  classA <- v[ labels == levels[1]]
  classB <- v[ labels == levels[2]]
  pval <- t.test(classA, classB, alternative = "less", var.equal = F)$p.value
  pval
}

allpvalues2 <- apply(data2, 1, my.ttest, labels)

## let's see which p values are smaller than a threshold
sig.inds2 <- which(allpvalues2 < s_threshold)
length(sig.inds2)  #how many genes
raw.data[sig.inds2,2] #which genes



### 5 ###
my.ttest <- function(v, labels)
{
  levels <- unique(labels)
  classA <- v[ labels == levels[1]]
  classB <- v[ labels == levels[2]]
  pval <- t.test(classA, classB, var.equal = F)$p.value
  pval
}

allpvalues3 <- apply(data2, 1, my.ttest, labels)

## let's see which p values are smaller than a threshold
sig.inds3 <- which(allpvalues3 < s_threshold)
length(sig.inds3)  #how many genes
raw.data[sig.inds3,2] #which genes


## 6 ###
my.ttest <- function(v, labels)
{
  levels <- unique(labels)
  classA <- v[ labels == levels[1]]
  classB <- v[ labels == levels[2]]
  pval <- t.test(classA, classB,var.equal = T)$p.value
  pval
}

allpvalues4 <- apply(data2, 1, my.ttest, labels)

## let's see which p values are smaller than a threshold
sig.inds4 <- which(allpvalues4 < s_threshold)
length(sig.inds4)  #how many genes
raw.data[sig.inds4,2] #which genes
