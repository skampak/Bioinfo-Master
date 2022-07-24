##PROJECT R 2020 ##
#1
#With grep we clean the soft file -- grep -v ^[!^#] GDS4879.soft > GDS4879.clean. I installed cygwin on windows to do that
raw.data <- read.table("GDS4879.clean", sep="\t", header=TRUE, stringsAsFactors = F,dec = '.')
dim(raw.data)

data <- raw.data[,-c(1,2)]
data <- sapply(data[,], as.numeric)

#2) Boxplot, is normalization needed?
boxplot(data) #blepoume oti xreiazetai na kanoume kanonikopoihsh sta dedomena mas epeidi exoume outliers
data.norm <- log2(data)
boxplot(data.norm)
dim(data.norm)


#3.Find all genes which havae statistical higher values in alcholohics vs non alcoholics
# Create labels for classA (alcoholics) and classB (Non-alcoholics)
data2=data[,c(1:6,13:26,7:12,27:39)] #allazw tin seira twn sthlwn sto dataset gia na einai prwta oi alkoolikoi kai meta oi control
datanorm2=as.data.frame(data.norm[,c(1:6,13:26,7:12,27:39)])
datanorm2=na.omit(datanorm2)  #vgazw ta NA
labels <- c(rep("alc", 20),rep("non-alc",19))  #ftiaxnw ta labels

alc <- datanorm2[, labels == "alc"]
noncalc <-datanorm2[,labels=="non-alc"]


my.ttest <- function(v, labels)
{
  levels <- unique(labels)
  v1 <- v[ labels == levels[1]]
  v2 <- v[ labels == levels[2]]
  stat<- t.test(v1, v2,alternative="greater")$statistics
  pval <- t.test(v1, v2, alternative="greater")$p.value
  pval
}

allpvalues <- apply(datanorm2, 1, my.ttest, labels)
sig.inds <- which(allpvalues < 0.05)
length(sig.inds)
genenames3 <- raw.data[sig.inds,2]


#4 female vs male
data.norm=as.data.frame(data.norm)
data.norm=na.omit(data.norm)
labels2 <- c(rep("f", 12),rep("m",27))
female <- data.norm[, labels2 == "f"]
male<-data.norm[,labels2=="m"]

my.ttest2 <- function(v, labels2)
{
  levels <- unique(labels2)
  v1 <- v[ labels2 == levels[1]]
  v2 <- v[ labels2 == levels[2]]
  stat<- t.test(v1, v2,alternative="less")$statistics
  pval <- t.test(v1, v2, alternative="less")$p.value
  pval
}
allpvalues2 <- apply(data.norm, 1, my.ttest2, labels2)
sig.inds2 <- which(allpvalues2 < 0.05)
length(sig.inds2)
genenames4 <- raw.data[sig.inds2,2]

#FDR & Bonferroni correction--from question 3
corpvalues <- allpvalues
corpvalues.FDR <- p.adjust(corpvalues, method = 'fdr')
corpvalues.Bonferroni <- p.adjust(corpvalues, method = "bonferroni")

cor.FDR <- which(corpvalues.FDR < 0.1)
cor.gn.FDR <- raw.data[cor.FDR, 2] #find gene names from raw data with FDR
sum(complete.cases(cor.gn.FDR))

cor.Bonferroni <- which(corpvalues.Bonferroni < 0.1)
cor.gn.Bonferroni <- raw.data[cor.Bonferroni, 2] #find gene names from raw data with Bonferroni
sum(complete.cases(cor.gn.Bonferroni))

#FDR & Bonferroni correction--from question 4
corpvalues <- allpvalues2
corpvalues.FDR <- p.adjust(corpvalues, method = 'fdr')
corpvalues.Bonferroni <- p.adjust(corpvalues, method = "bonferroni")

cor.FDR <- which(corpvalues.FDR < 0.1)
cor.gn.FDR <- raw.data[cor.FDR, 2] #find gene names from raw data with FDR
sum(complete.cases(cor.gn.FDR))

cor.Bonferroni <- which(corpvalues.Bonferroni < 0.1)
cor.gn.Bonferroni <- raw.data[cor.Bonferroni, 2] #find gene names from raw data with Bonferroni
sum(complete.cases(cor.gn.Bonferroni))


#6 gprofiler for genes from questions 3 and 4
library(gProfileR)
gprof <- gprofiler(query = genenames3 ,organism = "hsapiens", src_filter=c("GO:MF","GO:CC","GO:BP"))
gprof2 <- gprofiler(query = genenames4 ,organism = "hsapiens", src_filter=c("GO:MF","GO:CC","GO:BP"))
ord <- order(gprof$p.value, decreasing = F)
pvalues.ord <- gprof$p.value[ord]
categories <- gprof$term.name[ord]
categories
ord2 <- order(gprof2$p.value, decreasing = F)
pvalues.ord2 <- gprof2$p.value[ord2]
categories2 <- gprof2$term.name[ord2]
categories2

#PCA
library(plyr)
library(dplyr)
dim(data.norm)


colvec = c(rep("red", 12), rep("blue", 27))
symbol= c(rep(3, 6),rep(1, 6), rep(3,14),rep(1,13))
symbol

mypca3 = prcomp(t(data.norm),center=TRUE,  scale.=TRUE)
plot(mypca3$x[,1],mypca3$x[,2],col=colvec,pch=symbol)
###den exei omadopoihsh
