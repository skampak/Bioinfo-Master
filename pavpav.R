setwd('D:\\Master\\')
library(seqLogo)
install.packages("BiocManager")
x1<- "ACCTGGATGTGACGT"
X2<- "ACT"
raw <- read.table("seq5000.txt", colClasses = "character", h=F)
x<- raw[1,]
hre <- "ACTTCCG"
print(hre)
prdx1arr <- x[1:length(x)]
prdx1seq <- paste(prdx1arr, collapse="")
prdx1seq
len <- nchar(hre)
len

substr(prdx1seq, 1, 7)
leftlims <- 1:(nchar(prdx1seq) - (len - 7))
rightlims <- len:nchar(prdx1seq)
prdx1substrings <- mapply(substr, prdx1seq, leftlims, rightlims,
                          USE.NAMES=FALSE)
head(prdx1substrings)
x.vector <- unlist(strsplit(x=x, split=""))
y3<-c("A","T")
y1<-c("A","G","T","A","G","G","C","A","G")
y2 <- c("A","G")
motif<-c("A","C","T","T","C","C","G")
y1[1]
sum(y1==y2)
sum(y2!=y3)
length(y1)
a=y1
a<-a[1:(length(a)-length(y2))]
a
y2

newf<- function(a){
  y<-a[a:(length(y2)-1)]
  d<-distance(a,y)
  print(d)
}
distance <- function(a,y){
  if(length(a) != length(y)){
    stop("Not the same length")
  }
  <-sum(a!=y)
  return d
}
distance(a)
sapply(length(a),newf)

table(factor(all.motifs.matrix[,1]), levels=c("A","C","G","T"))

motif.freqs<- motif.counts/motif.sums
getwd()
seqLogo(makePWM(motif.freqs))
pdf(logo.pdf)
dev.off()