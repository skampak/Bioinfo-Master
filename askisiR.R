 setwd('D:\\Master\\')

#reading the file
raw <- read.table("seq5000.txt", colClasses = "character", h=F)
motif<-c("A","C","T","T","C","C","G")



x<- raw[1,]
xarr <- x[1:length(x)]
xarr
xseq <- paste(xarr, collapse="")
xseq
len <- length(motif)
len

substr(xseq, 1, 7)
leftlims <- 1:(nchar(xseq) - (len-1))
leftlims
rightlims <- len:nchar(xseq)
xsubstr <- mapply(substr, xseq, leftlims, rightlims,
                          USE.NAMES=FALSE)
head(xsubstr)

xsubstr


final<-sapply(xsubstr,newfun,motif)
final
f2<-xsubstr[final<=1]
f2




distance <- function(a,y){
  if(length(a)!=length(y)){
    stop("ERROR! Length among sequences differs")
  }
  d<-sum(a!=y)
  return(d)
}
newfun<-function(a,y){
  test<-unlist(strsplit(x=a, split=""))
  
  nd<-distance(test,y)
  
  return(nd)
}