setwd('D:\\Master\\')

#reading the file
raw <- read.table("seq5000.txt", colClasses = "character", h=F)
motif<-c("A","C","T","T","C","C","G")


new.fun<-function(e){
  xarr <- e[1:length(e)]
  xarr
  xseq <- paste(xarr, collapse="")
  
  len <- length(motif)
  len
  
  substr(xseq, 1, 7)
  leftlims <- 1:(nchar(xseq) - (len-1))
  leftlims
  rightlims <- len:nchar(xseq)
  xsubstr <- mapply(substr, xseq, leftlims, rightlims,
                    USE.NAMES=FALSE)
  return(xsubstr)
}

tmp<-lapply(raw[,1],new.fun)



distance <- function(a,y){
  if(length(a)!=length(y)){
    stop("ERROR! Length among sequences differs")
  }
  d<-sum(a!=y)
  return(d)
}
newfun<-function(a,y){
  test<-unlist(strsplit(x=a, split=""))
  tmp<-sapply(1:(length(test)-length(y)), function(x){
    tt<-test[x:(x+length(y)-1)]
    nd<-distance(tt,y)
    if(nd<=1){
    return(nd)
    }
  })
  
}
tmp[[1]]
# unlist(strsplit(x=tmp[[1]], split=""))
# final<-sapply(xsubstr,newfun,motif)
final<-lapply(tmp,newfun,motif)
final[[1]]
f2<-xsubstr[final<=1]
f2






test<-unlist(strsplit(x=xsubstr[1], split=""))

distance(test,motif)