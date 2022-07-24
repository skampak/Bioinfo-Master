

mtx<-matrix(nrow=5,ncol=10)
for (i in 1:nrow(mtx)){
  for (j in 1:ncol(mtx)){
    mtx[i,j]=i^j
  }
}

mtx1<-matrix(1:5,nrow=5,ncol=10)
mtx1
mtx2<-matrix(1:10,nrow=5,ncol=10,byrow=T)
mtx2
mtx1^mtx2

tmp<-outer(1:nrow(mtx), 1:ncol(mtx) , FUN="^")
tmp


###ASK 2###
mtx12<-matrix(1:10,nrow=10,ncol=10)
mtx22<-matrix(1:10,nrow=10,ncol=10,byrow=T)
B=abs(mtx12-mtx22)

diag(B)
mtx12-1
C=((mtx12-1)*10)+mtx22
C
##ASK 3###
D=matrix(1:100, nrow=10, ncol=10,byrow=T)
D2
D
D3<-2*D
D3
D2<-matrix(2,ncol=10,nrow=1)
E=D2%*%D
E
Z=mtx12%*%mtx22%*%D
Z
Z2=mtx12*mtx22*D
Z2
###ASK5###
test<-c(1:10)
test
v<-rep(list(0),10)
v
tmp<-lapply(v,function(x) x<-c(1:10))
tmp[[1]]
tmp
tmpm<-matrix(unlist(tmp),nrow=10,ncol=10,byrow=T)
tmpm

tmpm<-matrix(unlist(tmp),nrow=10,ncol=10)
tmpm


list2<-lapply(1:10, function(x) (c(1:10)))
list2
test1<-matrix(1:6,nrow=2,ncol=3)
test2<-matrix(1:12, nrow=3, ncol=4)
test1
test2
test1*test2
test1%*%test2
