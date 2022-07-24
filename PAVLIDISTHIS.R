setwd('D:\\Master\\')

l1<-list()
v<-c()


for (i in 1:10){
  for (j in 1:20){
  v[j]<- i*j
  }
  l1[[i]]<-v
}

class(l1)
l1<-matrix(unlist(l1),ncol=20,byrow = T)
l1



x<-c(sample(1:10,3))
y<-c(sample(1:10,3))
z<-c(sample(1:10,3))
nm<-cbind(x,y,z)
rn<-c("a","b","c")
rownames(nm)<-rn
nm



v12<-c(sample(1:10),12)
B<-matrix(v12,nrow=4,ncol=3)
rn12<-c("a","b","c","d")
cn<-c("x","y","z")
rownames(B)<-rn12
colnames(B)<-cn
B1<-matrix(v12,nrow=4,ncol=3,byrow = T)
B1
B

t(B)

t(B)%*%t(B)

data.raw <- read.table("GDS3309.clean", h=T, na.strings="null")
data <- data.raw[,3:ncol(data.raw)]
dim(data)
boxplot(data)
meancol <- apply(data, 2, function(x){ mean(x)})
meancol
stdcol <- apply(data, 2, function(x){ sd(x)})
stdcol
nmat<-matrix(nrow=nrow(data),ncol=ncol(data))
dim(nmat)
for (i in 1:nrow(data)){
  for(j in 1:ncol(data)){
    nmat[i,j]<-(data[i,j]-meancol[j])/stdcol[j]
  }
}

for (k in 1:ncol(nmat)){
  tmp<-which(nmat[,k]==max(nmat[,k]))
  print(data.raw[tmp,2])
}

for (l in 1:ncol(nmat)){
  tmp<-which(nmat[,l]==min(nmat[,l]))
  print(data.raw[tmp,2])
}

