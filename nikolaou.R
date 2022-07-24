library(car)

df<-read.table(file = 'ttest_vs_ANOVA.tsv', sep = '\t', header = TRUE)
tt<-read.table(file='Ward1998.txt',header = T)


create.df<-function(x){
  xt<-as.data.frame(t(x[,-1]))
  newdf<-data.frame("Gene"=rep(x[1,1],10),"Condition"="Transgenic",,"Value"=xt[1:10,1])
  newdf2<-data.frame("Gene"=rep(x[1,1],10),"Condition"="Transgenic","Value"=xt[11:20,1])
  finaldf<-rbind(newdf,newdf2)
  print(finaldf)
  for (i in 2:10) {
    newdf<-data.frame("Gene"=rep(x[i,1],10),"Condition"="Wildtype","Value"=xt[1:10,i])
    newdf2<-data.frame("Gene"=rep(x[i,1],10),"Condition"="Transgenic","Value"=xt[11:20,i])
    prev<-rbind(newdf,newdf2)
    finaldf<-rbind(finaldf,prev)
  }
  return(finaldf)
}
  
final<-create.df(df)

acanvar<-subset(final,final$Gene=="Acan")
genedft<-subset(final,final$Gene==final[21,1])
attach(acanvar)
detach(acanvar)
boxplot(Value~Condition)
V_AcanW <- Value[Condition=="Wildtype"]
V_AcanW <- Value[Condition=="Transgenic"]
var.test(V_AcanW, V_AcanT)
tmp<-leveneTest(Value, Condition)
tmp<-t.test(V_AcanW,V_AcanT)
print(tmp$`Pr(>F)`)
str(tmp$`Pr(>F)`)
detach(genedf)



for (i in seq(1, 200, by = 20)){
  genedf<-subset(final,final$Gene==final[i,1])
  attach(genedf)
  V_W <- Value[Condition=="Wildtype"]
  V_T <- Value[Condition=="Transgenic"]
  print(var.test(V_W, V_T))
  print(leveneTest(Value, Condition))
  
  detach(genedf)
}
