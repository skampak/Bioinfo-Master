#Loading the libraries


library(car)
library(data.table)   


#Reading the dataset
df<-read.table(file = 'ttest_vs_ANOVA.tsv', sep = '\t', header = TRUE)




#Creating a function that transfomrs the dataset as wanted
create.df<-function(x){
  xt<-as.data.frame(t(x[,-1]))
  newdf<-data.frame("Gene"=rep(x[1,1],10),"Condition"=colnames(x)[2],"Value"=xt[1:10,1])  #For first gene, creating a dataframe for all conditions
  newdf2<-data.frame("Gene"=rep(x[1,1],10),"Condition"=colnames(x)[12],"Value"=xt[11:20,1])
  finaldf<-rbind(newdf,newdf2)                                                              #row bind the dataframes that are created
  newdf3<-data.frame("Gene"=rep(x[1,1],10),"Condition"=colnames(x)[22],"Value"=xt[21:30,1])
  finaldf<-rbind(finaldf,newdf3)
  newdf4<-data.frame("Gene"=rep(x[1,1],10),"Condition"=colnames(x)[32],"Value"=xt[31:40,1])
  finaldf<-rbind(finaldf,newdf4)
  
  newdf5<-data.frame("Gene"=rep(x[1,1],10),"Condition"=colnames(x)[42],"Value"=xt[41:50,1])
  finaldf<-rbind(finaldf,newdf5)
  
  newdf6<-data.frame("Gene"=rep(x[1,1],10),"Condition"=colnames(x)[52],"Value"=xt[51:60,1])
  finaldf<-rbind(finaldf,newdf6)                                                            #first completed dataset for the first gene ONLY
  print(finaldf)
  for (i in 2:10) {
                                                                                            #we repeated the process for the 9 remaining genes
    newdf<-data.frame("Gene"=rep(x[i,1],10),"Condition"=colnames(x)[2],"Value"=xt[1:10,i])
    newdf2<-data.frame("Gene"=rep(x[i,1],10),"Condition"=colnames(x)[12],"Value"=xt[11:20,i])
    prev<-rbind(newdf,newdf2)
    newdf3<-data.frame("Gene"=rep(x[i,1],10),"Condition"=colnames(x)[22],"Value"=xt[21:30,i])
    prev<-rbind(prev,newdf3)
    newdf4<-data.frame("Gene"=rep(x[i,1],10),"Condition"=colnames(x)[32],"Value"=xt[31:40,i])
    prev<-rbind(prev,newdf4)
    
    newdf5<-data.frame("Gene"=rep(x[i,1],10),"Condition"=colnames(x)[42],"Value"=xt[41:50,i])
    prev<-rbind(prev,newdf5)
    
    newdf6<-data.frame("Gene"=rep(x[i,1],10),"Condition"=colnames(x)[52],"Value"=xt[51:60,i])
    prev<-rbind(prev,newdf6)
    finaldf<-rbind(finaldf,prev)                                                              #row bind the first dataset with the dataset that was created for the remaining genes
  }
  
  
  return(finaldf)
}


#Calling the function
final<-create.df(df)  
boxplot(final$Value~final$Condition)  #they have the same mean


detach(genedf)
#Task a, t.test for all genes on two conditions
for (i in seq(1, 600, by = 60)){
  genedf<-subset(final,(Condition=="Wild.type"| Condition=="Transgenic")& Gene==final[i,1]  , select=Condition:Value)
  attach(genedf)
  V_W <- Value[Condition=="Wild.type"]
  V_T <- Value[Condition=="Transgenic"]
  
  LT<-leveneTest(Value, Condition)
  print(LT)
  LTP<-LT$`Pr(>F)`
  ifelse(LTP>0.05,print(t.test(V_W,V_T, var.equal = T)),print(t.test(V_W,V_T)))
  
  detach(genedf)
}

detach(genedf)

#Task b: Anova test for all genes on all conditions
for (i in seq(1, 600, by = 60)){

  genedf<-subset(final, Gene==final[i,1]  , select=Condition:Value)
  
  model <- aov(genedf$Value  ~ genedf$Condition )
  
  print(summary(model))

}
