#Sofia Kampaki M.sc Bioinformatics 2018-2019

##Loading the libraries
library("Biostrings")
source("https://bioconductor.org/biocLite.R")
biocLite("seqbias")
library("Biostrings")
library("seqbias")
library("Rsamtools")





kmersfun<-function(file){    #creating the function for kmers, with parameter the name of the file
 
  dna=scanFa(file)
  kmercounts1 = as.data.frame(oligonucleotideFrequency(dna,1))   #frequencies of each nucleotide
  kmercounts2 = as.data.frame(oligonucleotideFrequency(dna,2))   #frequencies of 2-mers from the genome

  kmercounts1=apply(kmercounts1,2,FUN="sum")                     #getting the sum for each one of them because th fasta file has many sequences
  kmercounts2=apply(kmercounts2,2,FUN="sum")
  
  kmercounts1
  kmercounts2
  
  kmer=c()
  s=0
  for(i in seq(1,length(dna))){   #calculating the whole length of the  genome
    s=s+length(dna[[i]])
  }
  kmercounts2_oe=kmercounts2/s
  kmercounts2_oe
  tmp=kmercounts1/s
  tmpc=c()
  
  tmpc=c(tmp[1]*tmp[1])
  tmpc=c(tmpc,tmp[1]*tmp[2])
  tmpc=c(tmpc,tmp[1]*tmp[3])
  tmpc=c(tmpc,tmp[1]*tmp[4])
  tmpc=c(tmpc,tmp[1]*tmp[2])
  tmpc=c(tmpc,tmp[2]*tmp[2])                     #creating the o/e values,we need the probabilites P(A)*P(A) for AA etc.
  tmpc=c(tmpc,tmp[2]*tmp[3])
  tmpc=c(tmpc,tmp[2]*tmp[4])
  tmpc=c(tmpc,tmp[1]*tmp[3])
  tmpc=c(tmpc,tmp[2]*tmp[3])
  tmpc=c(tmpc,tmp[3]*tmp[3])
  tmpc=c(tmpc,tmp[3]*tmp[4])
  tmpc=c(tmpc,tmp[1]*tmp[4])
  tmpc=c(tmpc,tmp[2]*tmp[4])
  tmpc=c(tmpc,tmp[3]*tmp[4])
  tmpc=c(tmpc,tmp[4]*tmp[4])
  return(kmercounts2_oe/tmpc)
  

 
}

mouse_res=kmersfun("mouse")
human_res=kmersfun("human")
rat_res=kmersfun("rat")
yeast_res=kmersfun("yeast")
fly_res=kmersfun("fly")
celegans_res=kmersfun("celegans")

all_oe=data.frame()
all_oe=rbind(human_res,mouse_res,rat_res,fly_res,celegans_res,yeast_res)    #binding the results in order to create a matrix 
all_oe
dd<-dist(all_oe,method="manhattan")  #calculating the distance matrix
plot(hclust(dd))                     #plotting the results
dd


#Discussion: The distance table corresponds mostly to what we would expect. We can clearly see that there are two main clusters. The Mouse and rat species
#are very close as expected. The fly, yeast and c.elegans have large distances with the other three species and the minimum distance
#is between rat and mouse.One kind of strange thing is that the fly is closest to the yeast. The plot can easily explain all of these conclusions.
