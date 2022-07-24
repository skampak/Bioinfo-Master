import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.decomposition import PCA
from sklearn.datasets import load_iris
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
sns.set(color_codes=True)



##edw katahrizw to arxeio, ftiaxnw 2 arxeia ena pou exei mono noumera to file3(forpca) kai to testfile gia na kanw antistoixisi to onoma stilis me to group
with open('GDS5430.soft','r') as file:
    l1=[]
    l2=[]
    file2 = open('testfile.txt', 'w')
    for line in file:
        if line[0:2]=='#G':
            l1.append(line.split(':')[1].split(';')[0].split('_')[0:2])
            l2.append(line.split(':')[0].split('=')[0][1:-1])
        if line[0:2]=='ID':
            file2.write(line)
            break;
    for line in file.readlines()[:-1]:
            file2.write(line)
    file2.close()
with open('testfile.txt','r') as file2:
    print(len(file2.readlines()))
    file2.seek(0)
    file3=open('forpca.txt','w')
    for line in file2.readlines()[1:]:

        file3.write(" ".join(line.split()[2:])+'\n')

with open('forpca.txt','r') as file3:
    print(len(file3.readlines()))
######

n=[]
n=zip(l1,l2)
person_id=(list(n))
print(person_id)   #enwnw atomo me to id tou
nnl=[]
for i in person_id:
    nnl.append(tuple(i[0]))
unique_group=set(nnl)           #krataw ta monadika group diladi m alcoholic kai control kai to idio kai gia female
print(unique_group)
n_of_groups=range(0,len(unique_group))

match_label=zip(unique_group,n_of_groups)     # ta enwnw me ena noumero apo to 0 mexri to 3

final_list=list(match_label)       # ta apothikeuw
print(final_list)
####

##edw fortwnw to arxeio me ta id me pandas kai krataw tin prwti grammh san header
testdata = pd.read_csv('testfile.txt',sep="\t", header = None)
headers = testdata.iloc[0]
new_df  = pd.DataFrame(testdata.values[1:], columns=headers)

for i in person_id:
    for j in final_list:
        if j[0]==tuple(i[0]):     ### edw sthn ousia metatrepw tis times tou dataset se 0 1 2 3 gia na kanoun match me to atomo pou einai, diladi kathe stili exei to antistoixo noumero
            new_df[i[1]]=j[1]


nnew_df=pd.DataFrame(new_df.iloc[1:,2:])
nnew_df.columns = range(nnew_df.shape[1])
nnew_df=np.array(nnew_df)

my_y=nnew_df[1,:]
print(my_y)                       ###edw exw ftiaksei to labeling, diladi to y mou
data1=np.loadtxt('forpca.txt')
#print(nl)
#print(nnl4)

#print(data1.shape)

data1=data1.T
print(data1.shape)
D,N=data1.shape
scaler = StandardScaler()
X_std=scaler.fit_transform(data1)
Xmean=np.mean(X_std,axis=1,keepdims=True)
X_new=X_std-np.repeat(Xmean,N,axis=1)
pca=PCA()
X_new=pca.fit_transform(X_new)
variance=pca.explained_variance_ratio_
plt.plot(variance,'.') ## we keep 20 components
plt.title("Plotting the Explained Variance from PCA")
#plt.show()

### calling pca again with number of components equal to twenty
pca = PCA(n_components=20)
pca.fit(X_new)
X_new = pca.transform(X_new)

colors = ['navy', 'turquoise', 'darkorange','pink']
label_names=[]
for i in final_list:
    label_names.append(','.join(map(str,i[0])))
print(label_names)   #creating the label names



colors = ['navy', 'turquoise', 'darkorange','pink']
plt.figure(figsize=(8, 8))
for color, i, label_name in zip(colors, [0, 1, 2, 3], label_names):
       plt.scatter(X_new[my_y == i, 0], X_new[my_y == i, 1],color=color, lw=2, label=label_name)
plt.legend(loc=2)
plt.show()



