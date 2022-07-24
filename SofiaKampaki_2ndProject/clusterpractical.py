import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.cluster import KMeans
from sklearn import metrics
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.metrics import silhouette_samples, silhouette_score


import matplotlib.pyplot as plt
sns.set(color_codes=True)

plt.plot()
datal = np.loadtxt('forpca.txt')

K=8
colors = ['b', 'g', 'r','pink','orange','black','purple','yellow','magenta']

#X=np.array(list(zip(x1, x2))).reshape(len(x1), 2)
#print(X)

datal=datal.T
D,N=datal.shape
scaler = StandardScaler()
X_std=scaler.fit_transform(datal)
Xmean=np.mean(X_std,axis=1,keepdims=True)
X_new=X_std-np.repeat(Xmean,N,axis=1)

pca = PCA(n_components=2)
pca.fit(X_new)
X_new = pca.transform(X_new)
X_new=abs(X_new)
print(X_new.shape)
s=[]
for i in range(K):
    kmeans_model = KMeans(n_clusters=K).fit(X_new)
    cluster_labels = kmeans_model.labels_
    silhouette_avg = silhouette_score(X_new, cluster_labels)
    print("For n_clusters =", i,
          "The average silhouette_score is :", silhouette_avg)
    s.append(silhouette_avg)

print(s)
#The best silhouette score is for 3 clusters
kmeans_model = KMeans(n_clusters=3).fit(X_new)
cluster_labels = kmeans_model.labels_
for i, l in enumerate(kmeans_model.labels_):
    plt.plot(X_new[i,0],X_new[i,1], color=colors[l],marker='o',ls='None')
    plt.xlim([-100, 200])
    plt.ylim([-100, 200])

plt.show()

