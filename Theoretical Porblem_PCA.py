#!/usr/bin/env python
# coding: utf-8

# In[117]:


###EIGEN PCA###
from sklearn.datasets import make_blobs
import numpy as np
from numpy import mean
from numpy import cov
from numpy.linalg import eig
#from numpy.linalg import matrix_rank
X,y = make_blobs(n_samples=1000, n_features=20, centers=2, cluster_std=1.5, random_state=0)
X=X.T
print(X.shape) #check the dimensions, should be 20x1000



####Step 1####
XM = mean(X.T, axis=1)
X=X-XM

####Step 2####
S=(1/X.shape[0])*(np.dot(X,X.T))
print(S.shape)


####Step 3####
# calculate covariance matrix of centered matrix

values, vectors = eig(S)
print(vectors.shape) #checking for dimensions
print(values.shape)  #checking for dimensions

y2=[x/sum(values) for x in values]   #creating a vector that every value is an eigenvalue divided by the sum of the eigenvalues
                                     #useful for the plot
plt.plot(y2,'.')
plt.ylabel("eigvalue/sumvalues")
plt.xlabel("dimensions")
plt.show() 
#keeping only the first 2 dimensions
    
####Step 4####

XFINAL=np.dot(vectors.T,X)
plt.scatter(XFINAL[1,:][y==0],XFINAL[0,:][y==0])
plt.scatter(XFINAL[1,:][y==1],XFINAL[0,:][y==1])
plt.title("PCA with eigendecomposition")
plt.show()


# # svd

# In[116]:


###SVD PCA###
from sklearn.datasets import make_blobs
import numpy as np
from numpy import mean
from numpy import cov

#from numpy.linalg import matrix_rank
X,y = make_blobs(n_samples=1000, n_features=20, centers=2, cluster_std=1.5, random_state=0)
X=X.T
print(X.shape) #check the dimensions, should be 20x1000



####Step 1####
XM = mean(X.T, axis=1)
X=X-XM



####Step 2####
u, s, vh = np.linalg.svd(X, full_matrices=True)
print(u.shape) 

    
####Step 4####
XFINAL2=np.dot(u.T,X)
XFINAL2.shape
##keeping 2 dimensions
plt.scatter(XFINAL2[1,:][y==0],XFINAL2[0,:][y==0])
plt.scatter(XFINAL2[1,:][y==1],XFINAL2[0,:][y==1])
plt.title("PCA with SVD")
plt.show()


# # ppca

# In[130]:


####S_square_notzero####

from sklearn.datasets import make_blobs
from sklearn.preprocessing import StandardScaler
import numpy as np
import matplotlib.pyplot as plt


X,y = make_blobs(n_samples=1000, n_features=20, centers=2, cluster_std=1.5, random_state=0) # Here, X1 is the 1000 x 20 data and Y1 is cluster assignment for the 1000 samples


##Creating the ppca function
def myppca(X,y,maxiter,tol,L):

    X_std = StandardScaler().fit_transform(X) #standarization
    X_new = X_std - np.mean(X_std, axis=0)
    print (X_new.shape) #NXD matrix
    X_new=X_new.T
    print (X_new.shape) ## DxN matrix

    N = X_new.shape[1]
    D = X_new.shape[0]
    W = np.random.randn(D,L)
    s_sq = np.random.rand()
    for rep in range(maxiter):
        # Estep
        M = np.dot(W.T,W) + s_sq*np.eye(L,L)
        factor = np.dot(np.linalg.inv(M), W.T)
        E_zn = np.zeros((L,N))
        E_zn_znT = np.zeros((N,L,L))
        A = np.zeros((D,L))
        for i in range(N):
            E_zn[:,i] = np.dot(factor, X_new[:,i].reshape(-1,1)).squeeze()
            E_zn_znT[i,:,:] = s_sq*np.linalg.inv(M)+ np.dot(E_zn[:,i].reshape(-1,1),E_zn[:,i].reshape(-1,1).T)
            # Mstep
            A+= np.dot(X_new[:,i].reshape(-1,1),E_zn[:,i].reshape(-1,1).T)

        W_new = np.dot(A, np.linalg.inv(np.sum(E_zn_znT,axis=0)))
        s_sq_new = 0
        for j in range(N):
            s_sq_new += np.linalg.norm(X_new[:,j])**2 - 2*np.dot( E_zn[:,j].reshape(-1,1).T, np.dot(W.T, X_new[:,j].reshape(-1,1)))+np.trace(np.dot(E_zn_znT[j,:,:], np.dot(W_new.T, W_new)))    
        s_sq_new = s_sq_new.item()
        s_sq_new *= 1/(N*D)
        err1 = np.abs(s_sq_new-s_sq)
        err2 = np.sum((W_new-W)**2)/(L*D)    
        if err1 < tol and err2 <tol:        
            break
        else:
            W= W_new.copy()
            s_sq = s_sq_new  

    

    U,S,V = np.linalg.svd(W_new)
    X_proj = np.dot(U.T,X_new)
    plt.scatter(X_proj[0,:][y==0],X_proj[1,:][y==0])
    plt.scatter(X_proj[0,:][y==1],X_proj[1,:][y==1])
    plt.title("PPCA")
    
    
myppca(X,y,1000,1e-3,2)


# In[127]:


###S_square_zero###

def myppca2(X,y,maxiter,tol):

    X_std = StandardScaler().fit_transform(X) #standarization
    X_new = X_std - np.mean(X_std, axis=0)
    #print (X_new.shape) #NXD matrix
    X_new=X_new.T
    #print (X_new.shape) #DxN matrix

    N = X_new.shape[1]
    D = X_new.shape[0]
    W = np.random.randn(D,L)
    sigma_sq = 0                 ##The difference is here, in this case the s_square is set to 0 and no s_new is computed
    for rep in range(maxiter):
        #Estep
        factor2=np.dot(np.linalg.inv(np.dot(W.T,W)),W.T)
        omega_matrix=np.zeros((L,N))
        
        A2=np.zeros((D,L))

        for i in range(N):
            omega_matrix[:,i]=np.dot(factor2,X_new[:,i].reshape(-1,1)).squeeze()

            A2+=np.dot(X_new[:,i].reshape(-1,1),omega_matrix[:,i].reshape(-1,1).T)

        W_new1=np.dot(A2,np.linalg.inv(np.dot(omega_matrix,omega_matrix.T)))
        err3 = np.sum((W_new1-W)**2)/(L*D)
        if err3 < tol :
            break
        else:
            print ("Printing error value:\n",err3)
            W= W_new1.copy()
   
    U,S,V = np.linalg.svd(W_new1)
    X_proj = np.dot(U.T,X_new)
    plt.scatter(X_proj[0,:][y==0],X_proj[1,:][y==0])
    plt.scatter(X_proj[0,:][y==1],X_proj[1,:][y==1])
    plt.title("PPCA-ssquare=0")
myppca2(X,y,1000,1e-3)

