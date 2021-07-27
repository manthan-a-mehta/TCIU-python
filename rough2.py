
import numpy as np
dataz=np.random.randn(5)
print(dataz)
zs=np.array([1]*len(dataz))
zs.resize((1,len(zs)))

dataz.resize((1,len(dataz)))
zz=np.concatenate((zs.T,dataz.T),axis=1)
zt=dataz.copy()
for i in range(2,4+1):
        zt=zt*dataz
        zz=np.concatenate((zz,zt.T),axis=1)
zd=zt*dataz
tx=x1
xm=np.concate
