import numpy as np
import math
import matplotlib.pyplot as plt
def LT(datax,datay,dataz,k=3,fitwarning=False,mirror=False,range=2*math.pi):

    datax = datax.astype(np.float)
    datay=datay.astype(np.float)
    n=len(datax)
    x1=n/(n+0.5)*(datax-min(datax))/(max(datax-min(datax)))*range
    if(mirror):
        x1=np.array(np.concatenate((x1,np.flip(2*range-x1)))/2)
        n=2*n
        datay=np.concatenate((datay,np.flip(datay)))

    coefm=np.zeros((k+1,k+1))
    coefm[0][0]=1
    for i in range(k):
        column=([0]*(i+1))
        coefm[0:i+1,i+1]=column
        coef=[x*(i+1) for x in coef]
        coef.append(1)
        coefm[i+1,0:i+2]=coef
    
    zs=np.array([1]*len(dataz))
    zs.resize((1,len(zs)))

    dataz.resize((1,len(dataz)))
    zz=np.concatenate((zs.T,dataz.T),axis=1)
    zt=dataz.copy()
    for i in range(2,k+1):
        zt=zt*dataz
        zz=np.concatenate((zz,zt.T),axis=1)
    zd=zt*dataz

    xs=np.array([1]*len(x1))
    xs.resize((1,len(xs)))

    x1.resize((1,len(x1)))
    xz=np.concatenate((xs.T,x1.T),axis=1)
    xt=x1.copy()
    for i in range(2,k+1):
        xt=xt*x1
        xz=np.concatenate((xz,xt.T),axis=1)
    xd=xt*dataz
    result=[0]*len(inputz)
    ii=1
    while(ii+k<n):
        a=xz[ii:ii+k,:k+1]
        b=datay[ii:ii+k]

        polyc=int(np.linalg.inv(A).dot(b))
    if(fitwarning):
        xx=np.arange(A[0,1],(A[k,1],A[k,1]-A[0,1]/100))
        yy=np.poly1d(np.flip(polyc))(xx)
        
        if(max(abs(yy-np.mean(b)))>2*max(abs(b-np.mean(b)))):
            print("Poor fit at {}, Largest deviation is {}".format(ii,max(abs(yy-np.mean(b)))))
            print("Spline Polynomial is {}".format(polyc))
            plt.plot(xx,yy,color="blue")
            plt.plot(A[:,1],b,color="red")
            plt.ylab("")
            plt.legend(loc="upper left",labelcolor=["blue","red"])
            print(" ")


    m1=((polyc*coefm).T*A[0,:]).T
    m11=int()
    m2=((polyc*coefm).T*A[k,:]).T

    intgl=(math.exp(-inputz*A[0,1])*np.sum(zz.T*m11,axis=1)-math.exp(-inputz*A[k,1])*np.sum(zz.T*m22,axis=1))/zd
    result=result+intgl
    ii=ii+k

    if(ii<n):
        nk=n-ii
        A=xz[ii:ii+k,:n*k+1]
        b=datay[ii:ii+n*k]
        nc=int(np.linalg.inv(A).dot(b))
        nc=[nc,[0]*(k-nk)]
        A=xz[ii:ii+nk,:]
        m1=((nc*coefm).T*A[0,:]).T

        m2=((nc*coefm).T*A[nk,:]).T
        intgl=(math.exp(-inputz*A[0,1])*np.sum(zz.T*m11,axis=1)-math.exp(-inputz*A[k,1])*np.sum(zz.T*m22,axis=1))/zd
        result=result+intgl
    return result


def tiLT(LTF,tini=0.001,tend=2*math.pi,nnt=200):
    if(True):
        a=8
        ns=100
        nd=29

    N=ns+nd+1
    step=(tend*nnt/(nnt+0.5)-tini)/nnt
    radt=np.arange(tini,(tend*nnt/(nnt+0.5)),step=step)
    if(tini==0):
        pass
    alfa=np.arange(1,ns+1+nd,step=1)
    beta=alfa
    for k in np.arange(1,ns+1+nd):
        alfa[k]=a+(k-1)*math.pi*1j
        beta[k]=-math.exp(a)*(-1)**k

    n=np.arange(1,nd+1)
    bdif=gamma(nd+1)
    bdif=bdif/gamma(nd+n-2)
    bdif=bdif/gamma(n)
    bdif=np.cumsum(bdif)
    bdif=np.flip(bdif)
    bdif=bdif/(2**nd)
    temp=beta[np.arange(ns+2,ns+1+nd)]*bdif
    print(temp)
    beta[np.arange(ns+2,ns+1+nd)]=temp
    beta[0]=beta[0]/2
    ft2=np.arange(1,nnt)
    Qz=[]
    for kt in np.arange(1,nnt):
        tt=radt[kt]
        s=alfa/tt
        Qz.append(s)
    LTQz=LTF(Qz)
    for kt in np.arange(1,nnt):
        tt=radt[kt]
        s=alfa/tt
        bt=beta/tt
        btF=bt*LTQz[np.arange((kt-1)*N+1,kt*N)]
        ft2[kt]=np.sum(np.real(btF))
        if(np.isnan(ft2[kt])):
            print(kt)
            print(LTQz[np.arange((kt-1)*N+1,kt*N)])
            print(btF)
    return ft2