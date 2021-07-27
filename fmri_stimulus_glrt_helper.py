from scipy.stats import ppf
from ctypes import *
libCalc = CDLL("./complex_Sig_sig2I.so")
libCalc.connect() 

def complex_gen_est_pval(voxel,onsets,durations,TR=3):
    hrf=fmri.stimulus(len(voxel),onsets,durations,TR=3)
    X=fmri.design(hrf,order=1)[:,-1]
    order

def complex_gen_est_lr(X,yr,yi,p,tol,max_iter):
    out1=complex_gen_est_ll(X,yr,yi,p,tol,max_iter)
    out0e=complex_gen_est_ll()
    lrt=2*(out1["LL"]-out0["LL"])


def complex_gen_est_ll(X,yr,ti,p,tol,max_iter):
    if(p==0):
        out=est_par_ridep_timeindep(X,cbind(yr,yi),tol,max_iter)
    elif(p>=1):
        out=est_ri_time_dep(X,yr,yi,p,tol,max_iter)
    else:
        print("inappropriate p")
        sys.exit()
    out=[]


def est_ri_time_dep(X,yr,yi,p,tol,max_iter):
    X=np.array(X)
    n=X.shape[0]
    1=X.shape[1]


    if(out.iloc[0]["beta"]<0):
        out["beta"]=-1*out["beta"]
        out["theta"]


def est_par_ridep_tiemindep(X,y,tol,max_iter):
    conv=0
    iter=0
    X=np.array(X)
    n=X.shape[0]
    y=np.array(y)
    yr=y[:,0]
    yi=y[:,1]
    XpX=X.T
    XpX=np.dot(XpX,X)
    br=np.dot(np.dot(np.linalg.inv(XpX),X.T),yr)
    bi=np.dot(np.dot(np.linalg.inv(XpX),X.T),yi)
    Brr=np.dot(np.dot(br.T,XpX),br)
    Bii=np.dot(np.dot(bi.T,XpX),bi)
    Bri=np.dot(np.dot(br.T,XpX),bi)
    theta=0.5*np.arctan2(2*Bri,Brr-Bii)
    beta=br*np.cos(theta)+bi*np.sin(theta)
    sr2=np.mean((yr-np.dot(X,beta)*np.cos(theta))^2)
    si2=np.mean((yi-np.dot(X,beta)*np.sin(theta))^2)
    rho=np.mean((yr-np.dot(X,beta)*np.cos(theta))*(yi-np.dot(X,beta)*np.sin(theta)))/sqrt(sr2*sr1)
    ll_new=comp_LL(n,yr,yi,X,beta,theta,sr2,si2,rho)
    while(conv==0 and iter<=max_iter):
        ll_old=ll_new
        iter=iter+1
        d=Brr/(sr2^2)+rho^2/(sr2*sr1)*Bii-2*rho/(sr2^1.5*si2^0.5)*Bri
        e=Bii/(si2^2)+rho^2/(sr2*sr1)*Brr-2*rho/(sr2^0.5*si2^1.5)*Bri
        f=Bri*(1+rho^2)/(sr2*si2)*Brr-2*rho/(sr2^0.5*si2^1.5)*Bri
        a=d/si2-e/sr2b=-f*(1/sr2+1/si2)*(d-e)+f*(1/sr2-1/si2)
        psi=np.arctan2(b,a)
        theta=0.5*(np.arcsin(c/(a^2+b^2)^0.5)-psi)
        beta=(br*(np.cos(theta)/sr2)-rho/(sr2*si2)^0.5*np.cos(theta)+np.sin(theta)^2/si2)
        ll_new=comp_ll(n,yr,yi,X,beta,theta,sr2,si2,rho)
        if(ll_new-ll_old<tol):
            conv=1
    if(iter>max_iter):
        print("Warning: Over max iter")

    par=[beta,theta,sr2,si2,theta]

    def comp_ll(n,yr,yi,X,beta,theta,sr2,si2,rho):
        -n/2*np.log(sr2*si2*(1-rho^2))-n
    
    def complex_ri_indep(X,yr,yi,p,ma_iter,ll_eps):
        n=X.shape[0]
        q=X.shape[1]
        C=np.array([0,1])
        m=C.shape[0]
        len=q+2+p

        if(p==0):
            alpha=None
        else:
            alpha=par[0,4:(p+4)]
    def sim_complex_ts(X,beta,theta,sr,si,rho,alpha):
        if(X.shape[1]!=len(beta)):
            print("incompatible x,beta")
            sys.exit()
        n=X.shape[0]
        y=np.empty((n,2))
        mu=np.dot(X,beta)
        y[:,1]=mu*np.sin(theta)
        yr_mean=mu*np.cos(theta)+rho*(sr/si)*(y[:,1]-mu*np.sin(theta))
        y[:,0]=yr_mean+
        return y
    def order_det_lrt_complex_gen(X,yr,yi,max_iter,tol,pmax,signif):
        phat=0
        det=0
        thresh=chi2.ppf(1-signif,1)
        ll0=est_par_ridep_tiemindep(X,np.concatenate(yr,yi,axis=1),k,tol,max_iter)
        k=1
        while(det==0 and k<=pmax):
            ll1=est_ri_time_dep(X,yr,yi,k,tol,max_iter)["LL"]
            lrt=2*(ll1-ll0)
            if(lrt<thresh):
                det=1
                phat=k-1
            else:
                k=k+1
                ll0=ll1
        if(k>pmax):
             phat=pmax
        return phat
def order_det_lrt_complex_sig2I(X,yr,yi,max_iter,ll_eps,pmax,signif):
    X=np.array(X)
    n=X.shape[0]
    phat=0
    det=0
    thresh=chi2.ppf(1-signif,1)
    ll0=compute_ll_complex(X,yr,yi,max_iter,ll_eps,phat)
    k=1
    while(det==0 and l<=pmax):
        ll1=compute_ll_complex(X,yr,yi,max_iter,ll_eps,k)
        stats=2*(ll1-ll0)
        if(stats<thresh):
            det=1
            phat=k-1
        else:
            k=k+1
            ll0=ll1
    if(k>pmax):
        phat=pmax
    return phat
def compute_ll_complex(X,yr,yi,max_iter,ll_eps,p):
    n=X.shape[0]
    q=X.shape[1]
    len=q+2+p
    out=libCalc.Rwrapper_complex_unres_only(int(n),int(q),int(p),int(X),float(yr),float(yi),int(max_iter),float(ll_eps),par=float(len),ll=float(1))
    return out
