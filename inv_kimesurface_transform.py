import numpy as np
import math
# import sympy as sm
from sympy import *
def inv_kimesurface_fun(val,p):

        val1=re(val)+im(val)*1j
        mag=exp(sqrt(re(val1)**2+im(val1)**2)) 
        phase=arg(val1)
        value=re(mag*exp(1j*phase))+im(mag*exp(1j*phase))*1j
        return inverse_laplace_transform(value, val, p)
def inv_kimesurface_transform(time_points,array_2d,num_length=2,m=1,msg=True):
    gamma=0.5
    fail_val=1+0j
    nterms=31
    
    f2=np.array([complex(0,0) for i in range(len(time_points))])
    for t in range(len(time_points)):
        z=var('z',Imaginary=True)

        x1=ceiling(re(z))-1
        y1=floor(im(z))-1
        if(not x1==nan):
           if((x1-1).is_positive or ((x1-array_2d.shape[0]).is_positive)):
               x1=1
        
        if(not (y1==nan)):
            if((y1-1).is_positive or ((y1-array_2d.shape[0]).is_positive)):
                y1=1
        p=var('p',Imaginary=True)

        val=var('val',Imaginary=True)
        ans=inv_kimesurface_fun(val,p)
        f2[t]=ans.subs({p:time_points[t]})

        # print(ilt.subs({val:array_2d[1,1]}))
    
    tvalsn=np.arange(0,math.pi*2+1,step=((math.pi*2+1)/num_length))
    f3=np.array([complex(0,0) for i in range(len(time_points))])
    for t in range(len(time_points)):
        idx=np.ceil(t/num_length)
        print(np.ceil(t/num_length))
        f3[t]=f2[int(idx)]

x=inv_kimesurface_transform([0,1,2],np.random.randn(2,2))
print(x)    