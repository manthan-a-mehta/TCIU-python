from scipy.signal import detrend
from plotly.subplots import make_subplots
import numpy as np
import plotly.graph_objects as go
import pandas as pd
from kern_smooth import densCols
from fmri_simulate_func import fmri_simulate_func
import sys
def fmri_time_series(fmridata,voxel_location,is_4d=True,ref=None):
    fig = make_subplots(rows=5, cols=1)
    if(len(fmridata.shape)!=1 and len(fmridata.shape)!=4):
        print("Enter data with dimension 1 or 4")
        sys.exit()
    # fig = go.Figure()
    if(is_4d):
        x=voxel_location[0]
        y=voxel_location[1]
        z=voxel_location[2]
        fdata=fmridata[x-1,y-1,z-1,:]
        realnum=np.real(fdata)
        imgnum=np.imag(fdata)
        phasenum=np.angle(fdata)
        modnum=np.absolute(fdata)
    else:
        realnum=np.real(fmridata)
        imgnum=np.imag(fmridata)
        phasenum=np.angle(fmridata)
        modnum=np.absolute(fmridata)
    start=1
    dates=[a for a in range(start,len(realnum)+1)]
    realnum1=detrend(realnum,bp=np.array([a for a in range(21,160,20)]))
    ts = pd.Series(realnum, index=dates)
    ksmthrealnum = densCols(np.array(dates),ts.values,bandwidth=5)
    ksmthrealnum=pd.Series(ksmthrealnum)
    ksthrealnum=pd.DataFrame({"tsreal":ts,"ksmoothreal":ksmthrealnum})
    fig.add_trace(go.Scatter(y=ksthrealnum["tsreal"],x=ksthrealnum.index,name="tsreal"))
    fig.add_trace(go.Scatter(y=ksthrealnum["ksmoothreal"],x=ksthrealnum.index,name="ksmoothreal"))
    realnum1=detrend(phasenum,bp=np.array([a for a in range(21,160,20)]))
    ts = pd.Series(phasenum, index=dates)
    ksmthrealnum = densCols(np.array(dates),ts.values,bandwidth=5)
    ksmthrealnum=pd.Series(ksmthrealnum)
    ksthrealnum=pd.DataFrame({"tsphase":ts,"ksmoothphase":ksmthrealnum})
    fig.add_trace(go.Scatter(y=ksthrealnum["tsphase"],x=ksthrealnum.index,name="tsphase"),row=2,col=1)
    fig.add_trace(go.Scatter(y=ksthrealnum["ksmoothphase"],x=ksthrealnum.index,name="ksmoothphase"),row=2,col=1)
    realnum1=detrend(modnum,bp=np.array([a for a in range(21,160,20)]))
    ts = pd.Series(modnum, index=dates)
    ksmthrealnum = densCols(np.array(dates),ts.values,bandwidth=5)
    ksmthrealnum=pd.Series(ksmthrealnum)
    ksthrealnum=pd.DataFrame({"tsmod":ts,"ksmoothmod":ksmthrealnum})
    fig.add_trace(go.Scatter(y=ksthrealnum["tsmod"],x=ksthrealnum.index,name="tsmod"),row=3,col=1)
    fig.add_trace(go.Scatter(y=ksthrealnum["ksmoothmod"],x=ksthrealnum.index,name="ksmoothmod"),row=3,col=1)
    realnum1=detrend(imgnum,bp=np.array([a for a in range(21,160,20)]))
    ts = pd.Series(imgnum, index=dates)
    ksmthrealnum = densCols(np.array(dates),ts.values,bandwidth=5)
    ksmthrealnum=pd.Series(ksmthrealnum)
    ksthrealnum=pd.DataFrame({"tsimg":ts,"ksmoothimg":ksmthrealnum})
    fig.add_trace(go.Scatter(y=ksthrealnum["tsimg"],x=ksthrealnum.index,name="tsimg"),row=4,col=1)
    fig.add_trace(go.Scatter(y=ksthrealnum["ksmoothimg"],x=ksthrealnum.index,name="ksmoothimg"),row=4,col=1)
    
    if(ref is not None):

        fig.add_trace(go.Scatter(y=ref,name="ref"),row=5,col=1)
    fig.show()




x=fmri_simulate_func(dim_data = [64, 64, 40], mask = None, 
                                   ons = [1, 21, 41, 61, 81, 101, 121, 141], 
                                   dur = [10, 10, 10, 10, 10, 10, 10, 10])
data=x["fmridata"]
fmri_time_series(data,[20,30,20],True,data[10,20,30,:])

    
  

        

