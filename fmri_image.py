from scipy import signal
from plotly.subplots import make_subplots
import numpy as np
import pandas as pd
from kern_smooth import densCols
import plotly.graph_objects as go
from plotly.subplots import make_subplots
# from plotly.tools import mpl_to_plotly as ggplotly
from fmri_simulate_func import fmri_simulate_func
from plotnine import ggplot, aes, geom_vline,geom_hline,coord_cartesian
def fmri_image(fmridata,option="manually",voxel_location=None,time=None):

    """
    title
    -----
    Interactive graph objct of the fmri image

    description
    -----------
    fmri image visualization method, based on plotly

    Parameters
    ----------
    fmridata:4d array contains info on fmri spacetime image. the info shows the magnitude for the FMRI image.

    option: string; default is "manually". if choose "auto", then this function witll lead you to key in (x,y,z) parameters and time(time) parameter for this function to generate graphs.

    voxel_location:np.ndarray; a 3D array indicating the spatial location of the brain. If the option is auto, set the voxel location to None

    time: int; time location for the voxel

    details
    -------
    The Function fmri_image is used to create image for the front view, side and the top viewfpr the FMRI image. 
    When providing the 4D array the fmri spacetime image and input the x,y,z position of the voxel, three views of the image and time series of the voxel will be shown.

    return
    ------
    An interactive graph object of the fmri image.

    
    """
    fig = make_subplots(rows=2, cols=2)

    if(option=="auto"):
        print("print key in x,y,z and time in sequence")
        x=0
        y=0
        z=0
        t=0
        while(x>64 or x<1 or x%1!=0):
            x=input()
            if(x>64 or x<1 or x%1!=0):
                print("input x is not in range or is not an integer! Please retype!")
        while (y > 64 or y < 1 or y%1 != 0):
            y = input()
            if (y > 64 or y < 1 or y%1 != 0) :
                print("input y is not in range or is not an integer! Please retype!")
            
        
        while (z > 40 or z < 1 or z%1 != 0) :
            z = input()
            if (z > 40 or z < 1 or z%1 != 0) :
                print("input z is not in range or is not an integer! Please retype!")
            
        
        while (t > 160 or t < 1 or t%1 != 0) :
            t = input()
            if (t > 160 or t < 1 or t%1 != 0): 
                print("input time is not in range or is not an integer! Please retype!")
            
        
    elif (option == "manually"):
        x = voxel_location[0]
        y = voxel_location[1]
        z = voxel_location[2]
        t = time
    try1=fmridata[x,y,z,:]
    try1=signal.detrend(try1,bp=[a for a in range(21,160,20)])
    dates=[a for a in range(1,len(try1)+1)]
    ts=pd.Series(try1,index=dates)
    ksmth=densCols(np.array(dates),ts.values,bandwidth=5)
    fig.add_trace(go.Scatter(y=ksmth,x=dates,name="ksmooth"))
    fig.add_trace(go.Scatter(y=ts.values,x=dates,name="original"))
    




    fmridata=np.array(fmridata)
    zfmri=fmridata[:,:,z,t].T
    print(np.where(zfmri!=0))
    fig.add_trace(go.Contour(z=zfmri),row=1,col=2)
    fig.update_xaxes(range=[0, 64],row=1,col=2)
    fig.update_yaxes(range=[0, 64],row=1,col=2)
    fig.add_shape(type="line",
    x0=x,x1=x,y1=40,y0=0,
    line=dict(color="RoyalBlue",width=3),row=1,col=2)
    fig.add_shape(type="line",
    y0=y,y1=y,x1=64,x0=0,
    line=dict(color="RoyalBlue",width=3),row=1,col=2)


    xfmri=fmridata[x,:,:,t].T
    print(np.where(xfmri!=0))
    fig.add_trace(go.Contour(z=xfmri),row=2,col=1)
    fig.update_xaxes(range=[0, 64],row=2,col=1)
    fig.update_yaxes(range=[0, 40],row=2,col=1)
    fig.add_shape(type="line",
    x0=z,x1=z,y1=40,y0=0,
    line=dict(color="RoyalBlue",width=3),row=2,col=1)
    fig.add_shape(type="line",
    y0=y,y1=y,x1=64,x0=0,
    line=dict(color="RoyalBlue",width=3),row=2,col=1)
    
    yfmri=fmridata[:,y,:,t].T
    fig.add_trace(go.Contour(z=yfmri),row=2,col=2)
    fig.update_xaxes(range=[0, 64],row=2,col=2)
    fig.update_yaxes(range=[0, 40],row=2,col=2)


    fig.add_shape(type="line",
    x0=x,x1=x,y1=40,y0=0,
    line=dict(color="RoyalBlue",width=3),row=2,col=2)
    fig.add_shape(type="line",
    y0=z,y1=z,x1=64,x0=0,
    line=dict(color="RoyalBlue",width=3),row=2,col=2)
    
    # fig=make_subplots(rows=2)
    # fig.add_trace(TScore)
    # fig.add_trace(go.Scatter(zslice),row=1,col=2)
    # fig.add_trace(xslice,row=2,col=1)
    # fig.add_trace(yslice,row=2,col=2)
    fig.show()
    return fig


x=fmri_simulate_func(dim_data = [64, 64, 40], mask = None, 
                                   ons = [1, 21, 41, 61, 81, 101, 121, 141], 
                                   dur = [10, 10, 10, 10, 10, 10, 10, 10])
data=x["fmridata"]
fmri_image(data,"manually",[15,18,27],time=3)


    
