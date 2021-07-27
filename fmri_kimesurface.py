import random
from scipy.stats import laplace
import numpy as np
import math
import pandas as pd
from PIL import Image
import cv2
import plotly.express as px
import plotly.graph_objects as go
from fmri_visualize_helper import fmri_split_ab_bl
def fmri_kimesurface(fmridata,voxel_location=None,is_4d=True):

    """
    title
    -----
    interactive graph object of 3d kimeseries

    description
    ----------
    use plotly to display in 3D the kime-series as 2d manifolds over cartesian domain

    parameters
    ----------

    fmridata:np.ndarray; a 4d array which contains spatial and temporal record of fmri result or a single real valued vector.

    voxel_location:a 3d array indicating the location of the brain.

    is_4d:bool: Default is True, if changed to false, need to input vector instead of an array.

    return
    -----

    an interactive plot of the 3d kimesurface

    example
    ------
    
    """
    phi_8_vec=np.full([8,10],None)
    for t in range(10):
        random.seed(t)
        phi_8_vec[:,t]=laplace.rvs(0,0.5,8)
        phi_8_vec[:,t]=np.sort(phi_8_vec[:,t])
        for i in range(8):
            if(phi_8_vec[i,t]<-(math.pi)):
                phi_8_vec[i,t]=-(math.pi)
            else:
                phi_8_vec[i,t]=math.pi
    if(is_4d==True and voxel_location is not None):
        voxel=fmridata[voxel_location[0],voxel_location[1],voxel_location[2],:]
    else:
        voxel=fmridata
    fmri_on=[]
    fmri_off=[]
    switch=[]
    nums=np.array([a for a in range(1,11)]*16)
    for i in range(len(voxel)):
        if(int(i/10)%2==0):
            fmri_on.append(voxel[i])
            
        else:
            fmri_off.append(voxel[i])
            
    switch=[True]*10

    switch=switch+[False]*10
    switch=switch*8
    phis=[]
    for i in range(8):
        new=np.tile(phi_8_vec[i,:],2)
        phis=np.concatenate((phis,new),axis=0)
    df3D_on=pd.DataFrame({"time":nums,"phi":phis,"switch":switch,"fmri":voxel})
    matrix_on=np.zeros((21,21))
    matrix_off=np.zeros((21,21))
    for t in range(10):
        for p in range(8):
            x=int(11+t*np.cos(phi_8_vec[p,t]))-1
            y=int(11+t*np.sin(phi_8_vec[p,t]))-1
            matrix_on[x,y]=fmri_on[((p-1)*10)+t]
            matrix_off[x,y]=fmri_off[(p-1)*10+t]
    x=np.zeros(80)
    y=np.zeros(80)
    i=0
    for t in range(10):
        for p in range(8):
            x[i]=11+t*np.cos(phi_8_vec[p,t])
            y[i]=11+t*np.sin(phi_8_vec[p,t])
            i=i+1

    matrix_on_smooth=Image.fromarray((matrix_on ) , 'L')
    matrix_on_smooth=np.array(matrix_on_smooth)/255
    matrix_on_smooth=(1/10000)*(cv2.GaussianBlur(matrix_on_smooth,(5,5),cv2.BORDER_DEFAULT))

    matrix_off_smooth=Image.fromarray((matrix_off),'L')
    matrix_off_smooth=np.array(matrix_off_smooth)
    matrix_off_smooth=(1/10000)*(cv2.GaussianBlur(matrix_off_smooth,(5,5),cv2.BORDER_DEFAULT))
    hovertext=pd.DataFrame({"x":[x for x in range(1,22)]*21,"y":[x for x in range(1,22)]*21,"height":(matrix_on_smooth.flatten())})
    custom_text=np.empty([21,21])
    custom_text[:]=None
    hovertextoff=pd.DataFrame({"x":[x for x in range(1,22)]*21,"y":[x for x in range(1,22)]*21,"height":(matrix_off_smooth.flatten())})
    custom_text_off=np.empty([21,21])
    custom_text_off[:]=None




    for x in range(21):
        for y in range(21):
            t=((x-11)**2+(y-11)**2)**0.5
            p=np.arctan2(y-11,x-11)
    x=np.cos(np.linspace(-math.pi,math.pi,21))
    y=np.sin(np.linspace(-math.pi,math.pi,21))
    xx2=11+np.outer(np.linspace(-10,10,21),x)
    yy2=11+np.outer(np.linspace(-10,10,21),y)
    zz2=matrix_on_smooth
    ww2=matrix_off_smooth
    dd2=matrix_on_smooth-matrix_off_smooth
    dd2scale=fmri_split_ab_bl(dd2)
    print(xx2.shape,yy2.shape,zz2.shape)

    # f= {family = "Courier New, monospace", size = 18, color = "black"}
    # x= {title = "k1", titlefont = f}
    # y= {title = "k2", titlefont = f}
    # z= {title = "fMRI Kime-series", titlefont = f}
    # zd= {title = "fMRI Kime-ON/OFF difference", titlefont = f}
    fig= go.Figure(data=[go.Surface(z=zz2, x=xx2, y=yy2,colorscale=["#FFFFFF","#0000FF"],hoverinfo="text",showlegend=False)])

    # fig.add_trace(px.scatter_3d(x=[11]*15,y=[11]*15,z=[x for x in range(0,15)]))
    # plot1.update_layout(dragmode="turntable",title="ON-kime surface/Kimesurface at a fixed voxel location")
    fig2= go.Figure(data=[go.Surface(z=ww2, x=xx2, y=yy2,colorscale=["#FFFFFF","#0000FF"],hoverinfo="text",showlegend=False)])
    fig3= go.Figure(data=[go.Surface(z=dd2, x=xx2, y=yy2,colorscale=["#FFFFFF","#0000FF"],hoverinfo="text",showlegend=False)])

    return fig,fig2,fig3
fig,fig1,fig2=fmri_kimesurface(np.random.randn(64,64,40,160),[10,11,12],True)
fig.show()
    
fig1.show()
fig2.show()

#almost left