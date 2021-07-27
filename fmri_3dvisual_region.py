import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import plotly.graph_objects as go
from utils import floor_dec,ceiling_dec
from matplotlib import pyplot as plt
from matplotlib import _cntr as cntr
def fmri_3dvisual_region(pval,mask,label_index,label_name,top_num=None,p_threshold=0.05,method="scale_p",multi_pranges=True,color_pal="YlOrRd",rank=None,title=None):
        """
        Title
        -----
        visualization of the 2D brain (axial, sagittal 
        and coronal) with the activated areas

        Description
        -----------
        a visualization method, using ggplot2 to draw the brain 
        from axial, sagittal and coronal view with activated area identified by p-values

        Parameters
        ----------
        pval:a 3D array of p-values used to plot activated area of the brain
        
        mask:np.ndarray; a 3D nifti or 3D array of data to show the shell of the brain
        The default is False.

        label_index:np.ndarray; a 1D array listing  the label number in the mask

        label_name:np.ndarray; a 1D array corresponding to the name of the label number in the mask

        top_num:float; None by default. that used for 1D p-values. If specified, the output will 
        show the top num significant regions.
        
        p_threshold:float; None or a numeric value that can be selected randomly below 0.05 to 
        drop all p-values above the threshold. If 'low5_percent' method is used, 
        make 'p_threshold' as None. The default is 0.05.
                
        method:string; a string that represents method for the plot. There are 3 options: 'min_max', 'scale_p' and 'low5_percent'. The default is 'scale_p'.
        'min_max' is to draw plot based on the color scale of the minimum and maximum of the p value; 
        'scale_p' is to draw the plot with fixed color scale for fixed range of p value; 
        'low5_percent' is to draw the plot for the smallest 5 percent of p value when all the p values are not significant.

        color_pal:string; the name of the color palettes provided by brew.pal. The default is "YlOrRd".

        multi_pranges:bool; an option under 'scale_p' method to decide whether there are at most 9 colors 
        in the legend for the ranges of p value, or at most 4 colors. 
        The default is True, choosing the larger number of colors for the plot.

        rank: float or np.ndarray; the method that how the trace is ranked. The default is NULL.
        There are 2 options: 'value' and a vector.
        'value' is to draw the 1D p-values by the values from smallest to largest.
        a vector is to specific the rank of the regions in 3D p-values plot.

        title:string; title of the plot
        
        details
        -------

        The function fmri_3dvisual_region is used to visualize the 3D plot of the brain 
        with activated parts region by region. When providing a 1D/3D p-values data, a 3D interactive
        plot with surface of the brain shell will be generated with either scatter points representing 
        different stimulated levels or large color pieces representing different regions of the brain. 
        When providing a list of two 3D array of p-values, two 3D interactive brains with different scatter
        points corresponding to the two input 3D p-values will be given.
                
        return
        ------

        the 3d plot of the fMRI data drawn by plotly

        examples
        --------
        $ pip install sympy

        import sympy
        t, x = sympy.symbols('t, x')
        func=sympy.sin(t)
        xs=np.linspace(0,10,num=10)
        ys=np.linspace(0,10,num=10)
        print(kimesurface_transform(func,xs,ys))       
        """
        fig = go.Figure()

        mask=np.array(mask)
        xdim=mask.shape[0]
        ydim=mask.shape[1]
        zdim=mask.shape[2]
        pval=np.array(pval)
        if(title is not None):
            plt.title(title)
        if isinstance(pval, list): 
            pval1=pval[0]
            pval2=pval[1]
            pva1=np.array(pval1)
            pval2=np.array(pval2)
            flag=True
            # pval=np.array(pval)
            pval=np.ones((2*xdim,ydim,zdim))
            for i in range(xdim):
                for j in range(ydim):
                    for k in range(zdim):
                        pval[i,j,k]=pval1[i,j,k]
                        pval[i+xdim,j,k]=pval2[i,j,k]

            def newContour(z,struc_3d=newMask):
                dim_maskz=np.array(struc_3d[:,:,z-1]).shape
                x=np.linspace(1,dim_maskz[0],retstep=1)
                y=np.linspace(1,dim_maskz[1],retstep=1)
                contour_pt=cntr.Cntr(x, y, z)
                if(len(contour_pt)!=0):
                    boundry_pt_df=pd.concat(contour_pt[0]["x"],contour_pt[0]["y"],z)
            newMask=np.zeros((xdim,ydim,zdim))
            for i in range(xdim):
                for j in range(ydim):
                    for k in range(zdim):
                        if(mask[i,j,k]!=0):
                            newMask[i,j,k]=1
            
            outerBoundry=pd.concat([newContour(x) for x in range(newMask.shape[2])])
            outerBoundry.columns=["x","y","z"]
            outerTri=Delaunay(np.array(boundryContour))
            faceColor=["#F0FFFF"]*len(triContour[:,0])
            fig.add_trace(go.Scatter(outerBoundry,x=x,y=y,z=z,i=outerTri[:,0]-1,j=outerTri[:,1]-1,k=outerTri[:,2]-1,type="mesh3d",opacity=0.01,name="Brain Shell",facecolor=faceColor,contour={show=True,color="#000",width=15},showlegend=True))




            if(flag):
                innerMask=np.zeros((2*xdim,ydim,zdim))
                for i in range(xdim):
                    for j in range(ydim):
                        for k in range(zdim):
                            temp=mask[i,j,k]
                            for index in range(label_index):
                                if(temp==label_index[index]):
                                    innerMask[i,j,k]=index
                                    innerMask[i+xdim,j,k]=index
                                    break
            else:
                innerMask=np.zeros((xdim,ydim,zdim))
                for i in range(xdim):
                    for j in range(ydim):
                        for k in range(zdim):
                            temp=mask[i,j,k]
                            for index in range(label_index):
                                if(temp==label_index[index]):
                                    innerMask[i,j,k]=index
                                    break
            if not (isinstance(pval[0],list)):
                color_choice=
                if(top_num is not None):
                
                oldPval=pval
                newPval=sorted(pval)
                colorPval=np.zeros(len(pval))
                ranking=np.zeros(len(pval))
                for i in range(len(pval)):
                    for j in range(len(pval)):
                        if(oldPval[i]==newPval[j]):
                            if(top_num is not None and j>top_num):
                                colorPval[i]=-1
                            else:
                                colorPval[i]=j
                            ranking[j]=i
                if(rank is not None and rank=="value"):
                    ranking=ranking
                else:
                    ranking=np.linspace(1,len(pval),retstep=1)
                for x in ranking:
                    if(colorPval[x]>0):
                        newMask=np.zeros((xdim,ydim,zdim))
                    for i in range(xdim):
                        for j in range(ydim):
                            for k in range(zdim):
                                if(innerMask[i,j,k]==x):
                                    newMask[i,j,k]=1
                    boundryContour=pd.Dataframe()   
            else:
                dim_pval=pval.shape
                rep=np.repeat(np.linspace(1,dim_pval[1]),dim_pval[0])
                pval_df=pd.DataFrame({"x":np.repeat(np.linspace(1,dim_pval[0],retstep=1),dim_pval[1]*dim_pval[2]),"y":np.repeat(rep,dim_pval[2]),"z":np.repeat(np.linspace(1,dim_pval[2],retstep=1),dim_pval[1]*dim_pval[0])})
                pval_df["p_val"]=pval

                if(method=="scale_p"):
                    pval_df=pval_df[pval_df["p_val"]<=p_threshold]
                    if(multi_pranges==False):
                        
                        pval_df["cut_invs"]=pd.cut(pval_df["p_val"],[1e-7,1e-5,1e-3,5e-2])
                    else:
                        pval_df["cut_invs"]=pd.cut(pval_df["p_val"],[0,1e-8,1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,5e-2])
                    if(len(set(pval_df["cut_invs"]))<=4):
                        print("The number of p ranges is originally less than 4, it is recommended to turn 'multi_pranges' as FALSE.")                
                elif(method=="low5_percent"):
                    quantile5_pt=pval_df.quantile(0.05)
                    pval_df=pval_df[pval_df["p_val"]<quantile5_pt]
                    p_val_max_minus_min=max(pval_df["p_val"])-min(pval_df["p_val"])
                    if(p_val_max_minus_min!=0):
                        cut_pts=min(pval_df["p_val"])+np.linspace(0,9,retstep=1)*(p_val_max_minus_min/9)
                        cut_pts[len(cut_pts)-1]=ceiling_dec(cut_pts[len(cut_pts)],2)
                        cut_pts[0]=floor_dec(cut_pts[0],2)
                        cut_pts=np.unique([np.round(x,3) for x in cut_pts])
                        pval_df["cut_invs"]=pd.cut(pval_df["cut_invs"],bins=cut_pts)
                    else:
                        pval_df["cut_invs"]=np.round(min(pval)df["p_val"],3)
                        pval_df["colorgrp"]=1
                
            if(multi_pranges==False):
                color_choice=list(sns.color_palette(color_choice,9))[::2]
                color_choice.reverse()
                color_choices=sns.color_palette(color_choice)
            else:
                color_choices=sns.color_palette(color_choice,9)
                color_choices.reverse()
            pval_df["corresp_color"]=color_choices[pval_df["colorgp"]]
            pvalX=pval_df["x"]
            pvalY=pval_df["y"]
            pvalZ=pval_df["z"]
            groupname=[]
            groupindex=[]
            for i in range(len(pvalX)):
                tempIndex=innerMask[pvalX[i]-1,pvalY[i]-1,pvalZ[i]-1]
                if (not any(item in groupindex for item in tempIndex)):
                    groupindex=groupindex.extend(tempIndex)
                add_name=label_name[tempIndex]
                groupname=[groupname,add_name]
            pval_df["groupname"]=groupname
            for name in np.sort(np.unique(pval_df["groupname"])):
                pts_grp=pval_df[pval_df["groupname"]==name]
                pts_grp=pts_grp.sort_values(by="colorgrp",inplace=True)
                size=[]
                for i in range(len(pts_grp["colorgrp"])):
                    size.extend(np.flip([i for i in range(len(color_choices))]))
                    size=size[pts_grp.loc[i,"colorgrp"]]=2*(int(multi_pranges==False)+2)
                
                fig.add_trace(go.Scatter(x=pts_grp["x"],y=pts_grp["y"],z=pts_grp["z"],mode="markers",legendgroup=name,marker={opacity=0.6,symbol="circle",size=size,color=pts_grp["corresp_color"],line={color=pts_grp["corresp_color"],width=0.3},showlegend=False}
            if(rank is not None):
                ranking=[]
                for i in rank:
                   if (not any(item in groupindex for item in i)):
                       ranking.append(i)
            else:
                ranking=groupindex
            for x in ranking:
                newMask=np.zeros((xdim,ydim,zdim))
                for i in range(xdim):
                    for j in range(ydim):
                        for k in range(zdim):
                            if(innerMask[i,j,k]==x):
                                newMask[i,j,k]=1
                                break
            boundryContour=pd.concat([newContour(x) for x in range(newMask.shape[2])])
            boundryContour.columns=["x","y","z"]
            triContour=Delaunay(np.array(boundryContour))
            faceColor=["#F0FFFF"]*len(triContour[:,0])
            fig.add_trace(go.Scatter(boundryContour,x=x,y=y,z=z,type="mesh3d",opacity=0,facecolor=faceColor,name=label_name[x],legendgroup=label_name[x],contour={show=True,color="#000",width=15},showlegend=True))
            return figure


