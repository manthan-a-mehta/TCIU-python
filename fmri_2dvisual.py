import math,sys
from numbers import Number
import math
import numpy as np
import pandas as pd
import itertools
from fmri_3dvisual import fmri_3dvisual
from plotnine import ggplot,scale_fill_identity,ggtitle,theme,geom_density_2d,coord_fixed,aes
from plotnine.labels import labs
from plotnine.themes import element_text
from plotnine.scales import scale_alpha,scale_fill_gradient
def fmri_2dvisual(pval,
                axis_ls,
                mask,
                
                p_threshold = 0.05,
                legend_show = True,
                method = "scale_p",
                color_pal = "YlOrRd",
                multi_pranges = True,
                mask_width = 1.5,hemody_data = None):

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
                
                axis_ls: dictionary; a dictionary with two elements. The key is the character of 'x', 'y', 'z'.
                The value is an integer showing a specific slice on the fixed axis identified in the first element.
                
                hemody_data: np.array; a parameter to have the plot with/without hemodynamic contour. The default is None to make the plot 
                without hemodynamic contour, otherwise assign a 3D array of the hemodynamic data.
                
                mask:np.ndarray; a 3D nifti or 3D array of data to show the shell of the brain
                The default is False.
                
                p_threshold:float; None or a numeric value that can be selected randomly below 0.05 to 
                drop all p-values above the threshold. If 'low5_percent' method is used, 
                make 'p_threshold' as None. The default is 0.05.
                
                legend_show:bool; a logical parameter to specify whether the final plot has legend

                method:string; a string that represents method for the plot. There are 3 options: 'min_max', 'scale_p' and 'low5_percent'. The default is 'scale_p'.
                'min_max' is to draw plot based on the color scale of the minimum and maximum of the p value; 
                'scale_p' is to draw the plot with fixed color scale for fixed range of p value; 
                'low5_percent' is to draw the plot for the smallest 5 percent of p value when all the p values are not significant.

                color_pal:string; the name of the color palettes provided by brew.pal. The default is "YlOrRd".

                multi_pranges:bool; an option under 'scale_p' method to decide whether there are at most 9 colors 
                in the legend for the ranges of p value, or at most 4 colors. 
                The default is True, choosing the larger number of colors for the plot.

                mask_width:float; a numeric value to specify the width of mask contour. The default is 1.5.


                details
                -------

                The function fmri_2dvisual is used to find activated part of the brain 
                based on given p values from sagittal, axial and coronal view. When providing input of 
                the p-values, the specific plane and index to slice on, the mask data and 
                the hemodynamic data of the brain, a plot will be generated with the heat map 
                for the activated parts, the black contour showing the position of the brain, 
                and the blue contour representing the hemodynamic contour.
                
                return
                ------

                a plot drawn by ggplot2

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

                ls_0=list(axis_ls.keys())
                ls_1=list(axis_ls.values())
                
                def floor_dec(x,level=1):
                    return round(x - 5*10^(-level-1), level)
                
                def ceiling_dec(x,level=1):
                    return round(x+5*10^(-level-1), level)
                
                def prange_extract(name_range_one_minus_p):
                    numeric_range_one_minus_p=pd.DataFrame()
                    p_range=[]
                    for i in range(len(numeric_range_one_minus_p)):
                        p_2range=1-int(numeric_range_one_minus_p[i])
                        p_range.append("("+p_2range[0]+".1e,"+p_2range[1]+".1e]")
                    return(p_range)
                print(type(pval))
                if(type(pval)is not np.ndarray and type(pval) is not list):
                    print("line 40")
                    print("pval should be a 3d array")
                    sys.exit()
                print(type(axis_ls))
                if(type(axis_ls)is not dict and ls_0 not in ["x","y","z"] ):
                    print("'axis_ls' should be a list, with the first element as a string from 'x', 'y' or 'z'.")
                    sys.exit()

                elif(hemody_data is not  None):
                    if(type(hemody_data) is not list or len(hemody_data)!=3):
                        print("if 'hemody_data' is not null, then it should be a 3d array")

                elif(type(mask) is not  np.ndarray and len(mask)!=3):
                    print("'mask' should be a 3d array")  
                    sys.exit()
                
                elif(p_threshold is not None):
                    if(not isinstance(p_threshold,Number) or p_threshold>0.05 or p_threshold<=0):
                        print("'p_threshold should be Null or a numeric value'")    
                        sys.exit()
                elif(type(legend_show) is not bool):
                    print("legend show should be a logical True or False.")
                elif(method not in ["scale_p","min_max","low5_percent"]):
                    print("method should choose from 'scale_p', 'min_max' or 'low5_percent'")
                one_minus_p_3d=1-pval
                dim_xyz=one_minus_p_3d.shape
                dim_x=dim_xyz[0]
                dim_y=dim_xyz[1]
                dim_z=dim_xyz[2]
                dim_dict={"x":dim_x,"y":dim_y,"z":dim_z}
                dim_i_bound=dim_dict[ls_0[0]]
                contour_hemody_2d=None
                if(math.ceil(ls_1[1])!=ls_1[1] or ls_1[1]>dim_i_bound or ls_1[1]<=0):
                    print("the second element of axis_ls should be an integer that is not out of the range")
                if(ls_0[0]=="x"):
                    one_minus_p_3d=one_minus_p_3d.reshape((dim_x,dim_y*dim_z))
                    
                    d1=[i for i in range(1,dim_y+1)]*dim_z
                    s=[]
                    for i in range(1,dim_z+1):
                        for j in range(1,dim_y+1):
                            s.append(i)
                    mask_df=pd.DataFrame({"x":d1,"y":s})
                    one_minus_p_df=pd.DataFrame({"x":d1,"y":s})
                    one_minus_p_df["one_minus_p"]=one_minus_p_3d[ls_1[1]-1,:]
                    contour_mask_2d=mask[ls_1[0],:,:]
                    if(hemody_data is not None):

                        contour_hemody_2d=hemody_data[ls_1[1]-1,:,:]
                    contour_hemody_df=pd.DataFrame({"x":d1,"y":s})
                    title="Sagittal View of Brain for x="+str(ls_1[0])
                    plot_lab=labs(x="y",y="z",fill="p value")
                

                elif(ls_0[0]=="y"):
                    sh=one_minus_p_3d.shape
                    one_minus_p_3d=np.moveaxis(one_minus_p_3d,[0,1,2],[0,2,1])
                    one_minus_p_3d=one_minus_p_3d.reshape(dim_x*dim_z,dim_y)
                    d1=[i for i in range(1,dim_x+1)]*dim_z
                    s=[]
                    for i in range(1,dim_z+1):
                        for j in range(1,dim_x+1):
                            s.append(i)
                    mask_df=pd.DataFrame({"x":d1,"y":s})
                    one_minus_p_df=pd.DataFrame({"x":d1,"y":s})
                    one_minus_p_df["one_minus_p"]=one_minus_p_3d[ls_1[1]-1,:]
                    contour_mask_2d=mask[:,ls_1[1],:]
                    if(hemody_data is not None):

                        contour_hemody_2d=hemody_data[ls_1[1]-1,:,:]
                    contour_hemody_df=pd.DataFrame({"x":d1,"y":s})
                    title="Coronal view of Brain for y="+ls_1[0]
                    plot_lab=labs(x="x",y="z",fill="p value")

                elif(ls_0[0]=="z"):
                    one_minus_p_3d=one_minus_p_3d.reshape((dim_x*dim_y,dim_z))
                    d1=[i for i in range(1,dim_x+1)]*dim_y
                    s=[]
                    for i in range(1,dim_y+1):
                        for j in range(1,dim_y+1):
                            s.append(i)
                    mask_df=pd.DataFrame({"x":d1,"y":s})
                    one_minus_p_df=pd.DataFrame({"x":d1,"y":s})
                    one_minus_p_df["one_minus_p"]=one_minus_p_3d[ls_1[1]-1,:]
                    contour_mask_2d=mask[:,:,ls_1[2]]
                    if(hemody_data is not None):

                        contour_hemody_2d=hemody_data[ls_1[1]-1,:,:]
                    contour_hemody_df=pd.DataFrame({"x":d1,"y":s})
                    title="Axial View of Brain for z="+ls_1[0]
                    plot_lab=labs(x="x",y="y",fill="p value")
                
                mask_df["mask_val"]=None
                idx=0
                for j in set(mask_df["y"]):
                    for i in set(mask_df["x"]):
                        mask_df.loc[idx,"maskval"]=contour_mask_2d[i-1,j-1]
                        idx=idx+1
                contour_bin=0
                if(method in ["scale_p","low5_percent"]):
                    p_val_df=fmri_3dvisual(pval,mask,p_threshold,method,multi_pranges=multi_pranges,color_pal=color_pal)["pval_df"]

                    if(method=="scale_p"):
                        one_minus_p_df=one_minus_p_df[one_minus_p_df["one_minus_p"]>=1-p_threshold]
                        one_minus_p_df["p_val"]=1-one_minus_p_df["one_minus_p"]
                        if(one_minus_p_df.shape[0]==0):
                            print("There doesnt exist any valid data under this p_threshold, please change a p_threshold or change to low5_percent method ans make p_threshold to null")
                            sys.exit()
                        one_minus_p_df["cut_invs"]=pd.cut(one_minus_p_df["p_val"],bins=[0,1e-8,1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,5e-2])
                        one_minus_p_df["colorgrp"]=one_minus_p_df["cut_invs"].astype(int)
                        if(multi_pranges==False):
                            one_minus_p_df["cut_invs"]=pd.cut(one_minus_p_df["p_val"],bins=[0,1e-7,1e-5,1e-3,5e-2])
                            one_minus_p_df["colorgrp"]=one_minus_p_df["cut_invs"].astype(int)

                    elif(method=="low5_percent"):
                        one_minus_p_df["p_val"]=1-one_minus_p_df["one_minus_p"]
                        quantile5_pt=one_minus_p_df["p_val"].quantile(0.05)
                        one_minus_p_df=one_minus_p_df[one_minus_p_df["p_val"]<=quantile5_pt]
                        p_val_max_minus_min=max(one_minus_p_df["p_val"])-min(one_minus_p_df["p_val"])
                        if(p_val_max_minus_min!=0):
                            cut_pts=min(one_minus_p_df["p_val"])+np.array([i for i in range(0,10)])*p_val_max_minus_min/9
                            cut_pts[-1]=ceiling_dec(cut_pts[-1],2)
                            cut_pts[0]=floor_dec(cut_pts[0],2)
                            cut_pts=np.around(cut_pts,3)
                            one_minus_p_df["cut_invs"]=pd.cut(one_minus_p_df,bins=list(set(cut_pts)))
                            one_minus_p_df["colorgrp"]=one_minus_p_df["cut_invs"].astype(int)
                        else:
                            one_minus_p_df["cut_invs"]=str(round(min(one_minus_p_df["p_val"]),3))
                            for i in p_val_df["cut_invs"]:
                                one_minus_p_df["colorgrp"]=p_val_df[p_val_df["cut_invs"==1]]["colorgrp"]
                    
                    p_val_df=p_val_df[["colorgrp","corresp_color"]].drop_duplicates(subset=[["colorgrp","corresp_color"]])
                    one_minus_p_df=one_minus_p_df.merge(p_val_df,how="right",on="colorgrp")
                    x=list(set(one_minus_p_df["cut_invs"]))
                    x.reverse()
                    y=list(set(one_minus_p_df["corresp_color"]))
                    y.reverse()
                    motorinbrain=ggplot(data=one_minus_p_df,mapping=aes(x="x",y="y",fill="one_minus_p",alpha="one_minus_p"))+scale_fill_identity(labels=x,breaks=y,guide="legend")+plot_lab+geom_density_2d()+ggtitle(title)+theme(title=element_text(ha="center"))
                elif(method=="min_max"):
                    one_minus_p_df=one_minus_p_df[one_minus_p_df["one_minus_p"]>=1-p_threshold]
                    one_minus_p_df["p_val"]=1-one_minus_p_df["one_minus_p"]
                    if(one_minus_p_df.shape[0]==0):
                        print("There does not exist any valid data under this p_threshold . Please change a p_threshold or change to low5_percent and make p_threshold as NULL")

                    idx=list(one_minus_p_df[one_minus_p_df["one_minus_p"]!=0].index)
                    one_minus_p_min=min(one_minus_p_df.loc[idx,"one_minus_p"])
                    one_minus_p_max=max(one_minus_p_df.loc[idx,"one_minus_p"])
                    break_pt=[one_minus_p_min,(one_minus_p_min+one_minus_p_max)/2,one_minus_p_max]
                    motorinbrain=ggplot(data=one_minus_p_df,mapping=aes(x="x",y="y",fill="one_minus_p",alpha="one_minus_p"))+scale_alpha(range=(0,1),guide="F")+plot_lab+scale_fill_gradient(low="yellow",high="red",limits=[break_pt[0],break_pt[-1]],oob=np.nan,breaks=break_pt,labels=np.around(1-break_pt,4))+ggtitle(title)+theme(title=element_text(ha="center"))+geom_density_2d(data=mask_df,mapping=aes(x=mask_df["x"],y=mask_df["y"],z=mask_df["maskval"]),color="black",bins=1,size=mask_width)
                if(hemody_data is not None):
                    contour_hemody_df["modval"]=None
                    idx=0
                    for j in range(list(set(contour_hemody_df["y"]))):
                        for i in range(list(set(contour_hemody_df["x"]))):
                            contour_hemody_df.loc[idx,"modval"]=contour_hemody_2d[i,j]
                            idx=idx+1

                    motorinbrain=motorinbrain+geom_density_2d(data=contour_hemody_df,mapping=aes(x="x",y="y",z="modval"),color="blue4",bins=8,size=0.7)
                if(legend_show):
                    return(motorinbrain+coord_fixed())
                else:
                    return(motorinbrain+coord_fixed()+theme(legend_position="none"))
from fmri_simulate_func import fmri_simulate_func

# from fmri_stimulus_detect import fmri_stimulus_detect
x=fmri_simulate_func(dim_data = [64, 64, 40], mask = None, 
                                   ons = [1, 21, 41, 61, 81, 101, 121, 141], 
                                   dur = [10, 10, 10, 10, 10, 10, 10, 10])

# on_idx=[a for a in range(160) if (int(a/10)%2==0)]
# # print(on_idx)
# data=x["fmridata"]
p_val_3d=np.random.randn(64,64,40)

print(fmri_2dvisual(p_val_3d,{"x":35,"y":30,"z":22}, 
                       mask=np.random.randn(64,64,40), 
                      p_threshold = 0.05,hemody_data=None, legend_show = True, 
                      method = "scale_p",
                      color_pal = "YlOrRd", multi_pranges=True))


                        











