import numpy as np
import sys
# from math import prod
from fmri_simulate_func import fmri_simulate_func
import pandas as pd
from fmri_stimulus_helper import fmri_p_val,fmri_complex_p_val,fmri_on_off_volume,fmri_hrf_p_val
from statsmodels.stats.multitest import multipletests
def fmri_stimulus_detect(fmridata,stimulus_idx,rest_idx=None,method=None,fdr_corr=None,spatial_cluster_thr=None,spatial_cluster_size=None,ons=None,dur=None,mask=None):
    if(fmridata.shape is None):
        if(not all(np.iscomplex(fmridata)) and method in ["t-test","wolcoxon-test"]):
            p_val=fmri_p_val(fmridata,stimulus_idx,method)
            if(p_val is None):
                p_val=1
            return p_val
        elif(all(np.iscomplex(fmridata)) and method in ["HotellingsT2", "Wilks-Lambda", "gLRT"]):

            try:
                p_val=fmri_complex_p_val(fmri_complex_p_val(fmridata,stimulus_idx,method,ons,dur))[1]
            except Exception as e:
                print("there was an error in ",e.__class__)
        else:
            if(np.iscomplex(fmridata)):
                print("Invalid test type! You can only use HotellingsT2, Wilks-Lambda or gLRT for complex data!")
                sys.exit()
            else:
                print("Invalid test type you can only use t-test or wilcoxon on real data ")
                sys.exit()
    elif(len(fmridata.shape)<=3):
        p_dim=fmridata.shape[:-1]
        time_span=fmridata.shape[-1]
        fmri_mat=np.array(fmridata).reshape(np.prod(p_dim),time_span)
        p_vec=[]
        for i in range(fmri_mat.shape[0]):
            if ( not all(np.iscomplex(fmri_mat[i-1,:])) and method in ["t-test","wilcoxon-test"]):
                p_val=fmri_p_val(fmri_mat[i-1,:],stimulus_idx,method)
                if(p_val is  None):
                    p_val=1
            elif(all(np.iscomplex(fmri_mat[i-1,:])) and method in ["HotellingsT2", "Wilks-Lambda", "gLRT"]):
                try:
                    p_val=fmri_complex_p_val(fmri_mat[i-1,:],stimulus_idx,method,ons,dur)[1]

                except Exception as e:
                    print("Error is",e.__class__)
                    return 1
            else:
                if(np.iscomplex(fmridata)):
                    print("Invalid test type! You can only use HotellingsT2, Wilks-Lambda or gLRT for complex data!")
                    sys.exit()
                else:
                    print("Invalid test type you can only use t-test or wilcoxon on real data ")
                    sys.exit()
            
            p_vec.append(p_val)
        p_vec=np.array(p_vec)
        p_vec=p_vec.reshape(p_dim)
        return(p_vec)
    dim=fmridata
    dim1=dim.shape[0]
    dim2=dim.shape[1]
    dim3=dim.shape[2]
    time_span=dim.shape[3]
    p_val_3d=np.ones((dim1,dim2,dim3))
    on_idx=stimulus_idx
        
    off_idx={a for a in range(time_span)}.difference(set(on_idx))
    if(rest_idx is not None):
        off_idx=rest_idx

    if ( not all(np.iscomplex(fmri_mat[i-1,:])) and method in ["t-test","wilcoxon-test"]):
        if(method in ["t-test","wilcoxon-test"]):
            for x in range(dim1):
                 for y in range(dim2):
                        for z in range(dim3):
                             if(mask is not None):
                                 if(mask[x-1,y-1,z-1]==1):
                                    p_val_3d[x-1,y-1,z-1]=fmri_p_val(fmridata[x-1,y-1,z-1,:],stimulus_idx,method)
                                 else:
                                     p_val_3d[x-1,y-1,z-1]=fmri_p_val(fmridata[x-1,y-1,z-1,:],stimulus_idx,method)        

                 p_val_3d[x,:,:]=p_val_3d[x,:,:][np.isnan(p_val_3d[x,:,:])]=1

        elif(method=="on_off_diff" and mask is not None):
                volume_df=pd.DataFrame({"x":None,"y":None,"z":None,"volume":None})
                for i in range(dim1):
                    for j in range(dim2):
                        for k in range(dim3):
                            if (mask[i-1,j-1,k-1]==1):
                                volume_df=np.concatenate((volume_df,[i,j,k,fmri_on_off_volume(fmridata,i,j,k)]),axis=0)
                volume_df.dropna()
                volume_df["x"]=int(volume_df["x"])                  

        elif(method=="HRF" & mask is not None):
            p_val_3d=fmri_hrf_p_val(fmridata,ons=ons,dur=dur)
        else:
            print("Invalid test type you can only use t-test or wilcoxon or on_off_diff or HRF for real data")
            sys.exit()
    else:
        if(method not in ["HotellingsT2","Wilks-lambda","gLRT"]):
            print("invalid test type you can only do hotellingst2 or glrt or wilks-lambda test on complex data")
            sys.exit()
        for i in range(dim1):
            for j in range(dim2):
                for k in range(dim3):
                    try:
                        p=1
                        if(mask is not None):
                            if(mask[i,j,k]==1):
                                p=int(fmri_complex_p_val(fmridata[i,j,k,]),stimulus_idx,method=method,ons=ons,dur=dur)
                            else:
                    
                               p=int(fmri_complex_p_val(fmridata[i,j,k,:],stimulus_idx,method,ons,dur))
                        
                    except Exception as e:
                        raise Exception(e)
                    
                    p_val_3d[i,j,k]=p
            p_val_3d[i,:,:]=p_val_3d[i,:,:][np.isnan(p_val_3d[i,:,:])]=1
    if(fdr_corr is not None):
            p_val_3d=p_val_3d.reshape(dim1*dim2*dim3)
            p_val_3d=multipletests(p_val_3d,method=fdr_corr)
            p_val_3d=np.array(p_val_3d)
            p_val_3d=p_val_3d.reshape((dim1,dim2,dim3))

    if(spatial_cluster_size is not None):
            # spatial_cluster_filter=cluster_threshold(1-p_val_3d,level_thr=spatial_cluster_thr,size_thr=spatial_clsuter_size)
            spatial_cluster_filter=np.random.rand(p_val_3d.shape[0],p_val_3d.shape[1],p_val_3d.shape[2])
            p_val_3d=1-((1-p_val_3d)*spatial_cluster_filter)

    return(p_val_3d)
x=fmri_simulate_func(dim_data = [64, 64, 40], mask = None, 
                                   ons = [1, 21, 41, 61, 81, 101, 121, 141], 
                                   dur = [10, 10, 10, 10, 10, 10, 10, 10])

on_idx=[a for a in range(160) if (int(a/10)%2==0)]
# print(on_idx)
data=x["fmridata"]
p_val_3d=fmri_stimulus_detect(x,stimulus_idx=x["on_time"])

print(p_val_3d)


