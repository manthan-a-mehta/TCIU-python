import random
import numpy as np
import pandas as pd
# from rpy2.robjects.packages import importr
# from rpy2.robjects import FloatVector,r
# importr('fmri')
from hotelling.stats import hotelling_t2
from scipy.stats import ttest_ind,wilcoxon,laplace
from statsmodels.multivariate.manova import MANOVA
import math
from statsmodels.api import OLS
# from fmri_stimulus_glrt_helper import complex_gen_est_pval
from fmri_simulate_func import fmri_simulate_func
def fmri_p_val(fmridata,voxel_location=None,stimulus_idx=None,rest_idx=None,epoch_length=10,is_4d=True,test_type="t-test"):
    fmridata=np.array(fmridata)
    if(is_4d==True and voxel_location is not None):
        voxel=fmridata[voxel_location[0],voxel_location[1],voxel_location[2],:]
    else:
        voxel=fmridata
    time_span=len(voxel)  
    on_idx=stimulus_idx
    off_idx={a for a in range(1,time_span)}.difference(set(on_idx))
    if(rest_idx is not None):
        off_idx=rest_idx
    vox=voxel[on_idx]
    vox=np.array(vox).reshape(epoch_length,-1)
    group1_avg=np.mean(vox,1)
    voc2=voxel[list(off_idx)]

    voc2=np.array(voc2).reshape(epoch_length,-1)
    print(vox.shape,voc2.shape)
    group2_avg=np.mean(voc2,1)
    print(group2_avg)
    if(test_type=="t-test"):
        print(group1_avg)
        print(group2_avg)
        return ttest_ind(group1_avg,group2_avg,alternative="greater")[1]
    elif(test_type=="wilcoxon_test"):
        return wilcoxon(group1_avg,group2_avg,alternative="greater")[1]
    else:
        return "Please type a valid test type"
def fmri_complex_p_val(fmridata,voxel_location=None,method="HotellingsT2",stimulus_idx=None,rest_idx=None,is_4d=True,ons=None,dur=None):
    if(is_4d==True and voxel_location is not None):
        voxel=fmridata[voxel_location[0],voxel_location[1],voxel_location[2],:]
    
    else:
        voxel=fmridata
    # if(method=="glrt"):
    #     return (complex_gen_est_pval(voxel,onsets=ons,durations=dur))
    time_span=len(voxel)
    on_idx=stimulus_idx
    off_idx={a for a in range(1,time_span)}.difference(set(on_idx))
    off_idx=list(off_idx)
    if(rest_idx is not None):
        off_idx=rest_idx
    labels=np.ones(time_span)
    labels[off_idx]=0
    # labels=set(labels)
    Y1=np.concatenate((voxel[on_idx].real,voxel[on_idx].imag),axis=0)
    Y2=np.concatenate((voxel[off_idx].real,voxel[off_idx].imag),axis=0)
    print(Y1.shape)
    Y1=Y1.reshape((-1,2))
    Y2=Y2.reshape((-1,2))
    y12=np.concatenate((Y1,Y2),axis=0)
    y12=pd.DataFrame({"real":y12[:,0],"complex":y12[:,1]})
    y12["labels"]=labels
    # print(y12.shape,labels)
    if(method=="HotellingsT2"):
        test=hotelling_t2(y12)
    
    elif(method=="Wilks-Lambda"):
        maov = MANOVA.from_formula('real+complex~labels',y12)
        test=(((maov.mv_test().results)["labels"]["stat"]).loc["Wilks' lambda"])



        
    return test,method
    # return(test)
def fmri_hrf_p_val(fmridata,ons,dur,mask=None):
    fmridata=np.array(fmridata)
    sh=fmridata.shape
    dim1,dim2,dim3,dim4=sh[0],sh[1],sh[2],sh[3]
    fmridata_mod=np.absolute(fmridata)
    # dur=FloatVector(dur)
    # ons=FloatVector(ons)
    # fixed_stim=r['fmri.stimulus'](dim4,onsets=ons,durations=dur,TR=3)

    # design_matrix=r["fmri.design"](fixed_stim)
    
    # design_matrix=list(design_matrix)
    design_matrix=np.random.rand(dim4,4)
    
    p_val_3d=np.ones((dim1,dim2,dim3))

    for i in range(dim1):
        print("iteration: ",i)
        for j in range(dim2):
            for k in range(dim3):
                try:
                    p=1
                    if(mask is not None):
                        if(mask[i,j,k]==1):
                             fmri_d=fmridata_mod[i,j,k,:].reshape(-1,1)
                             print(fmri_d.shape)
                             lm_data=np.concatenate((fmri_d,design_matrix),axis=1)
                             print(lm_data.shape)
                             lm_data=pd.DataFrame(lm_data)
                             lm_data.columns=["Y","X1","X2","X3","X4"]
                             model=OLS(lm_data["Y"],lm_data[["X1","X2","X3","X4"]]).fit()
                             
                             html=(model.summary().tables[1].as_html())
                             p=(pd.read_html(html, header=0, index_col=0)[0][["coef","P>|t|"]].iloc[0,:][0],pd.read_html(html, header=0, index_col=0)[0][["coef","P>|t|"]].iloc[0,:][1])



                             
                    else:
                            
                            fmri_d=fmridata_mod[i,j,k,:].reshape(-1,1)
                            lm_data=np.concatenate((fmri_d,design_matrix),axis=1)
                            lm_data=pd.DataFrame(lm_data)
                            print(lm_data.shape)

                            lm_data.columns=["Y","X1","X2","X3","X4"]
                            model=OLS(lm_data["Y"],lm_data[["X1","X2","X3","X4"]]).fit()
                             
                            html=(model.summary().tables[1].as_html())
                            p=(pd.read_html(html, header=0, index_col=0)[0][["coef","P>|t|"]].iloc[0,:][0],pd.read_html(html, header=0, index_col=0)[0][["coef","P>|t|"]].iloc[0,:][1])


                except Exception as e:
                      raise Exception(e)
            # print(p)
            p_val_3d[i,j,k]=p[0]
        p_val_3d[i,:,:]=p_val_3d[i,:,:][np.isnan(p_val_3d[i,:,:])]=1
    return(p_val_3d)

def fmri_on_off_volume(data,x,y,z,coordinates="polar"):
    voxel_data=data[x-1,y-1,z-1,:]
    on_data=voxel_data[1]
    on_data=[]
    off_data=[]
    
    for i in range(160):
        if(int(i/10)%2==0):
            on_data.append(voxel_data[i])
        else:
            off_data.append(voxel_data[i])
    on_data=np.array(on_data)
    off_data=np.array(off_data)
    if(coordinates=="polar"):
        volum_diff=1/80*(np.sum(on_data**2)-np.sum(off_data**2))
        return(volum_diff)
    elif(coordinates=="cartesian"):
        phi_8_vec=np.tile((10,8),None)
        for t in range(10):
            random.seed(t)
            a=laplace.rvs(8)
            a.sort()
            phi_8_vec[:,t]=a
            for i in range(8):
                if(phi_8_vec[i,t]<-math.pi):
                    phi_8_vec[i,t]=-math.pi
                elif(phi_8_vec[i,t]>=math.pi):
                    phi_8_vec[i,t]=math.pi
        matrix_on=np.zeros((21,21))
        matrix_off=np.zeros((21,21))
        for t in range(10):
            for p in range(8):
                x=11+t*np.cos(phi_8_vec[p-1,t-1])
                y=11+t*np.sin(phi_8_vec[p-1,t-1])
                matrix_on[x,y]=on_data[(p-1)*10+t-1]
                matrix_off[x,y]=off_data[(p-1)*10+t-1]
        volume_diff=1/441*(np.sum(matrix_on**2) -np.sum(matrix_off**2))
        return(volume_diff)
    else:
        return "Please type a valid coordinate system type"  
         
x=fmri_simulate_func(dim_data = [64, 64, 40], mask = None, 
                                   ons = [1, 21, 41, 61, 81, 101, 121, 141], 
                                   dur = [10, 10, 10, 10, 10, 10, 10, 10])

on_idx=[a for a in range(160) if (int(a/10)%2==0)]
# print(on_idx)
data=x["fmridata"]
p_val=fmri_hrf_p_val(data,ons=None,dur=None)  
print(p_val)

                        

