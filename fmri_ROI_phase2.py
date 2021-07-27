import numpy as np
import sys
import pandas as pd
from rrr import ReducedRankRegressor
def fmri_stimulus(time_span, onsets , durations , TR = 3):
    return [np.random.rand(1) for i in range(time_span)]
def fmri_design(fixed_stim, order ):
    initial=np.column_stack((fixed_stim,np.ones(len(fixed_stim))))
    initial=np.column_stack((initial,np.random.rand(len(fixed_stim)),order))
    return initial


def fmri_ROI_phase2(fmridata,label_mask,label_dict,stimulus_idx,stimulus_dur,fmri_design_order=2,fmri_stimulus_TR=3,rrr_Rank=3,method="t-test",parallel_computing=False,ncor=max(detectCores()-2,1)):
    fmridata=np.array(fmridata)
    sh=fmridata.shape
    if(len(sh)!=4):
        print("fmridata should be a 4d array")
        sys.exit()
    if(not isinstance(label_mask,list) or len(label_mask.shape)!=3):
        print("mask should be 3d array or a list")
        sys.exit()
    elif(not all([label_mask.shape[a]==sh[a] for a in range(3)])):
        print("the dimenstion of a label_mask should be the same as the first three dimensions of fmridata")
        sys.exit()
    if(label_dict.shape[1]<2):
        print("label_dict should have atleast 2 colums as indices and names of the ROI")
    if(not isinstance(label_dict[:,0],int) and not isinstance(label_dict[:,1],object)):
        print("label_dict should have first column as integer and second as a categorcal type")

    def ROI_bounding_box(fmridata,label_mask,label_id):
        fmridata=np.array(fmridata)
        fmridata=np.mod(fmridata)
        sh=fmridata.shape
        timespan=sh[3]
        ROI_index=pd.DataFrame(np.where(label_mask==label_id))
        ROI_index.columns=["x","y","z"]
        x_min=min(ROI_index["x"])
        x_max=max(ROI_index["x"])
        y_min=min(ROI_index["y"])
        y_max=max(ROI_index["y"])
        z_min=min(ROI_index["z"])
        z_max=max(ROI_index["z"])
        ROI_index_move=pd.DataFrame(np.column_stack(ROI_index.iloc[:,0]-x_min+1,ROI_index.iloc[:,1]-y_min+1,ROI_index.iloc[:,2]-z_min+1))
        ROI_index_move.columns=["x","y","z"]
        dim_for_block=(x_max-x_min+1,y_max-y_min+1,z_max-z_min+1)
        bounding_box=np.zeros((dim_for_block,time_span))
        for t in range(timespan):
            bounding_box[np.array(np.column_stack(ROI_index_move,t))]=fmridata[np.array(np.column_stack((ROI_index,t)))]

        return bounding_box,ROI_index,ROI_index_move
    from scipy import stats
    def block_p_value(BOLD_coef,time_span,num_of_predictors):
        n=time_span
        dim_for_block=BOLD_coef.shape
        p=num_of_predictiors
        p_value=np.empty(dim_for_block)
        t_value=BOLD_coef/np.std(BOLD_coef)
        p_value=2*stats.t.cdf(abs(t_value),df=n-p-1)
        return p_value
    sh=fmridata.shape
    time_span=sh.shape[3]
    fixed_stim=fmri_stimulus(time_span,stimulus_idx,durations=stimulus_dur,TR=fmri_stimulus_TR)
    X_tensor=fmri_design(fixed_stim,order=fmri_design_order)
    X_tensor=np.array(X_tensor)
    X_tensor=X_tensor.reshape((time_span,fmri_design_order+2,1))
    overall_p_value=np.ones((sh[0:3]))
    label_list=label_dict.iloc[:,0]
    label_name=label_dict.iloc[:,1]
    if(method=="t-test"):
        V=[]
        if(parallel_computing==True):
            # cl=makeCluster(ncor)
            # registerDoparalled(cl)
            
            for i in range(len(label_list)):
                label_id=label_list[i]
                ROI=ROI_bounding_box(fmridata,label_mask,label_id)
                Y_tensor=np.transpose(ROI[0],[3,0,1,2])
                obj=ReducedRankRegressor(X_tensor,Y_tensor,rrr_rank)
                BOLD_coef=(obj.A)
                BOLD_coef=np.array(BOLD_coef)
                BOLD_coef=BOLD_coef[0,:,:,:]
                
                p_value=block_p_value(BOLD_coef,time_span,fmri_design_order+2)
                V.append((p_value,ROI))
            for i in range(label_list):
                overall_p_value[V[i][1][1]]=np.array(V[i][1])[V[i][1][2]]
        else:
            for i in range(label_list):
                label_id=label_list[i]
                ROI=ROI_bounding_box(fmridata,label_mask,label_id)
                Y_tensor=np.transpose(ROI[0],(3,0,1,2))
                obj=ReducedRankRegressor(X_tensor,Y_tensor,rrr_rank)
                BOLD_coef=(obj.A)
                BOLD_coef=np.array(BOLD_coef)
                BOLD_coef=BOLD_coef[0,:,:,:]
                p_value=block_p_value(BOLD_coef,time_span,fmri_design_order+2)
                overall_p_value[V[i][1][1]]=np.array(V[i][1])[V[i][1][2]]

            
    elif(method=="corrected-t-test"):
        if(parallel_computing==True):
            # cl=makeCluster(ncor)
            # registerDoParallel(cl)
            v=[]
            for i in range(len(label_list)):
                label_id=label_list[i]
                ROI=ROI_bounding_box(fmridata,label_mask,label_id)
                Y_tensor=np.transpose(ROI[0],(3,0,1,2))
                obj=ReducedRankRegressor(X_tensor,Y_tensor,rrr_rank)
                BOLD_coef=(obj.A)
                BOLD_coef=np.array(BOLD_coef)
                BOLD_coef=BOLD_coef[0,:,:,:]
                bb=ROI[0]
                sh=bb.shape
                bb=bb.reshape((sh[2],sh[3],sh[0],sh[1]))
                a=np.zeros((sh[0],sh[2],sh[3]))
                for i in range(sh[3]):
                    for j in range(sh[2]):
                        for k in range(sh[0]):
                            a[k,j,i]=np.std(bb[k,:,j,i])
                corrected_BOLD=BOLD_coef/a
                corrected_BOLD[np.where(not np.isfinite(corrected_BOLD))]=0
                p_value=block_p_value(corrected_BOLD,time_span,fmri_design_order+2)
                v.append((p_value,ROI))
            for i in range(len(label_list)):
                overall_p_value[V[i][1][1]]=np.array(V[i][1])[V[i][1][2]]
        else:
            for i in range(len(label_list)):
                label_id=label_list[i]
                ROI=ROI_bounding_box(fmridata,label_mask,label_id)
                Y_tensor=np.transpose(ROI[0],(3,0,1,2))
                obj2=ReducedRankRegressor(X_tensor,Y_tensor,rrr_rank)
                BOLD_coef=(obj2.A)
                BOLD_coef=np.array(BOLD_coef)
                BOLD_coef=BOLD_coef[0,:,:,:]
                bb=ROI[0]
                sh=bb.shape
                bb=bb.reshape((sh[2],sh[3],sh[0],sh[1]))
                a=np.zeros((sh[0],sh[2],sh[3]))
                for i in range(sh[3]):
                    for j in range(sh[2]):
                        for k in range(sh[0]):
                            a[k,j,i]=np.std(bb[k,:,j,i])
                corrected_BOLD=BOLD_coef/a
                corrected_BOLD[np.where(not np.isfinite(corrected_BOLD))]=0
                p_value=block_p_value(corrected_BOLD,time_span,fmri_design_order+2)
                overall_p_value[V[i][1][1]]=np.array(V[i][1])[V[i][1][2]]
    return overall_p_value

    



                
                