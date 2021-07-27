import sys
import pandas as pd
import numpy as np
from scipy.stats import ttest_1samp
from fmri_simulate_func import fmri_simulate_func
def fmri_ROI_phase1(fmridata,stimulus_idx,label_mask=None,label_dict=None,rest_idx=None,p_threshold=0.05):
    if(len(fmridata.shape)!=4):
        print("fmridata should be a 4d array")
        sys.exit()
    if(not isinstance(label_mask,np.ndarray) or len(label_mask.shape)!=3):
        print("The dimension of 'label_mask' should be the same as the first three dimension of 'fmridata'.")
        sys.exit()
    elif(not fmridata.shape[0:3]==label_mask.shape):
        print(fmridata.shape[0:3],label_mask.shape)
        print('The dimension of label_mask should be the same as the first three dimension of fmridata')
        sys.exit()
    # elif(not  isinstance(label_dict,(np.ndarray,list)) or  isinstance(label_dict,pd.DataFrame)):
    #     print("ROI_label_dict' should be an array or matrix or dataframe.")
    #     sys.exit()
    elif(label_dict.shape[1]<2):
        print("ROI_label_dict' should have at least two columns as indices and names of the ROI.")
    # elif(not isinstance(label_dict[:,0],int) and isinstance(label_dict,list)):

        # label_dict=pd.DataFrame(np.array(label_dict))
    fmridata=np.array(fmridata)
    time_span=fmridata.shape[3]
    on_idx=np.array(stimulus_idx)
    print(type(on_idx[0]))
    off_idx=set([a for a in range(1,time_span+1)]).difference(on_idx)
    if(rest_idx is not None):
        off_idx=rest_idx   
    off_idx=np.array(off_idx) 
    index=label_dict["index"]
    y_on_off=fmridata[:,:,:,on_idx]
    shapes=y_on_off.shape
    y_on_off=y_on_off.reshape((shapes[0]*shapes[1]*shapes[2],shapes[3]))
    mean_on_off=np.mean(y_on_off,axis=1)
    sd_on_off=np.std(y_on_off,axis=1)
    print(mean_on_off.shape)
    sd_on_off=sd_on_off+0.001
    cnr=mean_on_off/sd_on_off
    
    cnr=cnr.reshape((shapes[0],shapes[1],shapes[2]))
    pval_t=[]
    print(label_mask==index[0])
    for j in range(len(index)):
        pval_t.append(ttest_1samp(cnr[label_mask==index[j]],0)[1])
    all_roi=pd.DataFrame({"name":label_dict["name"],"pval_t":pval_t},index=index)
    all_roi=all_roi.sort_values(by="pval_t")
    sign_roi=all_roi[all_roi["pval_t"]<=p_threshold/len(index)]
    result={"all_roi":all_roi,"sign_roi":sign_roi}
    return result
x=fmri_simulate_func(dim_data = [64, 64, 40], mask = None, 
                                   ons = [1, 21, 41, 61, 81, 101, 121, 141], 
                                   dur = [10, 10, 10, 10, 10, 10, 10, 10])
data=x["fmridata"]
label_dict=pd.DataFrame({"index":[1,0],"name":["on","off"]})

print(fmri_ROI_phase1(data,x["ons"],label_mask=x["mask"],label_dict=label_dict))
