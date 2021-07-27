import numpy as np
# from rpy2.robjects.packages import importr
# from rpy2.robjects.vectors import FloatVector
import matplotlib.pyplot as plt
from fmri_pval_comparison_internal import fmri_pval_comparison_internal
from p_value_adjustment import correct_pvalues_for_multiple_testing
from plotnine import ggplot
# stats = importr('stats')
import matplotlib.pyplot as plt

def fmri_post_hoc(p_val_3d,spatial_cluster_size,spatial_cluster_thr,show_comparison = False,fdr_corr=None):
    p_val_3d=np.array(p_val_3d)
    dim_1=p_val_3d.shape[0]

    dim_2=p_val_3d.shape[1]
    dim_3=p_val_3d.shape[2]
    spatial_cluster={"size":spatial_cluster_size,"thr":spatial_cluster_thr}
    p_val_3d_original=np.copy(p_val_3d)
    gg_list=[]
    figs=[]
    axs=[]
    if(fdr_corr is not None):
        p_val_3d.shape=(1,dim_1*dim_2*dim_3)
        p_val_3d= correct_pvalues_for_multiple_testing(p_val_3d,fdr_corr)
        p_val_3d.shape=(dim_1,dim_2,dim_3)
        gg_list.append(fmri_pval_comparison_internal(p_val_3d_original,p_val_3d,names=["raw p values","p values after fdr correction"]))
        

    if(spatial_cluster_size is not None and spatial_cluster_thr is not None):
        sh=p_val_3d.shape
        spatial_cluster_filter=np.random.rand(sh[0],sh[1],sh[2])
        p_val_3d=1-((1-p_val_3d)*spatial_cluster_filter)
        gg_list.append(fmri_pval_comparison_internal(p_val_3d_original,p_val_3d,names=["raw p values","p values after post hoc"]))
        # fig2=plt.gcf()
        # axs.append(plt.gca())


    # if(show_comparison==True):
    #     if(len(gg_list)==2):
    #         display(gg_list[0].draw(),gg_list[1].draw())
    #     elif(len(gg_list)==1):
    #         print(gg_list[0])

    return p_val_3d
p_val_3d=np.random.rand(64,64,40)       
fmri_post_hoc(p_val_3d,0.05,0.05,True,"Bonferroni")





                

                