import seaborn as sns
import plotly.express as px
from fmri_3dvisual import fmri_3dvisual
def fmri_pval_comparison_3d(pval_3d_ls,
                                   mask,
                                   p_threshold,
                                   method_ls,
                                   color_pal_ls = ["YlOrRd", "YlGnBu"],
                                   multi_pranges = True):
                                    plot_comp1=fmri_3dvisual(pval_3d_ls[0],mask,p_threshold[0],method_ls[0],color_pal_ls[0],multi_pranges)["plot"]
                                    df_pval2=fmri_3dvisual(pval_3d_ls[1],mask,p_threshold[1],method_ls[1],color_pal_ls[1],multi_pranges)["pval_df"]

                                   
                                    if(multi_pranges==False):
                                        color_choice=list(sns.color_palette)[::2]
                                        color_choice.reverse()
                                        color_choices=sns.color_palette(color_choice)
                                    else:
                                        color_choice.reverse()
                                        color_choices=sns.color_palette(color_choice)
                                    for i in set(df_pval2["colorgrp"]):
                                        pts_grp=df_pval2[df_pval2["color_grp"]==i]
                                        sizes=[i for i in range(1,len(color_choice))]
                                        sizes.reverse()
                                        size=sizes[i]+2*int(multi_pranges)
                                        plot_comp1=px.scatter_3d(pts_grp,"x","y","z",color="corresp_color",title="p value in"+str(df_pval2["cut_invs"][i]),opacity=0.6,symbol="triangle",size=size,width=0.3)
                                        plot_comp1.show()

                                    return plot_comp1

                        
