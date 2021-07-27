def fmri_pval_comparison_2d(pval_ls,pval_name_ls,axis_i_lses,hemody_data=None,mask,p_threshold=0.05,legend_show=True,method="scale_p",color_pal="Yl0Rd",multi_pranges=True,mask_width=1.5):

        if( not all(len(x)==3 for x in axis_i_ls) ):
            print("Every axis_i_ls element must have length 3 for each of the coordinates")
            sys.exit()




        len_idx_sets=(len(axis_i_ls)/3)
        axis_vec=np.repeat(["x","y","z"],len_idx_sets)         



        pl_name_vec=[]
        ing_pls_name_vec=[]
        idx=0
        for i in range(len(pval_ls)):
            for axis_ele in axis_vec:
                idx=idx+1
                try:

        for plt_item in ing_pls_name_vec:
            start_idx=idx*len(axis_vec)+1
            end_idx=(idx+1)*len(axis_vec)
            int_plt_str=pl_name_vec[start+index:end_index]
            idx=idx+1
            text_str=pval_name_ls[idx]
            int_plt_str_trueval=list(map(get,int_plt_str)
            int_plts_form=int_plt_str_trueval.append({"nrow":1,"ncol":1+len(axis_vec)})
            int_plts_form_wtext=int_plts_form.append({})
        if(legend_show==False):
            complement_p_cut=[]
            one_minus_p_range=

            label_p_range=[]
        for i in range(len(numeri_range_one_minus_p)):
            p_2range=1-int(numeric_range_one_minus_p[i])
            label_p_range[i]="({},{}]".format(p_2range[1],p_2range[0])
        p_range_num=pd.Series(label_p_range)
        p_range_num=prange_num.astype("category")
        p_range_num=p_range_num.sort_values()
        p_range_num=p_range_num.astype("int64")
        one_minus_p_color=sns.color_palette("YlOrRd",9)[p_range_num]

        p_range_num_df=pd.DataFrame({"x":p_range_num,"y":p_range_num,"color_i":one_minus_p_color})
        color_i=one_minus_p_color
        p=ggplot()+geom_title(mapping=aes(x=p_range_num_df["x"],y=p_range_num_df["y"],fill=p_range_num_df["color_i"]))
        +scale_fill_manual(value=label_p_range,breaks=np.flip(np.sort(np.unique(p_range_num_df["color_i"])))+theme(legend_position="bottom")

        ing_pls_name_trueval=
        ing_pls_name_form=legend_p.extend(ing_pls_name_trueval)
        fig, axes = plt.subplots(nrows=3, ncols=2, figsize=(7, 7))

        else:
            ing_pls_name_trueval=apply(ing_pls_name_vec,get)
            fig,axes=plt.subplot(nrows=3,ncols=2,figsize=(7,7))