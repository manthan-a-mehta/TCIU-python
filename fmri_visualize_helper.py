from plotly.subplots import make_subplots
import plotly.graph_objects as go
import numpy as np
# def GTSplot(tsdata,newtitle="result",ylab="value",xlab="time",unit=None,ts_name=None,title_size=10,colo=None):
#     fig = go.Figure()

#     for i in range(tsdata.shape[1]):
#         tsd=tsdata[:,i-1]
#         tsn=tsname[i-1]
#         col=colo[i-1]
#         tsp=fig.add_trace(go.Scatter(x=, y=random_y2,
#                     mode='lines',
#                     name=tsn))


def fmri_split_ab_bl(vect, option="vector"):
    if(option=="list"):
        overallen=len(np.where(vect!=0))
        ab_len=len(np.where(vect>0))
        b1_len=len(np.where(vect<0))
        null_list=[x for x in range(overallen)]
        s=["#FFFF00", "#FFFF41", "#FFFF60" ,"#FFFF7A" ,"#FFFF92" ,"#FFFFA9" ,"#FFFFBF","#FFFFD4" ,"#FFFFEA" ,"#FFFFFF"]
        for i in range(b1_len):
            null_list[i]=[(i-1)/overallen,s[i]]
        s=[ "#FFFFFF", "#F0E5FF", "#E0CBFF", "#CFB1FF", "#BD98FF" ,"#A87FFF" ,"#9265FF","#774BFF" ,"#542EFF" ,"#0000FF"]
        for i in range(ab_len):
            null_list[b1_len-1+i]=[(b1_len-1+i-1)/overallen,s[i]]

    elif(option=="vector"):
        overallen=len(np.where(vect!=0))
        ab_len=len(np.where(vect>0))
        b1_len=len(np.where(vect<0))
        null_list=np.array([None]*overallen)
        s=["#FFFF00", "#FFFF41", "#FFFF60" ,"#FFFF7A" ,"#FFFF92" ,"#FFFFA9" ,"#FFFFBF","#FFFFD4" ,"#FFFFEA" ,"#FFFFFF"]
        for i in range(b1_len):
            null_list[i]=s[i]
        s=[ "#FFFFFF", "#F0E5FF", "#E0CBFF", "#CFB1FF", "#BD98FF" ,"#A87FFF" ,"#9265FF","#774BFF" ,"#542EFF" ,"#0000FF"]
        for i in range(ab_len):
            null_list[b1_len-2+i]=s[i-1]

    return null_list