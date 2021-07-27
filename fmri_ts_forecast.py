from scipy import signal
import numpy as np
import pandas as pd
from kern_smooth import densCols

import plotly.graph_objects as go
from pmdarima.arima import auto_arima
from fmri_simulate_func import fmri_simulate_func
def fmri_ts_forecast(fmridata,voxel_location,cut=10):
    layout = {
  "title": "Forecast from Auto.Arima", 
  "xaxis": {
    "title": "Year", 
    "domain": [0, 1]
  }, 
  "yaxis": {
    "title": "C:/Users/cip/Documents/Visual Studio 2015/WebSites/WebSite1/data/France/BabyCare.txt", 
    "domain": [0, 1]
  }, 
  "margin": {
    "b": 40, 
    "l": 60, 
    "r": 10, 
    "t": 25
  }
}
    data=signal.detrend(fmridata[voxel_location[0],voxel_location[1],voxel_location[2],:],bp=[a for a in range(21,160,20)])
    train=data[2:133]
    train_dates=[a for a in range(2,133)]
    test=data[133:160]
    dates=[a for a in range(133,160)]
    ts=pd.Series(test,index=dates)
    ks_train = densCols(np.array(train_dates),train,bandwidth=5)
    ks_test=densCols(np.array(dates),test,bandwidth=5)
    arima_model=auto_arima(ks_train,stepwise=False)
    prediction,conf_95=arima_model.predict(n_periods=27,return_conf_int=True)
    prediction,conf_80=arima_model.predict(n_periods=27,return_conf_int=True,alpha=0.2)

    # print(type(prediction),conf)
    fig=go.Figure(layout=layout)
    trace1 = {
  "line": {
    "color": "rgba(0,0,0,1)", 
  }, 
  "mode": "lines", 
  "name": "observed", 
  "type": "scatter", 
  "x": train_dates, 
  "y":ks_train,
  "xaxis": "x", 
  "yaxis": "y"
}
    trace2 = {
#   "fill": "toself", 
  "line": {
    "color": "green", 
  }, 
  "mode": "lines", 
  "name": "test_true", 
  "type": "scatter", 
  "x": dates, 
  "y": ks_test, 
  "xaxis": "x", 
  "yaxis": "y", 
  "hoveron": "points"
}
    trace3 = {
  "fill": "toself", 
  "line": {
    "color": "red", 
  }, 
  "mode": "lines", 
  "name": "test_predicted", 
  "type": "scatter", 
  "x": dates, 
  "y": prediction, 
  "xaxis": "x", 
  "yaxis": "y", 
  "hoveron": "points"
}
    trace4 = {
  "fill": "toself", 
  "line": {
    "color": "grey", 
  }, 
  "mode": "lines", 
  "name": "95 percent lower", 
  "type": "scatter", 
  "x": dates, 
  "y": conf_95[:,0], 
  "xaxis": "x", 
  "yaxis": "y", 
  "hoveron": "points"
}
    trace5 = {
  "fill": "toself", 
  "line": {
    "color": "yellow", 
  }, 
  "mode": "lines", 
  "name": "95 percent upper", 
  "type": "scatter", 
  "x": dates, 
  "y": conf_95[:,1], 
  "xaxis": "x", 
  "yaxis": "y", 
  "hoveron": "points"
}
    trace6 = {
  "fill": "toself", 
  "line": {
    "color": "orange", 
  }, 
  "mode": "lines", 
  "name": "80 percent lower", 
  "type": "scatter", 
  "x": dates, 
  "y": conf_80[:,0], 
  "xaxis": "x", 
  "yaxis": "y", 
  "hoveron": "points"
}
    trace7 = {
  "fill": "toself", 
  "line": {
    "color": "pink", 
  }, 
  "mode": "lines", 
  "name": "80 percent upper", 
  "type": "scatter", 
  "x": dates, 
  "y": conf_80[:,1], 
  "xaxis": "x", 
  "yaxis": "y", 
  "hoveron": "points"
}
        # fig
    fig.add_trace(trace1)
    fig.add_trace(trace2)
    fig.add_trace(trace3)
    fig.add_trace(trace4)
    fig.add_trace(trace5)
    fig.add_trace(trace6)
    fig.add_trace(trace7)

    fig.show()
x=fmri_simulate_func(dim_data = [64, 64, 40], mask = None, 
                                   ons = [1, 21, 41, 61, 81, 101, 121, 141], 
                                   dur = [10, 10, 10, 10, 10, 10, 10, 10])
data=x["fmridata"]
fmri_ts_forecast(data,[20,30,20])