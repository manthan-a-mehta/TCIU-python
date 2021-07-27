import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
z=np.random.randn(64,64,40)
x=[k for k in range(1,65)]
y=[k for k in range(1,65)]

cs = plt.contour(x,y,z[:,:,1], [0.30])

p = cs.collections[0].get_paths()[0]
v = p.vertices
x = v[:,0]
y = v[:,1]
df=pd.DataFrame({"x":x,"y":y,"z":[1]*len(x)})
print(df)