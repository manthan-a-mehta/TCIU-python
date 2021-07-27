
import multiprocessing as mp
from os import cpu_count
import numpy as np
import math
import sympy
import sys
def LT(func,comp):
    p=sympy.laplace_transform(func, t, x)
    return(p[0].subs({x:comp}))  

def processing(FUNCT,real_x,img_y):
    f_result=complex(LT(FUNCT,complex(real_x,img_y)))
    print(type(f_result))
    mag=np.log(np.sqrt(np.real(f_result)**2+np.imag(f_result)**2))
    phase=np.arctan2(np.imag(f_result),np.real(f_result))
    array_2d=mag*np.exp(1j*phase)
    return array_2d

def kimesurface_transform(FUNCT,real_x,img_y,parallel_computing=False,ncor=6):
    
    """
    Title
    -----
    kimesurface transform on a function with a specified 
    set of complex values

    Description
    -----------
    a function applies the kimesurface transform on a function with a specified set of complex values

    Parameters
    ----------
    FUNCT:sympy object; function object f(t) to conduct kimesurface transform on
    
    real_x: np.array; a list of numeric values, which is the real 
    part of a set of complex values
    
    img_y: np.array; a list of numeric values, which is the imaginary 
    part of the set of complex values stated above
    
    parallel_computing:bool; logical object to determine 
    whether to use parallel computing to speed up the function or not.
    The default is FALSE.
    
    ncor:int; number of cores for parallel computing. The default is 6.

    details
    -------

    This function applies the kimesurface transform on a 1D function f(t), to have it converted to a 2D function. The input
    is a set of complex values with the same number of real and imaginary parts. These two parts can specify a 2D plane 
    of the same length and width. The new 2D function is defined on this 2D plane. It mainly does a 
    Laplace Transform and modifies all the function values in a specific way to have them looks better in the plot. 
    
    return
    ------

    a 2d array that did kimesurface transform for the set of complex value (the real and imaginary parts can
    construct a 2d plane

    examples
    --------
    $ pip install sympy

    import sympy
    t, x = sympy.symbols('t, x')
    func=sympy.sin(t)
    xs=np.linspace(0,10,num=10)
    ys=np.linspace(0,10,num=10)
    print(kimesurface_transform(func,xs,ys))

    """
    
    if(parallel_computing==True):
        array_2d=[0]*(len(real_x))
        for i in range(len(real_x)):
            results=[]
            for j in range(len(img_y)):
                if ncor>mp.cpu_count():
                    print("input ncor less than {}".format(cpu_count))
                    sys.exit()
                pool = mp.Pool(ncor)
                results.append(pool.apply(processing, args=(FUNCT,real_x[i],img_y[j])))
            array_2d[i]=results    
        pool.close()
    
    else:
        f_result=np.zeros((len(real_x),len(img_y)),dtype=np.complex_)
        for i in range(len(real_x)):
            for j in range(len(img_y)):
                f_result[i,j]=LT(FUNCT,complex(real_x[i],img_y[j]))
        print(f_result.shape)
        mag=np.log(np.sqrt(np.real(f_result)**2+np.imag(f_result)**2))
        print(mag.shape)
        phase=np.arctan2(np.imag(f_result),np.real(f_result))
        array_2d=mag*np.exp(1j*phase)
    return array_2d

t, x = sympy.symbols('t, x')
func=sympy.sin(t)
xs=np.linspace(0,10,num=10)
ys=np.linspace(0,10,num=10)
print(kimesurface_transform(func,xs,ys,True,2))
