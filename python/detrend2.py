"""

--------------------

$ Copyright 2011 Arthur M. Greene $
$ Revised 2011-04-12 $

This software is provided under the Creative Commons
Attribution-NonCommercial-ShareAlike 3.0 Unported (CC BY-NC-SA
3.0)license (see http://www.creativecommons.org/licenses). The above
copyright notice, as well as this license information, must be
included in any restribution.

--------------------

Detrend a time series

Usage: xdt,tline,coeffs = detrend.dt(x), where

x is the time series
xdt is the detrended series
tline is the trend line (that was subtracted from the series)
coeffs is an array containing the line coefficients

"""

import numpy as np
from scipy.stats import linregress as lm

def dt(y):
  ly = len(y)
  x = np.arange(ly)
  fm = lm(x,y)
  coeffs = fm[:2]
  line = fm[1] + fm[0]*x
  
  xdt = y-line
  
  return xdt,line,coeffs

#############
