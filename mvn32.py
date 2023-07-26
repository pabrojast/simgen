"""

Compute a bivariate distribution from trivariate, conditional on
a value for one of the variables.

Usage: munew, signew = mvn32.comp(mu,sigma,x,ix), where

mu is the original (3-vector) mean,
sigma the original (3 x 3) covariance matrix,
x is the assigned value and
ix is which of the three indices it represents.

munew, signew are the conditional (2-D) mean and covariance matris,
respectively.

See notation at http://j.mp/bBdppe (Wikipedia)

"""

from numpy import allclose,cov,matrix
import pdb

def comp(mu,sig,x,ix):                            # Sigma_conditional
    """ Compute the conditional 2-D covariance matrix, ix = 1,2 or 3."""
#    pdb.set_trace()
    C = matrix(sig)
    CI = C.I
    anom = matrix(x - mu[ix])
    if ix == 0:
        mu1 = matrix([mu[1:]]).T
        sig11 = C[1:,1:]
        sig12 = C[1:,0]
        sig21 = C[0,1:]
        sig22 = matrix(C[0,0])
        ci = CI[1:,1:]
    elif ix == 1:
        mu1 = matrix([[mu[0],mu[2]]]).T
        sig11 = matrix([ [C[0,0],C[0,2]], [C[2,0],C[2,2]] ])
        sig12 = matrix([ [C[0,1],C[2,1]] ]).T
        sig21 = matrix([ [C[1,0],C[1,2]] ])
        sig22 = matrix([C[1,1]])
        ci = matrix(CI[:2,:2])
        ci[0,1] = CI[0,2]
        ci[1,0] = CI[2,0]
        ci[1,1] = CI[2,2]
    elif ix == 2:
        mu1 = matrix([mu[:2]]).T
        sig11 = C[:2,:2]
        sig12 = C[:2,2]
        sig22 = matrix(C[2,2])
        sig21 = C[2,:2]
        ci = matrix(CI[:2,:2])

    munew = mu1 + sig12*sig22.I*anom

# Method 1

    signew0 = ci.I

# Method 2

    signew1 = sig11 - sig12*sig22.I*sig21

    if allclose(signew0,signew1):
        return munew, signew0
    else:
        print 'Computation problem! Check!'
        return

#################
