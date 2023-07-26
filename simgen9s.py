"""

$ Copyright 2011, 2012, Arthur M. Greene $
$ Revised 2012-06-27 $

This software is provided under the Creative Commons
Attribution-NonCommercial-ShareAlike 3.0 Unported (CC BY-NC-SA
3.0)license (see http://www.creativecommons.org/licenses). The above
copyright notice, as well as this license information, must be
included in any restribution. For noncommercial use only, except by
written agreement. For commercial applications please contact the
author: amg@iri.columbia.edu or amg52@columbia.edu.

-----------------

This Python module is designed to generate stochastic simulations for
driving the ACRU hydrology model, developed at the University of
KwaZuluNatal, South Africa. In particular, this file is provided in
conjunction with the following technical report:

Greene, Arthur M., Lisa Goddard and James W. Hansen, A framework for
the simulation of regional decadal variability for agricultural and
other applications, 2012.

This code is described in an accompanying guide, entitled The simgen
software package: User guide and notes. The guide should be found in
the same directory as this code.

-----------------

New in version 9:

1. The multimodel mean global mean temperature signal, as well as the
regional precipitation response, is derived from the CMIP5 model
ensemble. In previous iterations the CMIP3 ensemble was utilized.

2. The 21C precipitation trend is selected by quantile, from a Normal
distribution fitted to the empirical distribution of trends in the
CMIP5 ensemble. This is computed for each model as the percentage
change in annual mean precipication (for the region 30-35S, 17-23E)
per degree of global warming.

3. Block resampling is now performed using a k-nearest-neighbor
scheme, conditional on either regional mean precipitation or on the
full three-component (pr, Tmax, Tmin) vector of climate variables. If
the M = 1 option is selected the Mahalanobis distance metric is used,
with weights assigned to pr, Tmax and Tmin. This is implemented in
function 'rkm' near the end of the module code.

Refer to the following article for additional details:

Greene, A.M., M. Hellmuth and T. Lumsden, Stochastic decadal climate
simulations for the Berg and Breede Water Management Areas, Western
Cape province, South Africa, Water Resourc. Res. (48), W06504,
doi:10.1029/2011WR011152, 2012

----------------

This script is designed to batch-processe a set of station records,
whose identifiers are provided in the Python list 'obsix' (see
description of the call, below). All stations processed in a single
batch will share not only the selected simulation instance (as
indicated by the index 'simix'), but will have coherent subannual
variations, which are batch-resampled across the entire domain.

Usage: datmat,fmat,scalemat = simgen9s.gen(obsix,simix,trendq,write=1,\
fname='sim_100kyr.dat',simlen=66, locate=2041, xval=0, M=1):

where

obsix is a Python list of 'obshis' file indices (4-digit integers or
strings, see the file residing in the directory 'input_sim'). The
stations indexed by this list are processed jointly, meaning that the
identical sequence of resampled subannual data years is applied across
the entire list,

simix is the starting index into the simulation file indicated by
fname. This index must be determined offline, by an examination of
decadal excursions and their relative likelihoods or by other criteria
determined by the modeler, prior to running simgen,

[NEW] trendq is the trend quantile, respecting that the 21C trend is
now drawn from a distribution. This distribution has been precomputed
using data from a number of the CMIP5 simulations, and is hard-coded
into simgen. It may be updated as more CMIP5 models come online (14
models are used in the current iteration). Note that this distribution
applies to annual mean precipitation in the domain of the Western
Cape, South Africa (30-35S, 17-23E). For simulation in other regions
corresponding regional values must be computed by the modeler,

[DEPRECATED, no longer accessible] alpha, which still exists in the
code, is a mixing coefficient. For a given catchment, the future
precipitation trend is now a weighted average of the imposed
(CMIP5-based) regional trend and a local trend derived from the 20C
annual mean precip observations, as regressed on global mean
temperature. The weights are 1/s for the regional trend and alpha/s
for the local signal, where s = 1 + alpha. Thus, alpha = 0 results in
the regional trend being applied uniformly across stations, while
alpha = 1 produces an equally-weighted average of regional and local
trends. Nonzero alpha values have the effect of causing the individual
catchment trends to scatter around the regional mean value,

fname is the name of the (long) simulation file, residing in the
directory 'input_sim',

simlen (lately 66, indicating the period 2000-2065) is the desired
simulation length, in years,

locate indicates the simulation year at which to place the sequence
beginning at index simix,

xval is a flag indicating whether to generate simulations for the
1950-2000 period for the purposes of cross validation. If xval = 1,
simlen will be reset to 50, regardless of the value input, since this
is the length of the observational period, and

M is a flag for using the Mahalanobis distance measure, for the
resampling scheme.

Outputs:

If 'obsix' is of unit length (i.e., one station), simgen returns three
files, viz.,

datmat: the daily output (i.e., simulation) matrix,
fmat: the matrix of annualized forced trends and
scalemat: the annualized simulation values exclusive of trends. To
plot the annualized simulation, including forced trends, use the sum
of fmat and scalemat.

When xval = 0, the actual pr,tmax,tmin is used for the 1950-1999
period . (The simulated segment is then grafted onto the end.) This
could be changed, but this is a less important option now that the
xval flag is available.

The observational years are not counted against the length specified
by 'simlen', so if the latter is 50 yr the total simulation length
will be 100 yrs. In the ACRU setting the observations begin in 1950,
so a 100-yr simulation would run from 1950-2049, inclusive. The
'stochastic' simulation then begins in year 2000, extending for 50
more years.

Note: For consistency in coding, the three variables, pr, Tmax and
Tmin, are typically denoted pr, ma, mi, respectively. This keeps
parallel statements to the same length and facilitates code checking.

"""

import cPickle as pickle, detrend2, numpy as np, os, readqc, sys
from numpy.random import rand
from numpy.linalg import inv
from scipy.stats import linregress as lm, norm
from simgenExtras import infoArchivo
from math import ceil
import sys

def gen(obsix=[2000],simix=2955,trendq=0.5,write=1,fname='sim_100kyr.dat',\
        simlen=62, locate=2041, xval=0, M=1):
    """ Main routine """

	#Obtengo informacion de la serie
    infoSerie = infoArchivo('obs/obshis_%d.txt' % obsix[0])
	
# First chore: Assemble the necessary files...
    inpre = 'obs/obshis_'
    print 'Now fetching simulation file...'
    sys.stdout.flush()
    regpre = 'input_sim/'
    regsim0 = np.loadtxt(regpre+fname)       # The long simulation sequence
    offset = infoSerie.anioFin+1 - locate
    startix = simix + offset                 # Start earlier if locate later...
    if xval == 0:
        regsim = regsim0[startix:startix+simlen,:]
    elif xval == 1:
        regsim = regsim0[startix-infoSerie.lenSerie:startix+simlen,:]  # Simulate starting 1950
    print 'File is read and sliced!'
    sys.stdout.flush()                       # Forces print to screen

    print 'Now fetching tMMM, for forced component...'
    sys.stdout.flush()
    tmmm = pickle.load(open('pickled/tasav_cmip5_comb_sm_1901-2095.p'))
    print 'File is read!'
    sys.stdout.flush()

# Read the 'regional' observational file
    regobs = np.loadtxt('dat/obsavdt.dat')   # Mean of the obs series
    obspr = regobs[:,0]                      # Years 1950-1999, obviously
    obsma = regobs[:,1]                      # Note: Detrended!
    obsmi = regobs[:,2]

# Recondition the simulation, which is regional in scale. This will be
# matched, variance-wise, to the station data.  Note that the
# simulated files have zero mean and have previously been (IPCC-)
# detrended.

    simpr0 = regsim[:,0]                     # If xval==1, 116 yr in length
    simma0 = regsim[:,1]
    simmi0 = regsim[:,2]
    simpr,dline,coeffs = detrend2.dt(simpr0) # Re-detrend sim segments
    simma,dline,coeffs = detrend2.dt(simma0)
    simmi,dline,coeffs = detrend2.dt(simmi0)

# The values below derive from offline computations using the RCP4.5
# simulations from 14 of the CMIP5 models, in which the log (ln) of
# regional (WCA2: 30-35S, 17-23E) mean, annual mean precipitation is
# regressed on global mean temperature (2006-2065). The coefficients
# then represent the fractional change in regional precip per degree
# of global warming, for each of the models. The mean and std of the
# 14 model coefficients are, respectively:

    trendmean =  -0.125968
    trendsd   =  1.76931

    fm21 = norm.ppf(trendq,trendmean,trendsd)# Value at specified quantile

# This completes the preparation of the (annually-resolved) simulation
# record, with simpr, simma, simmi ready to be "injected" into
# individual catchment simulations.
#
#
##############################
#
# Prepare global MMM signal, based on the same 14-member ensemble of
# CMIP5 models mentioned above. See the WRR paper for model details.
#

	#tmmm contiene datos de 1901 a 2095
	#tmmm[49:99] es una lista con los anios 1950 a 2000
	#tmmm[0] = 1901
	#tmmm[49] = 1950
	#tmmm[99] = 2000	
    #x = tmmm[49:99+simlen]                   # Multimodel for 1950-20xx
    inicio = infoSerie.anioInicio-1901
    x = tmmm[inicio:inicio+infoSerie.lenSerie+simlen]                   # Multimodel for 1950-20xx
    xmm = x - x.mean()                       # Shape is the thing
    x20c = xmm[:infoSerie.lenSerie]                          # 1950-2000        
    x21c = xmm[infoSerie.lenSerie:infoSerie.lenSerie+simlen]                 # 2000 onward    
    
# Now, set up the sequence of years that will be used for resampling
# interannual (co)variations. This is performed via a k-NN approach,
# with the "feature vector" either a simulated annual precipitation
# value (M = 0) or the annual three-component vector (M = 1). The
# feature vector is compared with values from the observed record, the
# k nearest neighbors being selected. The resampling year is chosen
# from among the k candidates, with probabilities 1/n, n = 1,2,...5
# (normalized).

# First, fetch the (non-detrended) pr...
    obs_raw   = np.loadtxt('dat/obsav.dat')
    obspr_raw = obs_raw[:,0]
    obsma_raw = obs_raw[:,1]
    obsmi_raw = obs_raw[:,2]

    fm20 = lm(x20c,np.log(obspr_raw))        # To simulate 1950-2000
    fm2000 = 0.5 * (fm20[0] + fm21)          # For the 2000-2005 "transition"

    C = np.cov(obs_raw.T)
    CI = inv(C)                              # Inverse covariance for func rkm
    print 'Computed(or fetched) CI!'

# Next, compute the regional mean future pr (and ma, mi) signal (20C
# as well as future, if xval==1), so we can construct the required
# feature vectors.

    pr21forced = 1.0E+20 * np.ones(simlen)   # Set up array
    M0 = obspr_raw[-10:].mean()              # Mean of final 10 yr of obs
    x0 = np.log(M0) - fm2000*x21c[0]         # Match level M0
    pr21forced[:6] = np.exp(fm2000*x21c[:6] + x0) # 2000-2005, inclusive
    M1 = 0.1*(obspr_raw[-4:].sum()+pr21forced[:6].sum()) # Mean, 1996-2005
    x1 = np.log(M1) - fm21*x21c[6]           # Shift, match again...
    pr21forced[6:] = np.exp(fm21*x21c[6:] + x1)   # Based on trendq...

    ma21forced = 1.0E+20 * np.ones(simlen)   # Need for M = 1
    mi21forced = 1.0E+20 * np.ones(simlen)
    fmma = lm(x20c,obsma_raw)                # Fit to 20C
    fmmi = lm(x20c,obsmi_raw)
    ma21forced = fmma[1] + fmma[0]*x21c      # Project into 21C
    mi21forced = fmmi[1] + fmmi[0]*x21c

    sim20total = 1.0E+20 * np.ones((infoSerie.lenSerie,3))
    sim21total = 1.0E+20 * np.ones((simlen,3))

    if xval == 0:
        pr21total = pr21forced + simpr       # len(simpr) = simlen
        ma21total = ma21forced + simma
        mi21total = mi21forced + simmi
        
    elif xval == 1:
        pr21total = pr21forced + simpr[infoSerie.lenSerie:]  # len(simpr) = 50 + simlen
        ma21total = ma21forced + simma[infoSerie.lenSerie:]
        mi21total = mi21forced + simmi[infoSerie.lenSerie:]

        M0 = obspr_raw[:10].mean()           # First 10 values...
        x0 = np.log(M0) - fm20[0]*x20c[0]

        pr20forced = np.exp(fm20[0]*x20c + fm20[1])    # Simpler = better!
        pr20total  = pr20forced + simpr[:infoSerie.lenSerie] # Same as done for catchments

        ma20forced = fmma[1] + fmma[0]*x20c
        ma20total = ma20forced + simma[:infoSerie.lenSerie]
        mi20forced = fmmi[1] + fmmi[0]*x20c
        mi20total = mi20forced + simmi[:infoSerie.lenSerie]

        if M == 1:                           # If M == 0 not need these arrays
            sim20total[:,0] = pr20total
            sim20total[:,1] = ma20total
            sim20total[:,2] = mi20total
            
# Put the simulated annual values in arrays, the better for k-NN
    if M == 1:                               # if M ==1 need these, even if
        sim21total[:,0] = pr21total          # xval == 0
        sim21total[:,1] = ma21total
        sim21total[:,2] = mi21total

# Now, the k-NN approach, to choose the years from which to
# appropriate subannual variations. For this we will require the
# non-detrended pr (ma, mi) values for comparison (although observed
# pr does not trend significantly).

    yrs2sim  = range(infoSerie.anioFin+1,infoSerie.anioFin+1+simlen)   # Not including 20C; actual yr nos
    normdic = {}                         # For obs pr values, non-leap yrs 
    leapdic = {}                         # For obs pr values, leap years
    normdicM = {}                        # For obs clim vectors, non-leap yrs
    leapdicM = {}                        # Obs clim vectors, leap years

    lpyrs20c = range(infoSerie.primerBisiesto,infoSerie.anioFin+1,4)        # 20C leap years
    yrs20c   = range(infoSerie.anioInicio,infoSerie.anioFin+1)          # 20C all years
    nolp20c  = [i for i in yrs20c if not i in lpyrs20c] # 20c non-leap yrs
    for j in yrs20c:                     # dicts with key=year, value=pr[year]
        if j in nolp20c:                 # Two dicts, leap/noleap(=norm)
            normdic[j] = obspr_raw[j-infoSerie.anioInicio]   # Dict for pr
            normdicM[j]= obs_raw[j-infoSerie.anioInicio,:]   # Dict for whole clim vector
        elif j in lpyrs20c:
            leapdic[j] = obspr_raw[j-infoSerie.anioInicio]   # Similar, but for leap years
            leapdicM[j]= obs_raw[j-infoSerie.anioInicio,:]   # Ditto
    lk = leapdic.keys()                  # 1952,1956,1960... Same for *dicM
    nk = normdic.keys()                  # 1950,1951,1953...
    lk.sort(); nk.sort()                 # Put (back) in order...
    ldat = np.zeros(len(lk))             # Arrays, used for the k-NN comps
    ndat = np.zeros(len(nk))
    ldatM = np.zeros((len(lk),3))
    ndatM = np.zeros((len(nk),3))
    for j in range(len(lk)):
        ldat[j] = leapdic[lk[j]]
        ldatM[j,:] = leapdicM[lk[j]]
    for j in range(len(nk)):
        ndat[j] = normdic[nk[j]]
        ndatM[j,:] = normdicM[nk[j]]

# So... the arrays ldat, ndat (ldatM, ndatM for the vectors) contain
# observed values for leap and non-leap years, respectively. As each
# FV is held up for comparison, its differences with these arrays,
# computed by either abs(diff) or Mahalanobis(diff w.r.t. CI)
# determine which years' values will become candidates for resampling.
        
# The following indices are for extracting the appropriate _daily_
# values; used later
    basic20c = infoSerie.bisiestosInicio             # 1950,1951,1952,1953    
    leap20long = basic20c*int(ceil(infoSerie.lenSerie/4.0))
    leap20norm = leap20long[:infoSerie.lenSerie]
    leap20norm.insert(0,0)
    breaksim20 = np.cumsum(leap20norm)       # Segment endpoints for 20c

    basic21c = infoSerie.bisiestosFin             # 2000,2001,2002,2003    
    leap21long = basic21c*int(ceil(simlen/4.0))                 # Through 2100
    leap21norm = leap21long[:simlen]
    
    leap21norm.insert(0,0)
    breaksim21 = np.cumsum(leap21norm)       # Segment endpoints for 21c

# Arrays of the resampled years, e.g., (1960,1957,1993...)
    jixar20 = np.zeros(infoSerie.lenSerie,dtype=int)
    jixar21 = np.zeros(simlen,dtype=int)

# Number of candidate NN. Vary as required.
    nnn = 5                                  # Number of nearest neighbors

# Construct resampling kernel, "choice" arrays, probabilities 1/n
# This is the same regardless the value of M
    pmf0 = 1./np.arange(1,nnn+1)             # Resampling kernel
    pmf  = pmf0/pmf0.sum()                   # Normalized
    cpmf = np.cumsum(pmf)                    # Partition the unit interval
    choose20 = rand(infoSerie.lenSerie)                      # Random uniform distribution
    choice20 = np.zeros(infoSerie.lenSerie,dtype=int)        # Array to fill
    choose21 = rand(simlen)                  # Random uniform distribution
    choice21 = np.zeros(simlen,dtype=int)    # Array to fill

    for j in range(infoSerie.lenSerie):                      # Yields an integer, 0 - 4
        choice20[j] = int((choose20[j] - cpmf > 0.).sum())
    for j in range(simlen):                  # Yields an integer, 0 - 4
        choice21[j] = int((choose21[j] - cpmf > 0.).sum())

# Now, construct the actual sequence of years for resampling
# First, 20C: Leap years, then normal years...
    if xval == 1:
        for i in range(infoSerie.lenSerie):                  # Do for 20C
            if leap(yrs20c[i]):              # Different for leap years...
                dk = lk                      # Presorted leapdic keys
                if M == 0:
                    dvec = abs(pr20total[i] - ldat)    # Euclidean distances
                    ddic = leapdic
                elif M == 1:
                    dvec = rkm(ldatM,sim20total[i,:],CI)    # M-distances
                    ddic = leapdicM          # Corresponding dictionary

            else:
                dk = nk                      # Presorted normdic keys
                if M == 0:
                    dvec0 = abs(pr20total[i] - ndat)
                    ddic0 = normdic
                elif M == 1:
                    dvec = rkm(ndatM,sim20total[i,:],CI)
                    ddic = normdicM

            dsort = np.argsort(dvec)         # Longer than 5...
            jixar20[i] = dk[dsort[choice20[i]]]

# Now, 21C: First leap, then normal...

    for i in range(len(yrs2sim)):            # range(50, 66... as specified)
        if leap(yrs2sim[i]):                 # Different for leap years...
            dk = lk
            if M == 0:
                dvec = abs(pr21total[i] - ldat)  # Euclidean distance vector
                ddic = leapdic                   # Corresponding dictionary
            elif M == 1:
                dvec = rkm(ldatM,sim21total[i,:],CI)
                ddic = leapdicM
                
        else:                                # Not leap year
            dk = nk
            if M == 0:
                dvec = abs(pr21total[i] - ndat)
                ddic = normdic
            elif M == 1:
                dvec = rkm(ndatM,sim21total[i,:],CI)
                ddic = normdicM

        dsort = np.argsort(dvec)             # Longer than 5...
        jixar21[i] = dk[dsort[choice21[i]]]

#    print 'jixar20 (zeros if xval==0):', jixar20  # Seems to work...
#    print 'jixar21 (never zeros):', jixar21
    
##########################
##########################

# Main loop: Cycle through the station list
    for obsno in obsix:
        infile = inpre+str(obsno)+'.txt'
        print 'Processing station file', infile
        sys.stdout.flush()
        pr,tmax,tmin,dummy = readqc.r(infile)     # Daily data
        print infile, 'is read!'
        sys.stdout.flush()
    
# Annual mean *station* signals.  The underlying (daily) series are
# not altered.             # With trend and mean; not log
        yrpr,breaks0 = yrgen(pr, dummy)
	yrma,breaks1 = yrgen(tmax, dummy)           # All breaks should be the same
	yrmi,breaks2 = yrgen(tmin, dummy)
	obslen = len(yrpr)
	print(obslen)
		
        fmpr = lm(x20c,np.log(yrpr))         # Regression of (log) station pr,
        prforced = np.exp(fmpr[0]*xmm + fmpr[1]) # 100 (116) yr of CC trend.
        fmma = lm(x20c,yrma)                 # Tmax, Tmin, on tMMM 20C.
        fmmi = lm(x20c,yrmi)

# Compute forced pr trend for injection. Note that while the percent
# change in pr per degree of global temperature increase has been set
# (in fm21), the level at which the forced signal begins, in year
# 2000, will differ from catchment to catchment. This is why the
# following computation is needed.

# The "alpha" parameter is used to induce some scatter around the 21C
# trend, based on 20C behavior of individual catchments. (The average
# 20C trend is null, but individual catchments may still exhibit
# nonzero correlations with 20C global mean temperature)
#
        alpha = 0.5                          # Fraction of local response
        fm21local = fmpr[0]                  # 21C imposed
        fm21x = fm21 + alpha*fm21local       # Weighted mixture
        fm2000x = 0.5*(fm21local + fm21x)    # 2000-2005 transition

        print 'Imposed pr trend:', fm21
        print 'Local pr trend:', fm21local
        print 'Weighted local trend departure:', alpha*fm21local
        print 'trend for 2000-2005 (transitional):', '%6.4f' % (fm2000x)
        print 'trend for 2005-2065:', '%6.4f' % (fm21x)

# This is all (well, almost all) for 21C...
#
        pr21forced = 1.0E+20 * np.ones(simlen)# Create array...
        M0 = prforced[49]                     # For smooth join with 20c forced
        x0 = np.log(M0) - fm2000x*x20c[-1]    # Last value of x20c...
        pr21forced[:7] = np.exp(fm2000x*x21c[:7] + x0) # 2000-2006, inclusive

        M1 = pr21forced[6]                    # Overwrite [6], continue...
        x1 = np.log(M1) - fm21x*x21c[6]       # New join at 2006
        pr21forced[6:] = np.exp(fm21x*x21c[6:] + x1)   # Based on trendq...

        prforced[infoSerie.lenSerie:] = pr21forced            # Fill in 21c part of forced
        maforced = fmma[1] + fmma[0]*xmm      # Actual values, not centered.
        miforced = fmmi[1] + fmmi[0]*xmm    

# yrpr, yrma, yrmi, prdt, madt, midt -- from the station

        prdt = yrpr - prforced[:obslen]      # Annual values minus forced 
        madt = yrma - maforced[:obslen]      # (pseudo-detrended)
        midt = yrmi - miforced[:obslen]
        prdt = prdt - prdt.mean()            # Remove last trace of means
        madt = madt - madt.mean()            # These series have len obslen:
        midt = midt - midt.mean()            # They are station obs detrended
    
# Compute 'decadal' regression coefficients, local on regsim
# obspr, obsma, obsmi are the (detrended) regional mean signals
# prdt, etc. -- the station signals -- are regressed on them: lm(x,y)
        fmregpr = lm(obspr,prdt)             # Why obspr, etc. must be len 50
        fmregma = lm(obsma,madt)
        fmregmi = lm(obsmi,midt)
        print 'Regression coeffs, catchment on regional sig:',\
              '%6.2f %6.2f %6.2f' % (fmregpr[0],fmregma[0],fmregmi[0])
#        print 'fmregpr:', fmregpr
        
# Compute fitted values, based on station (xxdt) sequences
        hatpr0 = fmregpr[1] + fmregpr[0]*obspr
        hatma0 = fmregma[1] + fmregma[0]*obsma
        hatmi0 = fmregmi[1] + fmregmi[0]*obsmi

# Scale raw sims to match the hat0 variables: New hats
        hatpr = (hatpr0.std()/simpr.std())*simpr  # If xval==1, full-length,
        hatma = (hatma0.std()/simma.std())*simma  # i.e., 20C + 21C
        hatmi = (hatmi0.std()/simmi.std())*simmi

# Add white noise to match variance, independent of other stations.
# The noise is uncorrelated with the hat variables
        if xval == 0:
            epspr = np.random.standard_normal(simlen)
            epsma = np.random.standard_normal(simlen)
            epsmi = np.random.standard_normal(simlen)
        elif xval ==1:
            epspr = np.random.standard_normal(obslen+simlen)
            epsma = np.random.standard_normal(obslen+simlen)
            epsmi = np.random.standard_normal(obslen+simlen)

# Scale simulated signals. hatpr.var() should now not be > prdt.var()...
        if prdt.var() >= hatpr.var():
            varfactpr = np.sqrt((prdt.var() - hatpr.var())/epspr.var())
            scalepr = hatpr + varfactpr*epspr
        else:
            print 'simvar > obsvar for pr!'
            varfactpr = np.sqrt((hatpr.var() - prdt.var())/epspr.var())
            scalepr = hatpr - varfactpr*epspr

        if  madt.var() >= hatma.var():
            varfactma = np.sqrt((madt.var() - hatma.var())/epsma.var())
            scalema = hatma + varfactma*epsma
        else:
            print 'simvar > obsvar for ma!'
            varfactma = np.sqrt((hatma.var() - madt.var())/epsma.var())
            scalema = hatma - varfactma*epsma

        if midt.var() >= hatmi.var():
            varfactmi = np.sqrt((midt.var() - hatmi.var())/epsmi.var())
            scalemi = hatmi + varfactmi*epsmi
        else:
            print 'simvar > obsvar for mi!'
            varfactmi = np.sqrt((hatmi.var() - midt.var())/epsmi.var())
            scalemi = hatmi - varfactmi*epsmi
            
        print 'Variance adjustment factors:',\
               '%6.4f %6.4f %6.4f' % (varfactpr,varfactma,varfactmi)

# Remove means once more...
        scalepr = scalepr - scalepr.mean()
        scalema = scalema - scalema.mean()
        scalemi = scalemi - scalemi.mean()

# These variances should match, not necessarily exactly
        print ('Variances for obs and sim...')
        fmtstr0 = '%6.4f %6.4f'
        print 'pr:', fmtstr0 % (prdt.var(), scalepr.var())
        print 'ma:', fmtstr0 % (madt.var(), scalema.var())
        print 'mi:', fmtstr0 % (midt.var(), scalemi.var())

# Now, remove annual signal from daily data for the resampled year and
# inject the scaled values, retaining the daily variations.

# prforced, maforced, miforced are the (50+simlen-yr-long)
# annually-resolved, smooth forced signals.

# For the decadal, we rescale one year at a time from the station
# data, first removing the data's own annual signal, then adding or
# subtracting the simulated signal, to the entire year's worth of
# values (for Tmax, Tmin), or multiplicatively scaling (the entire
# year) of pr.


# There are two scaling operations: annual and trend.  Both are
# performed year-by-year. (We are not up to the downscaling part
# yet...)

# Set up arrays

        newdalen = len(pr)+breaksim21[-1]    # e.g., 18262 + 18263 = 36525
        prsim = 1e20*np.ones(newdalen,'f')   # Like 100 years' worth of days
        masim = 1e20*np.ones(newdalen,'f')
        misim = 1e20*np.ones(newdalen,'f')

        if xval == 0:
            prsim[:len(pr)] = pr             # First 50 yr is orig data,
            masim[:len(pr)] = tmax           # don't overwrite!
            misim[:len(pr)] = tmin           ######## !! ######
        else:
            pass

        prmean = pr.mean()                   # Station mean, 1950-2000,
                                             # transformed to numpy float

# For descaling pr; len = obslen
        stripratio  =  yrpr/prmean
# For rescaling pr; len =  obslen+simlen
        injectratio = (scalepr+prmean)/prmean
        fmtstr1 = '%6.4f %6.4f'
        print 'Strip, inject means:',\
        fmtstr1 % (stripratio.mean(), injectratio.mean())
        sirmse = np.sqrt(((stripratio - injectratio[:infoSerie.lenSerie])**2).sum())
        print 'Strip, inject rmse (first 50 yr):', '%6.4f' % (sirmse)
        igt0 = (injectratio < 0.).sum()
        if igt0 > 0:
            """ Cannot simulate entire years of zero precip: Set a floor
            for acceptable injectratio, say 0.1, so at least some rainfall
            occurs..."""
            if igt0 == 1:
                print 'Nudging', igt0, 'injectratio...'
            elif igt0 > 1:
                print 'Nudging', igt0, 'injectratios...'
            lijr = len(injectratio)
            for i in range(lijr):
                if injectratio[i] <= 0.:
                    injectratio[i] = 0.1     # A simple kludge,
                                             # not needed often in S. Africa

# Now, downscale to the local level, starting with the resampled sequence... 
# First 50 years -- only simulated if xval == 1
        if xval == 1:
            for i in range(infoSerie.lenSerie):
                jix = jixar20[i] - infoSerie.anioInicio
                rstart = breaksim20[jix]     # Fetch the 20c chunk to rescale
                rend = breaksim20[jix + 1]   # (one year's worth of data)
                macorrect = scalema[i] - yrma[jix]
                micorrect = scalemi[i] - yrmi[jix]

                lpr = len(pr)
                start = breaksim20[i]        # Simulating 20C, year i
                end   = breaksim20[i+1]      # Should be correct no of days
                prseg  = pr[rstart:rend]     # Values to be de, rescaled
                maseg  = tmax[rstart:rend]
                miseg  = tmin[rstart:rend]
                
                prfact1 = injectratio[i]/stripratio[jix]
                prfact2 = prforced[i]/prmean
                prsim[start:end] = prseg*prfact1*prfact2
                masim[start:end] = maseg + macorrect + maforced[i]
                misim[start:end] = miseg + micorrect + miforced[i]

# Remainder of simulation -- always computed
        for i in range(len(yrs2sim)):        # range(50, 66... as specified)
            jix = jixar21[i] - infoSerie.anioInicio          # Resampled 21C years
            rstart = breaksim20[jix]         # 20c chunk to rescale
            rend = breaksim20[jix + 1]
            macorrect = scalema[i] - yrma[jix]
            micorrect = scalemi[i] - yrmi[jix]

            lpr = len(pr)
            start2 = lpr + breaksim21[i]     # 21C portion...
            end2   = lpr + breaksim21[i+1]   # Should have correct no of days
            prseg  = pr[rstart:rend]         # Values to be de, rescaled
            maseg  = tmax[rstart:rend]
            miseg  = tmin[rstart:rend]

            i2 = len(yrpr)+i                 # 51,52,53...

# Simulated vs reused 20C values --> different rescaling
            if xval == 0:
                prfact1 = injectratio[i]/stripratio[jix]
            elif xval == 1:
                prfact1 = injectratio[i2]/stripratio[jix]
                
            prfact2 = prforced[i2]/prmean
            prsim[start2:end2] = prseg*prfact1*prfact2
            masim[start2:end2] = maseg + macorrect + maforced[i2]
            misim[start2:end2] = miseg + micorrect + miforced[i2]

# Tmax must be greater than Tmin, according to ACRU.
        eps = 0.1
        ixs = masim <= misim + eps
        if ixs.sum() > 0:
            print 'Detected improper Tmax,Tmin values, attempting to fix...'
            ixar = np.array(np.where(ixs))[0,:]
            for j in ixar:
                print 'Offending indices, values, diff:'
                diff = misim[j] - masim[j]
                print j,masim[j],misim[j],-diff
                masim[j] += diff + 0.25
                print 'New values:'
                print masim[j],misim[j]
                print
        else:
            pass

# And pr must be >= 0.
        ixs = prsim < 0.
        if ixs.sum() > 0:
            print 'Detected prsim values < 0., attempting to correct...'
            ixar = np.array(np.where(ixs))[0,:]
            for j in ixar:
                print 'Offending index and value:', j, prsim[j]
                prsim[j] = 0.0
                print 'New value:', prsim[j]
                print
        else:
            pass
        
# Assemble trivariate arrays, for writing out and for returning.

        datmat = np.array(zip(prsim,masim,misim))        # daily
        fmat = np.array(zip(prforced,maforced,miforced)) # 116 yr, smoothed
        if xval == 0:
            totpr = np.concatenate((prdt,scalepr))
            totma = np.concatenate((madt,scalema))
            totmi = np.concatenate((midt,scalemi))
        elif xval == 1:
            totpr = scalepr
            totma = scalema
            totmi = scalemi

        scalemat = np.array(zip(totpr,totma,totmi))    #  116 yr, no trend

        if write:
            acru(datmat,simlen,infile,fname,simix,infoSerie)     # Write out the files
            head,tail = os.path.split(infile)
            inshort = tail[:-4]
            outpre = 'output_sim/'
            fhead,ftail = os.path.split(fname)
            fshort = 'yearly_simulation_'                     # ftail[-7:-4]
            outfile =outpre + fshort + inshort+'_'+str(simix).zfill(6)+'.txt'
            np.savetxt(outfile, (fmat+scalemat))        

        else:
            print 'Not writing output to file...'
            pass

        if len(obsix) == 1:                            # Single station
            return datmat,fmat,scalemat
        else: pass

    
############# End, main routine #################
#################################################

def yrgen(dat,datmat):
    """ Generate annual mean time series from daily data. """
    #tax = dat.getTime()
    #taxc = tax.asComponentTime()
    lt = len(datmat)
    start = int(datmat[0,0])
    #start = taxc[0].year
    end = int(datmat[-1,0])
    #end = taxc[-1].year
    yrlen = end - start + 1
	#    print 'Start, end years:', start, end
    yrdat = 1e20*np.ones(yrlen,'f')
	#    print 'len(yrdat)', yrlen
    yrs = np.zeros(lt,'f')
    for i in range(lt):
        #yrs[i] = taxc[i].year
        yrs[i] = int(datmat[i,0])
    breaks = []
    ct = 0
    for i in range(start,end+1):
        seglen = len(yrs[yrs==i])
        breaks.append(ct+seglen)
        try:
            ix = i-start
            yrdat[ix] = dat[ct:ct+seglen].mean()
        except IndexError:
            print ix,ct,seglen,ct+seglen,dat[ct:ct+seglen].mean()
            return yrdat
        ct += seglen

    return yrdat, breaks

##########################

def acru(simdat,simlen,infile,fname,simix,infoSerie):
    """ Write the ACRU-formatted simulation file..."""
    f = open(infile)
    head,tail = os.path.split(infile)
    inshort = tail[:-4]
    outpre = 'output_sim/'
    fhead,ftail = os.path.split(fname)
    fshort = ftail[:8]                       # ftail[-7:-4]
    outfile = outpre+fshort+'_'+inshort+'_'+str(simix).zfill(6)+'.txt'
    g = open(outfile,'w')
    fstr = '%5.1f %5.1f %5.1f '

    q = f.readline()
    fileid = q[:8]
    rowix = 0
    j2comp = getmoda(0)                      # Julian day lookup
    j2compleap = getmoda(1)                  #    "    "    " for leap years

    for i in range(infoSerie.anioInicio,infoSerie.anioFin+1+simlen):
        start = rowix
        datix = 0
        if not i/4 == i/4.:                  # Not leap year
            end = start+365
            for j in range(start,end):
                yr = str(i)
                moda = j2comp[datix]
                datestr = yr+moda
                if i == 2049 and datix == 364:    # Missing one steenkin' day!
                    datastr = fstr % (0.0,simdat[rowix-1,1],
                                  simdat[rowix-1,2])
                else:
                    datastr = fstr % (simdat[rowix,0],simdat[rowix,1],
                                  simdat[rowix,2])
                #outstr = fileid+datestr+datastr+'\n'
                outstr = datestr+datastr+'\n'
                g.write(outstr)
                datix += 1                    # Julian day
                rowix += 1                    # Index into simdat
        else:
            end = start+366
            for j in range(start,end):       # Rows into simdat
                yr = str(i)
                moda = j2compleap[datix]
                datestr = yr+moda
                datastr=fstr%(simdat[rowix,0],simdat[rowix,1],simdat[rowix,2])
                #outstr = fileid+datestr+datastr+'\n'
                outstr = datestr+datastr+'\n'
                g.write(outstr)
                datix += 1                    # Julian day
                rowix += 1                    # Index into simdat

    print 'Wrote',rowix,'lines!'
    f.close()
    g.close()

##########################

def getmoda(leap):
    """ Generate a year-long list of dates, for either leap or normal
    years"""
    
    datelist = []
    mos = [' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9','10','11','12']
    if leap == 0:
        days = [31,28,31,30,31,30,31,31,30,31,30,31]
    elif leap == 1:
        days = [31,29,31,30,31,30,31,31,30,31,30,31]

    for i in range(len(days)):
        for j in range(1,days[i]+1):
            datelist.append(mos[i]+str(j).rjust(2))

    return datelist

##########################

def leap(yr):
    if np.mod(yr,4) == 0:
        ans = 1
    else:
        ans = 0

    return ans

##########################

def rkm(x,xk,CI):                            # Mahalanobis distance
    """ Compute a vector of Mahalanobis distances, using the input
    vector x, the feature vector xk and the inverse covariance matrix
    CI. Note that the weight for precip here is double that of the
    combined temperature variables. The modeler may wish to modify
    these weights..."""
    
    wts0 = np.matrix(np.diag((4./6., 1./6., 1./6.)))
    wts = wts0/wts0.sum()
    xm  = np.matrix(x)                       # = Euclidean for C = I
    xkm = np.matrix(xk)
    d = xm - xkm                             # x is like (40,3), xk (1,3)
    dm = d * wts * CI * wts.T * d.T          # Row vector * CI * col vector 
    val = np.sqrt(np.diag(dm))               # Weighted distance vector

    return val

########################### T H E ### E N D #########
#####################

###################SIMGEN WINDOWS########################

#ejemplo: 2000,2001 8999 0.1 1 sim_100kyr.dat 84 2021 0 1

#####################################################
arg1 = map(int,sys.argv[1].split(","))
arg2 = int(sys.argv[2])
arg3 = float(sys.argv[3])
arg4 = int(sys.argv[4])
arg5 = sys.argv[5]
arg6 = int(sys.argv[6])
arg7 = int(sys.argv[7])
arg8 = int(sys.argv[8])
arg9 = int(sys.argv[9])
#####################################################

gen(arg1, arg2, arg3, write=arg4, fname=arg5, simlen=arg6, locate=arg7, xval=arg8, M=arg9)


