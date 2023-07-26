"""

Find examples in the simulation file where the 10-year mean pr lies at a
given quantile, while the corresponding means of Tmax, Tmin lie near their
expected values, given the specified value of pr.

A condition may also be imposed wherein the preceding decade has precip
relatively close to the median value -- so we don't simulate a 20-year
drought when we only intended to simulate a 10-year event, e.g.

Usage: outlist = gen(dat,pctile,eps), where

dat is like a matrix of 10-year means of pr, Tmax, Tmin (in that order),
q is the desired pr quantile for which to search and
eps (epsilon) is a three-vector indicating the the vicinity around each of
the variables in which to search.


"""

import mvn32
import numpy as np
import pdb,sys
from scipy.stats import scoreatpercentile as quant
from simgenExtras import promedio

PASO = 10
ARCHIVO_ENTRADA = 'sim_100kyr.dat'
ARCHIVO_MEDIAS = 'sim_mean2.dat'

def gen(dat,pctile):
# Agrego para que lea el archivo
    #dat=np.loadtxt("sim_mean.dat")    
    outl = []
    sd = dat.shape
    mu = dat.mean(axis=0)
    sig = np.cov(dat.T)
    sdd = np.std(dat,axis=0)
    if pctile == 50:
#        eps = (0.05, 0.1, 0.1, 0.1)          # Yields 99 instances for q50  
         eps = (0.1, 0.7, 0.7, 1.0)               # Yields 8184 instances for q50
    elif pctile == 10 or pctile == 90:
         eps = (0.1, 1.0, 1.0, 1.0)         # Yields 223 instances
 #       eps = (0.1, 0.6, 0.6, 1.0)          # Yields 2 instances


    pr = dat[:,0]; ma = dat[:,1]; mi = dat[:,2]

#    pdb.set_trace()                         # Runs in epy, hangs with ipy
     
    ppr = quant(pr,pctile)
    print 'Args to mvn32:', mu, sig, ppr, 0
    munew,signew = mvn32.comp(mu,sig,ppr,0)  # Assuming pr is specified

    print 'ppr,munew:', '%6.4f %6.4f %6.4f' % (ppr, munew[0], munew[1])
    sys.stdout.flush()

    prlims = [ppr-eps[0]*sdd[0], ppr+eps[0]*sdd[0]]
    malims = [munew[0]-eps[1]*sdd[1], munew[0]+eps[1]*sdd[1]]
    milims = [munew[1]-eps[2]*sdd[2], munew[1]+eps[2]*sdd[2]]
    prelims= [mu[0]-eps[3]*sdd[0], mu[0]+eps[3]*sdd[0]]
    
    print 'prlims:', '%6.4f %6.4f' % (prlims[0], prlims[1])
    print 'malims:', '%6.4f %6.4f' % (malims[0], malims[1])
    print 'milims:', '%6.4f %6.4f' % (milims[0], milims[1])
    print 'prelims:' '%6.4f %6.4f' % (prelims[0],prelims[1])
    
    prsort = np.sort(dat[:,0]); print 'sort 1 completed...'
    prargs = np.argsort(dat[:,0]); print 'sort 2 completed'
    
    ixlo = (prsort < prlims[0]).sum()       # How far into array is lower lim?
    ixhi = (prsort < prlims[1]).sum()
    ixrange = ixhi-ixlo

    ixmed = (prsort < ppr).sum()
    test = pr[prargs[ixmed]]
    print 'Should be about equal to ppr:', '%6.4f' % (test)

    print ixrange,'realizations to search, not a problem!'

    for i in range(ixlo,ixhi):
#        if i/1000 == i/1000.:
#            print 'Searching at index', i, 'in range', ixlo, ixhi
        cand   = dat[prargs[i],:]
        cand10 = dat[prargs[i]-10,:]         # Preceding decade not unusual...
        
        cond0 = malims[0]  < cand[1]   <  malims[1]
        cond1 = milims[0]  < cand[1]   <  milims[1]
        cond2 = prelims[0] < cand10[0] < prelims[1]

        if cond0 and cond1 and cond2:
            outl.append((prargs[i],cand))

    lo = len(outl)
    print lo, 'instances found!'
    
    return outl

#################

    
def generar(pctile,pasoMedias=PASO):
	generadorMedias = promedio(ARCHIVO_ENTRADA,pasoMedias)
	generadorMedias.generarMedias()
	generadorMedias.guardarMedias(ARCHIVO_MEDIAS)	
	medias = generadorMedias.getMediasNP()
	salida = gen(medias,pctile)
	print [x for x,y in salida]	
	
	
	
def main():
	if(len(sys.argv)>2):
		generar(int(sys.argv[1]),int(sys.argv[2]))
	elif(len(sys.argv)>1):
		generar(int(sys.argv[1]))
	else:
		print "uso: %s {pctile} [pasoMedias]" % sys.argv[0]

	return 0

if __name__ == '__main__':
	main()
#####################MODIFICADO 17-02-2017######################
	
arg1 = float(sys.argv[1])
arg2 = float(sys.argv[2])
generar(arg1, arg2)
