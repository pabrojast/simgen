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

Read the 'obssim_xxxx.txt' files. These are presently located in the
'dat' directory.

"""

import numpy as np
import cdms2 as cdms

k = 1.0E+20

def r(filename):
    fo = open(filename)
    nrec = len(fo.readlines())
    print nrec, 'days in file...'
    fo.seek(0)
    datmat = k*np.ones((nrec,6),'f')           # yr,mo,da,pr,Tmin,Tmax
    ct = 0
    pct = 0
    while 1:
        q = fo.readline()
        if len(q) < 1:
            print 'Done!'
            break
        qdate = q[:16]
        qdata = q[16:]
        ql = qdata.split()
            
        datmat[ct,0] = qdate[8:12]
        datmat[ct,1] = qdate[12:14]
        datmat[ct,2] = qdate[14:16]

        if ql[0].endswith('P'):
            pct += 1
            datmat[ct,3] = ql[0][:-1]
        else:
            datmat[ct,3] = ql[0]
        for i in range(4,6):
            datmat[ct,i] = ql[i-3]
        ct += 1

    vals = np.arange(nrec)
    tax = cdms.createAxis(vals)
    tax.designateTime()
    startyr = str(int(datmat[0,0]))
    startmo = str(int(datmat[0,1]))
    startda = str(int(datmat[0,2]))
    tax.units = 'days since '+startyr+'-'+startmo+'-'+startda
    tax.id = 'time'
    tb = tax.genGenericBounds()
    tax.setBounds(tb)

    prvar   = cdms.createVariable(datmat[:,3],axes=(tax,),id='pr')
    tmaxvar = cdms.createVariable(datmat[:,4],axes=(tax,),id='Tmax')
    tminvar = cdms.createVariable(datmat[:,5],axes=(tax,),id='Tmin') 
    fo.close()

    print pct, 'filled values, of', nrec
    return prvar,tmaxvar,tminvar,datmat


#############################        
        
