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

FORMATO_SIN_ID = False
#Desplazamiento desde el comienzo de la linea hasta la fecha
DELTA_FECHA = 8

k = 1.0E+20

def r(filename):
	#delta corresponde al numero de espacios antes de la fecha
    delta = DELTA_FECHA
    if(FORMATO_SIN_ID):
		delta = 0
	#abrimos el archivo
    fo = open(filename)
    nrec = len(fo.readlines())
    print nrec, 'days in file...'
	#vuelve a la linea 0 porque recorrio todo el archivo
    fo.seek(0)
	#np.ones crea un array de (numero de lineas, 6) f es Whether to store multidimensional data in C- or Fortran-contiguous (row- or column-wise) order in memory.
    datmat = k*np.ones((nrec,6),'f')           # yr,mo,da,pr,Tmin,Tmax
    ct = 0
    pct = 0
    while 1:
        q = fo.readline()
		#si no hay informacion en la linea
        if len(q) < 1:
            print 'es menor que 1 len(q) Done!'
            break
        qdate = q[:8+delta]
        qdata = q[8+delta:]
        ql = qdata.split()
        
        datmat[ct,0] = qdate[0+delta:4+delta]
        datmat[ct,1] = qdate[4+delta:6+delta]
        datmat[ct,2] = qdate[6+delta:8+delta]

        if ql[0].endswith('P'):
            pct += 1
            datmat[ct,3] = ql[0][:-1]
        else:
            datmat[ct,3] = ql[0]
        for i in range(4,6):
            datmat[ct,i] = ql[i-3]
        ct += 1

	vals = np.arange(nrec)
	startyr = str(int(datmat[0,0]))
	#https://github.com/UV-CDAT/cdat-site/blob/master/createaxes.md
    #tax = cdms.createAxis(vals)
    #tax.designateTime()
	
	startmo = str(int(datmat[0,1]))
	startda = str(int(datmat[0,2]))
    #editado de tax.units, tax.id a 
	taxunits = 'days since '+startyr+'-'+startmo+'-'+startda
    taxid = 'time'
    #tb = tax.genGenericBounds()
    #tax.setBounds(tb)
    prvar   = datmat[:,3]
    tmaxvar = datmat[:,4]
    tminvar = datmat[:,5] 
    fo.close()
    print pct, 'valores rellenados, de', nrec
    return prvar,tmaxvar,tminvar,datmat
#############################        
        
