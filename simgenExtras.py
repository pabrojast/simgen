#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  simgenExtras.py
#  
#  Copyright 2013 julian <juliansuhr@gmail.com>
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#  
#  

import numpy as np

#Salto de linea windows
SALTO_D_LINEA = '\r\n'

#Salto de linea GNU/Linux
SALTO_D_LINEA = '\n'

FORMATO_SIN_ID = False
#Desplazamiento desde el comienzo de la linea hasta la fecha
DELTA_FECHA = 8

class infoArchivo:
	"""
	Esta clase recava información de la serie historica para ser
	utilizada en el script simgen9s.py
	"""
	nombreArchivo = None
	anioInicio = None
	anioFin = None
	#Lista con las longitudes de los primeros cuatro años de la serie
	bisiestosInicio = None
	#Lista con las longitudes de los primeros cuatro años a partir del final de la serie(no incluido)	
	bisiestosFin = None
	#Primer año bisiesto de la serie
	primerBisiesto = None
	#Longitud de la serie medida en años
	lenSerie = None
	def __init__(self,nombreArchivo=None):
		
		delta = DELTA_FECHA
		if(FORMATO_SIN_ID):
			delta=0
			
		self.nombreArchivo = nombreArchivo		
		archivo = open(self.nombreArchivo)
		datos = archivo.read().split(SALTO_D_LINEA)
		self.anioInicio = int(datos[0][delta:4+delta])
		self.anioFin = int(datos[-2][delta:4+delta])
		self.lenSerie = self.anioFin-self.anioInicio+1
		self.calcularBisiestos()
	
		
	def calcularBisiestos(self):
		"""
		Calcula las listas de cuatro anios con las longitudes de los mismos.
		 A partir del anio inicio y del anio fin.
		"""
		anios = []
		#Lista con la longitud de cuatro anios a partir del inicial
		for anio in range(self.anioInicio,self.anioInicio+4):
			if((anio % 4 == 0 and anio % 100 != 0) or (anio % 100 == 0 and anio % 400 == 0)):
				anios.append(366)
				if(self.primerBisiesto == None): self.primerBisiesto = anio 
			else:
				anios.append(365)
				
		self.bisiestosInicio = anios	
		anios = []
		#Lista con la longitud de cuatro anios a partir del final+1
		for anio in range(self.anioFin+1,self.anioFin+5):
			if((anio % 4 == 0 and anio % 100 != 0) or (anio % 100 == 0 and anio % 400 == 0)):
				anios.append(366)
			else:
				anios.append(365)
		
		self.bisiestosFin = anios	

class promedio:
	"""
	Genera medias a partir de un archivo con formato "prec(float) tempMax(float) tempMin(float)"
	"""
	nombreArchivoIn = None	
	cantRegistros = None
	archivo = None
	medias = None
	generadas = False
	
	def __init__(self,nombreArchivoIn=None,cantRegistros=10):
		"""
		Inicialización de la clase:
		cantRegistros: Cantidad de registros a promediar
		"""		
		self.nombreArchivoIn = nombreArchivoIn		
		self.cantRegistros = cantRegistros
		self.archivo = open(nombreArchivoIn)
		
		
	def generarMedias(self):
		"""
		Genera las medias de 'self.cantRegistros' valores, avanzando de 'self.cantRegistros' en 'self.cantRegistros'.
		"""
		#Posiciono puntero de lectura de archivo al inicio
		self.archivo.seek(0)
		self.generadas = False
		self.medias = []
		contador = 0
		acumulados = [.0,.0,.0]
		
		for linea in self.archivo:
			datos = linea[0:-1].split(' ')
			acumulados[0] += float(datos[0])
			acumulados[1] += float(datos[1])
			acumulados[2] += float(datos[2])
			
			contador +=1
			if(contador == self.cantRegistros):
				self.medias.append((acumulados[0]/contador,acumulados[1]/contador,acumulados[2]/contador))
				contador = 0
				acumulados = [.0,.0,.0]
		self.generadas = True
				
	def generarMediasConPaso(self):
		"""
		Genera las medias de 'self.cantRegistros' valores, avanzando de a un registro.
		"""
		#Posiciono puntero de lectura de archivo al inicio
		self.archivo.seek(0)
		self.generadas = False
		self.medias = []
		prec = []
		tempMax = []
		tempMin = []
		
		for linea in self.archivo:
			datos = linea[0:-1].split(' ')
			prec.insert(0,float(datos[0]))
			tempMax.insert(0,float(datos[1]))
			tempMin.insert(0,float(datos[2]))
			if(len(prec) > self.cantRegistros):
				prec.pop()
				tempMax.pop()
				tempMin.pop()
				mediaPrec = np.mean(prec)
				mediaTempMax = np.mean(tempMax)
				mediaTempMin = np.mean(tempMin)
				self.medias.append((mediaPrec,mediaTempMax,mediaTempMin))
				
			
		self.generadas = True

	def guardarMedias(self,nombreArchivoOut):
		"""
		Guarda las medias generadas en el archivo 'nombreArchivoOut'.
		"""
		if(self.generadas):
			archivoSalida = open(nombreArchivoOut,'w')
			
			for fila in self.medias:
				archivoSalida.write("%f %f %f\n" % fila)
			archivoSalida.close()
			
	def getMediasNP(self):
		"""
		Retorna las medias generadas como matriz NumPy
		"""
		if(self.generadas):
			matriz = np.array(self.medias)
		else:
			matriz = None
			
		return matriz
		
