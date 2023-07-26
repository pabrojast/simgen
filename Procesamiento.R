
#UTF-8
#Encoding(current_path) <- "UTF-8"
#directorio de simgen y script
#opcion b
#setwd("D:/Dropbox-Temporal/cazalac/simgen-Limari_v3")

#Alternativa después de primera ejecución, requiere libreria rstudioapi
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

print(getwd())

#funciones
source("Procesamiento/libreria_preproceso.R")

#librerias
cargar_librerias()

#elimina TODO lo relacionado a otras generaciones, menos los input files ( Input_Datos ), generalmente no es necesario modificar
remover_archivos_antiguos(directorio = c("output_sim","obs","acru","Procesamiento_output","daily","input_sim"))

#relleno y formateo de datos, los datos deben estar en la carpeta  "Input_Datos"
formateo_datos_pais()

#correlacion con modelos, para una nueva cuenca usar clearcache = "YES"
corrcmip5models(LongMin = 19, LongMax = 16.5, LatMin = -32.77, LatMax = -31.38, umbral = 0.2, clearcache = "NO")
#corrcmip5models(LongMin = -71.71, LongMax = -70.28, LatMin = -31.42, LatMax = -30.25, umbral = 0.30, clearcache = "NO")

#preparacion de datos
preparacion_datos(codigo.estacion.inicial = 2000 )