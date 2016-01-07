#########################################################
# ANÁLISIS DEL NÚMERO DE PÁGINAS DE LAS TESIS EN ESPAÑA #
#########################################################

# Basado en la idea y el código de Marcus W Beck (fawda123)
# https://github.com/fawda123
# http://beckmw.wordpress.com/
# http://www.r-bloggers.com/average-dissertation-and-thesis-length-take-two/

# Basado en los datos disponibles en "Tesis en Red".
# http://www.tdx.cat/


# 1- OBTENER LOS DATOS DE UN ÚNICO REGISTRO

## Obtenemos la tabla de datos de la tesis y la almacenamos como dataframe
url = "http://www.tdx.cat/handle/10803/108690"
data_table = readHTMLTable(url, which=1)
row.names(data_table) = data_table[,1]
data_DF = as.data.frame.matrix(data_table) 

## Buscamos y almacenamos el número de páginas
pag_data = strsplit(data_DF["Páginas:",2], " ")
pag_data = sapply(pag_data[[1]], as.numeric)
pages = pag_data[[1]]
pages

## Buscamos y almacenamos la fecha
date = data_DF["Fecha de defensa:",2]
date

## Buscamos y almacenamos la materia
mat_data = strsplit(data_DF["Materia(s):",2], " ")
mat_data = sapply(mat_data[[1]], as.numeric)
materia = mat_data[[1]]
materia

## Creamos un registro para almacenar los datos
reg = c(date, materia, pages)
reg


# 2- CÓMO OBTENER LA URL DE UNO DE LOS REGISTROS EN LA PÁGINA DE BÚSQUEDA

## Partimos de la búsqueda por fecha descendente.
thepage = url("http://www.tdx.cat/browse?rpp=20&etal=-1&type=dateissued&sort_by=2&order=DESC&offset=20")
## Leemos la web
htmlCode = readLines(thepage)
## Buscamos aquellas líneas en las cuales aparezca el enlace a las fichas de las tesis
mypattern = '<a href=\"/handle/10803/([^<]*)\">'
## Almacenamos esos valores
datalines = grep(mypattern, htmlCode, value=TRUE)
datalines
## Nos quedamos sólo con el código identificatorio de la tesis
tesis_id = gsub('<a href=\"/handle/10803/', "", datalines[1])
tesis_id = gsub('\">', "", tesis_id)


# 3- CREAMOS UN CICLO PARA OBTENER LA INFORMACIÓN DE LOS DISTINTOS REGISTROS

regist.ident.tesis = c(0)
## En cada página hay 20 registros. 250 páginas son 5000 registros.
### Error en el 83.
counter.A = c(1:250)
for (i in counter.A) {
  url.base = 'http://www.tdx.cat/browse?rpp=20&etal=-1&type=dateissued&sort_by=2&order=DESC&offset='
  url.contador = i*20
  url.input = paste(url.base, url.contador, sep="")
  
  web.page = url(url.input)
  web.code = readLines(web.page)
  web.pattern = '<a href=\"/handle/10803/([^<]*)\">'
  web.lines = grep(web.pattern, web.code, value = TRUE)
  
  for (j in 1:20) {
    ident.tesis = gsub('<a href=\"/handle/10803/', "", web.lines[j])
    ident.tesis = gsub('\">', "", ident.tesis)
    regist.ident.tesis = c(regist.ident.tesis, ident.tesis)
  }
}
## Salvamos los registros
save(regist.ident.tesis, file = 'F:/R-Tesis/regist.ident.tesis.RData')

# 4- OBTENEMOS LA INFORMACIÓN DE CADA UNO DE LOS REGISTROS
load('F:/R-Tesis/regist.ident.tesis.RData')

library(XML)
matrix.aux = matrix(0, ncol = 4, nrow = 1)
reg.tesis.DF = data.frame(matrix.aux)
colnames(reg.tesis.DF) = c("code", "pag", "date", "mat")
### Hay un registro que da error. Actualmente está en el 95.
### Pero a medida que se añadan más registros, su posición variará.
### La solución definitiva sería eliminarle de la lista de registros...
counter.B = c(2:94, 96:5001)
for (k in counter.B) {
  url.tesis = paste('http://www.tdx.cat/handle/10803/', regist.ident.tesis[k], sep="")
  
  tesis.table = readHTMLTable(url.tesis, which=1)
  tesis.DF = as.data.frame.matrix(tesis.table)
  pag.aux.table = tesis.table[ tesis.table[,1]=="Páginas:" , ]
  pag.aux = pag.aux.table[1,2]
  pag.aux = sapply(pag.aux, as.character)
  pag.tesis.data = strsplit(pag.aux, " ")
  pag.tesis.data = sapply(pag.tesis.data, as.numeric)
  pag.tesis = pag.tesis.data[[1]]
  
  date.aux.table = tesis.table[ tesis.table[,1]=="Fecha de defensa:" , ]
  date.tesis = date.aux.table[1,2]
  date.tesis = sapply(date.tesis, as.character)
  
  mat.aux.table = tesis.table[ tesis.table[,1]=="Materia(s):" , ]
  mat.aux = mat.aux.table[1,2]
  mat.aux = sapply(mat.aux, as.character)  
  mat.tesis.data = strsplit(mat.aux, " ")
  mat.tesis.data = sapply(mat.tesis.data, as.numeric)
  mat.tesis = mat.tesis.data[[1]]
  
  reg.tesis = c(regist.ident.tesis[k], pag.tesis, date.tesis, mat.tesis)
  reg.tesis.DF = rbind(reg.tesis.DF, reg.tesis)
}

# PARA OBTENER LOS DATOS EJECUTAR 3. Y 4.
# Y SALVAMOS LOS DATOS.
reg.tesis.DF
save(reg.tesis.DF, file = 'F:/R-Tesis/reg.tesis.DF.RData')
