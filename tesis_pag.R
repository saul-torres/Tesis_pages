# OBTENEMOS LOS NOMBRES LARGOS DE LOS CÓDIGOS DE LAS MATERIAS
# (Sacados de TDR)
library(xlsx)
cod_mat = read.xlsx("F:/R-Tesis/codigos_mat.xlsx", sheetIndex = 1)
colnames(cod_mat) = c("mat", "mat_name")
save(cod_mat, file = 'F:/R-Tesis/codigos_mat.RData')


# REPRESENTANDO LOS DATOS

## cargamos los datos y como son "characters" los pasamos a "numeric"
load('F:/R-Tesis/reg.tesis.DF.RData')
reg.tesis.DF[,'pag'] = as.numeric(as.character(reg.tesis.DF[,'pag']))
reg.tesis.DF[,'mat'] = as.numeric(as.character(reg.tesis.DF[,'mat']))

## eliminamos valores nulos
reg.tesis.DF.PLUS = reg.tesis.DF
reg.tesis.DF.PLUS = na.omit(reg.tesis.DF.PLUS)

## cargamos los nombres largos de las materias 
load('F:/R-Tesis/codigos_mat.RData')
reg.tesis.DF.PLUS = merge(reg.tesis.DF.PLUS, cod_mat, by="mat")

## Tabla con las materias y el número de tesis en cada una
pop.mat = sort(table(reg.tesis.DF.PLUS$mat_name), decreasing = T)
pop.mat = data.frame(num.tesis = pop.mat)
pop.mat$mat_name = row.names(pop.mat)

## Preparamos los datos con la información que queremos mostrar
## Añadimos el número de tesis por materia
plot.DF = reg.tesis.DF.PLUS
plot.DF = merge(plot.DF, pop.mat, by = 'mat_name', all.x = T)
plot.DF$mat = paste0(plot.DF$mat, ' (', plot.DF$num.tesis, ')')
plot.DF$mat_name = paste0(plot.DF$mat_name, ' (', plot.DF$num.tesis, ')')

library(plyr)
## Calculamos la mediana del número de páginas
med.pag = ddply(plot.DF,
                 .(mat_name), 
                 summarize, 
                 median = median(pag) )
med.pag = med.pag[order(med.pag$median),]

## Añadimos la mediana
plot.DF$mat_name = factor(plot.DF$mat_name, labels = med.pag$mat_name, 
                       levels = med.pag$mat_name)
plot.DF = merge(plot.DF, med.pag, by = 'mat_name', all.x = T)

library(ggplot2)
library(RColorBrewer)

## Gráfico de boxplots para el número de páginas por materia
tesis.box = ggplot(plot.DF, aes(x = mat_name, y = pag, fill = median)) + 
  geom_boxplot(lwd = 0.3) +
  ylim(0,1000) +
  ylab('Número de páginas') +
  coord_flip() +
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  theme(legend.position = "none", axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 8))
tesis.box

## Gráfico de barras para el número de tesis por materia
plot_bars.DF = pop.mat
plot_bars.DF$mat = factor(pop.mat$mat, labels = pop.mat$mat, 
                       levels = pop.mat$mat)

tesis.num = ggplot(plot_bars.DF, aes(x = mat, y = num.tesis, fill = num.tesis)) + 
  geom_bar(lwd = 0.3, stat = 'identity') + 
  theme_bw() +
  scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
  ylab('Número de tesis analizadas') +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, 
                                   vjust = 0))
tesis.num
