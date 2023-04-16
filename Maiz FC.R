# Esta línea especifica el directorio de este archivo, pero sólo es necesario
# si se va a guardar el gráfico por código o el CSV no está en la misma carpeta,
# además debe actualizarse a cada usuario.
setwd("D:/Espectroscópicos")


# Esta función arroja el "vector" de pendientes de un vector de absorbancias
pend = function(lista){
        lista[ -length(lista)] - lista[-1]
}

# Esta función arroja el dataframe de lambda y pendientes a partir de un dataframe
# de lambda y absorbancias
framepend = function(df){
        framep = data.frame(
                lambda = unlist(df[1], use.names = FALSE)[-nrow(df)] 

        )
        for(i in 2:(length(colnames(df))) ){
                framep = cbind(framep, pend(unlist(df[i], use.names = FALSE)))
        }
        colnames(framep) = colnames(df)
        return(framep)
}


# Esta función recibe un dataframe, col1 tiene lambdas, el resto de columnas
# debe contener c/u un vector con la derivada de las absorbancias,
# arroja un "vector" con las lambdas correspondientes a
# las pendientes más cercanas a 0 entre las 2 lambdas, una por cada vector

ceros = function(df, lambdaizq=700, lambdader=800){
        cer = c()
        lam = c()
        a = abs(subset(df, df[1] > lambdaizq & df[1] < lambdader))
        rownames(a) = 1:nrow(a)
        for(i in 2:ncol(a) ) {
                #print(i)
                cero = min(a[i])
                #print(cero)
                cer = c(cer, cero)
                lamd = a[which(a[i] == cero),1]
                #print(lamd)
                lam = c(lam, lamd)
        }
        return(lam)
        
}




# Ejecución de ejemplo

# Estas líneas llaman a las librerías de las que se depende
library(ggplot2)
library(reshape2)

# Esta línea convierte el csv en data frame
ma = read.csv("Maiz.csv", header = T)

# Esta linea obtiene el dataframe de pendientes
map = framepend(ma)

# Esta línea obtiene el "vector" con las máximas absorbancias
maxabs = ceros(map)

# Esta línea cambia el formato del dataframe de pendientes para obtener otro df
# más cómodo de graficar
map2 = melt(map, id.vars = colnames(map)[1], variable.name = "Pendientes")

# limites en y
pinf = -0.0015
psup = 0.0015

# Este código construye el gráfico con Ggplot2
ggplot(map2, aes(lambda,value))+
  geom_line(aes(colour=Pendientes))+
  scale_x_continuous(name="Longitud de onda (nm)", breaks = seq(400,800,8), limits = c(400,800))+
  scale_y_continuous(name="Pendientes", breaks = seq(pinf,psup,0.0001), limits = c(pinf,psup))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = maxabs, colour=c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))


ggsave("5_series_de_maiz.png",
       width = 60,
       height = 40,
       units = "cm",
       dpi = 300
       )









# Código anterior no general

#plot(map2$lambda~map2$series, map2$value~map2$series)

# ggplot(data = map, aes(x=ï..lambda))+
#         geom_line(aes(ï..lambda, B08, colour="B08"))+
#         geom_line(aes(ï..lambda, C08, colour="C08"))+
#         geom_line(aes(ï..lambda, D08, colour="D08"))+
#         geom_line(aes(ï..lambda, E08, colour="E08"))+
#         geom_line(aes(ï..lambda, F08, colour="F08"))+
#         
#         #ylim(-0.001,0.001)+
#         #xlim(400,800)+
#         scale_x_continuous(name="Longitud de onda (nm)", breaks = seq(400,800,8), limits = c(400,800))+
#         scale_y_continuous(name="Absorbancia", breaks = seq(-0.001,0.001,0.0001), limits = c(-0.001,0.001))
#         
# 
# mad = data.frame(
#  lambda = ma$ï..lambda[-301],
#  dB08 = ma$B08..T1.R2.[-301] - ma$B08..T1.R2.[-1],
#  dC08 = ma$C08..T2.R2.[-301] - ma$C08..T2.R2.[-1],
#  dD08 = ma$D08..T3.R2.[-301] - ma$D08..T3.R2.[-1],
#  dE08 = ma$E08..T4.R2.[-301] - ma$E08..T4.R2.[-1],
#  dF08 = ma$F08..control.R2.[-301] - ma$F08..control.R2.[-1]
# )
# 
# png(filename = "maiz.png", width = 6000, height = 3500)
# plot(mad$lambda,
#      mad$dB08,
#      type="l",
#      xlim=c(350, 800),
#      ylim=c(-0.005, 0.005),
#      xaxt="n",
#      lwd=2)
# axis(1, at = seq(350,800, by=2))
# abline(h=0,
#        col="red")
# abline(v=seq(350, 798, by=2),
#        col=c("green", "blue"))
# #  grid()
# 
# dev.off()
# 
# 
# 
# 
# 
# 
# # Para obtener cercanos a 0
# which(mad$dB08[which(mad$lambda > 350)] > -0.00008 & mad$dB08[which(mad$lambda > 350)] < 0.00008)
# 
# which(abs(mad$dB08[which(mad$lambda > 350)]) < 0.00005)
# 
# min(abs(mad$dB08[which(mad$lambda > 350)]))
# 
