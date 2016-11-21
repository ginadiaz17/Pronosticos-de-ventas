# pron-sticos-de-ventas
##cargar base de datos tendencia global

    S <-  read.table("pronostico.txt", header = TRUE,stringsAsFactors=FALSE)

    library(forecast)
   
    library(xtable)
 
    attach(S)


####################################################################################################
#convertir datos en serie de tiempo

    ry = ts(S[,2],frequency=12,start=c(2011,1), end = c(2016,08))
    ts.plot(ry,main="Demanda real")

### descomposición STL

    m=stl(ry, s.window = "per")
    St = m$time.series[,1]
    Tt = m$time.series[,2]
    et = m$time.series[,3]
    plot(m, main = "Descomposicion de las componentes de la serie")
    plot(Tt)


#####################################################################################################
#Generar modelos lineal, cuadratico, cubico y exponencial de tendencia (Modelos Globales)
#####################################################################################################

# Generar datos para validacion cruzada: dejar el ultimo año

     T = length(ry)
    yi = ts(ry[1:(T-12)],frequency = 12)
    yf = ts(ry[(T-12+1):T],frequency = 12)



#Función medidas

    medidas = function(m,ry,k){
    T = length(ry)
    yest = fitted(m)
    sse = sum((yest-ry)^2)
    ssr = sum((ry-mean(ry))^2) 
    mse = sse/(T-k)
    R2 = 1 - sse/ssr
    Ra2 = 1 - (T-1)*(1-R2)/(T-k)
    aic = log((T-k)*exp(2*k/T)*mse/T)
    bic = log(T^(k/T)*(T-k)*mse/T)
  
    M = c(mse,Ra2,  aic, bic)
    names(M) = c("mse","R2-ad","log.aic","log.bic")
    return(M)
    }

#AJUSTAR 4 MODELOS: LINEAL, CUADRATICO, CUBICO, LOG-LIN

    t = seq(1:(T-3))
    t2 = t^2
    t3 = t^3
    lyi = log(yi)#el log no se puede sacar a valores negativos, genera mensaje de advertencia


#Modelos: lineal, cuadratico, cubico
# estimacion por minimos cuadrados ordinarios

    mod.lin = lm(yi~ t)
    mod.cuad = lm(yi~t+t2)
    mod.cub = lm(yi~t+t2+t3)
#parametros del modelo

    summary(mod.lin)
    summary(mod.cuad)
    summary(mod.cub)
    
#medidas de ajuste

    mod.lin = medidas(mod.lin,yi,2)
    mod.cuad = medidas(mod.cuad,yi,3)
    mod.cub = medidas(mod.cub,yi,4)

###función medidas modelos lineal, cubico, cuadratico

    (cbind(mod.lin,mod.cuad,mod.cub))



# Modelo exponencial

    mod.llin = lm(lyi~t) # paso 1) estimar el modelo log-lineal auxiliar
    Ds = data.frame(yi,t)# paso 2) guardar los datos en un data.frame
    b0.est = mod.llin$coefficient[1]# paso 3) guardar los parametros del log-lineal
    b1.est = mod.llin$coefficient[2]

     mod.exp = nls(yi~exp(beta0+beta1*t),# paso 4) usar la funcion nls
              data=Ds,
              start=list(beta0=b0.est, beta1=b1.est))
  
#parametros del modelo

    summary(mod.exp)
    mod.exp = medidas(mod.exp,yi,2)
    mod.exp

################################################################################################################
#Generar los mse, R2-ad, log.aci y el log.bic de los modelos lineal, cuadratico, cubico y exponencial de tendencia

#Función medidas

    medidas = function(m,ry,k){
    T = length(ry)
    yest = fitted(m)
    sse = sum((yest-ry)^2)
    ssr = sum((ry-mean(ry))^2) 
    mse = sse/(T-k)
    R2 = 1 - sse/ssr
    Ra2 = 1 - (T-1)*(1-R2)/(T-k)
    aic = log((T-k)*exp(2*k/T)*mse/T)
    bic = log(T^(k/T)*(T-k)*mse/T)
  
    M = c(mse,Ra2,  aic, bic)
    names(M) = c("mse","R2-ad","log.aic","log.bic")
    return(M)
    }

## medidas de ajuste lineal, cubico, cuadratico, esponencial

    M.exp = medidas(mod.exp,yi,2)
    (M = cbind(M.lin,M.cuad,M.cub,M.exp))

