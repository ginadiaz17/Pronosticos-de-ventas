# pronosticos de ventas
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


#################################################################################################
#Modelos con indicadoras
##################################################################################################

#Modelo Lineal con indicadoras 

    t = seq(1,length(yi))
    It = seasonaldummy(yi)
# estimacion por minimos cuadrados ordinarios    

    mod1 = lm(yi ~ t + It)
    
#parametros del modelo    

    summary(mod1)
    
## medidas de ajuste

    (M.lin.ind = medidas(mod1,yi,13))


#Modelo cuadratico con indicadoras

    t2 = t*t
# estimacion por minimos cuadrados ordinarios 

    mod2 = lm(yi ~ t + t2 + It)

#parametros del modelo 

    summary(mod2)

## medidas de ajuste

    (M.Cuad.ind = medidas(mod2,yi,14))

#Modelo cubico con indicadoras

    t3= t*t*t
    t2 = t*t
# estimacion por minimos cuadrados ordinarios 

    mod3 = lm(yi ~ t + t2 + t3 + It)

#parametros del modelo

     summary(mod3)
     
 ## medidas de ajuste    

     (M.Cub.ind = medidas(mod3,yi,15))

#Modelo exponencial-cubico-con indicadoras

     T = length(yi)
     Xt = cbind(rep(1,T),t,t2,t3,It)
     Ds = data.frame(yi,Xt)
    theta.0 = coef(mod3)
    mod4 = nls(yi~exp(Xt%*%theta),
           data=Ds, start= list(theta=theta.0))########????????? no me sale
#parametros del modelo           
           
    summary(mod4)# No sale el R ajustado

    yhat4 = fitted(mod4)
    plot(t,yi,type='b')
    lines(t, yhat4,col='blue')

## medidas de ajuste    

    (M.exp.cub = medidas(mod4,yi,7))

#################################################################################################
#Modelos trigonometricos
################################################################################################

#Modelo lineal trigonometrico

    t = seq(1,length(yi))
    It2 = fourier(yi,2)
    
# estimacion por minimos cuadrados ordinarios    
    
    mod5 = lm(yi ~ t + It2)
    
#parametros del modelo 

    summary(mod5)
    
## medidas de ajuste 

    (M.lin.trig = medidas(mod5,yi,6))

#Modelo cuadratico trigonometrico

     t2= t*t
     It2 = fourier(yi,2) 

# estimacion por minimos cuadrados ordinarios 

    mod7 = lm(yi ~ t + t2 + It2)
    summary(mod7)
    
## medidas de ajuste

    (M.cuad.trig = medidas(mod7,yi,7))

#Modelo cubico trigonometrico

    t3= t*t*t
    It2 = fourier(yi,2)
# estimacion por minimos cuadrados ordinarios 

    mod8 = lm(yi ~ t + t2 + t3 + It2)
    
#parametros del modelo 

    summary(mod8)
## medidas de ajuste

    (M.cub.trig = medidas(mod8,yi,8))
    
 
#####################################################################################################
#Generar modelos Loes modelos locales
#####################################################################################################

    library(Kendall)
    MannKendall(ry)

## Estimacion Modelo loess

    T = length(yi)
    t = seq(1,T)
    yw = loess(yi ~ t, control = loess.control(surface = "direct"))
    fitted(yw)
    yest.yw = yw$fitted
## medidas de ajuste

    (M.loess = medidas(yw,yi,2))#'
    LOESS <- round(M.loess, 4)
    LOESS
   
#Estimación Modelo Hol Winters
#Función
# y = serie, m1 = modelo, k1 = numero parametros
  #X = fitted(m4)
  
      medidas.hw = function(yfit,ry,k1){ 
      T = length(ry)
      yest.1 = yfit
      sse.1 = sum((yest.1-ry)^2)
      ssr.1 = sum((ry-mean(ry))^2) 
      mse.1 = sse.1/(T-k1)
      R2.1 = 1 - sse.1/ssr.1
      Ra2.1 = 1 - (T-1)*(1-R2.1)/(T-k1)
      aic.1 = log((T-k1)*exp(2*k1/T)*mse.1/T)
      bic.1 = log(T^(k1/T)*(T-k1)*mse.1/T)
  
      M1 = c(Ra2.1, (mse.1), aic.1, bic.1)
      names(M1) = c("R2-ajus","MSE","AIC","BIC")
      return(M1)
      }


      m4 = HoltWinters(yi)

      frequency(yi)
       (c(m4$alpha,m4$beta,m4$gamma))#
# la tendencia, componente estacional y y estimada
# se obtienen asi

    Tt = m4$fitted[,2] + m4$fitted[,3]
    St = m4$fitted[,4]
    Yt.hat = m4$fitted[,1]
## medidas de ajuste

    medidas.hw(Yt.hat,yi,3)

#pronosticos

     ypron = predict(m4,12)
     plot(m4, ypron)#

    M.HolWinters = medidas.hw(Yt.hat,ry,3)
    HOLTW <-round(M.HolWinters, 4)
    M.HolWinters
    accuracy(ypron, yf)

#Pronósticos

    tf = seq((T-n+1),T)
    tf2 = tf*tf
    tf3 = tf*tf2
    Itf = seasonaldummy(yf,12)


    pred.Hw <- ypron [ , 1]


    accuracy(pred.Hw, yf)


    plot(tf,yf, type = 'o',ylim=c(4000,15000))
    lines(tf,pred.Hw, type = 'b', pch = 5,col='red' )

    legend("topleft", 
       c("Obs","HW"),
       pch = c(1, 5),
       col = c("black","red"))


### Loess + indicadoras + ARMA
    T = length(yi)
    t = seq(1,T)
    t2 = t*t
    t3 = t2*t
    It = seasonaldummy(yi)

#loess + indicadoras

#paso 1

     mod.loess = loess(yi ~ t,span = 0.25, modelo = TRUE,
                  control = loess.control(surface = "direct"))

#paso 2

# generar la matriz de indicadoras con diciembre

    It4 = cbind(It,rep(0,nrow(It)))
    It4[,12] = rep(1,nrow(It)) - apply(It,1,sum)

#paso 3

# modelo estacional sin la tendencia

    Tt.loess = fitted(mod.loess)
    St.loess = yi - Tt.loess
    mod.est = lm(St.loess ~ -1+ It4)
    summary(mod.est)

# ajuste Loess + estacional
#paso 4

    yhat.loess = fitted(mod.est) + Tt.loess
    par(mfrow=c(1,1))
    plot(t,yi,type='l',ylab=("Ventas"), xlab=("Tiempo"))
    lines(t, yhat.loess,col='blue')
    k = 14
    (M.loess = medidas(yhat.loess,yi,k))##cual se pone primero
     legend("topleft", 
       c("Obs","Loess+Ind"),
       lty = c(1, 1),
       col = c("black","blue"))

##modelos arma

    r = yi-yhat.loess
    acf(r,36)
    pacf(r,36)
    r=ts(r,frequency=12)


#pruebas Ljung-Box

    Box.test(r, lag = 24 , type =  "Ljung-Box")


     library(lmtest)
     library(car)
     library(forecast)
     library(TSA)


#######################################################################
#######################################################################
#Funcion SELECT MODEL que identifica AR(p)

     n = length(yi)
    library(FitAR)
     pvec = SelectModel(r, ARModel="AR", 
                   Criterion="BIC", 
                   lag.max=floor(10*log10(n)), Best=1)

    (p=pvec)              #saco el orden p del AR
    mod2 = FitAR(r,p)
    mod2
    tabla = coef(mod2)
    phi = mod2$phiHat     #fitted parameters
    InvertibleQ(phi)
    par(mfrow=c(1,1))     #grafica raíces autorregresivas






#######################################################################
#######################################################################
#Funcion ARMASUBSETS

     res=armasubsets(y=r,
                nar=14,nma=14,
                y.name='r',
                ar.method='ols')
    par(mfrow=c(1,1))
    plot(res)


#######################################################################
#######################################################################
#Funcion AUTOARIMA

    library(forecast)
    auto.arima(r, d=NA, D=NA,
           max.p=5, max.q=5,max.P=2, max.Q=2,
           max.order=5, start.p=2, start.q=2, start.P=1, start.Q=1,
           stationary=TRUE, seasonal=TRUE,
           ic= "aicc", stepwise=TRUE, trace=FALSE,
           approximation=(length(r)>100 | frequency(r)>12))



#######################################################################
#######################################################################


#####ESTIMACIONES

#estimacion ARMA (4,0)

      mod5 = arima(r,order=c(4,0,0))

     mod5


    coeftest(mod5)
#estimacion ARMA (2,0,2)

    mod6 = arima(r,order=c(2,0,2))

    mod6 

    coeftest(mod6)
#estimacion sARMA ((6,1)(0,0)[12])

        mod8 = arima(r,
             order=c(6,0,1),
             seasonal = list(
               order = c(0,0,0), 
               period = 12))
     summary(mod8)
    library(lmtest)
    coeftest(mod8)

#validacion del mejor modelo

    at = mod6$residuals

    par(mfrow=c(2,2))
    plot(t,at,type='o',ylab='residuo')
    abline(h=0,lty=2)
    plot(density(at),xlab='x',main= '')
    acf(at,ci.type= "ma",36,main="")
    pacf(at,36,main="")
    qqnorm(at)
qqline(at,col=2)
cpgram(at)

#pruebas Ljung-Box

    Box.test(at, lag = 24 , type = "Ljung-Box")






#paso 5

    m=12
    tf = seq(T-12+1,T)
    yf.loess = predict(mod.loess, 
                   data.frame(t = tf))


#-paso 6

    Itf = seasonaldummy(yi,m)
    Itf4 = cbind(Itf,rep(0,nrow(Itf)))
    Itf4[,12] = rep(1,nrow(Itf)) - apply(Itf,1,sum)

#pronostico

     yf.est = predict(mod.est,
                 data.frame(It4 = I(Itf4)))



#Pronostico del componente del error####

    p.ar = predict(mod6,n.ahead=3)$pred
#paso 7

    m=12
    n = length(yi)
    yf = yi[(T-12+1):T]
    tf = seq(T-12+1,T)
    yf.total = yf.loess+yf.est

    accuracy(yf.total,yf)#Mape


#graficas

     par(mfrow=c(1,1))
     plot(tf,yf, type = 'o',ylim=c(4000,15000),ylab=("Ventas"), xlab=("Tiempo"))
    lines(tf,yf.total, type = 'b', pch = 5,col='red' )
    lines(tf,p.total, type = 'c', pch = 4,col='red' )




    legend("topleft", 
       c("Obs","Loess+Ind","Es+ARMA"),
       pch = c(1, 5),
       col = c("black","blue","red"))
       
# compara mape

    A=rbind(
    accuracy(yf.total,yf), 
    accuracy(p.total,yf))
    rownames(A)=c("estructural","E+arma(2,2)")
    (A)

    
    
    



