

#ARIMA
j=9

for(i in 1:7){
  
k = 196-i
Bio_ts <- ts(DADOSANALISE[1:k, c('Toxinas.Lipofílicas')])
fit <- auto.arima(Bio_ts,seasonal = isSeasonal(Bio_ts))
fcast <- forecast(fit, h=j)

print("Step ",j)
print(j)

print(accuracy(fcast$mean,x[(k+1):204,1]))
j <- j + 1

}

#VAR
j=9

for(i in 1:7){
k = 196-i
data <- cbind(as.numeric(DADOSANALISE$Toxinas.Lipofílicas[1:k]), as.numeric(DADOSANALISEFITO$V2[1:k]))
colnames(data) <- c("Bio", "Fito")
var_select <- VARselect(data)
var <- VAR(data,min(var_select$selection))
pred <- VARpred(var, h = j)

print("Step ",j)
print(j)

print(accuracy(pred$pred[,1],x[(k+1):204,1]))
j <- j + 1
}




#VARMA
c=9

for(l in 1:7){
  k = 196-l

  data <- cbind(as.numeric(DADOSANALISE$Toxinas.Lipofílicas[1:k]), as.numeric(DADOSANALISEFITO$V2[1:k]))
  Bestp = 0
  Bestq = 0
  
  SelectVarma = Eccm(data)
  SizeSelectVarma <- dim(SelectVarma$pEccm)
  min=10
  
  for(i in 1:SizeSelectVarma[1]){
    for(j in 1:SizeSelectVarma[2]){
      if(SelectVarma$pEccm[i,j] >= 0.05){
        sum <- i+j
        if(sum<min){
          min<-sum
          Bestp=i-1
          Bestq=j-1
        }
      }
    }
  }
  
  print("Parametros")
  print(Bestp)
  print(Bestq)
  
  varma <- VARMA(data, p=Bestp,q=Bestq)
  pred <- VARMApred(varma,h = c)
  
  print("Step ",c)
  print(c)
  
  print(pred$pred[,1])
  print(x[(k+1):204,1])
  
  print(k)
  
  print(accuracy(pred$pred[,1],x[(k+1):204,1]))
  
  c <- c + 1
}




#NNAR
j=9

for(i in 1:7){
  
  k = 196-i
  Bio_ts <- ts(DADOSANALISE[1:k, c('Toxinas.Lipofílicas')])
  fit_NNAR <- nnetar(Bio_ts)
  fcast <- forecast(fit_NNAR, h=j)
  
  
  print("Step ",j)
  print(j)
  
  print(accuracy(fcast$mean,x[(k+1):204,1]))
  j <- j + 1
  
}


# RNN

  
  Bio_ts <- ts(DADOSANALISE[, c('Toxinas.Lipofílicas')])
  diffed = diff(Bio_ts, differences = 1)
  Bio <- diffed
  
  lag_transform <- function(x, k= 1){
    
    lagged =  c(rep(NA, k), x[1:(length(x)-k)])
    DF = as.data.frame(cbind(lagged, x))
    colnames(DF) <- c( paste0('x-', k), 'x')
    DF[is.na(DF)] <- 0
    return(DF)
  }
  
  supervised = lag_transform(Bio, 1)
  
  N = nrow(supervised)
  print("N")
  print(N)
  n = round(N *0.99 digits = 0)
  train = supervised[1:n, ]
  test  = supervised[(n+1):N,  ]
  
  scale_data = function(train, test, feature_range = c(0, 1)) {
    x = train
    fr_min = feature_range[1]
    fr_max = feature_range[2]
    std_train = ((x - min(x) ) / (max(x) - min(x)  ))
    std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
    
    scaled_train = std_train *(fr_max -fr_min) + fr_min
    scaled_test = std_test *(fr_max -fr_min) + fr_min
    
    return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
    
  }
  
  invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
    min = scaler[1]
    max = scaler[2]
    t = length(scaled)
    mins = feature_range[1]
    maxs = feature_range[2]
    inverted_dfs = numeric(t)
    
    for( i in 1:t){
      X = (scaled[i]- mins)/(maxs - mins)
      rawValues = X *(max - min) + min
      inverted_dfs[i] <- rawValues
    }
    return(inverted_dfs)
  }
  
  Scaled = scale_data(train, test, c(-1, 1))
  
  y_train = Scaled$scaled_train[, 2]
  x_train = Scaled$scaled_train[, 1]
  
  y_test = Scaled$scaled_test[, 2]
  x_test = Scaled$scaled_test[, 1]
  
  dim(x_train) <- c(length(x_train), 1, 1)
  dim(y_train) <- c(length(y_train), 1, 1)


  model<-trainr(Y=y_train,X=x_train,learningrate = 0.5,hidden_dim = 10,numepochs=100)
    
  k = 205
  X = x_train[length(x_train)]
  
  for(t in 1:20){
    
  scaler = Scaled$scaler
  predictions = numeric(t)
  #X = x_test[length(x_test)]
  #X = x_train[length(x_train)]
  print("x")
  print(X)
  
  #Previsão de 5 semanas      
  for(i in 1:t){
    dim(X) = c(1,1,1)
    yhat = model %>% predictr(X)
    X = yhat
    # invert scaling
    yhat = invert_scaling(yhat, scaler,  c(-1, 1))
     # invert differencing
    yhat  = yhat 
    # store
    predictions[i] <- yhat
  }

  X = predictions[length(predictions)]
  
  print(predictions)
  
  print("step")
  print(t)
  print(accuracy(predictions,x[(k-t):204,1]))
  

  t=t+1
}

  
