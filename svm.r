#se carga la base de datos
bc_data <- read.table("wpbc.data", header = FALSE, sep = ",")

colnames(bc_data) = c("id", "c", "t","r1", "t1", "p1", "a1", "su1",
                      "com1", "con1", "pc1", "si1", "df1",
                      "r2", "t2", "p2", "a2", "su2",
                      "com2", "con2", "pc2", "si2", "df2",
                      "r3", "t3", "p3", "a3", "su3",
                      "com3", "con3", "pc3", "si3", "df3",
                      "tt", "eg")

#se eliminan registros con error
error = bc_data$eg == '?'
bc_data = bc_data[!error,]

#se modifica la ultima variable a numerica
bc_data$eg = as.numeric(bc_data$eg)

#eliminar variables que son necesarias
bc_data$id = NULL
bc_data$t = NULL

library(foreach)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

foreach(i=2:33) %dopar% {
  #se seta la semilla
  bc_data[i] = range01(bc_data[i])
}

#se inicia a aplicar svm
library(e1071)

#PROBAR CON UN MODELO LINEAL
obj = tune(svm, c~., data = bc_data, kernel = "linear", ranges = list(cost = 2^(-2:4)), tunecontrol = tune.control(sampling = "cross", cross = 10 ))
summary(obj)

# Parameter tuning of 'svm':
# 
#   - sampling method: 10-fold cross validation
# 
# - best parameters:
#   cost
# 8
# 
# - best performance: 0.2378947
# 
# - Detailed performance results:
#   cost     error dispersion
# 1  0.25 0.2631579 0.06291376
# 2  0.50 0.2886842 0.09449327
# 3  1.00 0.2886842 0.09449327
# 4  2.00 0.2631579 0.07899609
# 5  4.00 0.2426316 0.10243872
# 6  8.00 0.2378947 0.08961289
# 7 16.00 0.2631579 0.09590732

#SE CALCULA SU ACCURACY
#pred = predict(obj$best.model, x)

#PROBAR CON UN KERNEL = RADIAL
obj2 = tune(svm, c~., data = bc_data, kernel = "radial", ranges = list(gamma = 2^(-2:4), cost = 2^(-1:4), tunecontrol = tune.control(sampling = "cross", cross = 10 )))
summary(obj2)
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   gamma cost tunecontrol
# 0.25  0.5       FALSE
# 
# - best performance: 0.2371053 

#comparando con la seleccion de caracteristicas de randomForest
bc_fn = bc_data[c("c","eg","df1","tt","su3","r2","t2","t1","con2","t3","p2","r3","si2","si1","a3","pc2","com2","df2","p3","com1","con1","a2","su1","a1","df3","con3","pc3","pc1","su2","r1","si3","p1","com3")]

#CON KERNEL = LINEAL
obj3 = tune(svm, c~., data = bc_fn[,1:10], kernel = "linear", ranges = list(cost = 2^(-2:4)), tunecontrol = tune.control(sampling = "cross", cross = 10 ))
summary(obj3)

# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 0.25
# 
# - best performance: 0.2368421 
# 
# - Detailed performance results:
#   cost     error dispersion
# 1  0.25 0.2368421 0.09537633
# 2  0.50 0.2368421 0.09537633
# 3  1.00 0.2368421 0.09537633
# 4  2.00 0.2368421 0.09537633
# 5  4.00 0.2368421 0.09537633
# 6  8.00 0.2368421 0.09537633
# 7 16.00 0.2368421 0.09537633

#CON KERNEL = RADIAL
obj4 = tune(svm, c~., data = bc_fn[,1:10], kernel = "radial", ranges = list(gamma = 2^(-2:4), cost = 2^(-1:4), tunecontrol = tune.control(sampling = "cross", cross = 10 )))
summary(obj4)

# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   gamma cost tunecontrol
# 0.5    1       FALSE
# 
# - best performance: 0.2276316 
