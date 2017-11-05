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

library(randomForest)
#se inicia a aplicar svm
library(e1071)

#https://books.google.cl/books?id=ANIlBAAAQBAJ&pg=PA5&lpg=PA5&dq=random+forest+y+svm+%22wpbc%22&source=bl&ots=949aN4ymRU&sig=Zy5S0MvUD71zP_n9IRUG92BXoOM&hl=es-419&sa=X&ved=0ahUKEwjt2pfMnajXAhUED5AKHbn7C3cQ6AEIKDAB#v=onepage&q=random%20forest%20y%20svm%20%22wpbc%22&f=false

MaxAccuracy = 0
OptFeatureSet = c("c","r1", "t1", "p1", "a1", "su1",
                  "com1", "con1", "pc1", "si1", "df1",
                  "r2", "t2", "p2", "a2", "su2",
                  "com2", "con2", "pc2", "si2", "df2",
                  "r3", "t3", "p3", "a3", "su3",
                  "com3", "con3", "pc3", "si3", "df3",
                  "tt", "eg")

TmpFeatureSet = c("c","r1", "t1", "p1", "a1", "su1",
                  "com1", "con1", "pc1", "si1", "df1",
                  "r2", "t2", "p2", "a2", "su2",
                  "com2", "con2", "pc2", "si2", "df2",
                  "r3", "t3", "p3", "a3", "su3",
                  "com3", "con3", "pc3", "si3", "df3",
                  "tt", "eg")

GloOptFeatureSet = NULL

while(length(OptFeatureSet) > 5){
  
  print("NEW iteracion")
  set.seed(21)
  modelo = randomForest(c~ ., data=bc_data[,OptFeatureSet], ntree = 610, importance=TRUE, proximity=TRUE, ntry=2)
  tabla = modelo$importance
  
  #se obtiene la de menor importancia
  menor = tabla[,4][1]
  posicionMenor = 1
  for(i in 1:length(tabla[,4])){
    if(menor > tabla[,4][i]){
      menor = tabla[,4][i]
      posicionMenor = i
    }
  }
  
  d = OptFeatureSet[posicionMenor+1]
  OptFeatureSet = OptFeatureSet[-which(OptFeatureSet %in% d)]
  print(OptFeatureSet)
  #obj4 = tune(svm, c~., data = bc_fn[,OptFeatureSet], kernel = "radial", ranges = list(gamma = 2^(-2:4), cost = 2^(-1:4), tunecontrol = tune.control(sampling = "cross", cross = 10 )))
  obj4 = tune(svm, c~., data = bc_fn[,1:10], kernel = "linear", ranges = list(cost = 2^(-2:4)), tunecontrol = tune.control(sampling = "cross", cross = 10 ))
  SVMAverageAccuracy = 1.0 - obj4$best.performance
  print(SVMAverageAccuracy)
  print(length(OptFeatureSet))
  if(MaxAccuracy <= SVMAverageAccuracy){
    MaxAccuracy = SVMAverageAccuracy
    GloOptFeatureSet = OptFeatureSet
  }
}


#modelo radial
# [1] "NEW iteracion"
# [1] "c"    "r1"   "t1"   "p1"   "a1"   "su1"  "com1" "con1" "pc1"  "si1"  "df1" 
# [12] "r2"   "t2"   "p2"   "a2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"  
# [23] "t3"   "p3"   "a3"   "su3"  "con3" "pc3"  "si3"  "df3"  "tt"   "eg"  
# [1] 0.7623684
# [1] 32
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "con1" "pc1"  "si1"  "df1"  "r2"  
# [12] "t2"   "p2"   "a2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"  
# [23] "p3"   "a3"   "su3"  "con3" "pc3"  "si3"  "df3"  "tt"   "eg"  
# [1] 0.7628947
# [1] 31
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "con1" "pc1"  "si1"  "df1"  "r2"  
# [12] "t2"   "p2"   "a2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"  
# [23] "p3"   "a3"   "su3"  "con3" "pc3"  "si3"  "tt"   "eg"  
# [1] 0.7628947
# [1] 30
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "con1" "pc1"  "si1"  "df1"  "r2"  
# [12] "t2"   "p2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"   "p3"  
# [23] "a3"   "su3"  "con3" "pc3"  "si3"  "tt"   "eg"  
# [1] 0.7634211
# [1] 29
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "con1" "si1"  "df1"  "r2"   "t2"  
# [12] "p2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"   "p3"   "a3"  
# [23] "su3"  "con3" "pc3"  "si3"  "tt"   "eg"  
# [1] 0.7623684
# [1] 28
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"  
# [12] "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"   "p3"   "a3"   "su3" 
# [23] "con3" "pc3"  "si3"  "tt"   "eg"  
# [1] 0.7628947
# [1] 27
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"  
# [12] "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"   "p3"   "a3"   "su3" 
# [23] "con3" "pc3"  "tt"   "eg"  
# [1] 0.7628947
# [1] 26
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"  
# [12] "su2"  "com2" "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "con3"
# [23] "pc3"  "tt"   "eg"  
# [1] 0.7631579
# [1] 25
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "su2" 
# [12] "com2" "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "con3" "pc3" 
# [23] "tt"   "eg"  
# [1] 0.7610526
# [1] 24
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "su2" 
# [12] "com2" "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "con3" "tt"  
# [23] "eg"  
# [1] 0.7618421
# [1] 23
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2"
# [12] "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "con3" "tt"   "eg"  
# [1] 0.7615789
# [1] 22
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2"
# [12] "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7686842
# [1] 21
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2" "con2"
# [12] "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7631579
# [1] 20
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2" "con2"
# [12] "pc2"  "si2"  "r3"   "t3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7634211
# [1] 19
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2" "con2"
# [12] "pc2"  "r3"   "t3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7626316
# [1] 18
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "pc2" 
# [12] "r3"   "t3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7644737
# [1] 17
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"  
# [12] "t3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7634211
# [1] 16
# [1] "NEW iteracion"
# [1] "c"    "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"  
# [12] "a3"   "su3"  "tt"   "eg"  
# [1] 0.7676316
# [1] 15
# [1] "NEW iteracion"
# [1] "c"    "a1"   "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "a3"  
# [12] "su3"  "tt"   "eg"  
# [1] 0.7631579
# [1] 14
# [1] "NEW iteracion"
# [1] "c"    "a1"   "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3" 
# [12] "tt"   "eg"  
# [1] 0.7626316
# [1] 13
# [1] "NEW iteracion"
# [1] "c"    "a1"   "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3" 
# [12] "eg"  
# [1] 0.7618421
# [1] 12
# [1] "NEW iteracion"
# [1] "c"    "a1"   "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3"  "eg"  
# [1] 0.7731579
# [1] 11
# [1] "NEW iteracion"
# [1] "c"    "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3"  "eg"  
# [1] 0.7618421
# [1] 10
# [1] "NEW iteracion"
# [1] "c"    "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3" 
# [1] 0.7626316
# [1] 9
# [1] "NEW iteracion"
# [1] "c"    "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "su3" 
# [1] 0.7628947
# [1] 8
# [1] "NEW iteracion"
# [1] "c"    "df1"  "r2"   "t2"   "p2"   "con2" "r3"  
# [1] 0.7628947
# [1] 7
# [1] "NEW iteracion"
# [1] "c"    "r2"   "t2"   "p2"   "con2" "r3"  
# [1] 0.7626316
# [1] 6
# [1] "NEW iteracion"
# [1] "c"    "t2"   "p2"   "con2" "r3"  
# [1] 0.7615789
# [1] 5

#MODELO LINEAL

# [1] "NEW iteracion"
# [1] "c"    "r1"   "t1"   "p1"   "a1"   "su1"  "com1" "con1" "pc1"  "si1"  "df1" 
# [12] "r2"   "t2"   "p2"   "a2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"  
# [23] "t3"   "p3"   "a3"   "su3"  "con3" "pc3"  "si3"  "df3"  "tt"   "eg"  
# [1] 0.7623684
# [1] 32
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "con1" "pc1"  "si1"  "df1"  "r2"  
# [12] "t2"   "p2"   "a2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"  
# [23] "p3"   "a3"   "su3"  "con3" "pc3"  "si3"  "df3"  "tt"   "eg"  
# [1] 0.7628947
# [1] 31
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "con1" "pc1"  "si1"  "df1"  "r2"  
# [12] "t2"   "p2"   "a2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"  
# [23] "p3"   "a3"   "su3"  "con3" "pc3"  "si3"  "tt"   "eg"  
# [1] 0.7628947
# [1] 30
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "con1" "pc1"  "si1"  "df1"  "r2"  
# [12] "t2"   "p2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"   "p3"  
# [23] "a3"   "su3"  "con3" "pc3"  "si3"  "tt"   "eg"  
# [1] 0.7634211
# [1] 29
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "con1" "si1"  "df1"  "r2"   "t2"  
# [12] "p2"   "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"   "p3"   "a3"  
# [23] "su3"  "con3" "pc3"  "si3"  "tt"   "eg"  
# [1] 0.7623684
# [1] 28
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"  
# [12] "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"   "p3"   "a3"   "su3" 
# [23] "con3" "pc3"  "si3"  "tt"   "eg"  
# [1] 0.7628947
# [1] 27
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"  
# [12] "su2"  "com2" "con2" "pc2"  "si2"  "df2"  "r3"   "t3"   "p3"   "a3"   "su3" 
# [23] "con3" "pc3"  "tt"   "eg"  
# [1] 0.7628947
# [1] 26
# [1] "NEW iteracion"
# [1] "c"    "t1"   "p1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"  
# [12] "su2"  "com2" "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "con3"
# [23] "pc3"  "tt"   "eg"  
# [1] 0.7631579
# [1] 25
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "su2" 
# [12] "com2" "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "con3" "pc3" 
# [23] "tt"   "eg"  
# [1] 0.7610526
# [1] 24
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "su2" 
# [12] "com2" "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "con3" "tt"  
# [23] "eg"  
# [1] 0.7618421
# [1] 23
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2"
# [12] "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "con3" "tt"   "eg"  
# [1] 0.7615789
# [1] 22
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "su1"  "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2"
# [12] "con2" "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7628947
# [1] 21
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2" "con2"
# [12] "pc2"  "si2"  "r3"   "t3"   "p3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7631579
# [1] 20
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2" "con2"
# [12] "pc2"  "si2"  "r3"   "t3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7634211
# [1] 19
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "com2" "con2"
# [12] "pc2"  "r3"   "t3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7626316
# [1] 18
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "pc2" 
# [12] "r3"   "t3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7644737
# [1] 17
# [1] "NEW iteracion"
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"  
# [12] "t3"   "a3"   "su3"  "tt"   "eg"  
# [1] 0.7634211
# [1] 16
# [1] "NEW iteracion"
# [1] "c"    "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"  
# [12] "a3"   "su3"  "tt"   "eg"  
# [1] 0.7626316
# [1] 15
# [1] "NEW iteracion"
# [1] "c"    "a1"   "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "a3"  
# [12] "su3"  "tt"   "eg"  
# [1] 0.7631579
# [1] 14
# [1] "NEW iteracion"
# [1] "c"    "a1"   "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3" 
# [12] "tt"   "eg"  
# [1] 0.7626316
# [1] 13
# [1] "NEW iteracion"
# [1] "c"    "a1"   "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3" 
# [12] "eg"  
# [1] 0.7618421
# [1] 12
# [1] "NEW iteracion"
# [1] "c"    "a1"   "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3"  "eg"  
# [1] 0.7631579
# [1] 11
# [1] "NEW iteracion"
# [1] "c"    "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3"  "eg"  
# [1] 0.7618421
# [1] 10
# [1] "NEW iteracion"
# [1] "c"    "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "t3"   "su3" 
# [1] 0.7626316
# [1] 9
# [1] "NEW iteracion"
# [1] "c"    "df1"  "r2"   "t2"   "p2"   "con2" "r3"   "su3" 
# [1] 0.7628947
# [1] 8
# [1] "NEW iteracion"
# [1] "c"    "df1"  "r2"   "t2"   "p2"   "con2" "r3"  
# [1] 0.7628947
# [1] 7
# [1] "NEW iteracion"
# [1] "c"    "r2"   "t2"   "p2"   "con2" "r3"  
# [1] 0.7626316
# [1] 6
# [1] "NEW iteracion"
# [1] "c"    "t2"   "p2"   "con2" "r3"  
# [1] 0.7615789
# [1] 5

# > MaxAccuracy
# [1] 0.7644737
# > GloOptFeatureSet
# [1] "c"    "t1"   "a1"   "com1" "si1"  "df1"  "r2"   "t2"   "p2"   "con2" "pc2" 
# [12] "r3"   "t3"   "a3"   "su3"  "tt"   "eg" 

  