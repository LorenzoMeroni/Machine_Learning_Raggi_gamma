rm(list=ls()); graphics.off(); cat("\014")
# Librerie ------
library(caret)
library(kernlab)
library(corrplot)
library(data.table)
library(kernlab)
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(pROC)
library(ranger)
library(MASS)
library(e1071)
# Analisi esplorativa #####
setwd("C:/Users/lorenzo/OneDrive/Desktop/ML_Progetto")
Raggi.data <- read.csv("magic04.data")
colnames(Raggi.data) <- c('fLength','fWidth', 'fSize', 'fConc', 'fConc1', 'fAsym', 'fM3Long', 'fM3Trans', 'fAlpha', 'fDist', 'class')
View(Raggi.data)
str(Raggi.data)
#'data.frame':	19019 obs. of  11 variables:
#  $ fLength : num  31.6 162.1 23.8 75.1 51.6 ...
#$ fWidth  : num  11.72 136.03 9.57 30.92 21.15 ...
#$ fSize   : num  2.52 4.06 2.34 3.16 2.91 ...
#$ fConc   : num  0.5303 0.0374 0.6147 0.3168 0.242 ...
#$ fConc1  : num  0.3773 0.0187 0.3922 0.1832 0.134 ...
#$ fAsym   : num  26.27 116.74 27.21 -5.53 50.88 ...
#$ fM3Long : num  23.82 -64.86 -6.46 28.55 43.19 ...
#$ fM3Trans: num  -9.96 -45.22 -7.15 21.84 9.81 ...
#$ fAlpha  : num  6.36 76.96 10.45 4.65 3.61 ...
#$ fDist   : num  205 257 117 356 238 ...
#$ class   : chr  "g" "g" "g" "g" ...

#Convertiamo la variabile risposta in un factor, notiamo infatti dalla strcture
#del dataset come questa sia una variabile integer.
Raggi.data <- transform(Raggi.data,class=as.factor(class))
str(Raggi.data)
summary(Raggi.data)
#Output
#fLength            fWidth           fSize      
#Min.   :  4.284   Min.   :  0.00   Min.   :1.941  
#1st Qu.: 24.336   1st Qu.: 11.86   1st Qu.:2.477  
#Median : 37.149   Median : 17.14   Median :2.740  
#Mean   : 53.251   Mean   : 22.18   Mean   :2.825  
#3rd Qu.: 70.127   3rd Qu.: 24.74   3rd Qu.:3.102  
#Max.   :334.177   Max.   :256.38   Max.   :5.323  
#fConc            fConc1           fAsym         
#Min.   :0.0131   Min.   :0.0003   Min.   :-457.916  
#1st Qu.:0.2358   1st Qu.:0.1285   1st Qu.: -20.588  
#Median :0.3541   Median :0.1965   Median :   4.012  
#Mean   :0.3803   Mean   :0.2147   Mean   :  -4.333  
#3rd Qu.:0.5037   3rd Qu.:0.2853   3rd Qu.:  24.060  
#Max.   :0.8930   Max.   :0.6752   Max.   : 575.241  
#fM3Long           fM3Trans        
#Min.   :-331.78   Min.   :-205.8947  
#1st Qu.: -12.85   1st Qu.: -10.8498  
#Median :  15.31   Median :   0.6898  
#Mean   :  10.54   Mean   :   0.2502  
#3rd Qu.:  35.84   3rd Qu.:  10.9471  
#Max.   : 238.32   Max.   : 179.8510  
#fAlpha           fDist         class    
#Min.   : 0.000   Min.   :  1.283   g:12331  
#1st Qu.: 5.547   1st Qu.:142.499   h: 6688  
#Median :17.677   Median :191.857            
#Mean   :27.645   Mean   :193.824            
#3rd Qu.:45.884   3rd Qu.:240.565            
#Max.   :90.000   Max.   :495.561 


#Commento:
#Notiamo immediatamente che non sono presenti valori mancanti.


#VARIABILE DI INTERESSE OVVERO class

plot(Raggi.data$class, main="Distribuzione variabile risposta Raggi", ylab="Count", xlab="Potabilita'",
     ylim=c(0,15000),col=c(2,3))
#Le due classi sono sbilanciate con una predominanza della classe appartenente
#al non potabile, cio' tuttavia non risulta un problema.

#OUTLIER ANALISI
#Analizziamo la presenza di possibili outlier outlier

boxplot(Raggi.data$fLength,main="major axis of ellipse",col="orange")
boxplot(Raggi.data$fWidth,main="minor axis of ellipse",col="orange")
boxplot(Raggi.data$fSize,main="10-log of sum of content of all pixels",col="orange")
boxplot(Raggi.data$fConc,main="ratio of sum of two highest pixels over fSize",col="orange")
boxplot(Raggi.data$fConc1,main="distance from highest pixel to center",col="orange")
boxplot(Raggi.data$fAsym,main="3rd root of third moment along major axis",col="orange")
boxplot(Raggi.data$fM3Long,main="3rd root of third moment along minor axis",col="orange")
boxplot(Raggi.data$fM3Trans,main="angle of major axis with vector to origin",col="orange")
boxplot(Raggi.data$fAlpha,main="angle of major axis with vector to origin",col="orange")
boxplot(Raggi.data$fDist,main="distance from origin to center of ellipse",col="orange")

#Notiamo la presenza di outlier in quasi tutte le variabili, ci troviamo di fronte 
#ad un trade off ovvero eliminare questi outliers e proseguire l'analisi senza 
#oppoure tenerli. Inizialmente decido di tenerli e poi vedo cosa fare.

#DISTRIBUZIONI DELLE VARIABILI ESPLICATIVE
Raggi.data_temp_g <- Raggi.data[which(Raggi.data$class=="g"),]
Raggi.data_temp_h <- Raggi.data[which(Raggi.data$class=="h"),]
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fLength, main="fLength raggi gamma", ylab="Count", xlab="fLength gamma",col="red",
     breaks = 50)
hist(Raggi.data_temp_h$fLength, main="fLength raggi non gamma", ylab="Count", xlab="fLength hadron",col="blu",
     breaks = 50)
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fWidth, main="fWidth raggi gamma", ylab="Count", xlab="fWidth gamma",col="lightblue",
     breaks = 50)
hist(Raggi.data_temp_h$fWidth, main="fwidth raggi non gamma", ylab="Count", xlab="fWidth non gamma",col="lightblue",
     breaks = 50)
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fSize, main="fSize raggi gamma", ylab="Count", xlab="fSize gamma",col="orange",
     breaks = 50)
hist(Raggi.data_temp_h$fSize, main="fSize raggi non gamma", ylab="Count", xlab="fSize non gamma",col="orange",
     breaks = 50)
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fConc, main="fConc raggi gamma", ylab="Count", xlab="fConc gamma",col="green",
     breaks = 50)
hist(Raggi.data_temp_h$fConc, main="fConc raggi non gamma", ylab="Count", xlab="fConc non gamma",col="green",
     breaks = 50)
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fConc1, main="fConc1 raggi gamma", ylab="Count", xlab="fConc1 gamma",col="red",
     breaks = 50)
hist(Raggi.data_temp_h$fConc1, main="fConc1 raggi non gamma", ylab="Count", xlab="fConc1 non gamma",col="red",
     breaks = 50)
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fAsym, main="fAsym raggi gamma", ylab="Count", xlab="fAsym gamma",col="lightblue",
     breaks = 50)
hist(Raggi.data_temp_h$fAsym, main="fAsym raggi non gamma", ylab="Count", xlab="fAsym non gamma'",col="lightblue",
     breaks = 50)
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fM3Long, main="fM3Long raggi gamma", ylab="Count", xlab="fM3Long gamma",col="orange",
     breaks = 50)
hist(Raggi.data_temp_h$fM3Long, main="fM3Long raggi non gamma", ylab="Count", xlab="fM3Long non gamma",col="orange",
     breaks = 50)
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fM3Trans, main="fM3Trans raggi gamma", ylab="Count", xlab="fM3Trans",col="green",
     breaks = 50)
hist(Raggi.data_temp_h$fM3Trans, main="fM3Trans raggi non gamma", ylab="Count", xlab="fM3Trans",col="green",
     breaks = 50)
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fAlpha, main="fAlpha raggi gamma", ylab="Count", xlab="fAlpha",col="red",
     breaks = 50)
hist(Raggi.data_temp_h$fAlpha, main="fAlpha raggi non gamma", ylab="Count", xlab="fAlpha",col="red",
     breaks = 50)
par(mfrow=c(1,2))
hist(Raggi.data_temp_g$fDist, main="fDist gamma", ylab="Count", xlab="fDist",col="red",
     breaks = 50)
hist(Raggi.data_temp_h$fDist, main="fDist non gamma", ylab="Count", xlab="fDist",col="red",
     breaks = 50)

ggplot(Raggi.data, aes(x=fLength,fill=class))  + 
  geom_density(alpha=.3)

ggplot(Raggi.data, aes(x=fWidth,fill=class))  + 
  geom_density(alpha=.3)

ggplot(Raggi.data, aes(x=fSize,fill=class))  + 
  geom_density(alpha=.3)

ggplot(Raggi.data, aes(x=fConc,fill=class))  + 
  geom_density(alpha=.3)

ggplot(Raggi.data, aes(x=fConc1,fill=class))  + 
  geom_density(alpha=.3)

ggplot(Raggi.data, aes(x=fAsym,fill=class))  + 
  geom_density(alpha=.3)

ggplot(Raggi.data, aes(x=fM3Long,fill=class))  + 
  geom_density(alpha=.3)

ggplot(Raggi.data, aes(x=fM3Trans,fill=class))  + 
  geom_density(alpha=.3)

ggplot(Raggi.data, aes(x=fAlpha,fill=class))  + 
  geom_density(alpha=.3)

ggplot(Raggi.data, aes(x=fDist,fill=class))  + 
  geom_density(alpha=.3)
#DIAMO UNO SGUARDO AL CORRELOGRAMMA
par(mfrow=c(1,1))
corrplot(
  cor(Raggi.data[,-11]),
  type = "full",
  method = "circle",
  number.cex = .5,
  order = "original",
  tl.col = "black",
  tl.srt = 25
)
#Fsize, fwidth e fLength sono molto correlate tra di loro, lo stesso per fconc e fconc1.
oneway.test(fLength ~ class, data = Raggi_train, var.equal = FALSE)
oneway.test(fWidth ~ class, data = Raggi_train, var.equal = FALSE)
oneway.test(fSize ~ class, data = Raggi_train, var.equal = FALSE)
oneway.test(fConc ~ class, data = Raggi_train, var.equal = FALSE)
oneway.test(fConc1 ~ class, data = Raggi_train, var.equal = FALSE)
oneway.test(fAsym ~ class, data = Raggi_train, var.equal = FALSE)
oneway.test(fM3Long ~ class, data = Raggi_train, var.equal = FALSE)
oneway.test(fM3Trans ~ class, data = Raggi_train, var.equal = FALSE) 
oneway.test(fAlpha ~ class, data = Raggi_train, var.equal = FALSE)
oneway.test(fDist ~ class, data = Raggi_train, var.equal = FALSE)

#
#CLASSIFICAZIONE

#Data splitting
set.seed(123)
Index <- createDataPartition(Raggi.data$class, p = 0.75, list = FALSE)

Raggi_train <- Raggi.data[Index,]
Raggi_test <- Raggi.data[-Index,]

plot(Raggi_train$class)
plot(Raggi_test$class)
#Mantiene le proporzioni delle variabili, siccome abbiamo stratificato per la variabile risposta class.

#VARIABLE IMPORTANCE
library(randomForest)
set.seed(2222)
rf <- randomForest(class~.,data=Raggi_train,ntree=1000)
head(rf)
print(rf)
attributes(rf)
rf$importance
plot(rf)
#Gia' a 1000 alberi si stabilizza l'OOB error (linea nera), l'OOB e' la stima
#dell'errore di previsione su un campione bootstrap
var_importance <- data_frame(variable=setdiff(colnames(Raggi_train), "class"),
                             importance=as.vector(importance(rf)))
var_importance <- arrange(var_importance, desc(importance))
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
col <- rainbow(11)
p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
p <- p + geom_bar()
p <- p + xlab("Variables") + ylab("Variable Importance (Mean Decrease in Gini Index)") 
plot(p)


#CLASSIFICAZIONE CON MODELLI VISTI A LEZIONE

# KNN #########
set.seed(2222)
fitControl <- trainControl(
  method = "cv",
  number = 10)

knn_fit <- train(class ~., 
                 data = Raggi_train, 
                 method = "knn",
                 trControl=fitControl,
                 preProcess = c("center", "scale"),
                 tuneLength = 20)
test_pred <- predict(knn_fit, newdata = Raggi_test)
test_pred
confusionMatrix(test_pred,Raggi_test$class)

rocobj_knn <- roc(as.numeric(Raggi_test$class), as.numeric(test_pred))
auc_knn <- round(auc(as.numeric(Raggi_test$class), as.numeric(test_pred)),4)
#0.7863

# Random Forest con tuning -----
library(ranger)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)
set.seed(2222)
#ALTRO MODO DI AGIRE (forse migliore, più efficiente)
fitControl <- trainControl(
  method = "cv",
  number = 10)

set.seed(825)
rfFit1 <- train(class ~ ., 
                data = Raggi_train,
                method = "ranger", 
                trControl = fitControl,
                verbose = FALSE,
                num.trees=1000,
                tuneLength=9
)
rfFit1$bestTune
PredictionRf_Model <- predict(rfFit1$finalModel,Raggi_test)
confusionMatrix(PredictionRf_Model$predictions,Raggi_test$class)

#AUC
rocobj_rf <- roc(as.numeric(Raggi_test$class), as.numeric(PredictionRf_Model$predictions))
auc_rf <- round(auc(as.numeric(Raggi_test$class), as.numeric(PredictionRf_Model$predictions)),4)
#0.85 valore di AUC

#RANDOM FOREST CON PCA
Data.pca=prcomp(Raggi.data[,-c(11)],retx=TRUE) #PCA
prop.table(Data.pca$sdev)
plot(Data.pca$sdev,type="b")
Data.rotated=as.data.table(Data.pca$x)[,c(1:7)] #prendi solo le prime 7 componenti
Data.dump=cbind(Data.rotated,subset(Raggi.data,select=c(class))) # PCA dataset più variabile risposta per training
Raggi_train2 <- Data.dump[Index,]
Raggi_test2 <- Data.dump[-Index,]

rfFit2 <- train(class ~., 
                data = Raggi_train2,
                method = "ranger", 
                trControl = fitControl,
                verbose = FALSE,
                num.trees=1000,
                tuneLength=6
)
rfFit2$bestTune
PredictionRf_Model2 <- predict(rfFit2$finalModel,Raggi_test2)
confusionMatrix(PredictionRf_Model2$predictions,Raggi_test$class)
#Non migliora la random forest con tutte le variabili

# Linear SVM   ---------
set.seed(2222)
svm_model <- svm(class~., 
                 data=Raggi_train, 
                 kernel='linear',
                 cost=1)
summary(svm_model)
prediction_non_tune <- predict(svm_model,Raggi_train)
confusionMatrix(prediction_non_tune,Raggi_train$class)
prediction_non_tune2 <- predict(svm_model,Raggi_test)
confusionMatrix(prediction_non_tune2,Raggi_test$class)
#0.7918 accurancy svm con cost fissato di default a 1
tune.out <- tune(svm,
                 class~.,
                 data = Raggi_train,
                 kernel='linear',
                 ranges=list(cost=c(0.01,0.1,1,10)))
print(tune.out)
# Best parameter cost é 1, quindi il miglior modello è quello visto sopra

rocobj_lsvm <- roc(as.numeric(Raggi_test$class),as.numeric(prediction_non_tune))
auc_lsvm <- round(auc(as.numeric(Raggi_test$class),as.numeric(prediction_non_tune)),4)
#0.7468 valore di AUC lsvm

#SVM CON PCA
svm_model2 <- svm(class~., 
                 data=Raggi_train2, 
                 kernel='linear',
                 cost=1)
summary(svm_model2)
prediction_non_tune2 <- predict(svm_model2,Raggi_train2)
confusionMatrix(prediction_non_tune2,Raggi_train2$class)
prediction_non_tune22 <- predict(svm_model2,Raggi_test2)
confusionMatrix(prediction_non_tune22,Raggi_test2$class)
# Radial SVM  -------
tune.out.radial <- tune(svm,
                        class~.,
                        data = Raggi_train,
                        kernel='radial',
                        ranges=list(cost=c(0.01,0.1,1,10),gammas=c(0.1,1,2)))
print(tune.out.radial)

rsvm <- svm(class~., 
            data=Raggi_train, 
            kernel='radial',
            cost=1,
            gammas=0.1)
prediction_non_tune <- predict(rsvm,Raggi_train)
confusionMatrix(prediction_non_tune,Raggi_train$class)
prediction_non_tune <- predict(rsvm,Raggi_test)
confusionMatrix(prediction_non_tune,Raggi_test$class)
prediction_rsvm <- predict(rsvm,Raggi_test)
rocobj_rsvm <- roc(as.numeric(Raggi_test$class),as.numeric(prediction_rsvm))
auc_rsvm <- round(auc(as.numeric(Raggi_test$class),as.numeric(prediction_rsvm)),4)
#0.8313 valore di AUC rsvm


