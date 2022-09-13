rm(list=ls()); graphics.off(); cat("\014")
library(caret)
library(kernlab)
library(corrplot)
library(tidyverse)
Water_quality <- read.csv("C:/Users/lorenzo/OneDrive/Desktop/ML_Progetto/water_potability.csv",sep=",")
View(Water_quality)
str(Water_quality)
#Data Frame :	3276 obs. of  10 variables:
#$ ph             : num  NA 3.72 8.1 8.32 9.09 ...
#$ Hardness       : num  205 129 224 214 181 ...
#$ Solids         : num  20791 18630 19910 22018 17979 ...
#$ Chloramines    : num  7.3 6.64 9.28 8.06 6.55 ...
#$ Sulfate        : num  369 NA NA 357 310 ...
#$ Conductivity   : num  564 593 419 363 398 ...
#$ Organic_carbon : num  10.4 15.2 16.9 18.4 11.6 ...
#$ Trihalomethanes: num  87 56.3 66.4 100.3 32 ...
#$ Turbidity      : num  2.96 4.5 3.06 4.63 4.08 ...
#$ Potability     : int  0 0 0 0 0 0 0 0 0 0 ...

#Convertiamo la variabile risposta in un factor, notiamo infatti dalla strcture
#del dataset come questa sia una variabile integer.
Water_quality <- transform(Water_quality,Potability=as.factor(Potability))
str(Water_quality)
summary(Water_quality)
#Output
#ph            Hardness          Solids         Chloramines    
#Min.   : 0.000   Min.   : 47.43   Min.   :  320.9   Min.   : 0.352  
#1st Qu.: 6.093   1st Qu.:176.85   1st Qu.:15666.7   1st Qu.: 6.127  
#Median : 7.037   Median :196.97   Median :20927.8   Median : 7.130  
#Mean   : 7.081   Mean   :196.37   Mean   :22014.1   Mean   : 7.122  
#3rd Qu.: 8.062   3rd Qu.:216.67   3rd Qu.:27332.8   3rd Qu.: 8.115  
#Max.   :14.000   Max.   :323.12   Max.   :61227.2   Max.   :13.127  
#NA's   :491                                                         
#    Sulfate       Conductivity   Organic_carbon  Trihalomethanes     Turbidity    
# Min.   :129.0   Min.   :181.5   Min.   : 2.20   Min.   :  0.738   Min.   :1.450  
# 1st Qu.:307.7   1st Qu.:365.7   1st Qu.:12.07   1st Qu.: 55.845   1st Qu.:3.440  
# Median :333.1   Median :421.9   Median :14.22   Median : 66.622   Median :3.955  
# Mean   :333.8   Mean   :426.2   Mean   :14.28   Mean   : 66.396   Mean   :3.967  
# 3rd Qu.:360.0   3rd Qu.:481.8   3rd Qu.:16.56   3rd Qu.: 77.337   3rd Qu.:4.500  
# Max.   :481.0   Max.   :753.3   Max.   :28.30   Max.   :124.000   Max.   :6.739  
# NA's   :781                                     NA's   :162                      
# Potability
# 0:1998    
# 1:1278   


#Commento:
#Notiamo immediatamente che 3 variabili presentano valori mancanti, di questi 
#la variabile ph ha 491 Na's, la variabile sulfate ha 781 Na's ed infine 
#Trihalomethanesne ha 162.
#Dai valori max e min notiamo anche la presenza di outliers.

#IMPUTAZIONE DEGLI NA'S

#VARIABILE ph
aggregate(ph ~ Hardness*Sulfate, Water_quality, FUN=mean)   
fit.ph <- lm(ph ~ Hardness*Sulfate, data = Water_quality[!is.na(Water_quality$ph),])
#Water_quality$ph[is.na(Water_quality$ph)] <- predict(fit.ph, newdata=Water_quality[is.na(Water_quality$ph),])
#Ho provato a creare un modello lineare che preveda il ph per riempire gli NA's tuttavia l'assenza di dati
#anche nella varibile Sulfate fa in modo per cui il modello non funzioni, passo a imputare la media.
#Il modello lineare si basava sull'assunzione fisica per cui i valori di ph dell'acqua sono dovuti alla
#presenza di sali.

Water_quality_temp <- Water_quality[!is.na(Water_quality$ph),]
Water_quality_temp_1 <- Water_quality_temp[which(Water_quality_temp$Potability==1),]
mean(Water_quality_temp_1$ph)
Water_quality_temp_0 <- Water_quality_temp[which(Water_quality_temp$Potability==0),]
mean(Water_quality_temp_0$ph)

#VARIABILE sulfate

#Per la variabile sulftate non posso nemmeno provare a creare un modello che provi a prevedere
#i valori di sulfate nell'acqua siccome non ho variabili in grado di dirmi qualcosa in merito
#alla presenza di solfato nell'acqua.

Water_quality_temp <- Water_quality[!is.na(Water_quality$Sulfate),]
Water_quality_temp_1 <- Water_quality_temp[which(Water_quality_temp$Potability==1),]
mean(Water_quality_temp_1$Sulfate)
Water_quality_temp_0 <- Water_quality_temp[which(Water_quality_temp$Potability==0),]
mean(Water_quality_temp_0$Sulfate)

#Varibile Trihalomethanes  

Water_quality_temp <- Water_quality[!is.na(Water_quality$Trihalomethanes),]
Water_quality_temp_1 <- Water_quality_temp[which(Water_quality_temp$Potability==1),]
mean(Water_quality_temp_1$Trihalomethanes)
Water_quality_temp_0 <- Water_quality_temp[which(Water_quality_temp$Potability==0),]
mean(Water_quality_temp_0$Trihalomethanes)

#IMPUTIAMO 
# MI packager to fill NA
#miceresult <- mice(Water_quality,seed=1234,m=10,meth ="rf")
#Water_quality <- complete(miceresult,1)

Water_quality <- Water_quality %>% 
        group_by(Potability) %>%
        mutate(across(where(is.numeric), ~if_else(is.na(.), mean(., na.rm = T), as.numeric(.)))) %>% ungroup()


#VARIABILE DI INTERESSE OVVERO Potability

plot(Water_quality$Potability, main="Distribuzione variabile risposta Potability", ylab="Count", xlab="Potabilita'",
     ylim=c(0,2500),col=c(2,3))
#Le due classi sono sbilanciate con una predominanza della classe appartenente
#al non potabile, cio' tuttavia non risulta un problema.

#OUTLIER ANALISI
#Analizziamo la presenza di possibili outlier outlier

boxplot(Water_quality$ph,main="Ph",col="orange")
boxplot(Water_quality$Hardness,main="Hardness",col="orange")
boxplot(Water_quality$Solids,main="Solids",col="orange")
boxplot(Water_quality$Chloramines,main="Chloramines",col="orange")
boxplot(Water_quality$Sulfate,main="Sulfate",col="orange")
boxplot(Water_quality$Conductivity,main="Conductivity",col="orange")
boxplot(Water_quality$Organic_carbon,main="Organic_carbon",col="orange")
boxplot(Water_quality$Trihalomethanes,main="Trihalomethanes",col="orange")
boxplot(Water_quality$Turbidity,main="Turbidity",col="orange")


#Notiamo la presenza di outlier in quasi tutte le variabili, ci troviamo di fronte 
#ad un trade off ovvero eliminare questi outliers e proseguire l'analisi senza 
#oppoure tenerli. Siccome le osservazioni sono 3276 l'impatto che possono avere 
#sara' contenuto pertanto decido di continuare mantenendoli.

#DISTRIBUZIONI DELLE VARIABILI ESPLICATIVE
Water_quality_temp_0 <- Water_quality[which(Water_quality$Potability==0),]
Water_quality_temp_1 <- Water_quality[which(Water_quality$Potability==1),]
par(mfrow=c(1,2))
hist(Water_quality_temp_0$ph, main="Ph non potabile", ylab="Count", xlab="ph'",col="red",
     breaks = 50)
hist(Water_quality_temp_1$ph, main="Ph potabile", ylab="Count", xlab="ph'",col="red",
     breaks = 50)
hist(Water_quality_temp_0$Hardness, main="Hardness non potabile", ylab="Count", xlab="Hardness'",col="lightblue",
     breaks = 50)
hist(Water_quality_temp_1$Hardness, main="Hardness potabile", ylab="Count", xlab="Hardness'",col="lightblue",
     breaks = 50)
hist(Water_quality_temp_0$Solids, main="Solids non potabile", ylab="Count", xlab="Solids",col="orange",
     breaks = 50)
hist(Water_quality_temp_1$Solids, main="Solids potabile", ylab="Count", xlab="Solids",col="orange",
     breaks = 50)
hist(Water_quality_temp_0$Chloramines, main="Chloramines non potabile", ylab="Count", xlab="Chloramines",col="green",
     breaks = 50)
hist(Water_quality_temp_1$Chloramines, main="Chloramines potabile", ylab="Count", xlab="Chloramines",col="green",
     breaks = 50)
hist(Water_quality_temp_0$Sulfate, main="Sulfate non potabile", ylab="Count", xlab="Sulfate",col="red",
     breaks = 50)
hist(Water_quality_temp_1$Sulfate, main="Sulfate potabile", ylab="Count", xlab="Sulfate",col="red",
     breaks = 50)
hist(Water_quality_temp_0$Conductivity, main="Conductivity non potabile", ylab="Count", xlab="Conductivity'",col="lightblue",
     breaks = 50)
hist(Water_quality_temp_1$Conductivity, main="Conductivity potabile", ylab="Count", xlab="Conductivity'",col="lightblue",
     breaks = 50)
hist(Water_quality_temp_0$Organic_carbon, main="Organic_carbon non potabile", ylab="Count", xlab="Organic_carbon",col="orange",
     breaks = 50)
hist(Water_quality_temp_1$Organic_carbon, main="Organic_carbon potabile", ylab="Count", xlab="Organic_carbon",col="orange",
     breaks = 50)
hist(Water_quality_temp_0$Trihalomethanes, main="Trihalomethanes non potabile", ylab="Count", xlab="Trihalomethanes",col="green",
     breaks = 50)
hist(Water_quality_temp_1$Trihalomethanes, main="Trihalomethanes  potabile", ylab="Count", xlab="Trihalomethanes",col="green",
     breaks = 50)
hist(Water_quality_temp_0$Turbidity, main="Turbidity non potabile", ylab="Count", xlab="Turbidity",col="red",
     breaks = 50)
hist(Water_quality_temp_1$Turbidity, main="Turbidity potabile", ylab="Count", xlab="Turbidity",col="red",
     breaks = 50)

#Da un'analisi delle distribuzioni delle variabili esplicative emerge non ci siano particolari asimmetrie e 
#particolari criticita' nei dati.
out <- which((Water_train$Potability==1) & (Water_train$ph > 10))
Water_train <- Water_train[-c(out),]
out <- which((Water_train$Potability==1) & (Water_train$Sulfate > 400))
Water_train <- Water_train[-c(out),]
#DIAMO UNO SGUARDO AL CORRELOGRAMMA
par(mfrow=c(1,1))
corrplot(
        cor(Water_quality[,-10]),
        type = "full",
        method = "circle",
        number.cex = .5,
        order = "original",
        tl.col = "blue",
        tl.srt = 25,
        title  = "Correlation Plot of Water Quality Data "
)
#Non sono presenti correlazioni alte sintomo di non presenza di multicollinearita'.

#CLASSIFICAZIONE

#Data splitting
set.seed(123)
Index <- createDataPartition(Water_quality$Potability, p = 0.75, list = FALSE)

Water_train <- Water_quality[Index, ]
Water_test <- Water_quality[-Index, ]

plot(Water_train$Potability)
plot(Water_test$Potability)
#Mantiene le proporzioni delle variabili

#Random Forest senza tuning
library(randomForest)
library(ranger)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)
set.seed(2222)
#ALTRO MODO DI AGIRE (forse migliore, più efficiente)
fitControl <- trainControl(
        method = "cv",
        number = 10)
rfGrid <-  expand.grid(mtry = c(1:9),
                       min.node.size=c(1,2,3,4,5),
                       splitrule=c("gini"))

set.seed(825)
rfFit1 <- train(Potability ~ ., 
                data = Water_train, 
                method = "ranger", 
                trControl = fitControl,
                verbose = FALSE,
                tuneGrid = rfGrid,
                num.trees=800,
                tuneLength=10
                )
rfFit1$bestTune
PredictionRf_Model <- predict(rfFit1$finalModel,Water_test)
confusionMatrix(PredictionRf_Model$predictions,Water_test$Potability)
#mtry splitrule min.node.size
#67    5      gini             3

#KNN
knn_fit <- train(Potability ~., data = Water_train, method = "knn",
                 trControl=fitControl,
                 preProcess = c("center", "scale"),
                 tuneLength = 250)
test_pred <- predict(knn_fit, newdata = Water_test)
test_pred
confusionMatrix(test_pred,Water_test$Potability)

#SVM (support vector machine)
library(e1071)
# Linear SVM con err gen stimato cv  ---------
# Feature Scaling
Water_train[-10] = Water_train[-10]
Water_test[-10] = Water_test[-10]
svmGrid1 <-  expand.grid(cost = seq(from=0.1,to=1,by=0.1),weight=c(1,2,3,4,5))
lsvm=train(Potability~., data=Water_train, method='svmLinearWeights', trControl=fitControl,tuneGrid=svmGrid1,preProcess = c("center", "scale"))
# Errore empirico                            
1-sum(predict(lsvm, Water_test)==Water_test$Potability)/nrow(Water_test)
# Errore generalizzazione                 
1-lsvm$results[2]
# Misclassified instances and confusion matrix
table(predict(lsvm, Water_test),Water_test$Potability)
Water_test[which(predict(lsvm, Water_test)!=Water_test$Potability),]

# Radial SVM con err gen stimato cv
svmGrid <- expand.grid(C = c(1:50),sigma=c(1:10))
rsvm=train(Potability~., data=Water_train, method='svmRadial', trControl=fitControl, tuneGrid=svmGrid,preProcess = c("center", "scale"))
# Errore empirico                            
1-sum(predict(rsvm, Water_test)==Water_test$Potability)/nrow(Water_test)
# Errore generalizzazione                 
1-rsvm$results[2]
# Misclassified instances and confusion matrix
table(predict(rsvm, Water_test),Water_test$Potability)
confusionMatrix(predict(rsvm,Water_test),Water_test$Potability)
Water_test[which(predict(rsvm, Water_test)!=Water_test$Potability),]

# Radial SVM con err gen stimato cv
svmGrid <- expand.grid(C=seq(from=0.1,to=1,by=0.1),scale=c(1:5),degree=c(1:5))
psvm=train(Potability~., data=Water_train, method='svmPoly', trControl=fitControl, tuneGrid=svmGrid,preProcess = c("center", "scale"))
# Errore empirico                            
1-sum(predict(psvm, Water_test)==Water_test$Potability)/nrow(Water_test)
# Errore generalizzazione                 
1-psvm$results[2]
# Misclassified instances and confusion matrix
table(predict(rsvm, Water_test),Water_test$Potability)
confusionMatrix(predict(rsvm,Water_test),Water_test$Potability)
Water_test[which(predict(rsvm, Water_test)!=Water_test$Potability),]






#BAGGING
library(adabag)
fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)
BagGrid <-  expand.grid(mfinal=c(1:20)*100,
                        maxdepth=c(1:50))

set.seed(825)
BagFit1 <- train(Potability ~ ., 
                data = Water_train, 
                method = "AdaBag", 
                trControl = fitControl,
                verbose = FALSE,
                tuneGrid = BagGrid)
BagFit1$bestTune
PredictionBagModel <- predict(BagFit1$finalModel,Water_test)
confusionMatrix(PredictionBagModel$predictions,Water_test$Potability)




















