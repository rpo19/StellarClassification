install_packs = F

if(install_packs){
  # Install and load necessary packages
  install.packages("ggplot2")
  install.packages("corrplot")
  install.packages("e1071")
  install.packages("scales")
  install.packages("stringr")
  install.packages("caret")
  install.packages("multiROC")
  install.packages("dummies")
  install.packages("randomForest")
  install.packages("rpart")
  install.packages("rattle")
  install.packages("rpart.plot")
  install.packages("RColorBrewer")
  install.packages("MLmetrics")
}


library("ggplot2")
library("corrplot")
library("e1071")
library("scales")
library("stringr")
library("caret")
library("multiROC")
library("dummies")
library("randomForest")
library("rpart") 
library("rattle")
library("rpart.plot")
library("RColorBrewer")
library("MLmetrics")

#### https://www.kaggle.com/deepu1109/star-dataset

#### PREPARAZIONE DATASET

# TODO: valutare se togliere dal dataset la colonna colore
dataset = read.csv("stars.csv")
colnames(dataset) = c("Temp", "Lum", "Rad", "AbsMagn", "Type", "Color", "SpectrClass")
dataset = dataset[, c(1,2,3,4,6,7,5)]
dataset$Type = factor(dataset$Type)
types = c("BrownDwarf", "RedDwarf", "WhiteDwarf","MainSequence", "Supergiant", "Hypergiant")
dataset$Type = factor(sapply(dataset$Type, function (x) { types[x]}))

# cambio livelli SpectrClass secondo la giusta classificazione
dataset$SpectrClass = factor(dataset$SpectrClass, levels=c("O", "B", "A", "F", "G", "K", "M"))

# pulizia valori colori
dataset$Color = tolower(dataset$Color)
dataset$Color = gsub("-", " ", dataset$Color)
dataset$Color = trimws(dataset$Color)
sortWordStr = function(str){
  ret = str_split(str, " ")
  ret = unlist(ret)
  ret = str_sort(ret)
  ret =  paste(ret, collapse = " ")
  return(ret)
}
dataset$Color = sapply(dataset$Color, sortWordStr)
dataset$Color = factor(dataset$Color)

# scaling dataset
dataset.scaled = data.frame(cbind(scale(dataset[,1:4]), dataset[,5:7]))

#### ANALISI ESPLORATIVA

# verifica distribuzione della classe target
type.distributionTable = data.frame(table(dataset$Type))
colnames(type.distributionTable) = c("Type", "Value")
type.distributionTable
type.distributionBarPlot = ggplot(dataset, aes(Type))+ geom_bar(aes(fill = Type))
type.distributionBarPlot # La distribuzione delle classi risulta bilanciata

# verifica correlazione tra le covariate numeriche
dataset.cor = cor(dataset[, 1:4])
correlationMatrix = corrplot(dataset.cor, addCoef.col = T)
# non ci sono covariate fortemente correlate fuorche' AbsMagn e Lum
correlationMatrix

# verifico la separazione delle classi in base a coppie di covariate
type.plotPairs = featurePlot(x = dataset.scaled[,1:4], y = dataset.scaled[,7] ,
                             plot = "pairs",
                             auto.key=list(columns=3))
type.plotPairs
type.plotByAbsMagnLum = ggplot(dataset.scaled, aes(x = Lum, y = AbsMagn, color = Type)) + geom_point()
type.plotByAbsMagnRad = ggplot(dataset.scaled, aes(x = Rad, y = AbsMagn, color = Type)) + geom_point()
type.plotByAbsMagnTemp = ggplot(dataset.scaled, aes(x = Temp, y = AbsMagn, color = Type)) + geom_point()
type.plotByAbsMagnLum
type.plotByAbsMagnRad
type.plotByAbsMagnTemp # AbsMagn e Temp sono le features che separano linearmente i punti
# Approfondendo le distribuzioni si puo' verificare AbsMagn divide il target abbastanza bene,
# tranne per l'overlapping che c'e' tra RedDwarf e WhiteDwarf
type.distributionAbsMagn = ggplot(dataset.scaled, aes(x = AbsMagn, color = Type, fill = Type)) + geom_density(alpha = 0.2) + theme_minimal()
type.distributionTemp = ggplot(dataset.scaled[dataset.scaled$Type %in% c("RedDwarf", "WhiteDwarf"),], aes(x = Temp, color = Type, fill = Type)) + geom_density(alpha = 0.2) + theme_minimal()
type.distributionAbsMagn
type.distributionTemp
# dopo le seguenti osservazioni, notiamo che potremmo usare un modello svm per classificare con queste due features

# considerazioni su covariate categoriche
type.barplotSpectrClass = barplot(table(dataset$SpectrClass, dataset$Type),
                                  legend = levels(dataset$SpectrClass), main = "SpectrClass by Type") # TODO: mettere palette
# notiamo che anche la covariata SpectrClass distingue bene Red Dwarf e White Dwarf.
# SpectrClass potrebbe quindi essere usata insieme ad altre covariate numeriche per modelli che gestiscono features miste                                                                                                        

# altro punto di vista per confermare quanto appena detto
# https://en.wikipedia.org/wiki/Stellar_classification
type.plotByAbsMagnSpectrClass = ggplot(dataset, aes(x = SpectrClass, y = AbsMagn, color = Type)) + geom_point() + scale_y_reverse()
type.plotByAbsMagnSpectrClass

# TODO: spiegare scarto colore
# mi aspetto che colore sia coerente con lo spettro ed e' quindi sufficiente utilizzare quest'ultimo; da dimostrare
##### PROVA CORRELAZIONE Color e SpectrClass ###########
dmy = dummyVars(" ~ Temp + Lum + Rad + AbsMagn + Color + SpectrClass", data = dataset.scaled)
dataset.scaled.dummy = cbind(data.frame(predict(dmy, newdata = dataset.scaled)), dataset.scaled$Type)
colnames(dataset.scaled.dummy)[ncol(dataset.scaled.dummy)] = "Type"
# matriciona bestia della madonna
correlationMatrixDummy = corrplot(cor(dataset.scaled.dummy[,5:(ncol(dataset.scaled.dummy)-1)]))
correlationMatrixDummy # ridurre al minimo indispensabili

# sono quasi tutte correlate quindi usiamo la SpectrClass 


#### SPLIT DATASET

set.seed(2)
ind = sample(2, nrow(dataset.scaled), replace = TRUE, prob=c(0.7, 0.3)) # 70% trainset, 30% testset
trainset = dataset.scaled[ind == 1,]
testset = dataset.scaled[ind == 2,] 

#### COMPARING MODELS

# setup 10-fold cross validation
trainControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = T, classProbs = T, summaryFunction=multiClassSummary)

# training di un modello con svm
svmfold.model = train(Type ~ Temp + AbsMagn, data = trainset, method = "svmLinear", trControl = trainControl, metric = "Accuracy")
svmfold.pred = predict(svmfold.model, testset, type = "raw")
svmfold.confusion.matrix = confusionMatrix(svmfold.pred, testset$Type, mode = "prec_recall")
svmfold.confusion.matrix

# training di un modello con decistion tree
# TODO: vedere perche' usiamo le due covariate
dtfold.model = train(Type ~ AbsMagn + SpectrClass, data = trainset, method = "rpart",trControl = trainControl, metric = "Accuracy")
dtfold.pred = predict(dtfold.model, testset, type = "raw")
dtfold.confusion.matrix = confusionMatrix(dtfold.pred, testset$Type, mode = "prec_recall")
dtfold.confusion.matrix

# training di un modello con random forest
# TODO: vedere che covariate e il perche'
rffold.model = train(Type ~ ., data = trainset, method = "rf",trControl = trainControl, metric = "Accuracy")
rffold.pred = predict(rffold.model, testset, type = "raw")
rffold.confusion.matrix = confusionMatrix(rffold.pred, testset$Type, mode = "prec_recall")
rffold.confusion.matrix

# preparazione dati per multiROC
svmfold.prob = predict(svmfold.model, testset, type = "prob")
dtfold.prob = predict(dtfold.model, testset, type = "prob")
rffold.prob = predict(rffold.model, testset, type = "prob")
colnames(svmfold.prob) = paste(colnames(svmfold.prob), "_pred_SVM", sep = "")
colnames(dtfold.prob) = paste(colnames(dtfold.prob), "_pred_DT", sep = "")
colnames(rffold.prob) = paste(colnames(rffold.prob), "_pred_RF", sep = "")
trueLabels = dummies::dummy(testset$Type)
colnames(trueLabels) = paste(str_remove(colnames(trueLabels), "Type"), "_true", sep = "")
ROC.data = data.frame(cbind(trueLabels, svmfold.prob, dtfold.prob, rffold.prob))

# ROC/PR
ROC.results = multi_roc(ROC.data, force_diag=T)
ROC.results.plot <- plot_roc_data(ROC.results)
PR.results = multi_pr(ROC.data, force_diag=T)
PR.results.plot <- plot_pr_data(PR.results)

# plot ROC
ggplot(ROC.results.plot[ROC.results.plot$Method=="SVM",], aes(x = 1-Specificity, y=Sensitivity)) +
  xlab("FPR") +
  ylab("TPR") +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
ggplot(ROC.results.plot[ROC.results.plot$Method=="DT",], aes(x = 1-Specificity, y=Sensitivity)) +
  xlab("FPR") +
  ylab("TPR") +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
ggplot(ROC.results.plot[ROC.results.plot$Method=="RF",], aes(x = 1-Specificity, y=Sensitivity)) +
  xlab("FPR") +
  ylab("TPR") +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

# plot PR
ggplot(PR.results.plot[PR.results.plot$Method =="SVM",], aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group, linetype=Method), size=1.5) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
ggplot(PR.results.plot[PR.results.plot$Method =="DT",], aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group, linetype=Method), size=1.5) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
ggplot(PR.results.plot[PR.results.plot$Method =="RF",], aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group, linetype=Method), size=1.5) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
# print AUC macro
ROC.results$AUC$SVM$macro
ROC.results$AUC$DT$macro
ROC.results$AUC$RF$macro

macro.plot = cbind(ROC.results$AUC$SVM$macro, ROC.results$AUC$DT$macro, ROC.results$AUC$RF$macro)
colnames(macro.plot) = c("SVM", "DT", "RF")
barplot(macro.plot)

# plot riassuntivo delle macro per tutti e tre i modelli
ROC.results.merge = cbind(1-ROC.results$Specificity$SVM$macro, ROC.results$Sensitivity$SVM$macro, "SVM")
ROC.results.merge = rbind(ROC.results.merge, cbind(1-ROC.results$Specificity$DT$macro, ROC.results$Sensitivity$DT$macro, "DT"))
ROC.results.merge = rbind(ROC.results.merge, cbind(1-ROC.results$Specificity$RF$macro, ROC.results$Sensitivity$RF$macro, "RF"))
colnames(ROC.results.merge) = c("FPR", "TPR", "Method")
ROC.results.merge = data.frame(ROC.results.merge)
ROC.results.merge$Method = factor(ROC.results.merge$Method)
ROC.results.merge$FPR = as.numeric(as.character(ROC.results.merge$FPR))
ROC.results.merge$TPR = as.numeric(as.character(ROC.results.merge$TPR))

# TODO: vedere come fare le linee a partire da (0,0)
ggplot(data=ROC.results.merge, aes(x=FPR, y=TPR, group = Method, colour = Method)) +
  geom_line(size = 1.5) + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),colour='grey', linetype = 'dotdash')

# differenze tra i modelli prodotti dalla 10-fold cv ... potrebbe avere senso il confronto sui fold
cv.values = resamples(list(svm=svmfold.model, dt = dtfold.model, rf = rffold.model)) 
dotplot(cv.values, metric = "AUC") 
# TODO: 1 - dobbiamo farlo sui fold?
# TODO: 2 - dobbiamo farlo su quali misure?
# TODO: 3 - ? corretto prendere le Mean_..?
bwplot(cv.values) 
# ho capito come leggerle ma boh
splom(cv.values,metric="AUC") 
splom(cv.values,metric="Accuracy") 

#### Macro measures

# table format per le misure dei modelli
svm.data.conf.matrix = data.frame(svmfold.confusion.matrix$byClass)
svm.data.conf.matrix[is.na(svm.data.conf.matrix)] = 0
dt.data.conf.matrix = data.frame(dtfold.confusion.matrix$byClass)
dt.data.conf.matrix[is.na(dt.data.conf.matrix)] = 0
rf.data.conf.matrix = data.frame(rffold.confusion.matrix$byClass)
rf.data.conf.matrix[is.na(rf.data.conf.matrix)] = 0
precision.measures = c(mean(svm.data.conf.matrix$Precision), mean(dt.data.conf.matrix$Precision), mean(rf.data.conf.matrix$Precision))
recall.measures = c(mean(svm.data.conf.matrix$Recall), mean(dt.data.conf.matrix$Recall), mean(rf.data.conf.matrix$Recall))
f1.measure = c(mean(svm.data.conf.matrix$F1), mean(dt.data.conf.matrix$F1), mean(rf.data.conf.matrix$F1))
model.labels = c("SVM", "DT", "RF")
performance.measures = data.frame(model.labels, precision.measures, recall.measures, f1.measure)
colnames(performance.measures) = c("Model", "Precision", "Recall", "F1")

performance.measures

# plot format per le misure dei modelli
model.labels = c("SVM", "SVM", "SVM", "DT", "DT", "DT", "RF", "RF", "RF")
perf.measure.labels = c("Prec", "Rec", "F1", "Prec", "Rec", "F1", "Prec", "Rec", "F1")
perf.measure.values = c(mean(svm.data.conf.matrix$Precision), mean(svm.data.conf.matrix$Recall), mean(svm.data.conf.matrix$F1),
                        mean(dt.data.conf.matrix$Precision), mean(dt.data.conf.matrix$Recall), mean(dt.data.conf.matrix$F1),
                        mean(rf.data.conf.matrix$Precision), mean(rf.data.conf.matrix$Recall), mean(rf.data.conf.matrix$F1))

performance.measures.plot = data.frame(model.labels, perf.measure.labels, perf.measure.values)
colnames(performance.measures.plot) = c("Model", "Measure", "Value")
performance.measures.plot$Model = factor(performance.measures.plot$Model, levels=c("SVM", "DT", "RF"))
performance.measures.plot$Measure = factor(performance.measures.plot$Measure, levels=c("Prec", "Rec", "F1"))

ggplot(performance.measures.plot, aes(fill=Measure, y=Value, x=Model)) + 
  geom_bar(position="dodge", stat="identity", width=0.5)


#### Accuracy

svmfold.confusion.matrix$overall[1]
dtfold.confusion.matrix$overall[1]
rffold.confusion.matrix$overall[1]

