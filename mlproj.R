# TODO: aggiungere install.package per tutto... 
# codice deve essere pronto all'uso su qualsiasi PC durante l'a presentazione l'orale
library("ggplot2")
library("corrplot")
library("e1071")
library("scales")
library("stringr")
library("caret")
library("multiROC")
library("C50") 


#https://www.kaggle.com/deepu1109/star-dataset
dataset = read.csv("stars.csv")
colnames(dataset) = c("Temp", "Lum", "Rad", "AbsMagn", "Type", "Color", "SpectrClass")
dataset = dataset[, c(1,2,3,4,6,7,5)]
dataset$Type = factor(dataset$Type)
types = c("BrownDwarf", "RedDwarf", "WhiteDwarf","MainSequence", "Supergiant", "Hypergiant")
dataset$Type = factor(sapply(dataset$Type, function (x) { types[x]}))
dataset$SpectrClass = factor(dataset$SpectrClass, levels=c("O", "B", "A", "F", "G", "K", "M"))
dataset$Color = tolower(dataset$Color)
dataset$Color = gsub("-", " ", dataset$Color)
dataset$Color = trimws(dataset$Color)

# sort words in a string
sortWordStr = function(str){
  ret = str_split(str, " ")
  ret = unlist(ret)
  ret = str_sort(ret)
  ret =  paste(ret, collapse = " ")
  return(ret)
}

dataset$Color = sapply(dataset$Color, sortWordStr)
dataset$Color = factor(dataset$Color)

dataset.notarget = function(data) {
  return(data[1:length(data)-1])
}

#check distribution of target
targetTable = data.frame(table(dataset$Type))
colnames(targetTable) = c("Type", "Value")

targetPie = ggplot(targetTable, aes(x="", y=Value, fill=Type))+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  geom_text(aes(y = Value/3 + c(0, cumsum(Value)[-length(Value)]), 
                label = percent(Value/100)), size=5)

# meglio se riusciamo a farlo con ggplot
plot(dataset, col = dataset$Type)

# cor
dataset.cor = cor(dataset[, 1:4])
corrplot(dataset.cor, addCoef.col = T)

ggplot(dataset, aes(x = Temp, y = AbsMagn, color = Type)) + geom_point()
ggplot(dataset, aes(x = Lum, y = AbsMagn, color = Type)) + geom_point()
ggplot(dataset, aes(x = Rad, y = AbsMagn, color = Type)) + geom_point()

# scale numeric attributes
dataset.scaled = data.frame(cbind(scale(dataset[,1:4]), dataset[,5:7]))


# Ho provato a vedere se eraa vera la cosa di wikipedia che ho trovato. Sembra di sì. 
ggplot(dataset, aes(x = SpectrClass, y = AbsMagn, color = Type)) + geom_point() + scale_y_reverse()

# test pca: non dovrebbe servireeee
# dataset.pca = PCA(dataset[,1:4])
# fviz_eig(dataset.pca, addlabels = TRUE, ylim = c(0, 50)) 
# fviz_pca_ind(dataset.pca,
#              geom.ind = "point",
#              col.ind = dataset$Type,
#              addEllipses = TRUE,
#              legend.title = "Groups"           ) 
# 

# distribution of feature per type
ggplot(dataset, aes(x = AbsMagn, color = Type, fill = Type)) + geom_density(alpha = 0.2) + theme_minimal()
# -> AbsMagn divide il target abbastanza bene, tranne per l'overlapping che c'è tra RedDwarf e WhiteDwarf
ggplot(dataset, aes(x = Temp, color = Type, fill = Type)) + geom_density(alpha = 0.2) + theme_minimal()
# -> l'overlapping precedente potrebbe essere risolto da Temp
# --> teniamo questo --> SBAM, sulle Y distinguo 4 classi, sulle X 2
ggplot(dataset, aes(x = Temp, y = AbsMagn, color = Type)) + geom_point()

# split into trainingset and testset
ind = sample(2, nrow(dataset.scaled), replace = TRUE, prob=c(0.7, 0.3))
trainset = dataset.scaled[ind == 1,]
testset = dataset.scaled[ind == 2,] 


# SVM test
# tuning
tuned = tune.svm(Type ~ Temp + AbsMagn, data = trainset, kernel='linear',
                 cost=c(0.001, 0.01, 0.1, 1,5,10,100, 200, 300), gamma = c(0.001, 0.01, 0.1, 1), probability = T) 
tuned
#svm.model = svm(data = trainset, Type ~ Temp + AbsMagn, kernel = "linear", cost = tuned$best.parameters$cost, gamma = tuned$best.parameters$gamma)
svm.model = tuned$best.model
summary(svm.model)
plot(svm.model, trainset, Temp ~ AbsMagn)
svm.pred = predict(svm.model, testset, probability = T ) 
svm.table=table(svm.pred, testset$Type)
svm.table
# confusion matrix
svm.result1 = confusionMatrix(svm.pred, testset$Type)
svm.result1
# confusion matrix with precision and recall
svm.result2 = confusionMatrix(svm.pred, testset$Type, mode = "prec_recall")
svm.result2
# ROC
# probabilities of instances target
pred.prob = attr(svm.pred, "probabilities")
# preparing dataframe for multiclass ROC and Precision
predictive_scores = pred.prob
colnames(predictive_scores) = paste(colnames(predictive_scores), "_pred_SVM", sep = "")
true_labels = data.frame(pred.prob)
colnames(true_labels) = paste(colnames(true_labels), "_true", sep = "")
# NEED HELP: ogni riga deve avere 1 solo nella colonna della classe di appartenenza, 0 nel resto

data.roc = data.frame(cbind(truelabels, pred.prob))



