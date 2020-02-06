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
library("dummies")
library("randomForest")

#https://www.kaggle.com/deepu1109/star-dataset
dataset = read.csv("stars.csv")
colnames(dataset) = c("Temp", "Lum", "Rad", "AbsMagn", "Type", "Color", "SpectrClass")
dataset = dataset[, c(1,2,3,4,6,7,5)]
dataset$Type = factor(dataset$Type)
types = c("BrownDwarf", "RedDwarf", "WhiteDwarf","MainSequence", "Supergiant", "Hypergiant")
dataset$Type = factor(sapply(dataset$Type, function (x) { types[x]}))

# changed levels order to match the right classification
dataset$SpectrClass = factor(dataset$SpectrClass, levels=c("O", "B", "A", "F", "G", "K", "M"))

# cleaned colors
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

# check distribution of target
targetTable = data.frame(table(dataset$Type))
colnames(targetTable) = c("Type", "Value")
targetTable$Value = (targetTable$Value/sum(targetTable$Value))*100

# Pie
targetPie = ggplot(targetTable, aes(x="", y=Value, fill=Type))+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  geom_text(aes(y = Value/6 + c(0, cumsum(Value)[-length(Value)]),
                label = percent(Value/100)), size=5)
# BarPlot
targetBarPlot = ggplot(dataset, aes(Type))+ geom_bar(aes(fill = Type))


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


# test pca: non dovrebbe servireeee
# dataset.pca = PCA(dataset[,1:4])
# fviz_eig(dataset.pca, addlabels = TRUE, ylim = c(0, 50)) 
# fviz_pca_ind(dataset.pca,
#              geom.ind = "point",
#              col.ind = dataset$Type,
#              addEllipses = TRUE,
#              legend.title = "Groups"           ) 
# 

#### considerazioni su attributi numerici
# distribution of feature per type
ggplot(dataset, aes(x = AbsMagn, color = Type, fill = Type)) + geom_density(alpha = 0.2) + theme_minimal()
# -> AbsMagn divide il target abbastanza bene, tranne per l'overlapping che c'è tra RedDwarf e WhiteDwarf
ggplot(dataset, aes(x = Temp, color = Type, fill = Type)) + geom_density(alpha = 0.2) + theme_minimal()
# -> l'overlapping precedente potrebbe essere risolto da Temp
# --> teniamo questo --> SBAM, sulle Y distinguo 4 classi, sulle X 2
ggplot(dataset, aes(x = Temp, y = AbsMagn, color = Type)) + geom_point()


###### vedere se sono piu carini questi ################
featurePlot(x = dataset.scaled[,1:4], y = dataset.scaled[,7] ,
            plot = "box",
            scales=list(x=list(relation="free"), y=list(relation="free")),
            auto.key=list(columns=3))

featurePlot(x = dataset.scaled[,1:4], y = dataset.scaled[,7] ,
            plot = "density",
            scales=list(x=list(relation="free"), y=list(relation="free")),
            auto.key=list(columns=3))

featurePlot(x = dataset.scaled[,1:4], y = dataset.scaled[,7] ,
            plot = "pairs",
            auto.key=list(columns=3))


#### considerazioni su attributi categorici
ggplot(dataset, aes(x = AbsMagn, color = Type, fill = Type)) + geom_density(alpha = 0.2) + theme_minimal()
barplot(table(dataset$SpectrClass, dataset$Type), legend = levels(dataset$SpectrClass), main = "Title")
# -> potrbbero bastare AbsMagn e SpectrClass per fare albero... 
#   "Dove AbsMagn non riesce a distingure, se non è M allora è WhiteDwarf"


# Ho provato a vedere se eraa vera la cosa di wikipedia che ho trovato. Sembra di sì. 
ggplot(dataset, aes(x = SpectrClass, y = AbsMagn, color = Type)) + geom_point() + scale_y_reverse()

# Sembra ci sia correlazione tra SpectrClass e Colore
counts = table( dataset$SpectrClass, dataset$Color)
barplot(counts, legend = levels(dataset$SpectrClass), main = "Title")
# Dato che colore era un attributo sporco (mischiava white-blue blue-white ecc., abbiamo provato a pulirlo)
#   lo scartiamo e teniamo solo SpectrClass


#################################
# split into trainingset and testset
set.seed(2)
ind = sample(2, nrow(dataset.scaled), replace = TRUE, prob=c(0.7, 0.3))
trainset = dataset.scaled[ind == 1,]
testset = dataset.scaled[ind == 2,] 

######################################################
# SVM test
# tuning
tuned = tune.svm(Type ~ Temp + AbsMagn, data = trainset, kernel='linear',
                 cost=c(0.001, 0.01, 0.1, 1,5,10,100, 200, 300), gamma = c(0.001, 0.01, 0.1, 1), probability = T) 

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
true_labels = dummies::dummy(testset$Type)
colnames(true_labels) = paste(str_remove(colnames(true_labels), "Type"), "_true", sep = "")


data.roc = data.frame(cbind(true_labels, predictive_scores))

roc_res <- multi_roc(data.roc, force_diag=T)
pr_res <- multi_pr(data.roc, force_diag=T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)


ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) + 
  xlab("FPR") +
  ylab("TPR") +
  geom_path(aes(color = Group, linetype=Method), size=1.3) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

ggplot(plot_pr_df, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group, linetype=Method), size=1.3) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
##########################################################################
# decision tree prova
library(rpart) 
library(rattle)
library(rpart.plot)
library(RColorBrewer)
decisionTree = rpart(Type ~ AbsMagn + SpectrClass, data=trainset, method="class")
dt.pred <- predict(decisionTree, testset, type = "class") 
dt.confusion.matrix = confusionMatrix(dt.pred, testset$Type)
dt.confusion.matrix
fancyRpartPlot(decisionTree)

# pruning
cp= decisionTree$cptable[which.min(decisionTree$cptable[,"xerror"]),"CP"]
prunedDecisionTree = prune(decisionTree, cp= cp) 
fancyRpartPlot(prunedDecisionTree)
dt.pred <- predict(prunedDecisionTree, testset, type = "class") 
dt.prob <- predict(prunedDecisionTree, testset, type = "prob") 
dt.confusion.matrix = confusionMatrix(dt.pred, testset$Type, mode = "prec_recall")
dt.confusion.matrix

# ROC
# probabilities of instances target
# preparing dataframe for multiclass ROC and Precision
predictive_scores = dt.prob
colnames(predictive_scores) = paste(colnames(predictive_scores), "_pred_DT", sep = "")
true_labels = dummies::dummy(testset$Type)
colnames(true_labels) = paste(colnames(dt.prob), "_true", sep = "")


data.roc = data.frame(cbind(true_labels, predictive_scores))

roc_res <- multi_roc(data.roc, force_diag=T)
pr_res <- multi_pr(data.roc, force_diag=T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)


ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

ggplot(plot_pr_df, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group, linetype=Method), size=1.5) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

#########################################################
# Random Forest
rf <- randomForest(Type ~ ., data = trainset, importance = TRUE)
rf.pred <- predict(rf, testset, type = "class") 
rf.prob <- predict(rf, testset, type = "prob") 
rf.confusion.matrix = confusionMatrix(rf.pred, testset$Type, mode = "prec_recall")
rf.confusion.matrix
# ROC
# probabilities of instances target
# preparing dataframe for multiclass ROC and Precision
predictive_scores = rf.prob
colnames(predictive_scores) = paste(colnames(predictive_scores), "_pred_RF", sep = "")
true_labels = dummies::dummy(testset$Type)
colnames(true_labels) = paste(colnames(rf.prob), "_true", sep = "")


data.roc = data.frame(cbind(true_labels, predictive_scores))

roc_res <- multi_roc(data.roc, force_diag=T)
pr_res <- multi_pr(data.roc, force_diag=T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)


ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

ggplot(plot_pr_df, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group, linetype=Method), size=1.5) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

#########################################################
### RIMUOVERE RIGA SUCCESSIVAAAA ###
#testset = dataset.scaled
# 10-fold per svm con multi ROC

trainControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = T, classProbs = T)
svmfold.model = train(Type ~ Temp + AbsMagn, data = trainset, method = "svmLinear", trControl = trainControl)

svmfold.prob = predict(svmfold.model, testset, type = "prob")
svmfold.pred = predict(svmfold.model, testset, probability = T)
svmfold.confusion.matrix = confusionMatrix(svmfold.pred, testset$Type, mode = "prec_recall")
svmfold.confusion.matrix

# preprocessing per multiROC

scores.svmfold = svmfold.prob
colnames(scores.svmfold) = paste(colnames(scores.svmfold), "_pred_SVM", sep = "")
true_labels_svmfold = dummies::dummy(testset$Type)
colnames(true_labels_svmfold) = paste(str_remove(colnames(true_labels_svmfold), "Type"), "_true", sep = "")
ROCsvmfold.data = data.frame(cbind(true_labels_svmfold, scores.svmfold))

# multiROC

svmfold_roc = multi_roc(ROCsvmfold.data, force_diag=T)
svmfold_pr = multi_pr(ROCsvmfold.data, force_diag=T)

plot_svmfoldroc_df <- plot_roc_data(svmfold_roc)
plot_svmfoldpr <- plot_pr_data(svmfold_pr)

ggplot(plot_svmfoldroc_df, aes(x = 1-Specificity, y=Sensitivity)) +
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

ggplot(plot_svmfoldpr, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group, linetype=Method), size=1.5) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

# 10-fold per decision tree con multi ROC

dtfold.model = train(Type ~ AbsMagn + SpectrClass, data = trainset, method = "rpart",trControl = trainControl)
dtfold.prob = predict(dtfold.model, testset, type = "prob")
dtfold.pred = predict(dtfold.model, testset, probability = T)
dtfold.confusion.matrix = confusionMatrix(dtfold.pred, testset$Type, mode = "prec_recall")
dtfold.confusion.matrix
# preprocessing per multiROC

scores.dtfold = dtfold.prob
colnames(scores.dtfold) = paste(colnames(scores.dtfold), "_pred_DT", sep = "")
true_labels_dtfold = dummies::dummy(testset$Type)
colnames(true_labels_dtfold) = paste(str_remove(colnames(true_labels_dtfold), "Type"), "_true", sep = "")
ROCdtfold.data = data.frame(cbind(true_labels_dtfold, scores.dtfold))

# multiROC

dtfold_roc = multi_roc(ROCdtfold.data, force_diag=T)
dtfold_pr = multi_pr(ROCdtfold.data, force_diag=T)

plot_dtfoldroc_df <- plot_roc_data(dtfold_roc)
plot_dtfoldpr <- plot_pr_data(dtfold_pr)


ggplot(plot_dtfoldroc_df, aes(x = 1-Specificity, y=Sensitivity)) +
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

ggplot(plot_dtfoldpr, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group, linetype=Method), size=1.5) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

# 10-fold per random forest con multi ROC

rffold.model = train(Type ~ AbsMagn + Rad, data = trainset, method = "rf",trControl = trainControl)
rffold.prob = predict(rffold.model, testset, type = "prob")
rffold.pred = predict(rffold.model, testset, probability = T)
rffold.confusion.matrix = confusionMatrix(rffold.pred, testset$Type, mode = "prec_recall")
rffold.confusion.matrix
# preprocessing per multiROC

scores.rffold = rffold.prob
colnames(scores.rffold) = paste(colnames(scores.rffold), "_pred_RF", sep = "")
true_labels_rffold = dummies::dummy(testset$Type)
colnames(true_labels_rffold) = paste(str_remove(colnames(true_labels_rffold), "Type"), "_true", sep = "")
ROCrffold.data = data.frame(cbind(true_labels_rffold, scores.rffold))

# multiROC

rffold_roc = multi_roc(ROCrffold.data, force_diag=T)
rffold_pr = multi_pr(ROCrffold.data, force_diag=T)

plot_rffoldroc_df <- plot_roc_data(rffold_roc)
plot_rffoldpr <- plot_pr_data(rffold_pr)

ggplot(plot_rffoldroc_df, aes(x = 1-Specificity, y=Sensitivity)) +
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

ggplot(plot_rffoldpr, aes(x=Recall, y=Precision)) + 
  geom_path(aes(color = Group, linetype=Method), size=1.5) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

# compare models stats BOHHHHHHHHHH
cv.values = resamples(list(svm=svmfold.model, rpart = dtfold.model, rforest=rffold.model)) 
dotplot(cv.values) 

# print AUC macro
svmfold_roc$AUC[1]$SVM$macro
dtfold_roc$AUC[1]$DT$macro
rffold_roc$AUC[1]$RF$macro







######### feature selection random forest #############
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(dataset.scaled, dataset.scaled$Type, sizes=c(1:6), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

