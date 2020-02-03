library("ggplot2")
library("corrplot")


dataset = read.csv("stars.csv")
colnames(dataset) = c("Temp", "Lum", "Rad", "AbsMagn", "Type", "Color", "SpectrClass")
dataset = dataset[, c(1,2,3,4,6,7,5)]
dataset$Type = factor(dataset$Type)
types = c("BrownDwarf", "RedDwarf", "WhiteDwarf","MainSequence", "Supergiant", "Hypergiant")
dataset$Type = factor(sapply(dataset$Type, function (x) { types[x]}))
dataset$SpectrClass = factor(dataset$SpectrClass, levels=c("O", "B", "A", "F", "G", "K", "M"))

dataset.notarget = function(data) {
  return(data[1:length(data)-1])
}

#check distribution of target
targetTable = table(dataset$Type)
pie(targetTable)
plot(dataset, col = dataset$Type)

# cor
dataset.cor = cor(dataset.notarget(dataset[, 1:5]))
dataset.cor.plot = corrplot(dataset.cor)

ggplot(dataset, aes(x = Temp, y = AbsMagn, color = Type)) + geom_point()
ggplot(dataset, aes(x = Lum, y = AbsMagn, color = Type)) + geom_point()
ggplot(dataset, aes(x = Rad, y = AbsMagn, color = Type)) + geom_point()

# scale numeric attributes
dataset.scaled = data.frame(cbind(scale(dataset[,1:4]), dataset[,5:7]))


# Ho provato a vedere se eraa vera la cosa di wikipedia che ho trovato. Sembra di sì. 
ggplot(dataset, aes(x = SpectrClass, y = AbsMagn, color = Type)) + geom_point() + scale_y_reverse()


