install.packages("readr") # Library to read CSV dataset
install.packages("dplyr") # %>%
install.packages("purrr") # map_df
install.packages("ggplot2") # ggplot
install.packages("gridExtra") # grid.arrange
install.packages("lsr") # correlation matrix (cramersV)
# librerie per estrazione e visualizzazione dell'output della MCA
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("caret") # train, trainControl, confusionMatrix
install.packages("rattle") # fancyRpartPlot
# Ml model performance evaluation
install.packages("MLeval") 
install.packages("klaR")
install.packages("MASS")
install.packages("naivebayes") # Training with Naive Bayes



library(readr) 
library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(lsr)
library(FactoMineR)
library(factoextra)
library(caret)
library(MLeval)
library(rattle)
library(naivebayes)



#### Importazione del dataset e cleaning ####
dataset <- read_csv("data/mushrooms.csv")
dataset <- dataset %>% map_df(function(.x) as.factor(.x))
# Rinomino i nomi delle colonne del dataset per essere R-compliant
colnames(dataset) <- c("edibility", "cap.shape", "cap.surface", 
                       "cap.color", "bruises", "odor", 
                       "gill.attachment", "gill.spacing", "gill.size", 
                       "gill.color", "stalk.shape", "stalk.root", 
                       "stalk.surface.above.ring", "stalk.surface.below.ring", "stalk.color.above.ring", 
                       "stalk.color.below.ring", "veil.type", "veil.color", 
                       "ring.number", "ring.type", "spore.print.color", 
                       "population", "habitat")
mushrooms <- dataset

# Associo ai valori degli attributi nomi piu' descrittivi
levels(dataset$edibility) <- c("edible", "poisonous")
levels(dataset$cap.shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(dataset$cap.color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                               "green", "purple", "white", "yellow")
levels(dataset$cap.surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(dataset$bruises) <- c("no", "yes")
levels(dataset$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(dataset$gill.attachment) <- c("attached", "free")
levels(dataset$gill.spacing) <- c("close", "crowded")
levels(dataset$gill.size) <- c("broad", "narrow")
levels(dataset$gill.color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                "pink", "green", "purple", "white", "yellow")
levels(dataset$stalk.shape) <- c("enlarging", "tapering")
levels(dataset$stalk.root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(dataset$stalk.surface.above.ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(dataset$stalk.surface.below.ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(dataset$stalk.color.above.ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                            "green", "purple", "white", "yellow")
levels(dataset$stalk.color.below.ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                            "green", "purple", "white", "yellow")
levels(dataset$veil.type) <- "partial"
levels(dataset$veil.color) <- c("brown", "orange", "white", "yellow")
levels(dataset$ring.number) <- c("none", "one", "two")
levels(dataset$ring.type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(dataset$spore.print.color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                       "green", "purple", "white", "yellow")
levels(dataset$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(dataset$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")
# Controllo se ho dati mancanti
map_dbl(dataset, function(.x) {sum(is.na(.x))})
# Ho solo zeri per ogni attributi, cio significa che non mancano dati


#### Prima analisi esplorativa ####
# Stampo un riassunto del dataset
summary(dataset)
# Definisco una funzione che calcola la moda per un singolo attributo
calc.mode <- function(x) {
  as.character(names(sort(-table(x),decreasing=FALSE))[1])
}
# Applico la funzione a tutte le colonne del dataset


modes <- sapply(dataset, calc.mode)
# Stampa le mode per ogni attributo del dataset
print(modes)

# Analisi bivariate, in particolare diamo un'idea di come gli attributi si
# relazionano rispetto alla variabile target e come sono distribuiti nelle
# relative classi di appartenenza (edible, poisonuos)
p1 <- ggplot(aes(x = cap.shape), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Cap Shape")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p2 <- ggplot(aes(x = cap.surface), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Cap Surface")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p3 <- ggplot(aes(x = cap.color), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Cap Color")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p4 <- ggplot(aes(x = bruises), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Bruises")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p5 <- ggplot(aes(x = odor), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Odor")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p6 <- ggplot(aes(x = gill.attachment), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Gill Attachemnt")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p7 <- ggplot(aes(x = gill.spacing), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Gill Spacing")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p8 <- ggplot(aes(x = gill.size), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Gill Size")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p9 <- ggplot(aes(x = gill.color), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Gill Color")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p10 <- ggplot(aes(x = stalk.shape), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Stalk  Shape")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p11 <- ggplot(aes(x = stalk.root), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Stalk Root")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p12 <- ggplot(aes(x = stalk.surface.above.ring), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Stalk Surface Above Ring")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p13 <- ggplot(aes(x = stalk.surface.below.ring), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Stalk Surface Below Ring") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p14 <- ggplot(aes(x = stalk.color.above.ring), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Stalk Color Above Ring")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p15 <- ggplot(aes(x = stalk.color.below.ring), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Stalk Color Below Ring")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p16 <- ggplot(aes(x = veil.type), data = dataset) +
  geom_histogram(stat = "count") +
  xlab("Veil Type")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p17 <- ggplot(aes(x = veil.color), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Veil Color")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p18 <- ggplot(aes(x = ring.number), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Ring Number")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

p19 <- ggplot(aes(x = ring.type), data = dataset) +
  geom_histogram(stat = "count") +
  facet_wrap(~edibility) +
  xlab("Ring Type")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))

grid.arrange(p1, p2, p3, p4, ncol = 2)
grid.arrange(p5, p6, p7, p8, ncol = 2)
grid.arrange(p9, p10, p11, p12, ncol = 2)
grid.arrange(p13, p14, p15, p16, ncol = 2)
grid.arrange(p17, p18, p19, ncol = 2)

# Rimuovo veil.type perchè e' un factor ad un solo livello
dataset <- dataset[-17]


# Funzioni per calcolare il chi-quadrato e la correlazione tra l'attributo ed edibility

tbl1 <- table(dataset$edibility, dataset$cap.shape)
cat("cap.shape ")
chisq.test(tbl1)

tbl2 <- table(dataset$edibility, dataset$cap.surface)
cat("cap.surface ")
chisq.test(tbl2)

tbl3 <- table(dataset$edibility, dataset$cap.color)
cat("cap.color ")
chisq.test(tbl3)

tbl4 <- table(dataset$edibility, dataset$bruises)
cat("bruises ")
chisq.test(tbl4)

tbl5 <- table(dataset$edibility, dataset$odor)
cat("odor ")
chisq.test(tbl5)

tbl6 <- table(dataset$edibility, dataset$gill.attachment)
cat("gill.attachment ")
chisq.test(tbl6)

tbl7 <- table(dataset$edibility, dataset$gill.spacing)
cat("gill.spacing ")
chisq.test(tbl7)

tbl8 <- table(dataset$edibility, dataset$gill.size)
cat("gill.size ")
chisq.test(tbl8)

tbl9 <- table(dataset$edibility, dataset$gill.color)
cat("gill.color ")
chisq.test(tbl9)

tbl10 <- table(dataset$edibility, dataset$stalk.shape)
cat("stalk.shape ")
chisq.test(tbl10)

tbl11 <- table(dataset$edibility, dataset$stalk.root)
cat("stalk.root ")
chisq.test(tbl11)

tbl12 <- table(dataset$edibility, dataset$stalk.surface.above.ring)
cat("stalk.surface.above.ring ")
chisq.test(tbl12)

tbl13 <- table(dataset$edibility, dataset$stalk.surface.below.ring)
cat("stalk.surface.below.ring ")
chisq.test(tbl13)

tbl14 <- table(dataset$edibility, dataset$stalk.color.above.ring)
cat("stalk.color.above.ring ")
chisq.test(tbl14)

tbl15 <- table(dataset$edibility, dataset$stalk.color.below.ring)
cat("stalk.color.below.ring ")
chisq.test(tbl15)

tbl16 <- table(dataset$edibility, dataset$veil.color)
cat("veil.color ")
chisq.test(tbl16)

tbl17 <- table(dataset$edibility, dataset$ring.number)
cat("ring.number ")
chisq.test(tbl17)

tbl18 <- table(dataset$edibility, dataset$ring.type)
cat("ring.type ")
chisq.test(tbl18)

tbl19 <- table(dataset$edibility, dataset$spore.print.color)
cat("spore.print.color ")
chisq.test(tbl19)

tbl20 <- table(dataset$edibility, dataset$population)
cat("population ")
chisq.test(tbl20)

tbl21 <- table(dataset$edibility, dataset$habitat)
cat("habitat ")
chisq.test(tbl21)



apply_chi_square <- function(data, target) {
  # Seleziona tutte le colonne tranne la variabile target
  cols_to_test <- names(data)[!names(data) %in% target]
  
  # Esegue il test chi-quadrato per ogni colonna e salva i risultati in una lista
  results <- lapply(cols_to_test, function(col) {
    tbl <- table(data[[target]], data[[col]])
    result <- chisq.test(tbl)
    return(result)
  })
  
  # Restituisce una lista di risultati
  return(results)
}


# al fine di non incorrere in errori, i valori degli attributi stalk.color.above.ring e stalk.color.below.ring
# sono stati riportati ad una lettera sola

dataset$stalk.color.above.ring <- mushrooms$stalk.color.above.ring
dataset$stalk.color.below.ring <- mushrooms$stalk.color.below.ring

results <- apply_chi_square(dataset, "edibility")
x.squared <- sapply(results, function(x) x$statistic)
x.squared <- as.numeric(x.squared)
x.squared[is.nan(x.squared)] <- 0
df <- data.frame(Attribute = colnames(dataset[, -1]), ChiSquared = x.squared)
ggplot(df, aes(x = Attribute, y = ChiSquared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Attributi") +
  ylab("Valore del test del Chi-Quadro") +
  ggtitle("Test del Chi-Quadro tra Target e Attributi")

levels(dataset$stalk.color.above.ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                            "green", "purple", "white", "yellow")
levels(dataset$stalk.color.below.ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                            "green", "purple", "white", "yellow")

# grafico che plotta odor e spore.print.color con la variabile target

ggplot(dataset, aes(odor, spore.print.color, edibility)) +
  geom_point(aes(shape = factor(edibility), color = factor(edibility)), size = 4.5) + 
  scale_shape_manual(values = c('+', 'x')) +
  scale_colour_manual(values = c("green", "red"))




#### Modelli di classificazione senza trainControl ####
# Divido il dataset in testset e trainset
ind = sample(2, nrow(dataset), replace = TRUE, prob=c(0.7, 0.3)) 
trainset = dataset[ind == 1,]
testset = dataset[ind == 2,] 

# DECISION TREE 
decision.tree.model <- train(x = trainset[, -1], y = trainset$edibility, method = "rpart")
# Dopo aver trainato il modello, genero la matrice di confusione
dt.pred <- predict(decision.tree.model$finalModel, testset, type = "class")
confusion.matrix.dt <- confusionMatrix(testset$edibility, dt.pred)
confusion.matrix.dt

# Stampo l'albero di decisione
fancyRpartPlot(decision.tree.model$finalModel)


# Naive Bayes 
model <- naive_bayes(edibility ~ ., data = trainset, usekernel = T) 
# Dopo aver trainato il modello, genero la matrice di confusione
nb.pred <- predict(model, testset, type="class")
confusion.matrix.nb <- confusionMatrix(testset$edibility, nb.pred)
confusion.matrix.nb




#### Sezione esperimenti: 10-Fold cross validation 3-repeated ####

# Importo klaR a causa di shadowing tra dipendenze
library(klaR)

# Definisco un trainControl per eseguire la 10 Fold Cross validation 
control <- trainControl(method = "repeatedcv",
                        number = 10, 
                        repeats = 3,
                        summaryFunction = twoClassSummary, 
                        classProbs = TRUE, 
                        savePredictions = TRUE,
                        verboseIter = TRUE)


# DECISION TREE 
decision.tree.model <- train(x = dataset[, -1], y = dataset$edibility, method = "rpart", metric="ROC", trControl = control)
# Dopo aver trainato il modello, genero la matrice di confusione
confusion.matrix.decision <- confusionMatrix(decision.tree.model, norm="none")
confusion.matrix.decision
# Stampo l'albero di decisione
fancyRpartPlot(decision.tree.model$finalModel)
# Mostro i corrispettivi grafici di ROC-AUC, AUC-PR, AUC-PGR
res.dt <- evalm(decision.tree.model, positive = 'edible')

# Calcolo delle seguenti misure di performance: accuracy, precision, recall, fmeasure
# macroPrecision, macroRecall, macroF1

n.dt = sum(confusion.matrix.decision$table) # number of instances
nc.dt = nrow(confusion.matrix.decision$table) # number of classes
diag.dt = diag(confusion.matrix.decision$table) # number of correctly classified instances per class 
rowsums.dt = apply(confusion.matrix.decision$table, 1, sum) # number of instances per class
colsums.dt = apply(confusion.matrix.decision$table, 2, sum) # number of predictions per class
accuracy.dt = sum(diag.dt) / n.dt 
precision.dt = diag.dt / colsums.dt 
precision.dt
recall.dt = diag.dt / rowsums.dt
recall.dt
fmeasure.dt = 2 * precision.dt * recall.dt / (precision.dt + recall.dt) 
fmeasure.dt
macroPrecision.dt = mean(precision.dt)
macroPrecision.dt
macroRecall.dt = mean(recall.dt)
macroRecall.dt
macroF1.dt = mean(fmeasure.dt) #Macro AVG Accuracy 
macroF1.dt


# Naive bayes
nb.model <- train(x = data.frame(dataset[, -1]), y = dataset$edibility, method = "nb", metric="ROC", trControl = control)
# Dopo aver trainato il modello, genero la matrice di confusione
confusion.matrix.nb <- confusionMatrix(nb.model, norm="none")
confusion.matrix.nb
# Mostro i corrispettivi ROC-AUC, AUC-PR, AUC-PGR
res.nb <- evalm(nb.model, positive = 'edible')


# Calcolo delle seguenti misure di performance: accuracy, precision, recall, fmeasure
# macroPrecision, macroRecall, macroF1
n.nb = sum(confusion.matrix.nb$table) # number of instances
nc.nb = nrow(confusion.matrix.nb$table) # number of classes
diag.nb = diag(confusion.matrix.nb$table) # number of correctly classified instances per class 
rowsums.nb = apply(confusion.matrix.nb$table, 1, sum) # number of instances per class
colsums.nb = apply(confusion.matrix.nb$table, 2, sum) # number of predictions per class
accuracy.nb = sum(diag.nb) / n.nb 
precision.nb = diag.nb / colsums.nb 
precision.nb
recall.nb = diag.nb / rowsums.nb
recall.nb
fmeasure.nb = 2 * precision.nb * recall.nb / (precision.nb + recall.nb) 
fmeasure.nb 
macroPrecision.nb = mean(precision.nb)
macroPrecision.nb
macroRecall.nb = mean(recall.nb)
macroRecall.nb
macroF1.nb = mean(fmeasure.nb) #Macro AVG Accuracy 
macroF1.nb

#### No Odor ####
dataset.no.odor <- dataset[, -6]
# DECISION TREE
decision.tree.model <- train(x = dataset.no.odor[, -1], y = dataset.no.odor$edibility, method = "rpart", metric="ROC", trControl = control)
decision.tree.model
# Dopo aver trainato il modello, genero la matrice di confusione
confusion.matrix.decision <- confusionMatrix(decision.tree.model, norm="none")
confusion.matrix.decision
# Stampo l'albero di decisione
fancyRpartPlot(decision.tree.model$finalModel, cex=0.7)
# Mostro i corrispettivi ROC-AUC, AUC-PR, AUC-PGR
res <- evalm(decision.tree.model, positive = 'edible')
# Calcolo delle seguenti misure di performance: accuracy, precision, recall, fmeasure
# macroPrecision, macroRecall, macroF1
n.dt = sum(confusion.matrix.decision$table) # number of instances
nc.dt = nrow(confusion.matrix.decision$table) # number of classes
diag.dt = diag(confusion.matrix.decision$table) # number of correctly classified instances per class 
rowsums.dt = apply(confusion.matrix.decision$table, 1, sum) # number of instances per class
colsums.dt = apply(confusion.matrix.decision$table, 2, sum) # number of predictions per class
accuracy.dt = sum(diag.dt) / n.dt 
precision.dt = diag.dt / colsums.dt 
precision.dt
recall.dt = diag.dt / rowsums.dt
recall.dt
fmeasure.dt = 2 * precision.dt * recall.dt / (precision.dt + recall.dt) 
fmeasure.dt
macroPrecision.dt = mean(precision.dt)
macroPrecision.dt
macroRecall.dt = mean(recall.dt)
macroRecall.dt
macroF1.dt = mean(fmeasure.dt) #Macro AVG Accuracy 
macroF1.dt



# Naive bayes
nb.model <- train(x = dataset.no.odor[, -1], y = dataset.no.odor$edibility, method = "nb", metric="ROC", trControl = control)
# Dopo aver trainato il modello, genero la matrice di confusione
confusion.matrix.nb <- confusionMatrix(nb.model, norm="none")
confusion.matrix.nb
# Mostro i corrispettivi ROC-AUC, AUC-PR, AUC-PGR
res <- evalm(nb.model, positive = 'edible')
# Calcolo delle seguenti misure di performance: accuracy, precision, recall, fmeasure
# macroPrecision, macroRecall, macroF1
n.nb = sum(confusion.matrix.nb$table) # number of instances
nc.nb = nrow(confusion.matrix.nb$table) # number of classes
diag.nb = diag(confusion.matrix.nb$table) # number of correctly classified instances per class 
rowsums.nb = apply(confusion.matrix.nb$table, 1, sum) # number of instances per class
colsums.nb = apply(confusion.matrix.nb$table, 2, sum) # number of predictions per class
accuracy.nb = sum(diag.nb) / n.nb 
precision.nb = diag.nb / colsums.nb 
precision.nb
recall.nb = diag.nb / rowsums.nb
recall.nb
fmeasure.nb = 2 * precision.nb * recall.nb / (precision.nb + recall.nb) 
fmeasure.nb 
macroPrecision.nb = mean(precision.nb)
macroPrecision.nb
macroRecall.nb = mean(recall.nb)
macroRecall.nb
macroF1.nb = mean(fmeasure.nb) #Macro AVG Accuracy 
macroF1.nb




