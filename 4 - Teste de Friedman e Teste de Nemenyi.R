library(dplyr)
library(rstatix)
library(reshape)
library(PMCMRplus)
library(ggplot2)
library(caret)
library(FSelector)
library(doParallel)

# ================== doParallel ==================
cl <- makeCluster(8)
registerDoParallel(cl)

#stopCluster(cl)
#rm(cl)

# ================= PREPARAÇÃO DOS DADOS =================
bd <- Dry_Bean_Dataset
bd$Class <- as.factor(bd$Class)
set.seed(10)
bd <- dplyr::slice(bd, sample(1:nrow(bd)))

set.seed(10)
index.treino <- createDataPartition(bd$Class, p = .8, list = FALSE)
treino0 <- bd[index.treino,]

# Remove variaveis com correlação acima de 0.98:
set.seed(10)
c <- findCorrelation(cor(treino0[-ncol(treino0)]), cutoff = .98)
paste("Removendo", length(c), "variáveis:", toString(colnames(treino0)[c]))
treino1 <- treino0[-c]
rm(c, treino0)

# Faz ranking de importância das variáveis:
rfiSelecao <- random.forest.importance(Class ~ ., treino1)
rfiSelecao$nomes <- row.names(rfiSelecao)
rfiSelecao <- plyr::arrange(rfiSelecao, attr_importance, decreasing = TRUE)

# Termina preparação dos dados
treinoF <- treino1[c(rfiSelecao$nomes[1:5], "Class")] # Utiliza as 5 melhores variáveis
teste <- bd[-index.treino, colnames(treinoF)]
rm(treino1, index.treino, rfiSelecao)

print("Support Vector Machines with Linear Kernel")
set.seed(10)
svm <- train(Class ~ ., treinoF, method = "svmLinear",
             trControl = trainControl(method = "cv", number = 10),
             tuneGrid = expand.grid(C = 1))
#tuneLengh = 4)

print("Bagged CART")
set.seed(10)
dt <- train(Class ~ ., treinoF, method = "treebag",
            trControl = trainControl(method = "cv", number = 10)) # trocar por repeatedcv fez o modelo melhorar)

print("k-Nearest Neighbors")
set.seed(10)
knn <- train(Class ~ ., treinoF, method = "kknn",
             trControl = trainControl(method = "cv", number = 10),
             tuneGrid = expand.grid(kmax = 23, distance = 2, kernel = "optimal"))
#tuneLengh = 4)

print("Naive Bayes")
set.seed(10)
nb <- train(Class ~ ., treinoF, method = "nb",
            trControl = trainControl(method = "cv", number = 10),
            tuneGrid = expand.grid(fL = 0, usekernel = TRUE, adjust = 1))
#tuneLengh = 4)

print("Random Forest")
set.seed(10)
rf <- train(Class ~ ., treinoF, method = "rf",
            trControl = trainControl(method = "cv", number = 10),
            tuneGrid = expand.grid(mtry = 2))
#tuneLengh = 4)

# -======== > COMO O MLP NÃO SE ADAPTA, ELE INVALIDARIA O TESTE
#print("Multi-Layer Perceptron")
#set.seed(10)
#mlp <- train(Class ~ ., treinoF, method = "mlp",
#             trControl = trainControl(method = "cv", number = 10),
#             tuneGrid = expand.grid(size=2))
#tuneLength = 10)
#mlp <- RSNNS::mlp(x=treinoF, targetsTest=treinoF[[17]], size=c(12, 3))

stopCluster(cl)
rm(cl)

# salva as acurácias do algoritmos em cada fold:
accFolds <- data.frame(as.factor(svm$resample$Resample),
                       as.numeric(svm$resample$Accuracy),
                       as.numeric(dt$resample$Accuracy),
                       as.numeric(knn$resample$Accuracy),
                       as.numeric(nb$resample$Accuracy),
                       as.numeric(rf$resample$Accuracy))
rm(treinoF, teste, svm, dt, knn, nb, rf)
colnames(accFolds) <- c("Fold", "SVM", "CART", "KNN", "NB", "RF")

accFolds <- melt(accFolds,
                  id = "Fold",
                  measured = c("SVM", "CART", "KNN", "NB", "RF"))
colnames(accFolds) <- c("Fold", "Algorithm", "Acc")
accFolds <- sort_df(accFolds, vars = "Fold")
#View(accFolds)
glimpse(accFolds)

accFolds$Acc <- accFolds$Acc * 100

# APLICA O TESTE DE FRIEDMAN
friedman.test(Acc ~ Algorithm | Fold, data = accFolds)
# APLICA O TESTE DE NEMENYI
frdAllPairsNemenyiTest(accFolds$Acc, accFolds$Algorithm, accFolds$Fold, p.adjust = "bonferroni") # A diferença é considerável quando o p.value é menor do que 5% (0.05)


# Boxplot Acc em cada fold x Algoritmo:
boxplot(Acc ~ Algorithm, data = accFolds)
ggplot(accFolds, aes(Algorithm, Acc, fill = Algorithm)) + geom_boxplot()
