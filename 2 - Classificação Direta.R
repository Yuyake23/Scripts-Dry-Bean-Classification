library(caret)
library(doParallel)

# ================== doParallel ==================
cl <- makeCluster(8)
registerDoParallel(cl)

#stopCluster(cl)
#rm(cl)

# ============= Preparação dos dados =============
Dry_Bean_Dataset <- readxl::read_excel("C:\\Users\\bruno\\OneDrive\\Documentos\\IFGoiano\\Projeto de Pesquisa\\Dataset\\Dry_Bean_Dataset.xlsx")
bd <- Dry_Bean_Dataset
bd$Class <- as.factor(bd$Class)
set.seed(10)
bd <- dplyr::slice(bd, sample(1:nrow(bd)))

set.seed(10)
index.treino <- createDataPartition(bd$Class, p = .8, list = FALSE)
treino <- bd[index.treino,]
teste <- bd[-index.treino,]
rm(bd, index.treino)

# ============================= INICIO DA APLICAÇÃO DOS ALGORITMOS =============================
print("Support Vector Machines with Linear Kernel")
set.seed(10)
svm <- train(Class ~ ., treino, method = "svmLinear",
             trControl = trainControl(method = "cv", number = 10),
             #tuneGrid = expand.grid(C = 1))
             tuneLengh = 20)
cm <- confusionMatrix(predict(svm, teste[-ncol(teste)]), as.factor(teste$Class))
print(cm$overall[1])

print("Bagged CART")
set.seed(10)
dt <- train(Class ~ ., treino, method = "treebag",
            trControl = trainControl(method = "cv", number = 10)) # trocar por repeatedcv fez o modelo melhorar)
cm <- confusionMatrix(predict(dt, teste[-ncol(teste)]), as.factor(teste$Class))
print(cm$overall[1])

print("k-Nearest Neighbors")
set.seed(10)
knn <- train(Class ~ ., treino, method = "kknn",
             trControl = trainControl(method = "cv", number = 10),
             #tuneGrid = expand.grid(kmax = 23, distance = 2, kernel = "optimal"))
             tuneLengh = 4)
cm <- confusionMatrix(predict(knn, teste[-ncol(teste)]), as.factor(teste$Class))
print(cm$overall[1])

print("Naive Bayes")
set.seed(10)
nb <- train(Class ~ ., treino, method = "nb",
            trControl = trainControl(method = "cv", number = 10),
            tuneGrid = expand.grid(fL = 0, usekernel = TRUE, adjust = 1))
            #tuneLengh = 4)
cm <- confusionMatrix(predict(nb, teste[-ncol(teste)]), as.factor(teste$Class))
print(cm$overall[1])

print("Random Forest")
set.seed(10)
rf <- train(Class ~ ., treino, method = "rf",
            trControl = trainControl(method = "cv", number = 10),
            tuneGrid = expand.grid(mtry = 2))
            #tuneLengh = 4)
cm <- confusionMatrix(predict(rf, teste[-ncol(teste)]), as.factor(teste$Class))
print(cm$overall[1])

print("Multi-Layer Perceptron")
set.seed(10)
mlp <- train(Class ~ ., treino, method = "mlp",
             trControl = trainControl(method = "cv", number = 10),
             tuneGrid = expand.grid(size=2))
             #tuneLength = 10)
#mlp <- RSNNS::mlp(x=treinoF, targetsTest=treinoF[[17]], size=c(12, 3))
cm <- confusionMatrix(predict(mlp, teste[-ncol(teste)]), as.factor(teste$Class))
print(cm$overall[1])
rm(cm)
# ============================= FIM DA APLICAÇÃO DOS ALGORITMOS =============================

confusionMatrix.train(svm, norm = "none")
confusionMatrix(predict(svm, teste[-ncol(teste)]), as.factor(teste$Class))



stopCluster(cl)
rm(cl)

