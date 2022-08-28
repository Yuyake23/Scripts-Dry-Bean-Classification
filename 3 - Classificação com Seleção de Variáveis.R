library(doParallel)
library(caret)
library(FSelector)
library(readxl)

# ================== doParallel ==================
cl <- makeCluster(8)
registerDoParallel(cl)

#stopCluster(cl)
#rm(cl)

# ================= PREPARAÇÃO DOS DADOS =================
Dry_Bean_Dataset <- read_excel("C:\\Users\\bruno\\OneDrive\\Documentos\\IFGoiano\\Projeto de Pesquisa\\Dataset\\Dry_Bean_Dataset.xlsx")
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


# Gráfico:
s <- rfiSelecao
colnames(s) <- c("Importance", "Feature")
idx <- order(s$Importance, decreasing = TRUE)
s$Feature <- factor(s$Feature, levels = s$Feature[order(s$Importance, decreasing = FALSE)], ordered=TRUE)
ggplot(s, aes(y = Importance, x = Feature)) +
  geom_bar(stat = "identity", fill = "#88BBAA") + # forestgreenn, lightseagreen
  coord_flip() +
  labs(x=NULL, y = "Importance") + theme_light()
  rm(s, idx)


# Etapa de preparação e aplicação dos algoritmos:
aplica <- function(treinoF, teste){
  cm <- list()
  print("Support Vector Machines with Linear Kernel")
  set.seed(10)
  svm <- train(Class ~ ., treinoF, method = "svmLinear",
               trControl = trainControl(method = "cv", number = 10),
               #tuneGrid = expand.grid(C = 1))
  tuneLengh = 4)
  cm$svm <- confusionMatrix(predict(svm, teste[-ncol(teste)]), as.factor(teste$Class))
  print(cm$overall[1])
  
  print("Bagged CART")
  set.seed(10)
  dt <- train(Class ~ ., treinoF, method = "treebag",
              trControl = trainControl(method = "cv", number = 10)) # trocar por repeatedcv fez o modelo melhorar)
  cm$dt <- confusionMatrix(predict(dt, teste[-ncol(teste)]), as.factor(teste$Class))
  print(cm$overall[1])
  
  print("k-Nearest Neighbors")
  set.seed(10)
  knn <- train(Class ~ ., treinoF, method = "kknn",
               trControl = trainControl(method = "cv", number = 10),
               #tuneGrid = expand.grid(kmax = 23, distance = 2, kernel = "optimal"))
  tuneLengh = 4)
  cm$knn <- confusionMatrix(predict(knn, teste[-ncol(teste)]), as.factor(teste$Class))
  print(cm$overall[1])
  
  print("Naive Bayes")
  set.seed(10)
  nb <- train(Class ~ ., treinoF, method = "nb",
              trControl = trainControl(method = "cv", number = 10),
              tuneGrid = expand.grid(fL = 0, usekernel = TRUE, adjust = 1))
  #tuneLengh = 4)
  cm$nb <- confusionMatrix(predict(nb, teste[-ncol(teste)]), as.factor(teste$Class))
  print(cm$overall[1])
  
  print("Random Forest")
  set.seed(10)
  rf <- train(Class ~ ., treinoF, method = "rf",
              trControl = trainControl(method = "cv", number = 10),
              tuneGrid = expand.grid(mtry = 2))
  #tuneLengh = 4)
  cm$rf <- confusionMatrix(predict(rf, teste[-ncol(teste)]), as.factor(teste$Class))
  print(cm$overall[1])
  
  print("Multi-Layer Perceptron")
  set.seed(10)
  mlp <- train(Class ~ ., treinoF, method = "mlp",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = expand.grid(size=2))
  #tuneLength = 10)
  #mlp <- RSNNS::mlp(x=treinoF, targetsTest=treinoF[[17]], size=c(12, 3))
  cm$mlp <- confusionMatrix(predict(mlp, teste[-ncol(teste)]), as.factor(teste$Class))
  print(cm$overall[1])
  return(cm)
  rm(cm)
}


resultados <- list()
for (i in 1:length(rfiSelecao$nomes)){
  treinoF <- treino1[c(rfiSelecao$nomes[1:i], "Class")]
  teste <- bd[-index.treino, colnames(treinoF)]
  
  print(paste0("Utilizando (", i, ") ", toString(colnames(treinoF[-ncol(treinoF)])), ": "))
  r <- aplica(treinoF, teste)
  resultados <- rbind(resultados, c("Support Vector Machines with Linear Kernel", r$svm$overall[1] * 100, paste0("#", ifelse(i < 10, "0", ""), i)))
  resultados <- rbind(resultados, c("Bagged CART", r$dt$overall[1] * 100, paste0("#", ifelse(i < 10, "0", ""), i)))
  resultados <- rbind(resultados, c("K-Nearest Neighbors", r$knn$overall[1] * 100, paste0("#", ifelse(i < 10, "0", ""), i)))
  resultados <- rbind(resultados, c("Naive Bayes", r$nb$overall[1] * 100, paste0("#", ifelse(i < 10, "0", ""), i)))
  resultados <- rbind(resultados, c("Random Forest", r$rf$overall[1] * 100, paste0("#", ifelse(i < 10, "0", ""), i)))
  resultados <- rbind(resultados, c("Multi-Layer Perceptron", r$svm$overall[1] * 100, paste0("#", ifelse(i < 10, "0", ""), i)))
  rm(r)
}
rm(index.treino, rfiSelecao, teste, treino1, treinoF, i, aplica)
stopCluster(cl)
rm(cl)
registerDoSEQ()

# Organiza os resultados:
resultados <- as.data.frame(resultados)
colnames(resultados) <- c("Algorithm", "Accuracy", "Feature")
resultados$Algorithm <- as.character(resultados$Algorithm)
resultados$Accuracy <- as.numeric(resultados$Accuracy)
resultados$Feature <- as.character(resultados$Feature)
resultados$Feature <- factor(resultados$Feature, ordered = TRUE,
                             levels = as.character(unique(resultados$Feature[])))
View(resultados)

# GRÁFICO DE BARRAS AGRUPADAS:
library(ggplot2)
ggplot(resultados, aes(x = Feature, y = Accuracy, fill = Algorithm)) +
  geom_bar(width=.6, position=position_dodge(.6), stat="identity",  colour="gray92") +
  geom_line(aes(group = Algorithm, colour = Algorithm), position=position_dodge(.6), size=.8) + geom_point(aes(colour = Algorithm), position=position_dodge(.6)) +
  geom_text(aes(label = paste0(round(Accuracy*100)/100, "%"), angle=90), hjust=-.1, position=position_dodge(.6), size=2) +
  ylim(0, 100)









