library(caret)
library(FSelector)
library(doParallel)

cl <- makeCluster(10)
registerDoParallel(cl)

# PREPARAÇAO DOS DADOS:
Dry_Bean_Dataset <- readxl::read_excel("C:\\Users\\bruno\\OneDrive\\Documentos\\IFGoiano\\Projeto de Pesquisa\\Dataset\\Dry_Bean_Dataset.xlsx")
bd <- Dry_Bean_Dataset
bd$Class <- as.factor(bd$Class)
set.seed(10)
bd <- dplyr::slice(bd, sample(1:nrow(bd)))

# Cria partição de dados para treino e teste
set.seed(10)
index.treino <- createDataPartition(bd$Class, p = .8, list = FALSE)
treino0 <- bd[index.treino,]

# Remove variaveis com correlação acima de 0.98
set.seed(10)
c <- findCorrelation(cor(treino0[-ncol(treino0)]), cutoff = .98)
paste("Removendo", length(c), "variÃ¡veis:", toString(colnames(treino0)[c]))
treino1 <- treino0[-c]
rm(c)

# CRIA RANKING DE VARIAVEIS POR RFI:
FeatureSelec <- random.forest.importance(Class ~ ., treino1)
FeatureSelec$nomes <- row.names(FeatureSelec)
FeatureSelec <- plyr::arrange(FeatureSelec, attr_importance, decreasing = TRUE)

# FUNÇÃO QUE APLICACA O SVM:
aplica <- function(treinoF, teste){
  r <- list()
  set.seed(10)
  svm <- train(Class ~ ., treinoF, method = "svmLinear",
               trControl = trainControl(method = "cv", number = 10),
               #tuneGrid = expand.grid(C = 1))
               tuneLengh = 4)
  r$treino <- confusionMatrix.train(svm)
  r$teste <- confusionMatrix(predict(svm, teste[-ncol(teste)]), as.factor(teste$Class), mode="everything")
  # print(resultado$overall[1])
  print(r$treino)
  print(r$teste)
  
  return(r)
}


# Aplicando modelo:
resultados <- list()
for (i in 1:length(FeatureSelec$nomes)){
  treinoF <- treino1[c(FeatureSelec$nomes[1:i], "Class")]
  teste <- bd[-index.treino, colnames(treinoF)]
  
  print(paste0("Utilizando (", i, ") ", toString(colnames(treinoF[-ncol(treinoF)])), ": "))
  r <- aplica(treinoF, teste)
  resultados <- rbind(resultados, c("Support Vector Machines with Linear Kernel", r$teste$overall[1] * 100, paste0("#", ifelse(i < 10, "0", ""), i)))

  rm(r)
}

# Para o processamento paralelo:
rm(index.treino, FeatureSelec, teste, treino0, treino1, treinoF, i, aplica)
stopCluster(cl)
rm(cl)
registerDoSEQ()

# Organiza os resultados
resultados <- as.data.frame(resultados)
colnames(resultados) <- c("Algorithm", "Accuracy", "Feature")
resultados$Algorithm <- as.character(resultados$Algorithm)
resultados$Accuracy <- as.numeric(resultados$Accuracy)
resultados$Feature <- as.character(resultados$Feature)
resultados$Feature <- factor(resultados$Feature, ordered = TRUE,
                             levels = as.character(unique(resultados$Feature[])))
#View(resultados)


library(ggplot2)

ggplot(resultados, aes(x=Feature, y=Accuracy)) +
  scale_fill_gradient(low="#ed4811", high="#2aab1b") + # red2 e seagreen4
  geom_bar(stat="identity", aes(fill=Accuracy)) + 
  ylim(0, 100) +
  geom_text(aes(label = paste0(round(Accuracy*100)/100, "%"), angle=0), hjust=0.5, vjust=-.3, position=position_dodge(.5), size=3.5) +
  labs( x = "Subconjuntos", y = "Acurácia", caption = "xx") +
  theme_bw()


# Utilizando apenas o subconjunto 05:
treinoF <- treino1[c(FeatureSelec$nomes[1:5], "Class")]
teste <- bd[-index.treino, colnames(treinoF)]

print(paste0("Utilizando (", 5, ") ", toString(colnames(treinoF[-ncol(treinoF)])), ": "))
r <- aplica(treinoF, teste)
print(r$teste)
