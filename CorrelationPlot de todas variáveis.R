library(corrplot)
library(caret)
library(readxl)

Dry_Bean_Dataset <- readxl::read_excel("C:\\Users\\bruno\\OneDrive\\Documentos\\IFGoiano\\Projeto de Pesquisa\\Dataset\\Dry_Bean_Dataset.xlsx")
bd <- Dry_Bean_Dataset
bd$Class <- as.factor(bd$Class)

set.seed(10)
index.treino <- createDataPartition(bd$Class, p = .8, list = FALSE)
treino0 <- bd[index.treino,]

corrplot(cor(treino0[-17]), method ="circle", type = "upper")

corrplot(cor(treino0[-17]), method = "square", type = "upper",
         addCoef.col = "black")

