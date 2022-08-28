library(caret)
library(factoextra)
library(dplyr)

bd <- Dry_Bean_Dataset
set.seed(10)
bd <- bd[createDataPartition(bd$Class, p = .8, list = FALSE),]
#dbd <- dplyr::slice(dbd, sample(1:nrow(dbd)))

pca <- prcomp(bd[-17], center = TRUE, scale = TRUE, retx = TRUE)
summary(pca)

# GRÁFICO DA PORCENTAGEM DE EXPLICAÇÃO DE CAFA PC:
fviz_eig(pca)

# GRÁFICO DE INDIVÍDUOS:
fviz_pca_ind(pca,
             axes = c(1, 2),
             col.ind = bd$Class, # cor por forma
             #palette = "uchicago",
             palette = c("#14a73a",  "#4b4b8a", "#821281", "#775221", "#0c5cdb", "#888888", "#000000"),
             addEllipses = FALSE,
             #ellipse.type = "convex",
             legend.title = "Class", 
             label = "none",
             ellipse.level = 0.9,
             title = ""
) + scale_shape_manual(values=rep(16, 7))
#scale_shape_manual(values=0:6)
#scale_shape_manual(values=rep(16, 7))
#scale_shape_manual(values=c(16, 17, 15, 18, 7, 8, 13))
rm(bd, pca)
