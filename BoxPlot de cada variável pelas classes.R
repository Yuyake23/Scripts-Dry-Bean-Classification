library(ggplot2)
library(gridExtra)
bd <- Dry_Bean_Dataset
set.seed(10)
bd <- bd[caret::createDataPartition(bd$Class, p = .8, list = FALSE),]

grid.arrange(
  ggplot(bd, aes(Class, Area, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, Perimeter, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, MajorAxisLength, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, MinorAxisLength, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, AspectRation, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, Eccentricity, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, ConvexArea, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, EquivDiameter, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, Extent, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, Solidity, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, roundness, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, Compactness, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, ShapeFactor1, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, ShapeFactor2, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, ShapeFactor3, fill = Class)) + geom_boxplot(),
  ggplot(bd, aes(Class, ShapeFactor4, fill = Class)) + geom_boxplot()
)
