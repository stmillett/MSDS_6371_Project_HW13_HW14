housing_data <- read.csv("Data/train.csv")

head(housing_data)

unique(housing_data$Neighborhood)

#Subsetting master dataset with Neighborhoods BrkSide, Edwards, and NAmes
analysis1_data <- subset(housing_data,Neighborhood=="BrkSide"|Neighborhood=="Edwards"|Neighborhood=="NAmes",
                         select = c(Neighborhood,GrLivArea,SalePrice))
head(analysis1_data)

#Creating basic matrix scatterplot to see relationship of untransformed data
pairs(~Neighborhood + GrLivArea + SalePrice, data=analysis1_data,
      main="Simple Scatterplot of Housing Data")

#Create variables for log transformation of price and living area
analysis1_data$logSalePrice <- log(analysis1_data$SalePrice)
analysis1_data$logGrLivArea <- log(analysis1_data$GrLivArea)

pairs(~Neighborhood + logGrLivArea + logSalePrice, data=analysis1_data,
      main="Simple Scatterplot of Housing Data")

analysis1_data <- within(analysis1_data, Neighborhood <- relevel(x=Neighborhood, ref = "NAmes"))

#Model of interaction with the neighborhood on log(GrLivArea) to predict log(SalePrice)
summary(lm(logSalePrice~logGrLivArea+Neighborhood+logGrLivArea:Neighborhood,data = analysis1_data))
