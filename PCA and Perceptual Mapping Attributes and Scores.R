#perceptual analysis
######################################
## Perceptual and Preference Mapping #
######################################

## Install Packages (if needed)
install.packages("data.table")

## Load Packages and Set Seed
library(data.table)
set.seed(1)


## Read in perception data
per <- read.csv(file.choose()) ## Choose perceptions.csv file

## Run Principal Components Analysis on Perceptions
pca <- prcomp(per[,2:length(per)], retx=TRUE, scale=TRUE)


# Generate PCA Loading
pca

# Generate PCA Scores
pca$x

## Perceptual Map Data - Attribute Factors and CSV File
attribute <- as.data.table(colnames(per[,2:length(per)])); setnames(attribute, 1, "Attribute")
attribute
factor1 <- pca$rotation[,1]*pca$sdev[1]; factor2 <- pca$rotation[,2]*pca$sdev[2]; path <- rep(1, nrow(attribute))
pca_factors <- subset(cbind(attribute, factor1, factor2, path), select = c(Attribute, factor1, factor2, path))
pca_factors
pca_origin <- cbind(attribute, factor1 = rep(0,nrow(attribute)), factor2 = rep(0,nrow(attribute)), path = rep(0,nrow(attribute)))
pca_origin
pca_attributes <- rbind(pca_factors, pca_origin)
pca_attributes

write.csv(pca_attributes, "/Users/user/Documents/Attributes.csv",row.names = FALSE)


## Perceptual Map Data - Brand Factors and CSV File
score1 <- (pca$x[,1]/apply(abs(pca$x),2,max)[1])
score2 <- (pca$x[,2]/apply(abs(pca$x),2,max)[2])
pca_scores <- subset(cbind(per, score1, score2), select = c(Profile, score1, score2))
pca_scores

write.csv(pca_scores, "/Users/user/Documents/Scores.csv",row.names = FALSE)


