# Install Package needed to run conjoint analysis
install.packages("conjoint")

# Load library and Set Seed to an integer
library(conjoint)
set.seed(40387286)
        
## Set up attributes and levels as a list from the data given
attrib.level <- list(Brand = c("Apple", "Lenovo", "Dell", "Acer"),
                     HardDrive = c("128 GB", "256 GB", "512 GB"),
                     RAM = c("2 GB", "4 GB", "8 GB", "16 GB"),
                     Screensize = c("12.1 in", "15.4 in", "17.3 in"),
                     Price = c("$900", "$1200", "$1500", "$2000"))

design <-read.csv(file.choose()) #read in design file

print(cor(caEncodedDesign(design)))


pref <- read.csv(file.choose()) ##read in transposed preferences



attrib.vector <- data.frame(unlist(attrib.level,use.names=FALSE))
colnames(attrib.vector) <- c("levels")
part.worths <- NULL
for (i in 1:ncol(pref)){
  temp <- caPartUtilities(pref[,i], design, attrib.vector)
  Base_Brand <- temp[,"Apple"]; Base_HardDrive <- temp[,"128 GB"]; Base_RAM <- temp[,"2 GB"]
  Base_Screensize <- temp[,"12.1 in"]; Base_Price <- temp[,"$900"]
  temp[,"intercept"] <- temp[,"intercept"] - Base_Brand - Base_HardDrive - Base_RAM -
    Base_Screensize - Base_Price
  L1 <- length(attrib.level$Brand) + 1 ## Add 1 for the intercept
  for (j in 2:L1){temp[,j] <- temp[,j] - Base_Brand}
  L2 <- length(attrib.level$HardDrive) + L1
  for (k in (L1+1):L2){temp[,k] <- temp[,k] - Base_HardDrive}
  L3 <- length(attrib.level$RAM) + L2
  for (l in (L2+1):L3){temp[,l] <- temp[,l] - Base_RAM}
  L4 <- length(attrib.level$Screensize) + L3
  for (m in (L3+1):L4){temp[,m] <- temp[,m] - Base_Screensize}
  L5 <- length(attrib.level$Price) + L4
  for (n in (L4+1):L5){temp[,n] <- temp[,n] - Base_Price}
  part.worths <- rbind.data.frame(part.worths,temp)
}

part.worths

rownames(part.worths) <- colnames(pref)

## Export part-worths from analysis
write.csv(part.worths, "/Users/user/Desktop/goodpartworths_result.csv",row.names = FALSE)
