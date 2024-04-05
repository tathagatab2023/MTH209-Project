#### PCA analysis #############################################################

data=regression_data
data=as.data.frame(regression_data)
Total_vol_rem=data$...5
DRA_rem=na.omit(as.numeric(data$...6))
Approve_rem=na.omit(as.numeric(data$...7))
BD_rem=na.omit(as.numeric(data$...8))
DRS_rem=na.omit(as.numeric(data$...9))
Total_volume_ben=na.omit(as.numeric(data$Response))
Approve_ben=na.omit(as.numeric(data$...11))
BD_ben=na.omit(as.numeric(data$...12))
Total_vol_rem=na.omit(as.numeric(Total_vol_rem))

pca_data=cbind(Total_vol_rem,Total_volume_ben,DRA_rem,Approve_rem,BD_rem,DRS_rem,Approve_ben,BD_ben)
dim(pca_data)
# Assuming your data is stored in a data frame called 'data'
# Remove any rows with missing values
pca_data <- pca_data[complete.cases(pca_data), ]

# Standardize the data
scaled_data <- scale(pca_data)

# Perform PCA
pca_result <- prcomp(scaled_data)

# Summary of PCA results
summary(pca_result)

# Extracting principal component scores
pc_scores <- pca_result$x

# Biplot (optional)
biplot(pca_result)
# Assuming pca_result is the result of PCA obtained using prcomp()

# Extracting standard deviations of each principal component
pc_std_dev <- pca_result$sdev

# Calculating variance explained by each principal component
var_explained <- (pc_std_dev^2) / sum(pc_std_dev^2) * 100
round(var_explained)
# Scree plot
plot(1:length(var_explained), var_explained, type = "b", 
     xlab = "Principal Component", ylab = "Percentage of Variance Explained",
     main = "Scree Plot")
write.xlsx(pca_data,file="pca_data.xlsx")
pca_data=as.data.frame(pca_data)

################################################################################