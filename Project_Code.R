####data cleaning
df=data
### here we clean the vectors contains NA values 
## because pca dont work if there are NA values
vec <- ifelse(df$Total_Volume == "NA", NA, df$Total_Volume)   ###total volume
vec_numeric <- as.numeric(vec)
vec_numeric[is.na(vec_numeric)] <- mean(vec_numeric, na.rm = TRUE)
df$Total_Volume=vec_numeric
mean(vec_numeric)


vec <- ifelse(df$DRA == "NA", NA, df$DRA)   ### Debit reversal amount
vec_numeric <- as.numeric(vec)
vec_numeric[is.na(vec_numeric)] <- mean(vec_numeric, na.rm = TRUE)
df$DRA=vec_numeric
mean(vec_numeric)

###total volume
vec1 <- ifelse(new.transform.data$Total_Volume.ben == "NA", NA, new.transform.data$Total_Volume.ben)
vec_numeric2 <- as.numeric(vec1)
vec_numeric2[is.na(vec_numeric2)] <- mean(vec_numeric2, na.rm = TRUE)
new.transform.data$Total_Volume.ben=vec_numeric2
mean(vec_numeric2)


###   pca


df_scaled <- scale(df)  # Centers to mean 0 and scales to sd 1

pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# Print summary of PCA results
summary(pca_result)

# Biplot
biplot(pca_result)

# Calculate variance explained by each principal component
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumsum(var_explained)
# Plotting the scree plot
plot(var_explained, xlab = "Principal Component", ylab = "Variance Explained", 
     type = 'b', pch = 19, main = "Scree Plot")

# Assuming you have a vector of explained variances for each component
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Calculate the differences in explained variance between successive components
diffs <- diff(var_explained)

# Find the component just before the explained variance starts to level off significantly
elbow_point <- which.max(diffs) + 1

print(elbow_point)

###
###### regression analysis

y=new_transform_data$Total_Volume.ben

Total_Volume=df$Total_Volume
DRA=df$DRA
Approve=df$Approve
BD=df$BD
DRS=df$DRS
BD.Ben=df$BD.Ben
Approve.Benefi=df$Approve.Benefi
reg=lm(y ~ 1+Total_Volume+DRA+Approve+BD+DRS+BD.Ben+Approve.Benefi)
summary(reg)

