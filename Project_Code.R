####data cleaning
d1 = read.csv("Beneficiary_Bank_Data.csv")[,-1]
d2 = read.csv("Remitter_Bank_Data.csv")[,-1]

head(d1)
head(d2)


d2[,5] = as.numeric(substr(d2[,5],1,5))
d2[,6] = as.numeric(substr(d2[,6],1,4))
d2[,7] = as.numeric(substr(d2[,7],1,4))
#d1[,8] = as.numeric(substr(d1[,8],1,4))
#head(d1)
d2[,4] = gsub(",","",d2[,4])
d2[,4] = as.numeric(d2[,4])

d2[,9]=as.numeric(gsub("%","",d2[,9]))


d2[,5] = d2[,5]*d2[,4]/100
d2[,6] = d2[,6]*d2[,4]/100
d2[,7] = d2[,7]*d2[,4]/100
d2[,9] = d2[,9]*d2[,8]/100
#d1[,8] = d1[,8]*d1[,4]/100
head(d2)
d2[49,]
write.csv(d2,"Remitter_Bank_Data.csv")

aggregate(d2[,5]~d2[,3],data=d2,length)
aggregate(d1[,5]~d1[,3],data=d1,length)

library(stringr)
head(d1)
d1[,3] = str_to_title(d1[,3])
d2[,3] = str_to_title(d2[,3])
d1 = read.csv("Beneficiary_Bank_Data.csv")[,-c(1:2)]
head(d1)
d2[which(d2[,3] == "Axis Bank Ltd."),3]="Axis Bank Ltd"
d2[which(d2[,3] == "Citi"),3]="Citibank"
d2[which(d2[,3] == "Citi Bank"),3] = "Citibank"
d2[which(d2[,3] == "Rbl"),3]="Rbl Bank"
d2[which(d2[,3] == "Tri O Tech Solutions Private Limited (Ppi)"),3]="Tri O Tech Solutions Private Limited"
d2[which(d2[,3] == "Fino Payments Bank Limited"),3] = "Fino Payments Bank"
d2[which(d2[,3] == "Fino Payments Bank Limited Fip"),3]="Fino Payments Bank"
d2[which(d2[,3] == "Equitas Bank"),3]="Equitas Small Finance Bank"
d2[which(d2[,3] == "Dbs Bank Ltd"),3] = "Dbs Bank India Limited"

aggregate(d2[,5]~d2[,3],data=d2,length)
aggregate(d1[,5]~d1[,3],data=d1,length)

write.csv(d1,"Beneficiary_Bank_Data.csv")
write.csv(d2,"Remitter_Bank_Data.csv")


df=data   ## regression data xlsx
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

