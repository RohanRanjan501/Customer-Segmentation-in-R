library(ggplot2) 
library(gridExtra) 
library(e1071) 
library(class) 
library(dplyr)  #select()
library(ggcorrplot)
library(corrplot)
library(fastDummies)
library(pROC)
library(NbClust)
library(factoextra) #fviz_cluster
train <- read.csv("Train.csv", stringsAsFactors = F)
test  <- read.csv("Test.csv", stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data
summary(full)

#Missing Data
full["Work_Experience_to_Age_Ratio"] <- full$Work_Experience/full$Age

#removing blanks and spaces
full[full == "" | full == " "] <- NA

my_mode <- function(x) {                # Create mode function 
  unique_x <- unique(x)
  mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
  mode
}

full$Ever_Married[is.na(full$Ever_Married)] <- my_mode(full
                                                       $Ever_Married[!is.na(full$Ever_Married)])


full$Graduated[is.na(full$Graduated)] <- my_mode(full
                                                 $Graduated[!is.na(full$Graduated)])



full$Profession[is.na(full$Profession)] <- my_mode(full
                                                   $Profession[!is.na(full$Profession)])


full$Var_1[is.na(full$Var_1)] <- my_mode(full
                                         $Var_1[!is.na(full$Var_1)])


#calculating Mean
full$Family_Size <- ifelse(is.na(full$Family_Size),
                           ave(full$Family_Size, FUN = function(x) mean(x, na.rm = TRUE)),
                           full$Family_Size)

#removing duplicate rows
full <- full[!duplicated(full), ]

#Checking Na
colSums(is.na(full))


#Replacing 0 with NA
full$Work_Experience <- replace(full$Work_Experience, full$Work_Experience ==0,NA)

#Filling all the NAs z*Age
z<-sum(full$Work_Experience,na.rm = TRUE)/sum(full$Age,na.rm = TRUE)
for (i in 1:dim(full)[1]) {
  if (is.na(full$Work_Experience[i])) {
    full$Work_Experience[i]=z*full$Age[i]
  } 
}

full$Work_Experience_to_Age_Ratio[is.na(full$Work_Experience_to_Age_Ratio)] <- 0

for (i in dim(full)) {
  if (full$Work_Experience_to_Age_Ratio[i] == 0) {
    full$Work_Experience[i]=full$Work_Experience[i]/full$Age[i]
  } 
}

#Correlation of numeric variables
corr<-cor(full[sapply(full, is.numeric)], use='pairwise')
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE) +
  ggtitle("Correlelogram plot of  variables in the dataset")+
  theme(plot.title = element_text(hjust = 1.5, vjust = 10.5))

#Dropping ID
full <- subset(full,select = -c(ID))

for (variable in c(3,6,8)) {
  table <- cbind.data.frame(colnames(full[variable]),full[variable])
  names(table) <- c("variable","Count")
  cols = rainbow(26, s=.6, v=.9)[sample(1:25,10,F)]
  assign(paste("graficoVAR", variable, sep=""),ggplot(table, aes(x=Count, fill=variable)) + 
           geom_density(alpha=.3) + scale_fill_manual(values=cols[variable])) 
}
grid.arrange(graficoVAR3,graficoVAR6,graficoVAR8, nrow = 2) 


#creating dummy columns
full <- dummy_cols(full, select_columns = c('Gender', 'Ever_Married', 'Graduated', 'Profession'))

#Creating a column  Spending Score level(Encoding)
full$Spending_Score_Level <- factor(full$Spending_Score,
                                    levels = c('Low', 'Average','High'),
                                    labels = c(1, 2, 3))

full <- dummy_cols(full, select_columns = 'Var_1')

full$Segmentation_Level <- factor(full$Segmentation,
                                  levels = c('A', 'B','C','D'),
                                  labels = c(1, 2, 3, 4))

full <- dummy_cols(full, select_columns = 'Segmentation')            

p1 <- ggplot(full, aes(x = Profession, y = Spending_Score_Level, fill = factor(Spending_Score_Level))) +
  geom_bar(stat = "identity") +  coord_flip() +  theme_minimal() +  ggtitle("Fig. A: Default fill colors")
grid.arrange(p1)

# Color by group
ggplot(full, aes(factor(Spending_Score_Level),
                 fill = factor(Spending_Score_Level))) +
  geom_bar()+ggtitle("Spending Score Count")+
  theme(plot.title = element_text(hjust = 0.55))

ggplot(full, aes(x = Spending_Score_Level, fill = Gender)) +
  geom_bar() +  theme_classic()+  geom_bar()+ggtitle("Spending Score Count(Group by Gender)")+
  theme(plot.title = element_text(hjust = 0.55))

# Box plot 
a<-ggplot(full,aes(x=Spending_Score_Level,y=Age,fill=Segmentation_Level))+geom_boxplot(outlier.color='green'
                                                                                       ,outlier.size=4,outlier.shape=5)+ggtitle("Boxplot")
print(a)

# Finding Corelation of all the variables
correlations <- cor(full[sapply(full, is.numeric)], use='pairwise')
corrplot(correlations,title = "\n\n Correlation Plot",
         method = "circle", tl.cex = 0.54, tl.col = 'black', order = "hclust", addrect = 4)

# removing irrelevant columns
full <- subset(full, select = -c(Segmentation,Gender,Ever_Married,Graduated,Profession,Spending_Score,Var_1,
                                 Work_Experience_to_Age_Ratio))

full$Segmentation_Level <- as.factor(as.character(full$Segmentation_Level))

set.seed(123)
full<-full[order(runif(10390)),]
full_mod<-full[sapply(full, is.numeric)]

n<-function(b){
  (b-min(b))/(max(b)-min(b))
}

full_new<-as.data.frame(lapply(full_mod,n))

x_train<-full_new[1:7793,]
x_test<-full_new[7794:10390,]
x_train_label<-full[1:7793,27]
x_test_label<-full[7794:10390,27]

# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(x_test), 2, nstart = 50)
# K-means clusters showing the group of each individuals
res.km$cluster


# Elbow method
fviz_nbclust(x_test,kmeans,nstart=2,method="wss")+geom_vline(xintercept = 4, linetype = 3) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

fviz_cluster(res.km, data = x_test[, -30],
             palette = c("#2E9FDF", "#00AFBB"),
                         #"#E7B800","#FF0000"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


#KNN
p<-knn(x_train,x_test,x_train_label,k=3)
t<-table(actual=x_test_label,predicted=p)
print(t)

accuracy<-sum(diag(t))/sum(t)
precision<-t[2,2]/sum(t[,2])
sensitivity<-t[2,2]/sum(t[2,])
specificity<-t[1,1]/sum(t[1,])

cat("\naccuracy of model in KNN: ", accuracy)
cat("\nprecision of model in KNN: ", precision)
cat("\nsensitivity of model in KNN: ", sensitivity)
cat("\nspecificity of model in KNN: ", specificity)


x_train$Segmentation_Level <- x_train_label
x_test$Segmentation_Level <- x_test_label

#SVM
svm1 <- svm(Segmentation_Level~., data=x_train, 
            method="C-classification", kernal="radial", 
            gamma=0.1, cost=10)

summary(svm1)

t_svm <- table(Prediction = predict(svm1, x_test),Truth = x_test$Segmentation_Level)
t_svm

accuracy<-sum(diag(t_svm))/sum(t_svm)
precision<-t_svm[2,2]/sum(t_svm[,2])
sensitivity<-t_svm[2,2]/sum(t_svm[2,])
specificity<-t_svm[1,1]/sum(t_svm[1,])

cat("\naccuracy of model is SVM: ", accuracy) 
cat("\nprecision of model is SVM: ", precision)
cat("\nsensitivity of model is SVM: ", sensitivity)
cat("\nspecificity of model is SVM: ", specificity)


# ROC curve
# true positive rate = sensitivity
# false positive rate = 1 - specificity

#ROC Curve using AUC values
result_radial_1<-svm(Segmentation_Level~.,x_train,
                     kernel="radial",gamma=10^-1)

result_radial_2<-svm(Segmentation_Level~.,x_train,
                     kernel="radial",gamma=10^-2)

result_radial_3<-svm(Segmentation_Level~.,x_train,
                     kernel="radial",gamma=10^-3)

result_radial_4<-svm(Segmentation_Level~.,x_train,
                     kernel="radial",gamma=10^-4)

pre_1<-as.numeric(predict(result_radial_1,x_test))
pre_2<-as.numeric(predict(result_radial_2,x_test))
pre_3<-as.numeric(predict(result_radial_3,x_test))
pre_4<-as.numeric(predict(result_radial_4,x_test))

a1<-multiclass.roc(x_test$Segmentation_Level,pre_1)
a2<-multiclass.roc(x_test$Segmentation_Level,pre_2)
a3<-multiclass.roc(x_test$Segmentation_Level,pre_3)
a4<-multiclass.roc(x_test$Segmentation_Level,pre_4)

plot.roc(a1[["rocs"]][[3]],print.auc = T,col="blue",print.auc.y = .5,legacy.axes = TRUE,lty=4,,main="ROC Curve")
plot.roc(a2[["rocs"]][[3]],print.auc = T,col="red",print.auc.y = .4,add=T,legacy.axes = TRUE,lty=3)
plot.roc(a3[["rocs"]][[3]],print.auc = T,col="green",print.auc.y = .3,add=T,legacy.axes = TRUE,lty=2)
plot.roc(a4[["rocs"]][[3]],print.auc = T,col="yellow",print.auc.y = .2,add=T,legacy.axes = TRUE,lty=1)
