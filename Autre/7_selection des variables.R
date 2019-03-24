#####################################
#Selection des variables#############
#####################################
library(glmnet)#contrainte
library(randomForest)#Foret
library(gbm)#Boosting
library(e1071)#SVM
library(rpart)#Arbre
library(plyr)
library(caret)#pour l'utilisation des données Caret
library(dplyr)
don <- read.csv("data/nhanes_hyper_mice.csv")
don$X <- NULL
levels(don$Y) <- c(0,1)
summary(don)
don <- don[-which(don$Doctor_told_you_have_diabetes=="Borderline"),]
levels(don$Doctor_told_you_have_diabetes)[1] <- "No"
XX <- as.matrix(model.matrix(~.,don)[,-ncol(model.matrix(~.,don))])
YY <- as.matrix(model.matrix(~.,don)[,ncol(model.matrix(~.,don))])

# Création de la fonction pour calculer l'importance des variables
# x est le modele; k est le nombre de variable
# valeur de t: (1 pour glm et randomforest, 2 pour glmnet, 3 pour gbm)
variable_imp <- function(x,k=10,t=1,mot=""){
  switch(t,
         x <- varImp(x), #on utilise varImp de Caret pour glm et randomforest
         x <- as.data.frame(as.matrix(x)), # utile pour mise au format dataframe glmnet
         x[,1] <- NULL # utile pour enlever une colonne en trop pour gbm
         )
  tempo <- cbind(row.names(x),x)
  row.names(tempo) <- NULL
  colnames(tempo)[1] <- "variable"
  colnames(tempo)[2] <-  "importance"
  tempo[,1] <- gsub("YES$","",tempo[,1],ignore.case = TRUE)
  tempo[,1] <- gsub("NO$","",tempo[,1],ignore.case = TRUE)
  tempo[,2] <- round((tempo[,2]-mean(tempo[,2]))/sqrt(var(tempo[,2])),3) # on centre réduit
  tempo <- arrange(tempo, desc(importance))[1:k,]
  colnames(tempo)[2] <-  paste("imp",mot,sep = "_")
  tempo$rank <- seq(1:k)
  colnames(tempo)[3] <- paste("rang",mot,sep = "_")
  return(tempo)
}
set.seed(1234)
# 1)Importance variable pour le modele logistique
mod_log <- glm(Y~.,data=don,family="binomial")
varimplog <- variable_imp(x=mod_log,t=1,mot="log")

# 2)Importance variable pour le modele ridge
tmp <- cv.glmnet(XX,YY,alpha=0,family="binomial")
mod_ridge  <- glmnet(XX,YY,alpha=0,lambda=tmp$lambda.min, family="binomial")
varimpridge <- variable_imp(x=abs(mod_ridge$beta),t=2,mot="ridge")

# 3)Importance variable pour le modele lasso (colnames(XX)[mod_lasso$beta@i])
tmp <- cv.glmnet(XX,YY, alpha=1, family="binomial")
mod_lasso <- glmnet(XX,YY,alpha=1, lambda =tmp$lambda.1se,family="binomial" )
varimplasso <- variable_imp(x=abs(mod_lasso$beta),t=2, mot="lasso")

# 4)Importance variable pour le modele elastic
tmp <- cv.glmnet(XX,YY, alpha=0.5, family="binomial")
mod_elastic <- glmnet(XX,YY,alpha = 0.5, lambda = tmp$lambda.min, family="binomial")
varimpelastic <- variable_imp(abs(mod_elastic$beta),t=2,mot = "elastic")

# 5)Importance variable pour le modele Foret
mod_foret <- randomForest(Y~., data = don, ntree=500)
varimpforet <- variable_imp(mod_foret,t=1,mot="foret")

# 6)Importance variable pour le modele adaboost
tmp <- gbm(as.numeric(Y)-1~.,data = don, distribution = "adaboost", interaction.depth = 2,
           shrinkage = 0.1,n.trees = 500)
M <- gbm.perf(tmp)[1]
mod_adaboost <- gbm(as.numeric(Y)-1~.,data = don, distribution = "adaboost", interaction.depth = 2,
           shrinkage = 0.1,n.trees = M)
varimpada <- variable_imp(summary(mod_adaboost),t=3, mot="adaboost")

# 7)Importance variable pour le modele logiboost
tmp <- gbm(as.numeric(Y)-1~.,data=don, distribution="bernoulli", interaction.depth = 2,
           shrinkage=0.1,n.trees=500)
M <- gbm.perf(tmp)[1]
mod_logiboost <- gbm(as.numeric(Y)-1~.,data=don, distribution="bernoulli", interaction.depth = 2,
           shrinkage=0.1,n.trees=M)
varimplogibo <- variable_imp(summary(mod_logiboost),t=3,mot="logiboost")

# ?)Importance variable pour le modele SVM (je ne sais pas appliquer la feature selection)
#mod_svm <- svm(Y~.,data=don, kernel="linear",probability=T)
#tmp <- tune(svm,Y~.,data=don, kernel="linear",probability=T,range=list(cost=c(0.1,1,10)))
#mod <- tmp$best.model

# Croisement des tables d'importance des variables
choix_var <- varimplog %>%
  full_join(varimpridge) %>%
  full_join(varimplasso) %>%
  full_join(varimpelastic) %>%
  full_join(varimpforet) %>%
  full_join(varimpada) %>%
  full_join(varimplogibo)

choix_var <- cbind(choix_var[,1],choix_var[,c(which(grepl("^imp",names(choix_var))))])

choix_var <- as.data.frame(choix_var)
names(choix_var)[1] <- "variable"
write.csv2(choix_var,"data/choix_var.csv",row.names = FALSE)
