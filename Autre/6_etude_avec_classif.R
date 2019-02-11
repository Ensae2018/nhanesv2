################################
#Etude classification###########
################################

# Chargement du jeu de donnée
don <- read.csv("data/nhanes_hyper_mice.csv", row.names = 1)
don <- subset(don,select= -c(Total_number_of_people_in_the_Family, Annual_family_income,
                             Systolic_Blood_pres_2nd_rdg_mm_Hg, Diastolic_Blood_pres_2nd_rdg_mm_Hg,
                             Systolic_Blood_pres_3rd_rdg_mm_Hg, Diastolic_Blood_pres_3rd_rdg_mm_Hg,
                             Money_spent_on_nonfood_items, Money_spent_on_food_at_other_stores,
                             Money_spent_on_eating_out, Money_spent_on_carryout_delivered_foods,
                             Moderate_work_activity, Walk_or_bicycle, Vigorous_recreational_activities,
                             Moderate_recreational_activities, Minutes_sedentary_activity
))

# trouver les variables quali
ind <- which(sapply(colnames(don), function(x) class(don[,x]))=="factor")

# table qu'avec les variables quali
don[,ind[-17]]

# table avec les variables quantis
don[,-ind]

# librairie utile pour construire la table disjonctive
library(ade4)

#ponderation par la frequence des indicatrices
PF <- function(x){
  m <- mean(x)
  return(x/sqrt(m)) # choix sqrt(m) permet d'accorder une importance aux valeurs rares
}

# On transforme la table avec les variables quali en tableau disjonctif et on pondere par la frequence
don_disj <- sapply(acm.disjonctif(don[,ind]),PF)

# On réunit la table des variable quanti centrés réduits et le tableau disjonctif
donb <- cbind(scale(don[,-ind]),scale(don_disj))

donb <- don[,which(colnames(don)=="Energy_kcal"):which(colnames(don)=="Moisture_gm")]

# ACP
library(FactoMineR)
acp <- PCA(donb,scale.unit=T,graph = F)
barplot(acp$eig[,2])
plot(acp, choix = "ind", cex=0.5)
plot(acp, choix = "var", cex=0.5)
score()
acp$eig
acp$var
library(factoextra)
get_eigenvalue(acp)
fviz_eig(acp,addlabels = T)
var <- get_pca_var(acp)
fviz_pca_var(acp, col.var = "cos2", gradient.cols = c("black", "Blue", "red"),
             repel = TRUE ,alpha.var="cos2")
var$cos2
library("corrplot")
corrplot(var$cos2, is.corr = F)
fviz_cos2(acp, choice = "var", axes = 1:2)
corrplot(var$contrib, is.corr = F)
fviz_contrib(acp, choice = "var", axes = 1, top = 10)
fviz_contrib(acp, choice = "var", axes = 1:2, top = 10)
fviz_pca_var(acp, col.var = "contrib", gradient.cols = c("black", "Blue", "red"),
             repel = TRUE ,alpha.var="cos2")
var
partition <- 1:15
for (i in 1:15){
  kmk <- kmeans(var$coord,centers = i, nstart = 10)
  partition[i]= kmk$tot.withinss/kmk$totss*100
}
plot(partition, type="h")

km <- kmeans(var$coord, center=4, nstart = 25)
grp <- as.factor(km$cluster)

fviz_pca_var(acp, col.var = grp, palette = c("black", "Blue", "red", "orange"),
             repel = TRUE ,alpha.var="cos2")


# On applique l'ACP avec le critère de distance Ward
cah <- hclust(dist(scale(donb)),method = "ward.D2")
plot(as.dendrogram(cah))
rect.hclust(cah,h=140)
cutree(cah, h=140)

# Methode de Kmeans
partition <- 1:30
km <- kmeans
for (i in 1:30){
  kmk <- kmeans()
  kmki]
}
names(km)
