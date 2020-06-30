########################################################################
############# Projet DataMining                                        #
#############                                                          #
############ Realise par :                                             #
############           Danis JIOGUE                                    #
############                Khariratou DIALLO                          #
#                                                                      #
#____________________________janvier 2020______________________________#
########################################################################


#Packages & Library 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("fBasics")
install.packages("outliers")
install.packages("questionr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("randomForest")
install.packages("adabag")
install.packages("nnet")
install.packages("caret")
install.packages("pROC")

library(dplyr) #Library pour explorer les bases de données
library(ggplot2) #Pour de jolis rendus graphiques
library(outliers) #Pour tester les valeurs aberrantes (test de grubbs)
library(fBasics) #Contient la fonction de test de normalit? telque dagotTest
library(ggpubr)# Comporte la fonction ggscatter pour le scatter plot
library(MASS) #pour les fonction AIC, BIC, StepWise
library(rpart) #Arbre de décision
library(rpart.plot) #Plot Arbre de décision
library(e1071) #Pour le SVM
library(randomForest) #Pour le randomForest
library(adabag) #Pour le bagging
library(nnet) #Pour le r?seau de neurone perceptron
library(pROC) # Pour tracer l'AUC (Area Under Cuve) de ROC
library(caret)# Comprend la fonction confusionMatrix qui nous aidera ? recuperer les indicateurs

setwd('D:\\Danis_ITS4\\Data_Mining\\2019-2020\\Projet_DataMining\\Projet (1)\\Projet')
donnee = read.csv("heart.csv",sep = ",",dec = ".",header = TRUE)
glimpse(donnee)
summary(donnee$ï..age)
summary(donnee)
##_______________Analyse pr?liminaire____________________##
##Definition de la nature des varaibles
#renommee la var age qui est bizarre dans la base
colnames(donnee)[colnames(donnee) == 'ï..age'] <- 'age' 
#D?finition des var qualitatives
donnee$sex <- as.factor(donnee$sex)
donnee$cp <- as.factor(donnee$cp)
donnee$fbs <- as.factor(donnee$fbs)
donnee$exang <- as.factor(donnee$exang)
donnee$restecg <- as.factor(donnee$restecg)
donnee$slope <- as.factor(donnee$slope)
donnee$ca <- as.factor(donnee$ca)
donnee$thal <- as.factor(donnee$thal)
donnee$target <- as.factor(donnee$target)

summary(donnee)
glimpse(donnee)
##Analyse univariee
#****************Variables discrètes***********************
  #*Age
summary(donnee$age)
table(donnee$age)
hist(donnee$age)
ggplot(donnee,aes(age)) + 
  geom_histogram(aes(y=..density..),binwidth=5,colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666")+ 
  theme_bw() +
  scale_x_continuous("age des patients") + 
  scale_y_continuous("densit?")


ggplot(donnee,aes(x="",y=age))+geom_boxplot()+ 
  theme_bw() +
  scale_x_discrete("") + 
  scale_y_continuous("age des patients")
#test numérique de normalite
jarqueberaTest(donnee$age)

  #*pression artérielle au repos
summary(donnee$trestbps)
ggplot(donnee,aes(trestbps)) + 
  geom_histogram(aes(y=..density..),binwidth=10,colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666")+
  theme_bw() +
  scale_x_continuous("pression art?rielle au repos des patients") + 
  scale_y_continuous("densit?")
table(donnee$trestbps)
ggplot(donnee,aes(x="",y=trestbps))+
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1) + 
  theme_bw() +
  scale_x_discrete("") + 
  scale_y_continuous("pression artérielle au repos des patients")

pression = boxplot(donnee$trestbps, range = 2)
table(pression$out) #Afficher les outliers de pressions
#test numérique
jarqueberaTest(donnee$trestbps)
#Methode de grubbs
grubbs.test(donnee$trestbps, opposite = FALSE) #Valeur maximale
pression = donnee$trestbps
while (grubbs.test(pression)$p.value<0.05) {
  pression <- pression[pression!=max(pression,na.rm=T)]
}
pression_ab <- setdiff(donnee$trestbps,pression)
print(pression_ab)

#* cholestoral sérique en mg / dl
summary(donnee$chol)
hist(donnee$chol)
boxplot(donnee$chol)$out
table(boxplot(donnee$chol)$out)
ggplot(donnee,aes(chol)) + 
  geom_histogram(aes(y=..density..),binwidth=20,colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666")+
  theme_bw() +
  scale_x_continuous("cholestoral s?rique en mg / dl des patients") + 
  scale_y_continuous("densit?")

ggplot(donnee,aes(x="",y=chol))+
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1) + 
  theme_bw() +
  scale_x_discrete("") + 
  scale_y_continuous("cholestoral sérique en mg / dl des patients")
#test numérique de normalite
jarqueberaTest(donnee$chol)
#Methode de grubbs
grubbs.test(donnee$chol,opposite = FALSE) #Tester le max
grubbs.test(donnee$chol,opposite = TRUE) #tester le min
#*** function de test
chol_max = donnee$chol
while (grubbs.test(chol_max)$p.value<0.05) {
  chol_max <- chol_max[chol_max!=max(chol_max,na.rm=T)]
}
max_out <- setdiff(donnee$chol,chol_max)
print(max_out)

chol_min = donnee$chol
while (grubbs.test(chol_min, opposite = TRUE)$p.value<0.05) {
  chol_min <- chol_min[chol_min!=chol_min(chol_min,na.rm=T)]
}
min_out <- setdiff(donnee$chol,chol_min)
print(min_out)

  #* fréquence cardiaque maximale atteinte
summary(donnee$thalach)
hist(donnee$thalach)
boxplot(donnee$thalach)$out
table(donnee$thalach)
ggplot(donnee,aes(thalach)) + 
  geom_histogram(aes(y=..density..),binwidth=15,colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666")+
  theme_bw() +
  scale_x_continuous("fr?quence cardiaque maximale atteinte des patients") + 
  scale_y_continuous("densit?")

ggplot(donnee,aes(x="",y=thalach))+
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1) + 
              theme_bw() +
  scale_x_discrete("") + 
  scale_y_continuous("fréquence cardiaque maximale atteinte des patients")
#test numérique
jarqueberaTest(donnee$thalach)
#Test de grubbs
#Comme on a une seule valeur aberrante selon boxplot la fonction de test n'est pas necessaire
grubbs.test(donnee$thalach,opposite = FALSE)
grubbs.test(donnee$thalach,opposite = TRUE)


  #*) depression ST induite par l'exercice par rapport au repos
summary(donnee$oldpeak)
hist(donnee$oldpeak)
boxplot(donnee$oldpeak)$out

ggplot(donnee,aes(oldpeak)) + 
  geom_histogram(aes(y=..density..),binwidth=0.5,colour="black", fill="white") +
  geom_density(alpha=.5, fill="#FF6666")+
  theme_bw() +
  scale_x_continuous("D?pression des patients au repos") + 
  scale_y_continuous("densit?")

ggplot(donnee,aes(x="",y=oldpeak))+
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1) + 
  theme_bw() +
  scale_x_discrete("") + 
  scale_y_continuous("Dépression des patients au repos")
#test numérique normalite
jarqueberaTest(donnee$trestbps)
##Test de grubbs
grubbs.test(donnee$oldpeak,opposite = FALSE)
oldpeak = donnee$oldpeak
while (grubbs.test(oldpeak)$p.value<0.05) {
  oldpeak <- oldpeak[oldpeak!=max(oldpeak,na.rm=T)]
}
max_out <- setdiff(donnee$oldpeak,oldpeak)
print(max_out)

oldpeak_min = donnee$oldpeak
while (grubbs.test(oldpeak_min, opposite = TRUE)$p.value<0.05) {
  oldpeak_min <- oldpeak_min[oldpeak_min!=min(oldpeak_min,na.rm=T)]
}
min_out <- setdiff(donnee$chol,chol_min)
print(min_out)
  #*) 
#****************Variables Qualitatives***********************
  #*Sexe
table(donnee$sex)
ggplot(donnee, aes(x=factor(1), fill=factor(sex)))+
  geom_bar()+
  coord_polar("y") +
  scale_fill_brewer(palette = "Set2", name = "Sexe des patients") +
  scale_y_discrete("Sexe des patients\n 1=homme, 0=Femme") + scale_x_discrete("")



  #*3) type de douleur thoracique
table(donnee$cp)
ggplot(donnee, aes(x=factor(cp))) +
         geom_bar(stat = "count", width = 0.3) +
         theme_classic() +
         scale_x_discrete("type de douleur thoracique") +
         coord_flip()
  #*6) glycémie à jeun> 120 mg / dl
table(donnee$fbs)
ggplot(donnee, aes(x=factor(1), fill=factor(fbs)))+
  geom_bar()+
  coord_polar("y") +
  scale_fill_brewer(palette = "Set2", name = "Glycemie > 120 mg/dl") 

  #*7) résultats électrocardiographiques au repos
table(donnee$restecg)
ggplot(donnee, aes(x=factor(1), fill=factor(restecg)))+
  geom_bar()+
  coord_polar("y") +
  scale_fill_brewer(palette = "Set2", name = "resultat électrocardiographique\n au repos")
 
 #*9) angine de poitrine induite par l'exercice
table(donnee$exang)
ggplot(donnee, aes(x=factor(1), fill=factor(exang)))+
  geom_bar()+
  coord_polar("y") +
  scale_fill_brewer(palette = "Set2", name = "angine de poitrine induite \n par l'exercice")
  #*11) pente du segment ST maximal de l'exercice
table(donnee$slope)
ggplot(donnee, aes(x=factor(1), fill=factor(slope)))+
  geom_bar()+
  coord_polar("y") +
  scale_fill_brewer( name = "pente du segment ST maximal\n de l'exercice")
  #*12) nombre de vaisseaux principaux (0-3) colorés par une fluoroscopie
table(donnee$ca)
table(donnee$ca)
ggplot(donnee, aes(x=factor(ca))) +
  geom_bar(stat = "count", width = 0.3) +
  theme_classic() +
  scale_x_discrete("type de douleur thoracique") +
  coord_flip()

  #*13) thal: 3 = normal; 6 = défaut fixe; 7 = défaut réversible
table(donnee$thal)

  #*14) target présence d'une maladie cardiaque
table(donnee$target)
ggplot(donnee, aes(x=factor(1), fill=factor(target)))+
  geom_bar()+
  coord_polar("y") +
  scale_fill_brewer(palette = "Set2", name = "Présence de la maladie cardiaque")


##Analyse bivariée
#****************quanti*quanti***********************

#* Age*pression artérielle au repos
summary(donnee$trestbps)
ggscatter(donnee, x = "age", y = "trestbps", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "?ges des patients", ylab = "Pression art?rielle des patients au repos")

#* Age*cholesterol sérique
summary(donnee$chol)
ggscatter(donnee, x = "age", y = "chol", 
          add 
          = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "?ges des patients", ylab = "Cholest?rol s?rique des patients")
#* Age*fréquence cardiaque maximale atteinte
summary(donnee$thalach)
ggscatter(donnee, x = "age", y = "thalach", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "?ges des patients", ylab = "fr?quence cardiaque maximale des patients")
#*)Age * dépression ST induite par l'exercice par rapport au repos
summary(donnee$oldpeak)
ggscatter(donnee, x = "age", y = "oldpeak", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "?ges des patients", ylab = "d?pression ST induite par l'exercice \n par rapport au repos")

#****************quali*quali***********************
#*sexe*type de douleur
table(donnee$sex,donnee$cp)
chisq.test(table(donnee$sex,donnee$cp))
cprop(table(donnee$sex,donnee$cp))
lprop(table(donnee$sex,donnee$cp))
mosaicplot(table(donnee$sex,donnee$cp), main="sexe par type de fréquence")
#*sexe*presence de la maladie cerdique
table(donnee$sex,donnee$target)
chisq.test(table(donnee$sex,donnee$target))
cprop(table(donnee$sex,donnee$cp))
lprop(table(donnee$sex,donnee$cp))
mosaicplot(table(donnee$sex,donnee$target), main="présence de la maladie cardiaque")
#*Presence de la maladie cardiaque*type douleur thoracique
table(donnee$target,donnee$cp)
chisq.test(table(donnee$target,donnee$cp))
#*Presence maladie* glycémie à jeun> 120 mg / dl
table(donnee$target,donnee$fbs)
chisq.test(table(donnee$target,donnee$fbs))
#*Presence maladie* résultats électrocardiographiques au repos (valeurs 0,1,2)
table(donnee$target,donnee$restecg)
chisq.test(table(donnee$target,donnee$restecg))
#*Presence maladie*Angine de poitrine induit par l'exercice
table(donnee$target, donnee$exang)
chisq.test(table(donnee$target, donnee$exang))
#*Presence maladie*Dépression ST
table(donnee$target,donnee$slope)
chisq.test(table(donnee$target,donnee$slope))
#*presence maladie*nombre de vaisseaux principaux (0-3) colorés par une fluoroscopie
table(donnee$target,donnee$ca)
chisq.test(table(donnee$target,donnee$ca))
#*presence maladie*thal
table(donnee$target,donnee$thal)
chisq.test(table(donnee$target,donnee$thal))


#****************quanti*quali***********************
#*age*sexe
ggplot(donnee,aes(x=factor(sex),y=age)) +
  geom_boxplot() +
  scale_x_discrete("Sexe des patients") +
  scale_y_continuous("age des patients")
boxplot(donnee$age~donnee$sex)
#* Age*cp
ggplot(donnee,aes(x=factor(cp),y=age)) +
  geom_boxplot() +
  scale_x_discrete("Type de douleur thoracique") +
  scale_y_continuous("age des patients")
#*Pression artérielle*age
ggplot(donnee,aes(x=target,y=age)) +
  geom_boxplot() +
  scale_x_discrete("Présence absence de maladie cardiaque") +
  scale_y_continuous("age des patients")
#*pression artérielle*target
donnee$trestbps
ggplot(donnee,aes(x=factor(target),y=trestbps)) +
  geom_boxplot() +
  scale_x_discrete("Présence absence de maladie cardiaque") +
  scale_y_continuous("pression artérielle au repos")
#*Cholesterol*target
donnee$trestbps
ggplot(donnee,aes(x=factor(target),y=chol)) +
  geom_boxplot() +
  scale_x_discrete("Présence absence de maladie cardiaque") +
  scale_y_continuous("cholestérol sérique")
#*frequence*target
donnee$thalach
ggplot(donnee,aes(x=factor(target),y=thalach)) +
  geom_boxplot() +
  scale_x_discrete("Présence absence de maladie cardiaque") +
  scale_y_continuous("fréquence cardiaque maximale atteinte")
#*depréssion ST*target
donnee$oldpeak
ggplot(donnee,aes(x=factor(target),y=oldpeak)) +
  geom_boxplot() +
  scale_x_discrete("Présence absence de maladie cardiaque") +
  scale_y_continuous("dépression ST induite par l'exercice \n par rapport au repos")

#****************Traitement des outliers***********************
#__________Imputation par la moyenne___________
#*pression Arterielle
#La valeur ab?rrente identifi? suivant grubs est 200
pres_out <- donnee$trestbps
unique(boxplot(pres_out)$out)
pres_out[pres_out == 200] <- mean(pres_out)
setdiff(donnee$trestbps,pres_out)
donnee$trestbps <- pres_out


#*Cholesterol serique
#La valeur ab?rrente identifi? suivant grubs est 564
chol_out <- donnee$chol
table(chol_out)
chol_out[chol_out == 564] <- mean(chol_out)
setdiff(donnee$chol, chol_out)
donnee$chol <- chol_out

#*frequence
# La valuer min est consid?r? coe outlier
freq_out <- donnee$thalach
freq_out[freq_out == min(donnee$thalach)] <- mean(freq_out)
setdiff(donnee$thalach, freq_out)
donnee$thalach <- freq_out

#*Depression
#Selon Grubbs on les valeurs max sont consid?r?s coe outlier
dep_out <- donnee$oldpeak
dep_out[dep_out > 5] <- mean(dep_out)
setdiff(donnee$oldpeak, dep_out)
donnee$oldpeak <- dep_out


###________________Sélection de variables_____________##
#*Définition des variables du modèle trivial et complet
triviale <- "~1"
complet <- "~age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal"

#*forward
attach(donnee)
modele_depart <- glm(target~1, data = donnee, family = binomial)
modele_forward <- stepAIC(modele_depart, scope = list(lower = triviale, upper = complet), trace = TRUE, data = donnee, direction = "forward")
summary(modele_forward)

#*backward
modele_depart <- glm(paste("target",complet), data = donnee, family = binomial)
modele_backward <- stepAIC(modele_depart, scope = list(lower = triviale, upper = complet), trace = TRUE, data = donnee, direction = "backward")
summary(modele_backward)

#*stepwise
modele_depart <- glm(target~1, data = donnee, family = binomial)
modele_both <- stepAIC(modele_depart, scope = list(lower = triviale, upper = complet), trace = TRUE, data = donnee, direction = "both")
summary(modele_both)


#_______________Technique d'apprentissage supervisée_________________#
# Decoupage de la base de donn?e
index <- sample(1:nrow(donnee),0.7*nrow(donnee),replace = F)

echan_apprend <- donnee[index,]
print(nrow(echan_apprend))

echan_test <- donnee[-index,]
print(nrow(echan_test))

#fonction de calcul du taux d'erreur
error_rate <- function(yobs,ypred){
  #matrice de confusion
  mc <- table(yobs,ypred)
  #taux d'erreur
  err <- 1.0 - sum(diag(mc))/sum(mc)
  return(err) 
}

#fonction de sensibilite
sens_fun <- function(yobs,ypred){
  #matrice de confusion
  mc <- table(yobs,ypred)
  #taux d'erreur
  sens = mc[2,2]/sum(mc[2,])
  return(sens) 
}

#fonction de sensibilite
spe_fun <- function(yobs,ypred){
  #matrice de confusion
  mc <- table(yobs,ypred)
  #taux d'erreur
  spe = mc[1,1]/sum(mc[1,])
  return(spe) 
}

set.seed(1)

#**************Regression logistique
regression <- glm(target~thal + ca + cp + oldpeak + slope + sex + exang + trestbps + chol, data = echan_apprend, family = binomial)
res <- summary(regression)
print(res)
pred_reg <- predict(regression,newdata=echan_test,type="response")
prediction <- as.factor(ifelse(pred_reg > 0.5,"present","absent"))
mc_reg <- table(echan_test$target,prediction)
print(mc_reg)
error_rate(echan_test$target,prediction)
#erreur
prediction1 <- as.factor(ifelse(pred_reg > 0.5,"1","0"))
confusionMatrix(echan_test$target,as.factor(prediction1))



#**************Arbre de decision
#Modele
parametre <- rpart.control(minsplit = 10, minbucket = 1)
arbre <- rpart(target ~ ., data = echan_apprend, control = parametre)
print(arbre)
#Post elagage
printcp(arbre)
#definition du modele elager
arbre_elager <- prune(arbre, cp = 0.02)
print(arbre_elager)
#Prediction
pred <- predict(arbre_elager, newdata = echan_test, type = "class")
#matrice de confusion

mc_arbre <- table(echan_test$target,pred)
print(mc_arbre)
err_rate_arbre <- 1.0 - sum(diag(mc_arbre))/sum(mc_arbre)
print(err_rate_arbre)
#erreur
print(error_rate(echan_test$target,pred))
#infos sur le modele
confusionMatrix(echan_test$target,pred)



#**************bagging
parametre <- rpart.control(minsplit = 5, minbucket = 1)
bag <- bagging(target ~ ., data = echan_apprend,control = parametre)
#pr?diction
predbag <- predict(bag,newdata = echan_test)
#matrice de confusion
mcbag <- table(echan_test$target,predbag$class)
print(mcbag)
#taux d'erreur
err_rate_bag <- 1.0 - sum(diag(mcbag))/sum(mcbag)
print(err_rate_bag)
print(error_rate(echan_test$target,predbag$class))
#resultat matrice de confusion
confusionMatrix(echan_test$target,as.factor(predbag$class))


#**************Random Forest
randomf <- randomForest(target ~ ., data = echan_apprend,ntree=1000)
#pr?diction
predrf <- predict(randomf,newdata=echan_test,type="class")
#matrice de confusion
mcrf <- table(echan_test$target,predrf)
print(mcrf)
#erreur
err_rate_rf <- 1.0 - sum(diag(mcrf))/sum(mcrf)
print(err_rate_rf)
print(error_rate(echan_test$target,predrf))
#resultat matrice de confusion
confusionMatrix(echan_test$target, predrf)


#**************SVM
svm_model <- svm(target~., data=echan_apprend, kernel="linear",scale=F)
#prediction
pred_svm <- predict(svm_model,echan_test,type = "class")
#matrice de confusion
mc_svm <- table(echan_test$target,pred_svm)
print(mc_svm)
#taux d'erreur 
err_rate_svm <- 1.0 - sum(diag(mc_svm))/sum(mc_svm)
print(err_rate_svm)
#Parm?tre de la matrice de confusion
confusionMatrix(echan_test$target,pred_svm)

#R?seau de neurone perceptron
neurone <- nnet(target~.,data=echan_apprend,size=5,decay=1,maxit=500)
attributes(neurone)
neurone$fitted.values
pred_neurone <- predict(neurone,echan_test)
attributes(pred_neurone)

prediction_neurone <- as.factor(ifelse(pred_neurone > 0.5,"present","absent"))

mc_neurone <- table(echan_test$target,prediction_neurone)
print(mc_neurone)
#taux d'erreur 
err_rate_neurone <- 1.0 - sum(diag(mc_neurone))/sum(mc_neurone)
err_rate_neurone
print(err_rate_neurone)error_rate(echan_test$target,prediction_neurone)


plot(tune.nnet(target~.,data=echan_apprend,size=c(2,3,4),decay=c(1,2,3),maxit=500))
plot(tune.nnet(target~.,data=echan_apprend,size=4:5,decay=1:10))

#erreur
prediction_neur <- as.factor(ifelse(pred_neurone > 0.5,"1","0"))
confusionMatrix(echan_test$target,as.factor(prediction_neur))


###________________Choix de la meilleur de pr?diction_____________##

#########_____Courbe de roc_____
#AUC Regression logistique
par(pty="s")
roc(echan_apprend$target, regression$fitted.values, print.auc=T, plot=TRUE, legacy.axes=TRUE,xlab="1 - specificite", ylab="Sensibilite", col="#377eb8", lwd=2)

#AUC de l'arbre de d?cision
pred_arbre_auc <- predict(arbre_elager, echan_test, type = 'prob')
auc <- auc(echan_test$target,pred_arbre_auc[,2])
plot(roc(echan_test$target, pred_arbre_auc[,2]), add = TRUE, col="red", lwd=2)

#AUC Bagging
pred_bag_auc <- predict(bag, echan_test, type = 'prob')
auc <- auc(echan_test$target,pred_bag_auc$prob[,2])
plot(roc(echan_test$target, pred_bag_auc$prob[,2]), add = TRUE, col="yellow", lwd=2)


#AUC du randomForest
plot.roc(echan_apprend$target,randomf$votes[,1], col="#4daf4a", lwd=2, add=TRUE)

#AUC de SVM
pre_1<-as.numeric(predict(svm_model,echan_test))
a1<-multiclass.roc(echan_test$target,pre_1)
attributes(a1)
a1$rocs
plot.roc(a1[["rocs"]][[1]], legacy.axes = TRUE,lwd = 4, col='gray',add = TRUE)

#ROC de r?seau de neurone
roc(echan_apprend$target, neurone$fitted.values, plot=TRUE, legacy.axes=TRUE,col="black", lwd=2, add = TRUE)
legend("bottomright", legend=c("Regression logistique", "Arbre de decision", "Bagging", "Random Forest", "SVM", "R?seau de neurone"), col=c("#377eb8", "red","yellow","#4daf4a", "gray","black"), lwd=4)


#*AUC de ROC
mat <- confusionMatrix(data=pred,reference=spamTest$spam,positive="yes")
#*k de Coehn

#*Taux d'erreur
taux_reg = print(error_rate(echan_test$target,prediction))
taux_reg = print(error_rate(echan_test$target,pred))
#*Sensibilit?
print(sens_fun(echan_test$target,prediction))
#*Sp?cificit?
print(spe_fun(echan_test$target,prediction))

