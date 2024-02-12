---
title: "Projet Dataminig"
author: "Wilfried"
date: "02/06/2020"
output: rmdformats::readthedown
rmarkdown::html_document:
    theme: journal
    highlight: github
---


```{r include=FALSE}
require(readr)
require(dplyr)
require(questionr)
require(base)
require(vcd)
require(rpart)
require(TH.data)
require(tidyr)
require(tidyverse)
require(MASS)
require(partykit)
require(rattle)
require(rpart.plot)
require(RColorBrewer)
require(lubridate)
require(broom)
require(GGally)
require(forestmodel)
require(stringr)
require(readxl)
require(ggplot2)
require(kableExtra)
require(formattable)
require(rmdformats)
```


```{r include=FALSE}
Data2 <- Base <- read_delim("C:/Users/Wilfried/Downloads/Base.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)
```

```{r include=FALSE}
Data2$DATEDEBUTADHESION<-as.Date(Data2$DATEDEBUTADHESION,"%d/%m/%Y %H:%M")
Data2$DATEFINADHESION <- as.Date(Data2$DATEFINADHESION,"%d/%m/%Y %H:%M")
Data2$DATENAISSANCE <- as.Date(Data2$DATENAISSANCE,"%d/%m/%Y %H:%M")
Data2$DATEREADHESION <- as.Date(Data2$DATEREADHESION,"%d/%m/%Y %H:%M")
```

```{r include=FALSE}
Data_me <-Data2 %>% group_by(IDCLIENT) %>% mutate( Rentabilit =case_when(CA_2016_S1+CA_2016_S2>= 400 ~1,
                                                                                        CA_2016_S1+CA_2016_S2 < 400 ~ 0),REGION=str_sub(CODEINSEE))
```

```{r include=FALSE}
Data_me$VIP <- as.factor(Data_me$VIP)
Data_me$Rentabilit<- as.factor(Data_me$Rentabilit)
```


# Partie Exploration des donnée:

 Nous avons choisi la problématique suivant : quel est le profil potentiellement le plus rentable et qui peut générer plus de 400 euro.

 Ce sujet pourrait aider le departement marketing à mieux cibler sa clientèle et gérer le budget de ses campagnes marketing en réalisant une segmentation par catégorie la plus rentable,  choisir le bon moment pendant lequel un profil potentiel génera un max de chiffre d'affaire oubien définir pour chaque année la saisonnalité de promotion,recrutement.
 Notre base de donnée contient 157 variables. Nous allons choisir notre variable cible « Rentabilit= CA_2016_S1+CA_2016_S2>= 400 ~ 1 ».
 Afin de construire le modèle, bien évidement nous n’allons pas intégrer les  157 variables.
Nous commençons alors à distinguer les variables les plus significatives et influençant notre variable cible.
Nous avons : 25 variables qualitatives, 114 variables continues et 18 variables discrètes.
 Afin de trouver  les variables signficatives, nous allons  analyser et interpreter les données par trimestre et chercher des corrélations
```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=15 , fig.height=10}
ggpairs(Data_me[, c("prix_ticket_moy_2016","CA_2016_T1","CA_2016_T2","CA_2016_T3","CA_2016_T4","CA_2016","CA_2017","Rentabilit")], aes(colour= Rentabilit))
```
 On remarque que le comportement de la visualisation du **Prix moyen tickect** VS **Chiffre d'affaire en trimestre 2**  ressemble à celui du **Prix moyen tickect** VS **Chiffre d'affaire 2016  global**.
 
Le comportement du client en trimestre 2 a t- il une certaine influence ? 
*1ère reflexion*: garder le prix moyen des tickets 
 On va creuser dans les tickets et analyser la relation entre nombre de tickets et la rentabilité par trimestre :
```{r}
qinf<- quantile(Data_me$nbtic_2016_T1,0.01)
qsup<- quantile(Data_me$nbtic_2016_T1,0.99)
Data_tic <- Data_me %>% filter(nbtic_2016_T1 >= qinf & nbtic_2016_T1<=qsup) 
```
```{r echo=FALSE}
ggplot(Data_tic,aes(Rentabilit,nbtic_2016_T1,colour=Rentabilit))+
        geom_jitter(width=0.25)+
        geom_boxplot(alpha=0.5, outlier.shape=NA)+  
        xlab(label = "Rentabilité") +
        ylab(label = " Nombre ticket Trimestre 1") +
        theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
        theme(legend.position="none")+
       scale_fill_brewer(palette="Blues")+
        theme_classic()+
        ggtitle("Boxplot avec les observations")
```
```{r echo=FALSE}
qinf<- quantile(Data_me$nbtic_2016_T2,0.01)
qsup<- quantile(Data_me$nbtic_2016_T2,0.99)
Data_tic2 <- Data_me %>% filter(nbtic_2016_T2 >= qinf & nbtic_2016_T2<=qsup) 
ggplot(Data_tic2,aes(Rentabilit,nbtic_2016_T2,colour=Rentabilit))+
        geom_jitter(width=0.25)+
        geom_boxplot(alpha=0.5, outlier.shape=NA)+  
        xlab(label = "Rentabilité") +
        ylab(label = " Nombre ticket Trimestre 2") +
        theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
        theme(legend.position="none")+
       scale_fill_brewer(palette="Blues")+
        theme_classic()+
        ggtitle("Boxplot avec les observations")
```
```{r echo=FALSE}
qinf<- quantile(Data_me$nbtic_2016_T3,0.01)
qsup<- quantile(Data_me$nbtic_2016_T3,0.99)
Data_tic3 <- Data_me %>% filter(nbtic_2016_T3 >= qinf & nbtic_2016_T3<=qsup) 
ggplot(Data_tic3,aes(Rentabilit,nbtic_2016_T3,colour=Rentabilit))+
        geom_jitter(width=0.25)+
        geom_boxplot(alpha=0.5, outlier.shape=NA)+  
        xlab(label = "Rentabilité") +
        ylab(label = " Nombre ticket Trimestre 3") +
        theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
        theme(legend.position="none")+
       scale_fill_brewer(palette="Blues")+
        theme_classic()+
        ggtitle("Boxplot avec les observations")
```
```{r echo=FALSE}
qinf<- quantile(Data_me$nbtic_2016_T4,0.01)
qsup<- quantile(Data_me$nbtic_2016_T4,0.99)
Data_tic4 <- Data_me %>% filter(nbtic_2016_T4 >= qinf & nbtic_2016_T4<=qsup) 
ggplot(Data_tic4,aes(Rentabilit,nbtic_2016_T4,colour=Rentabilit))+
        geom_jitter(width=0.25)+
        geom_boxplot(alpha=0.5, outlier.shape=NA)+  
        xlab(label = "Rentabilité") +
        ylab(label = " Nombre ticket Trimestre 4") +
        theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
        theme(legend.position="none")+
       scale_fill_brewer(palette="Blues")+
        theme_classic()+
        ggtitle("Boxplot avec les observations")
```
 Premier constat après l'analyse de ces boxplot à travers ces 4 trimestres (T1,T2,T3,T4) est une importante difference à la fois au niveau de la mediane du nombre de tickect mais également de la dispersion des deux groupe.
 En effet on peut constater que la répartition du nombre de ticket chez les individus dit *rentable* varie **entre 1 et plus de 4 tickects** à l'inverse les individus dit *non rentable* dépasse assez rarement les **1,5 tickets** par trimestre.
 
 On pourrait donc fairre l'hypothèse ici, que la variable *nombre tickect* à une importance dans la rentabilité ou non d'un individus.
 On va examiner ensuite la relation  des variables Civilité, Date debut d’adhesion et VIP
```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=15 , fig.height=10}
ggpairs(Data_me[, c("CIVILITE2","DATEDEBADH_mois", "CA_2016_T1","CA_2016_T2","CA_2016_T3","CA_2016_T4","Rentabilit")], aes(colour= Rentabilit ))
```
  Les femmes génèrent un CA > 400 plus que les hommes dans tous les trimestres >> on va garder la variable CIVILITE2
  
  Les adhérants durant la fin de trimestre 1 et 2 semnle concorder avec notre problématique au vue du  rythme d'achat durant l'année. 
  
  A l'inverse, les profiles adherant entre les trimestres 3 et 4 réalisent une rentabilité vers la fin de l'année. On pourrait supposer que ces clients sont essentiellement des saisonniers.
  
  Dans le cadre de notre étude on va conserver les variables liés à l'adhésion des clients.

## Relation de la variable: VIP
```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=15 , fig.height=10}
ggpairs(Data_me[, c("VIP", "CA_2016_T1","CA_2016_T2","CA_2016_T3","CA_2016_T4","Rentabilit")], aes(colour= Rentabilit ))
```

  Pour cette variable *VIP*, il faut la croiser avec d'autres informations de l'entreprise pour savoir sur quel critère, ce statut à été attribué :

- Volume d'achat 
- Frequence d'achat
- Valeur d'achat
- Date d'adhesion.

  Ici, selon le graphe , il y a par exemple des "NON VIP" qui sont rentable selon notre condition et cette population est presque égale en terme de rentabilité à ceux considéré VIP. Par contre les VIP génèrent un max de chiffre d'affaire pendant la trimestre 4 de 2016.
```{r echo=FALSE}
qinf<- quantile(Data_me$CA_2016,0.01)
qsup<- quantile(Data_me$CA_2016,0.99)
Data_CA <- Data_me %>% filter(CA_2016 >= qinf & CA_2016<=qsup) 
ggplot(Data_CA,aes(VIP,CA_2016,colour=VIP))+
        geom_jitter(width=0.25)+
        geom_boxplot(alpha=0.5, outlier.shape=NA)+  
        xlab(label = "Rentabilité") +
        ylab(label = " Nombre ticket Trimestre 1") +
        theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
        theme(legend.position="none")+
       scale_fill_brewer(palette="Blues")+
        theme_classic()+
        ggtitle("Boxplot avec les observations")
```
## Relation avec l'age :
```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=15 , fig.height=10}
ggpairs(Data_me[, c("age_QL","Rentabilit")], aes(colour= Rentabilit ))
```
 La valeur du test de correlation nous confirme bien que les variables age et rentabilité sont corrélés.

 De plus selon on coonstate à l'aide du graphe que la tranche d'age [50-70] ans génére plus de 400 euro de chiffre d'affaire en général.
## Relation avec localité (MAGASIN,PAYS et Région)
```{r include=FALSE}
Repart_magain <- Data_me %>% group_by(MAGASIN) %>% summarise(nb_client = n_distinct(IDCLIENT),nb_VIP = (table(VIP==1,exclude = FALSE)),nb_rentable= (table(Rentabilit==1,exclude = FALSE)),nb_rentable_vip =(table(VIP==1,Rentabilit==1,exclude = FALSE)))
Repart_magain$nb_rentable_vip <- as.numeric(Repart_magain$nb_rentable_vip)
Repart_magain$nb_VIP <- as.numeric(Repart_magain$nb_VIP)
Repart_magain <-Repart_magain %>% group_by(MAGASIN) %>% mutate(percent = round((nb_rentable_vip/nb_VIP)*100)) %>% arrange(desc(percent))
```
```{r echo=FALSE, fig.width=15 , fig.height=10}
Repart_magain[,1:6]%>%kable(escape = F,align = "r") %>%
  row_spec(2:4, bold = T, color = "white", background = "#D7261E")%>% kable_styling(bootstrap_options = c("striped", "hover","condensed","responsive"),full_width = F,position = "c",font_size = 11 )%>% row_spec(0, bold = T, color = "white", background = "grey")  %>% kable_styling(c("striped", "bordered")) %>% scroll_box(width = "800px", height = "300px") 
```
Comme on peut le voir les MAGASIN *CLI* , *VIV* et *GEX* ont une proportion(*percent*) de VIP rentable assez élevé au alentour de 84-88%.
C'est un constat pour le moment mais on pourrai supposer que la proportion de VIP rentable joue un rôle plus ou moins important dans notre modèle.

```{r include=FALSE}
Repart_pays <- Data_me %>% group_by(PAYS) %>% summarise(nb_client = n_distinct(IDCLIENT),nb_VIP = (table(VIP==1,exclude = FALSE)),nb_rentable= (table(Rentabilit==1,exclude = FALSE)),nb_rentable_vip =sum(table(VIP==1,Rentabilit==1,exclude = FALSE)))

Repart_pays$nb_rentable_vip <- as.numeric(Repart_pays$nb_rentable_vip)
Repart_pays$nb_VIP <- as.numeric(Repart_pays$nb_VIP)

Repart_pays <-Repart_pays %>% group_by(PAYS) %>% mutate(percent = round(sum((nb_rentable_vip/nb_VIP))*100)) %>% arrange(desc(percent))

```

```{r echo=FALSE}
Repart_pays[,1:6]%>%kable(escape = F,align = "r") %>%
  row_spec(2:3, bold = T, color = "white", background = "#D7261E")%>% kable_styling(bootstrap_options = c("striped", "hover","condensed","responsive"),full_width = F,position = "c",font_size = 11                         )%>% row_spec(0, bold = T, color = "white", background = "grey")   %>% kable_styling(c("striped", "bordered"))  %>% scroll_box(width = "800px", height = "300px")                    
```

On fait quasiment le même constat ici concernant la variable VIP et rentabilité à travers une répartition par pays , cependant notre échantillon est accés essentiellement autour de la FRANCE. Il aurait été préférable d'avoir plus de données afin d'évaluer l'évolution et la répartion de ces metrics en Suisse(*CHE*).
```{r include=FALSE}
Repart_region <- Data_me %>% group_by(REGION=str_sub(CODEINSEE,1,2)) %>% summarise(nb_client = n_distinct(IDCLIENT),nb_VIP = (table(VIP==1,exclude = FALSE)),nb_rentable= (table(Rentabilit==1,exclude = FALSE)),nb_rentable_vip =sum(table(VIP==1,Rentabilit==1,exclude = FALSE)))
Repart_region$nb_rentable_vip <- as.numeric(Repart_region$nb_rentable_vip)
Repart_region$nb_VIP <- as.numeric(Repart_region$nb_VIP)
Repart_region <-Repart_region %>% group_by(REGION) %>% mutate(percent = round(sum((nb_rentable_vip/nb_VIP))*100)) %>% arrange(desc(nb_rentable_vip))
```
```{r echo=FALSE}
Repart_region[,1:6]%>%kable(escape = F,align = "r") %>%
  row_spec(1:6, bold = T, color = "white", background = "#D7261E")%>% kable_styling(bootstrap_options = c("striped", "hover","condensed","responsive"),full_width = F,position = "c",font_size = 11) %>% row_spec(0, bold = T, color = "white", background = "grey")  %>% kable_styling(c("striped", "bordered")) %>%
  kable_styling()%>% scroll_box(width = "800px", height = "300px")
```
  Conclusion du travail d'exploration:

  Nous allons garder pour notre modèle les variables suivantes :

-   CIVILITE2
-   toutes les variables en relation avec l'adhesion(DATEDEBADH_mois, DATEREADH_mois DATEDEBUTADHESION,DATEREADHESION,DATEFINADHESION)
-   AGE
-   VIP,
-   MAGASIN
-   PAYS
-   Moy_ticket
-   nombre ticket
-   Région
Afin de confirmer nos hupothèses , nous allons effectuer une régression logistique(modèle stepwise)
# Regression logistique méthode stepwise
## Construction data.set
```{r}
Test_data <- Data2 %>% group_by(IDCLIENT) %>% summarise(CIVILITE2,
                                 DATENAISSANCE, 
                                 Age=2017-year(DATENAISSANCE),
                                 DATEDEBUTADHESION=DATEDEBUTADHESION,
                                 DATEREADHESION=DATEREADHESION,
                                 DATEFINADHESION=DATEFINADHESION,
                                 Dureé_réadhésion= DATEFINADH_an - DATEREADH_an,
                                 Duree_adhesion=DATEREADH_an-DATEDEBADH_an,
                                 Region=str_sub(CODEINSEE,1,2),
                                 Ca_global= CA_2016_S1+CA_2016_S2,
                                 Rentabilit =case_when(CA_2016_S1+CA_2016_S2>= 400 ~1,
                                                                                        CA_2016_S1+CA_2016_S2 < 400 ~ 0),
                                 nb_tickets=nbtic_2016_S1+nbtic_2016_S2,
                                 VIP=VIP,
                                 MAGASIN=MAGASIN,
                                 PAYS=PAYS,
                                 Moy_ticket=round((CA_2016_S1+CA_2016_S2)/(nbtic_2016_S1+nbtic_2016_S2),2),
                                 age_disc10=age_disc10,
                                 nb_ticket = nbtic_2016, 
                                 nbtic_2016_T1=nbtic_2016_T1,
                                 nbtic_2016_T2=nbtic_2016_T2,
                                 nbtic_2016_T3=nbtic_2016_T3, 
                                 nbtic_2016_T4=nbtic_2016_T4,
                                 DATEDEBADH_mois=DATEDEBADH_mois, 
                                 DATEREADH_mois=DATEREADH_mois)
```
```{r include=FALSE}
Test_data$age_disc10 <- as.factor(Test_data$age_disc10)
Test_data$Region <- as.factor(Test_data$Region)
Test_data$CIVILITE2 <- as.factor((Test_data$CIVILITE2))
Test_data$MAGASIN <- as.factor(Test_data$MAGASIN)
Test_data$VIP <- as.factor(Test_data$VIP)
Test_data$PAYS <- as.factor(Test_data$PAYS)
Test_data$DATEDEBADH_mois <- as.factor(Test_data$DATEDEBADH_mois)
Test_data$DATEREADH_mois <- as.factor((Test_data$DATEREADH_mois))
Test_data$Rentabilit <- as.factor(Test_data$Rentabilit)
```
## Data.set pour la régression
```{r}
Test_data2 <-  Test_data %>% dplyr::select(IDCLIENT,CIVILITE2,
                                           Age,
                                           Dureé_réadhésion,
                                           Duree_adhesion,
                                           Region,MAGASIN,
                                           Rentabilit,
                                           VIP,
                                           PAYS,
                                           Moy_ticket,
                                           nb_ticket,
                                           age_disc10,
                                           nb_ticket,
                                           nbtic_2016_T1,
                                           nbtic_2016_T2,
                                           nbtic_2016_T3,
                                           nbtic_2016_T4, 
                                           DATEDEBADH_mois, 
                                           DATEREADH_mois)
Test_data2 <- na.omit(Test_data)
```
  Afin d'optimiser le scoring, on a fait le choix de supprimer les valeurs manquantes à ce niveau dans notre jeu de donnée.


## Construction echantillon d'apprentissage et de test

 Pour la mise en place de notre dataset d'apprentissage, nous avons repartit l'echantillon en 75% train et donc 25% test.
```{r}
set.seed(200)
nb_lignes <- floor((nrow(Test_data2)*0.75)) #Nombre de lignes de l’échantillon d’apprentissage : 75% du dataset
Add_lignes <- Test_data2[sample(nrow(Test_data2)), ] #Ajout de numéros de lignes
Data.train <- Add_lignes[1:nb_lignes, ] #Echantillon d’apprentissage
Data.test <- Add_lignes[(nb_lignes+1):nrow(Add_lignes), ] #Echantillon de test
```



```{r include=FALSE}
# modèle trivial réduit à la constante
str_constant <- "~ 1"
# modèle complet incluant toutes les explicatives potentielles
str_all <- "~CIVILITE2+Age+MAGASIN+VIP+PAYS+Region+age_disc10+nb_ticket+nbtic_2016_T1+nbtic_2016_T2+nbtic_2016_T3+nbtic_2016_T4+DATEDEBADH_mois+DATEREADH_mois"
```

## Affichage score final
```{r include=FALSE}
modele <- glm(Rentabilit~1,data=Data.train,family=binomial)
modele.stepwise <- stepAIC(modele, scope = list(lower = str_constant, upper = str_all), trace = TRUE, data = appren, direction = "both")
```

```{r echo=FALSE}
summary(modele.stepwise)
```
A la suite du modèle stepwise, on a obtenue le résultat suivant pour l'AIC le plus faible (5641.9):
  **Rentabilit ~ nb_ticket + VIP + nbtic_2016_T1 + age_disc10 + MAGASIN + nbtic_2016_T4 + PAYS + CIVILITE2**
  Il semble être le modéle le plus performant.
  Premier constat par rapport à notre première partie exploration, les metrics:
  -   CIVILITE2
  -   AGE
  -   VIP,
  -   MAGASIN
  -   PAYS
  -   nombre ticket
  Elles ont une influence dans la modélisation de notre variable y~Rentabilité.
## Ods Ratio
```{r include=FALSE}
odds.ratio(modele.stepwise)
```
```{r include=FALSE}
tmp <- tidy(modele.stepwise, conf.int = TRUE, exponentiate = TRUE)
```
```{r echo=FALSE, warning=TRUE}
knitr::kable(tmp)
```
# Construction de l’arbre de décision
```{r echo=FALSE}
Reg_tree <- rpart(Rentabilit~  VIP + age_disc10 + PAYS+ 
    nbtic_2016_T1 + nbtic_2016_T4 ,data=Data.train, control = rpart.control(minsplit = 15,cp=0.003),parms = list(split = "gini"),method = "class")
```
## Choix cp
```{r echo=FALSE}
printcp(Reg_tree)
plotcp(Reg_tree)
```
## Arbre de décision
```{r echo=FALSE}
library(RColorBrewer)
library(rattle)
fancyRpartPlot(Reg_tree, caption = NULL)
```
```{r echo=FALSE}
plot(Reg_tree, uniform= TRUE, branch =  0.5, margin= 0.1 )
text(Reg_tree, all = FALSE, use.n = TRUE)
```
## Prédiction
```{r}
Reg_test_predict<-predict(Reg_tree,newdata=Data.test,type="class")
summary(Reg_test_predict)
```
## Matrice de confusion
```{r echo=FALSE}
mc<-table(Data.test$Rentabilit,Reg_test_predict)
print(mc)
```
## Erreur de classement
```{r echo=FALSE}
erreur.classement<-1.0-(mc[1,1]+mc[2,2])/sum(mc)
print(erreur.classement)
```
## Taux de prédiction
```{r echo=FALSE}
prediction=mc[2,2]/sum(mc[2,])
print(prediction)
```
## Qualité du modèle
```{r echo=FALSE}
Qualit=(mc[1,1]+mc[2,2])/sum(mc)
print(Qualit)
```
