
library(dplyr)
data<-read.table(file=file.choose(),header=T,sep=",")

View(data)
summary(data)
str(data)
colSums(is.na(data))
table(data$ConfirmedIndianNational)

# Convertion de ConfirmedIndianNational et ConfirmedForeignNational en integer
columns_to_fix <- c("ConfirmedIndianNational", "ConfirmedForeignNational")
# Suppression des caracteres non numeriques et conversion
data[columns_to_fix] <- lapply(data[columns_to_fix], function(x) {
  as.numeric(gsub("[^0-9]", "", x))  # Supprime tout sauf les chiffres
})
str(data)

#Traitement des outliers 

#Fonction de Traitement des outliers 
handle_outliers <- function(data, column, Q1, Q3) {
  Vmin <- Q1 - 1.5 * (Q3 - Q1)
  Vmax <- Q3 + 1.5 * (Q3 - Q1)
  
  cat(column, "- Vmin:", Vmin, "\n")
  cat(column, "- Vmax:", Vmax, "\n")
  
  count_below_vmin <- sum(data[[column]] < Vmin, na.rm = TRUE)
  count_above_vmax <- sum(data[[column]] > Vmax, na.rm = TRUE)
  
  cat(column, "- nombre < Vmin:", count_below_vmin, "\n")
  cat(column, "- nombre > Vmax:", count_above_vmax, "\n")
  
  data[[column]] <- replace(data[[column]], data[[column]] > Vmax, NA)
  
  summary(data[[column]])
  boxplot(data[[column]], main = paste("Boxplot of", column))
  
  count_below_vmin <- sum(data[[column]] < Vmin, na.rm = TRUE)
  count_above_vmax <- sum(data[[column]] > Vmax, na.rm = TRUE)
  
  cat(column, "- nombre < Vmin:", count_below_vmin, "\n")
  cat(column, "- nombre > Vmax:", count_above_vmax, "\n")
  
  return(data)
}

# Cured
boxplot(data$Cured)
summary(data$Cured)
data <- handle_outliers(data, "Cured", Q1 = 3360, Q3 = 278870)

# Deaths
boxplot(data$Deaths)
summary(data$Deaths)
data <- handle_outliers(data, "Deaths", Q1 = 32, Q3 = 3644)

# Confirmed
boxplot(data$Confirmed)
summary(data$Confirmed)
data <- handle_outliers(data, "Confirmed", Q1 = 4377, Q3 = 300150)

# ConfirmedIndianNational
boxplot(data$ConfirmedIndianNational)
summary(data$ConfirmedIndianNational)
data <- handle_outliers(data, "ConfirmedIndianNational", Q1 = 1.0, Q3 = 13.0)

# ConfirmedForeignNational
boxplot(data$ConfirmedForeignNational)
summary(data$ConfirmedForeignNational)
data <- handle_outliers(data, "ConfirmedForeignNational", Q1 = 0.0, Q3 = 1.0)

#traitement des valeurs NA imputation avec KNN
library(VIM)
columns_to_impute <- c("ConfirmedIndianNational", "ConfirmedForeignNational","Cured","Deaths","Confirmed")
CovidIndia<-kNN(data, variable = columns_to_impute)
summary(CovidIndia)
str(CovidIndia)
# L'imputation par KNN a été choisie pour ces champs spécifiques car elle permet de :
# 1. Préserver les relations complexes et non linéaires entre les variables (par exemple, entre les décès, les cas actifs et confirmés).
# 2. Gérer les données avec des valeurs très petites et très grandes, ce que les méthodes simples comme la moyenne ou la médiane ne peuvent pas faire de manière adéquate.
# 3. Exploiter la taille importante du dataset, qui permet à KNN de trouver des voisins pertinents pour imputer les valeurs manquantes de manière réaliste.
# 4. Conserver la variance naturelle des données et éviter d'introduire un biais systématique comme le fait la moyenne/médiane.
# 5. Gérer efficacement les outliers sans les éliminer, contrairement aux méthodes comme la suppression des lignes ou les imputation par valeurs centrales.
# Ainsi, KNN offre une solution plus robuste et fidèle aux caractéristiques des données dans ce contexte.




# Normalization Function (Min-Max Scaling)
normalize_column <- function(data, column_name) {
  x_max <- max(data[[column_name]], na.rm = TRUE)
  x_min <- min(data[[column_name]], na.rm = TRUE)
  
  cat(column_name, "- x_max:", x_max, "\n")
  cat(column_name, "- x_min:", x_min, "\n")
  
  data[[column_name]] <- (data[[column_name]] - x_min) / (x_max - x_min)
  
  return(data)
}

# Example usage
CovidIndia <- normalize_column(CovidIndia, "ConfirmedIndianNational")
CovidIndia <- normalize_column(CovidIndia, "Cured")
CovidIndia <- normalize_column(CovidIndia, "Confirmed")
CovidIndia <- normalize_column(CovidIndia, "ConfirmedForeignNational")
CovidIndia <- normalize_column(CovidIndia, "Deaths")

# Display the structure of the data
str(CovidIndia)
summary(CovidIndia)
num_observations <- nrow(CovidIndia)
cat("Nombre d'observations:", num_observations, "\n")
#ona 18110 observations ==> on ne peut pas utiliser shapiro.test pour tester la normalité

#TESTER la normalité (methode graphique\statique)
library(ggplot2)
plot(density(CovidIndia$Confirmed, na.rm = TRUE), main = "Densité avec courbe normale", col = "blue")
curve(dnorm(x, mean = mean(CovidIndia$Confirmed, na.rm = TRUE), sd = sd(CovidIndia$Confirmed, na.rm = TRUE)), 
      col = "red", add = TRUE)
legend("topright", legend = c("Densité des données Confirmed", "Courbe normale"), col = c("blue", "red"), lty = 1)
#d'apres le graphe la variable Confirmed ne suit pas la loi normale 

plot(density(CovidIndia$ConfirmedForeignNational, na.rm = TRUE), main = "Densité avec courbe normale", col = "blue")
curve(dnorm(x, mean = mean(CovidIndia$ConfirmedForeignNational, na.rm = TRUE), sd = sd(CovidIndia$ConfirmedForeignNational, na.rm = TRUE)), 
      col = "red", add = TRUE)
legend("topright", legend = c("Densité des données ConfirmedForeignNational", "Courbe normale"), col = c("blue", "red"), lty = 1)
#d'apres le graphe la variable ConfirmedForeignNational ne suit pas la loi normale 

plot(density(CovidIndia$Cured, na.rm = TRUE), main = "Densité avec courbe normale", col = "blue")
curve(dnorm(x, mean = mean(CovidIndia$Cured, na.rm = TRUE), sd = sd(CovidIndia$Cured, na.rm = TRUE)), 
      col = "red", add = TRUE)
legend("topright", legend = c("Densité des données Cured", "Courbe normale"), col = c("blue", "red"), lty = 1)
#d'apres le graphe la variable Cured  ne suit pas la loi normale 

plot(density(CovidIndia$ConfirmedIndianNational, na.rm = TRUE), main = "Densité avec courbe normale", col = "blue")
curve(dnorm(x, mean = mean(CovidIndia$ConfirmedIndianNational, na.rm = TRUE), sd = sd(CovidIndia$ConfirmedIndianNational, na.rm = TRUE)), 
      col = "red", add = TRUE)
legend("topright", legend = c("Densité des données ConfirmedIndianNational", "Courbe normale"), col = c("blue", "red"), lty = 1)
#d'apres le graphe la variable ConfirmedIndiannNational ne suit pas la loi normale 

plot(density(CovidIndia$Deaths, na.rm = TRUE), main = "Densité avec courbe normale", col = "blue")
curve(dnorm(x, mean = mean(CovidIndia$Deaths, na.rm = TRUE), sd = sd(CovidIndia$Deaths, na.rm = TRUE)), 
      col = "red", add = TRUE)
legend("topright", legend = c("Densité des données Deaths", "Courbe normale"), col = c("blue", "red"), lty = 1)#d'apres le graphe la variable Deaths  ne suit pas la loi normale

install.packages("e1071")
library(e1071)
#verification des interpretations graphiques
#pour une loi normale K=3 et S=0

K=kurtosis(CovidIndia$Confirmed)
S=skewness(CovidIndia$Confirmed)
K #-0.08306497
S #1.198376
#confirmed no suit pas la loi normale

K=kurtosis(CovidIndia$Cured)
S=skewness(CovidIndia$Cured)
K #-0.1771851
S #1.184826
#Cured no suit pas la loi normale

K=kurtosis(CovidIndia$ConfirmedIndianNational)
S=skewness(CovidIndia$ConfirmedIndianNational)
K #15.40074
S #-4.005988
#confirmedIndianNational no suit pas la loi normale

K=kurtosis(CovidIndia$Deaths)
S=skewness(CovidIndia$Deaths)
K #0.566349
S #1.315803
#Deaths no suit pas la loi normale

K=kurtosis(CovidIndia$ConfirmedForeignNational)
S=skewness(CovidIndia$ConfirmedForeignNational)
K #2.105702
S #1.939862
#confirmedForeignNational no suit pas la loi normale


# analyser les modalites pour toutes les variable quantitatives

table(CovidIndia$ConfirmedIndianNational)
table(CovidIndia$Confirmed)
table(CovidIndia$Cured)
table(CovidIndia$Deaths)
table(CovidIndia$ConfirmedForeignNational)
###aucune des variables ne suit la loi normale donc on passe pour les tests non parametriques 

# Exemple ici avec 'Deaths' en tant que variable pour l'initialisation
CovidIndia$deaths_group <- CovidIndia$Deaths  # Assurez-vous de remplacer par la bonne variable

# Créez un groupe High/Low basé sur la médiane
CovidIndia$deaths_group <- ifelse(CovidIndia$deaths_group >= median(CovidIndia$deaths_group, na.rm = TRUE), "High", "Low")

# Convertissez en facteur
CovidIndia$deaths_group <- as.factor(CovidIndia$deaths_group)

# Vérifiez les résultats
summary(CovidIndia$deaths_group)
numeric_columns<-c("ConfirmedIndianNational", "ConfirmedForeignNational","Cured","Confirmed")

# Boucle pour appliquer le test de Wilcoxon
for (col in numeric_columns) {
  cat("Test de Wilcoxon pour :", col, "par Death_group\n")
  
  if (length(unique(CovidIndia$deaths_group)) == 2 && !all(is.na(CovidIndia[[col]]))) {
    result <- wilcox.test(CovidIndia[[col]] ~ CovidIndia$deaths_group, data = CovidIndia, exact = FALSE)
    print(result)
  } else {
    cat("Données insuffisantes ou problème avec les groupes\n")
  }
  cat("\n")
}
#interprétation des résultats:
#p-value < 0.05 : Les distributions des deux groupes sont significativement différentes.
#p-value ≥ 0.05 : Aucune différence significative détectée entre les groupes

# Test de Wilcoxon pour "ConfirmedIndianNational" par "Death_group"
# - Statistique W = 38754989, p-value < 2.2e-16
# - Cette p-value très faible (< 2.2e-16) indique une différence significative entre les groupes de décès pour cette variable.
#   Ainsi, on rejette l'hypothèse nulle et on conclut qu'il existe une différence significative dans les nombres de cas
#   confirmés pour les citoyens indiens selon les groupes de décès.

# Test de Wilcoxon pour "ConfirmedForeignNational" par "Death_group"
# - Statistique W = 39796185, p-value = 5.352e-07
# - La p-value est également très petite (5.352e-07), ce qui indique une différence statistiquement significative 
#   entre les groupes de décès pour les cas confirmés des ressortissants étrangers. On rejette l'hypothèse nulle,
#   ce qui montre une relation significative entre les groupes de décès et les cas confirmés pour les étrangers.

# Test de Wilcoxon pour "Cured" par "Death_group"
# - Statistique W = 81243533, p-value < 2.2e-16
# - La p-value est extrêmement faible (< 2.2e-16), suggérant qu'il existe une différence statistiquement significative
#   entre les groupes de décès en fonction du nombre de guérisons. Les guérisons varient de manière significative 
#   entre les groupes de décès.

# Test de Wilcoxon pour "Confirmed" par "Death_group"
# - Statistique W = 81315167, p-value < 2.2e-16
# - Une fois de plus, la p-value est extrêmement faible (< 2.2e-16), indiquant une différence statistiquement significative
#   entre les groupes de décès pour le nombre de cas confirmés. Cela montre qu'il y a une relation marquée entre les cas
#   confirmés et les groupes de décès.

# Conclusion :
# Tous les tests de Wilcoxon ont montré des p-values très faibles, bien inférieures au seuil de 0.05, ce qui indique
# que les variables testées (ConfirmedIndianNational, ConfirmedForeignNational, Cured, Confirmed) diffèrent significativement 
# entre les groupes de décès. Ces résultats suggèrent qu'il existe des différences importantes dans les distributions 
# des variables selon les groupes de décès, ce qui peut être pertinent pour les analyses épidémiologiques.






# Boucle pour appliquer le test de Kruskal-Wallis
for (col in numeric_columns) {
  cat("Test de Kruskal-Wallis pour :", col, "par Death Groups\n")
  
  result <- kruskal.test(CovidIndia[[col]] ~ CovidIndia$deaths_group, data = CovidIndia)
  print(result)
  cat("\n")
}


#interprétation de kruskkal-Wallis
#H0: Les médianes des groupes sont égales (pas de différence significative entre les groupes).
#p≤0.05 : Rejet de H0 les médianes des groupes sont significativement différentes.
#p>0.05 : On ne rejette pas H0 aucune différence significative entre les groupes.
# Les tests de Kruskal-Wallis ci-dessous examinent les différences de distributions entre les groupes "Death_groups"
# (groupes de décès) pour différentes variables. Ce test est utilisé pour comparer les distributions de ces variables
# entre plus de deux groupes indépendants sans supposer de normalité dans les données.

# Test de Kruskal-Wallis pour "ConfirmedIndianNational" par "Death_groups"
# - Chi-carré de Kruskal-Wallis = 134.85, df = 1, p-value < 2.2e-16
# - Hypothèse alternative : il existe une différence significative entre les distributions des groupes de décès pour cette variable
# - La p-value très faible (< 2.2e-16) indique que les distributions des cas confirmés pour les citoyens indiens
#   sont significativement différentes entre les groupes de décès. Ainsi, on rejette l'hypothèse nulle et conclut qu'il existe 
#   une différence marquée dans les données selon les groupes de décès.

# Test de Kruskal-Wallis pour "ConfirmedForeignNational" par "Death_groups"
# - Chi-carré de Kruskal-Wallis = 25.133, df = 1, p-value = 5.352e-07
# - Hypothèse alternative : il existe une différence significative entre les distributions des groupes de décès pour cette variable
# - La p-value est également très faible (5.352e-07), ce qui suggère une différence significative entre les groupes de décès 
#   en fonction du nombre de cas confirmés chez les ressortissants étrangers. On rejette l'hypothèse nulle et on conclut 
#   qu'il existe une relation statistiquement significative entre les groupes de décès et les cas confirmés des étrangers.

# Test de Kruskal-Wallis pour "Cured" par "Death_groups"
# - Chi-carré de Kruskal-Wallis = 13091, df = 1, p-value < 2.2e-16
# - Hypothèse alternative : il existe une différence significative entre les distributions des groupes de décès pour cette variable
# - La p-value est extrêmement faible (< 2.2e-16), ce qui indique une différence significative entre les groupes de décès
#   pour le nombre de guérisons. Les distributions des guérisons varient de manière significative entre les groupes de décès.

# Test de Kruskal-Wallis pour "Confirmed" par "Death_groups"
# - Chi-carré de Kruskal-Wallis = 13136, df = 1, p-value < 2.2e-16
# - Hypothèse alternative : il existe une différence significative entre les distributions des groupes de décès pour cette variable
# - La p-value très faible (< 2.2e-16) indique qu'il y a une différence significative entre les groupes de décès pour le nombre
#   de cas confirmés. On rejette l'hypothèse nulle et on conclut qu'il existe une relation importante entre les groupes de décès 
#   et les cas confirmés.

# Conclusion :
# Tous les tests de Kruskal-Wallis montrent des p-values très faibles, bien inférieures au seuil de 0.05, ce qui indique
# qu'il existe des différences significatives entre les groupes de décès pour chacune des variables testées (ConfirmedIndianNational, 
# ConfirmedForeignNational, Cured, Confirmed). Ces résultats suggèrent que les variables étudiées sont affectées de manière
# significative par les groupes de décès et qu'une analyse plus approfondie pourrait être nécessaire pour explorer les causes
# et les implications de ces différences.


# Test de corrélation entre ConfirmedIndianNational et Deaths
cor_test_1 <- cor.test(CovidIndia$ConfirmedIndianNational, CovidIndia$Deaths)
print(cor_test_1)

# Test de corrélation entre ConfirmedForeignNational et Deaths
cor_test_2 <- cor.test(CovidIndia$ConfirmedForeignNational, CovidIndia$Deaths)
print(cor_test_2)

# Test de corrélation entre Cured et Deaths
cor_test_3 <- cor.test(CovidIndia$Cured, CovidIndia$Deaths)
print(cor_test_3)

# Test de corrélation entre Confirmed et Deaths
cor_test_4 <- cor.test(CovidIndia$Confirmed, CovidIndia$Deaths)
print(cor_test_4)
# Test de corrélation entre ConfirmedIndianNational et Deaths
# La corrélation entre le nombre de cas confirmés chez les citoyens indiens et les décès est négative mais faible (r = -0.0813).
# Cela signifie qu'il y a une relation inverse légère, mais cette corrélation est statistiquement significative (p-value < 2.2e-16).
# Cependant, la faible valeur du coefficient indique que l'impact de cette relation est limité.

# Test de corrélation entre ConfirmedForeignNational et Deaths
# Une corrélation positive faible est observée entre le nombre de cas confirmés chez les ressortissants étrangers et les décès (r = 0.0661).
# Cela montre une légère relation positive, mais la corrélation reste faible, bien que statistiquement significative (p-value < 2.2e-16).

# Test de corrélation entre Cured et Deaths
# Une forte corrélation positive (r = 0.8715) entre le nombre de guéris et le nombre de décès est observée.
# Cela suggère que l'augmentation des guérisons est fortement liée à l'augmentation des décès, bien que cette relation puisse être due à une dynamique plus complexe.
# Cette corrélation est extrêmement significative (p-value < 2.2e-16).

# Test de corrélation entre Confirmed et Deaths
# Une corrélation très forte (r = 0.8774) est trouvée entre le nombre de cas confirmés et le nombre de décès.
# Cela indique qu'à mesure que le nombre de cas confirmés augmente, le nombre de décès augmente également de manière significative.
# Cette relation est également hautement significative (p-value < 2.2e-16).






# Régression simple : ConfirmedIndianNational
model1 <- lm(Deaths ~ ConfirmedIndianNational, data = CovidIndia)
summary(model1)
summary(resid(model1))
qqnorm(resid(model1), main = "Q-Q Plot des résidus pour ConfirmedIndianNational")
qqline(resid(model1), col = "red")

# Régression simple : ConfirmedForeignNational
model2 <- lm(Deaths ~ ConfirmedForeignNational, data = CovidIndia)
summary(model2)
summary(resid(model2))
qqnorm(resid(model2), main = "Q-Q Plot des résidus pour ConfirmedForeignNational")
qqline(resid(model2), col = "red")

# Régression simple : Cured
model3 <- lm(Deaths ~ Cured, data = CovidIndia)
summary(model3)
summary(resid(model3))
qqnorm(resid(model3), main = "Q-Q Plot des résidus pour Cured")
qqline(resid(model3), col = "red")

# Régression simple : Confirmed
model4 <- lm(Deaths ~ Confirmed, data = CovidIndia)
summary(model4)
summary(resid(model4))
qqnorm(resid(model4), main = "Q-Q Plot des résidus pour Confirmed")
qqline(resid(model4), col = "red")


# Régression multiple : Modèle avec toutes les variables explicatives
model_multiple <- lm(Deaths ~ ConfirmedIndianNational + ConfirmedForeignNational + Cured + Confirmed, data = CovidIndia)
summary(model_multiple)
summary(resid(model_multiple))
qqnorm(resid(model_multiple), main = "Q-Q Plot des résidus pour le modèle multiple")
qqline(resid(model_multiple), col = "red")

# Modèle 1 : Régression linéaire entre "Deaths" (décès) et "ConfirmedIndianNational" (cas confirmés parmi les citoyens indiens)
# - Le coefficient de l'intercept est 0.32223 avec une p-value < 2e-16, indiquant que l'intercept est statistiquement significatif.
# - Le coefficient pour "ConfirmedIndianNational" est -0.18341 avec une p-value < 2e-16, ce qui signifie que chaque augmentation
#   du nombre de cas confirmés parmi les citoyens indiens est associée à une diminution des décès de 0.18341, avec une forte
#   signification statistique. 
# - R-squared = 0.006616, ce qui signifie que seulement 0.66% de la variance des décès est expliquée par les cas confirmés indiens.
#   Ce faible R-squared indique que d'autres facteurs non inclus dans le modèle expliquent la majorité de la variance des décès.
# - F-statistic = 120.6 avec une p-value < 2.2e-16, ce qui montre que le modèle est globalement significatif.

# Modèle 2 : Régression linéaire entre "Deaths" (décès) et "ConfirmedForeignNational" (cas confirmés parmi les ressortissants étrangers)
# - L'intercept est 0.202608 avec une p-value < 2e-16, indiquant une signification statistique élevée.
# - Le coefficient pour "ConfirmedForeignNational" est 0.056247 avec une p-value < 2e-16, ce qui suggère que chaque augmentation
#   du nombre de cas confirmés parmi les ressortissants étrangers est associée à une augmentation des décès de 0.056247, également
#   statistiquement significative.
# - Le R-squared est 0.004375, ce qui signifie que cette variable explique seulement 0.44% de la variance des décès, suggérant
#   une faible relation entre les cas confirmés étrangers et les décès.
# - Le F-statistic est de 79.56 avec une p-value < 2.2e-16, ce qui montre également que ce modèle est statistiquement significatif.

# Modèle 3 : Régression linéaire entre "Deaths" (décès) et "Cured" (guéris)
# - L'intercept est 0.032068 avec une p-value < 2e-16, ce qui le rend significatif.
# - Le coefficient pour "Cured" est 0.709429 avec une p-value < 2e-16, ce qui signifie que chaque guérison supplémentaire
#   est associée à une augmentation des décès de 0.709429, une relation très forte et statistiquement significative.
# - Le R-squared est 0.7596, ce qui indique que 75.96% de la variance des décès est expliquée par le nombre de guérisons, ce qui
#   suggère une relation importante et significative entre ces deux variables.
# - Le F-statistic est de 57200 avec une p-value < 2.2e-16, indiquant que ce modèle est extrêmement significatif.

# Modèle 4 : Régression linéaire entre "Deaths" (décès) et "Confirmed" (cas confirmés)
# - L'intercept est 0.028091 avec une p-value < 2e-16, ce qui montre qu'il est significatif.
# - Le coefficient pour "Confirmed" est 0.732841 avec une p-value < 2e-16, ce qui suggère que chaque cas confirmé supplémentaire
#   est associé à une augmentation des décès de 0.732841, une relation fortement significative.
# - Le R-squared est de 0.7699, ce qui indique que 76.99% de la variance des décès peut être expliquée par le nombre de cas confirmés.
#   Cela montre que les cas confirmés sont un bon prédicteur des décès.
# - Le F-statistic est de 60570 avec une p-value < 2.2e-16, ce qui montre que le modèle est significatif.

# Modèle multiple : Régression linéaire avec plusieurs variables explicatives ("ConfirmedIndianNational", "ConfirmedForeignNational",
# "Cured" et "Confirmed") pour prédire "Deaths" (décès)
# - L'intercept est 0.001347 avec une p-value de 0.807, indiquant qu'il n'est pas significatif dans ce modèle multiple.
# - Le coefficient pour "ConfirmedIndianNational" est 0.046935 avec une p-value < 2e-16, ce qui signifie que l'augmentation des
#   cas confirmés parmi les citoyens indiens est positivement associée aux décès, bien que l'impact soit faible.
# - Le coefficient pour "ConfirmedForeignNational" est -0.019386 avec une p-value < 2e-16, ce qui montre une association négative
#   entre les cas confirmés étrangers et les décès. Chaque augmentation du nombre de cas confirmés étrangers est associée à une
#   diminution des décès.
# - Le coefficient pour "Cured" est 0.203429 avec une p-value < 2e-16, ce qui indique que l'augmentation du nombre de guérisons
#   est fortement associée à une augmentation des décès.
# - Le coefficient pour "Confirmed" est 0.532050 avec une p-value < 2e-16, indiquant que les cas confirmés ont un impact positif
#   significatif sur le nombre de décès.
# - Le R-squared est 0.7735, ce qui signifie que ce modèle explique environ 77.35% de la variance des décès, suggérant qu'il est
#   un bon modèle prédictif pour les décès.
# - Le F-statistic est de 15460 avec une p-value < 2.2e-16, ce qui confirme que le modèle est significatif dans son ensemble.

