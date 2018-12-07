# setwd("C:/Users/ULG/ULiege/RStudio/DMSP-DMSO_ts") # to be able to compile report from File - Compile report
Sys.setenv(TZ = "UTC") # to define cfr you wrk with date
Sys.setlocale("LC_TIME", "English") # English as local language

# install package if required only
if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")} 
if(!require(lubridate)){install.packages("lubridate")} 
if(!require(zoo)){install.packages("zoo")} 
if(!require(pastecs)){install.packages("pastecs")} 
#if(!require(xlsx)){install.packages("xlsx")} 
if(!require(ggplot2)){install.packages("ggplot2")} 
if(!require(tidyr)){install.packages("tidyr")} 
if(!require(reshape2)){install.packages("reshape2")} 
if(!require(reshape)){install.packages("reshape")} 
if(!require(RColorBrewer)){install.packages("RColorBrewer")} 
if(!require(wesanderson)){install.packages("wesanderson")} 
if(!require(FactoMineR)){install.packages("FactoMineR")} 
if(!require(factoextra)){install.packages("factoextra")} 
if(!require(SciViews)){install.packages("SciViews")} 
if(!require(corrplot)){install.packages("corrplot")} 
if(!require(vegan)){install.packages("vegan")} 
if(!require(BBmisc)){install.packages("BBmisc")} 
if(!require(dendextend)){install.packages("dendextend")} 
if(!require(tidyverse)){install.packages("tidyverse")} 


New_DATA <- read_delim("MyData.csv", delim = ";", trim_ws = TRUE) # importation des data avec red_delim te permet d'avoir directement les variables au bon format
New_DATA$Year.Month <- paste(New_DATA$Year, New_DATA$Month, sep = ".")

######## 1. Serie la plus longue et la plus complete : sept 2015 - sept 2016 (pour 8 stations)

## data frame management
Sept.15 <- subset(New_DATA, Year.Month == "2015.Sept", c("Site","Temperature"))
names(Sept.15)[2] <- "Sept.15"
Oct.15 <- subset(New_DATA, Year.Month == "2015.Oct", c("Temperature"))
names(Oct.15)[1] <- "Oct.15"
Nov.15 <- subset(New_DATA, Year.Month == "2015.Nov", c("Temperature"))
names(Nov.15)[1] <- "Nov.15"
Dec.15 <- subset(New_DATA, Year.Month == "2015.Dec", c("Temperature"))
names(Dec.15)[1] <- "Dec.15"
Jan.16 <- subset(New_DATA, Year.Month == "2016.Jan", c("Temperature"))
names(Jan.16)[1] <- "Jan.16"
Feb.16 <- subset(New_DATA, Year.Month == "2016.Feb", c("Temperature"))
names(Feb.16)[1] <- "Feb.16"
Mar.16 <- subset(New_DATA, Year.Month == "2016.Mar", c("Temperature"))
names(Mar.16)[1] <- "Mar.16"
Apr.16 <- subset(New_DATA, Year.Month == "2016.Apr", c("Temperature"))
names(Apr.16)[1] <- "Apr.16"
May.16 <- subset(New_DATA, Year.Month == "2016.May", c("Temperature"))
names(May.16)[1] <- "May.16"
Jun.16 <- subset(New_DATA, Year.Month == "2016.Jun", c("Temperature"))
names(Jun.16)[1] <- "Jun.16"
Jul.16 <- subset(New_DATA, Year.Month == "2016.Jul", c("Temperature"))
names(Jul.16)[1] <- "Jul.16"
Aug.16 <- subset(New_DATA, Year.Month == "2016.Aug", c("Temperature"))
names(Aug.16)[1] <- "Aug.16"
Sept.16 <- subset(New_DATA, Year.Month == "2016.Sept", c("Temperature"))
names(Sept.16)[1] <- "Sept.16"
New2_DATA <- unique(cbind(Sept.15,Oct.15,Nov.15,Dec.15,Jan.16,Feb.16,Mar.16,Apr.16,May.16,Jun.16,Jul.16,Aug.16,Sept.16))
rownames(New2_DATA) <- New2_DATA$Site
New2_DATA$Site <- NULL


## PCA
res.pca <- prcomp(New2_DATA, scale  = T)
summary(res.pca)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Colorer par le cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)


## Hierarchical clustering
New2_DATA <- unique(cbind(Sept.15,Oct.15,Nov.15,Dec.15,Jan.16,Feb.16,Mar.16,Apr.16,May.16,Jun.16,Jul.16,Aug.16,Sept.16))
station_labels <- New2_DATA$Site
New2_DATA$Site <- NULL

New2_DATA_sc <- as.data.frame(scale(New2_DATA))
summary(New2_DATA_sc)
dist_mat <- dist(New2_DATA_sc, method = "euclidian")
hclust_avg <- hclust(dist_mat, method = "average")
plot(hclust_avg, labels = station_labels, hang = -1, cex = 1)

cut_avg <- cutree(hclust_avg, h = 3)
rect.hclust(hclust_avg , h = 3)


## AMF
New2_DATA <- unique(cbind(Sept.15,Oct.15,Nov.15,Dec.15,Jan.16,Feb.16,Mar.16,Apr.16,May.16,Jun.16,Jul.16,Aug.16,Sept.16))
DATA_Depth <- New_DATA %>%
  group_by(Site) %>%
  summarize(Depth = mean(depth, na.rm = T), sd.Depth = sd(depth, na.rm = T), nb = n())
New2_DATA$Depth <- DATA_Depth$Depth
New2_DATA$Type_ME <- as.factor(c("Fond de baie","Eau cotiere Sud-Ouest","Eau cotiere Nord Est","Eau cotiere Nord Est","Eau lagunaire","Eau lagunaire","Eau cotiere Sud-Ouest","Eau cotiere Sud-Ouest"))
New2_DATA$Recif_type <- as.factor(c("Frangeant","Interne","Interne","Frangeant","Barriere","Barriere","Frangeant","Frangeant"))

AMF <- as.data.frame(New2_DATA)
AnaMF <- MFA (AMF, group =c (1,13,1,1,1) , type = c("n","s","s","n","n"),
              ind.sup = NULL,  name.group = c("Site","Temperature","Profondeur","Recif_type","Type_ME"),  
              num.group.sup = NULL , graph = TRUE, weight.col.mfa = NULL, 
              row.w = NULL, axes = c(1,5), tab.comp=NULL)
plotellipses(AnaMF)
plot(AnaMF)

print(AnaMF)
eig.val <- get_eigenvalue(AnaMF) #extraction des valeurs propres/variances des composantes principales
fviz_screeplot(AnaMF) #visualisation des proportions des variances
group <- get_mfa_var(AnaMF, "group") #extraction des résultats pour les groupes de variables
group
fviz_mfa_var(AnaMF, "group")
# Contribution a la premiere dimension
fviz_contrib (AnaMF, "group", axes = 1)
# Contribution a la deuxieme dimension
fviz_contrib (AnaMF, "group", axes = 2)
quanti.var <- get_mfa_var(AnaMF, "quanti.var")
quanti.var 
fviz_mfa_var(AnaMF, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)
fviz_mfa_var(AnaMF, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")
ind <- get_mfa_ind(AnaMF)
ind
fviz_mfa_ind(AnaMF, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_mfa_ind(AnaMF, 
             habillage = "Label", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE 
) 
fviz_ellipses(AnaMF, c("Recif_type", "Type_ME"), repel = TRUE)
fviz_mfa_axes (AnaMF)


### 2. Missing data : trying to fill up missing data in my serie
