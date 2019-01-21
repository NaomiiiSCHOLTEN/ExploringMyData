#########
# Mayotte lagoon subsurface water temperature data exploration
# created on December 2018
# script copyright: Naomi Scholten & Jonathan Richir
#########


setwd("~/GitHub/ExploringMyData") # set working directory to load data, compile report etc.

Sys.setenv(TZ = "UTC") # to define cfr you wrk with date
Sys.setlocale("LC_TIME", "English") # English as local language


## install package if required only

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


## Import data

New_DATA <- read_delim("MyData.csv", delim = ";", trim_ws = TRUE) # data importation with red_delim load variables in the right format
New_DATA <- data.frame(New_DATA) # to remove all unecessary lines from imported data file


## Data mining

unique(New_DATA$Site) # 8 stations monitored for temperature in the lagoon
unique(New_DATA$Year) ; unique(New_DATA$Month)


## Create new variables

# Year.Month variable
New_DATA$Year.Month <- paste(New_DATA$Year, New_DATA$Month, sep = ".")

# Month in number, see https://rpubs.com/sediaz/Month_Abb
# R variable called month.abb
month.abb
unique(New_DATA$Month)
setdiff(New_DATA$Month, month.abb) ; setdiff(month.abb, New_DATA$Month) # diff between Month chr vectors
# replace Month that do not match with R
New_DATA$Month <- ifelse(New_DATA$Month == "Sept", "Sep", New_DATA$Month)
New_DATA$Month.nb <- match(New_DATA$Month, month.abb)
New_DATA$Date <- as.Date(paste(New_DATA$Year, New_DATA$Month.nb, "01", sep = "-"), origin = "1970-01-01")
# Sort by vector name Site then Date
New_DATA <- New_DATA[with(New_DATA, order(Site, Date)), ] 


## Plot for visual representation of the data set

unique(New_DATA$Year.Month)
(site <- unique(New_DATA$Site))

par(mar = c(5, 4, 4, 2) + 0.1) 

plot(x = subset(New_DATA, Site == site[1])$Date, y = subset(New_DATA, Site == site[1])$Temperature, xlab = "", xaxt = "n", ylab = "Temperature (�C)", type = 'b', pch = 19, col = 1, ylim = c(min(New_DATA$Temperature), max(New_DATA$Temperature)))
axis(1, subset(New_DATA, Site == site[1])$Date, format(subset(New_DATA, Site == site[1])$Date, "%b %d %y"), las = 2)
points(x = subset(New_DATA, Site == site[2])$Date, y = subset(New_DATA, Site == site[2])$Temperature, type = 'b', pch = 19, col = 2)
points(x = subset(New_DATA, Site == site[3])$Date, y = subset(New_DATA, Site == site[3])$Temperature, type = 'b', pch = 19, col = 3)
points(x = subset(New_DATA, Site == site[4])$Date, y = subset(New_DATA, Site == site[4])$Temperature, type = 'b', pch = 19, col = 4)
points(x = subset(New_DATA, Site == site[5])$Date, y = subset(New_DATA, Site == site[5])$Temperature, type = 'b', pch = 19, col = 5)
points(x = subset(New_DATA, Site == site[6])$Date, y = subset(New_DATA, Site == site[6])$Temperature, type = 'b', pch = 19, col = 6)
points(x = subset(New_DATA, Site == site[7])$Date, y = subset(New_DATA, Site == site[7])$Temperature, type = 'b', pch = 19, col = 7)
points(x = subset(New_DATA, Site == site[8])$Date, y = subset(New_DATA, Site == site[8])$Temperature, type = 'b', pch = 19, col = 8)

# function to add legend outside the plot; or use mtext function
add_legend <- function(...) {
  opar <- par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0),
              mar = c(0, 0, 0, 0), new = TRUE)
  on.exit(par(opar))
  plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend(...)
}

add_legend("topright", legend = c(site[1], site[2], site[3], site[4], site[5], site[6], site[7], site[8]), pch = c(19), col = c(1, 2, 3, 4, 5, 6, 7, 8), bty = 'n', cex = 1, ncol = 3)

c(5, 4, 4, 2) + 0.1 # default value



# I do not agree with below analysis. You consider Year.Month are new variables, then you perform spatial analyses on these 13 new variables. They are not different variables, there are all temperature. # If you intend to compare mean seasonnal temperature, then the above graph is enough. To do an ACP, a CA, you need several variables like temperature, salinity, etc; not only one. You mix comparison of site mean monthly temperature and spatial analysis. In contrast, the MFA could be an appropriate analysis since you have 5 variables, 2 numeric: temp and depth; 3 factorial: site, recif type, type_ME. 


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


# AMF on New_DATA df. 

New_DATA$Type_ME <- New_DATA$Site
New_DATA$Type_ME <- ifelse(New_DATA$Type_ME == "Boueni", "Fond de baie", New_DATA$Type_ME)
New_DATA$Type_ME <- ifelse(New_DATA$Type_ME %in% c("Double_barriere", "Saziley", "Tanaraki"), "Eau cotiere Sud-Ouest", New_DATA$Type_ME)
New_DATA$Type_ME <- ifelse(New_DATA$Type_ME %in% c("Hajangoua", "Longoni"), "Eau cotiere Nord-Est", New_DATA$Type_ME)
New_DATA$Type_ME <- ifelse(New_DATA$Type_ME %in% c("Passe_bateau", "Passe_S_ext"), "Eau lagunaire", New_DATA$Type_ME)
unique(paste(New_DATA$Site, New_DATA$Type_ME)) # check new variable

AMF <- New_DATA[,c(1,2,5,7,13)]
AMF$Site <- as.factor(AMF$Site)
AMF$Recif_type <- as.factor(AMF$Recif_type)
AMF$Type_ME <- as.factor(AMF$Type_ME)
AnaMF <- MFA (AMF[,c(2:5)], group =c (1,1,1,1) , type = c("n","s","s","n"),
              ind.sup = NULL,  name.group = c("Recif_type","Temperature","depth","Type_ME"),  
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


## AMF
New2_DATA <- unique(cbind(Sept.15,Oct.15,Nov.15,Dec.15,Jan.16,Feb.16,Mar.16,Apr.16,May.16,Jun.16,Jul.16,Aug.16,Sept.16))
DATA_Depth <- New_DATA %>%
  group_by(Site) %>%
  summarize(Depth = mean(depth, na.rm = T), sd.Depth = sd(depth, na.rm = T), nb = n())
New2_DATA$Depth <- DATA_Depth$Depth
New2_DATA$Type_ME <- as.factor(c("Fond de baie","Eau cotiere Sud-Ouest","Eau cotiere Nord Est","Eau cotiere Nord Est","Eau lagunaire","Eau lagunaire","Eau cotiere Sud-Ouest","Eau cotiere Sud-Ouest"))
New2_DATA$Recif_type <- as.factor(c("Frangeant","Interne","Interne","Frangeant","Barriere","Barriere","Frangeant","Frangeant"))

AMF <- as.data.frame(New2_DATA)
AMF$Site <- as.factor(AMF$Site)
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
