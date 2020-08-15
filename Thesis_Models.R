# This code was written by Arbian Halilaj for the Bachelor's Thesis
# For questions contact arbian.halilaj@hotmail.com
# Last tested: 14/08/2020

# Setup and data preparation

library(xtable)
library(naniar) # replace_with_na
library(tidyverse)
library(car) #Multicollinearity test
library(reshape2) #Correlation matrix
library(stargazer) #Output
library(ggpubr)
library(ggfortify) #Autoplot diagnostics
library(lmtest) #Breusch-Pagan Test (Heteroskedasticity)
library(mfx) #Marginal effects

rm(list = ls()) # memory cleaning

# Preparing data import from stata 
# Loading foreign library 
library(foreign)

# Importing data from stata file
data <- read.dta("file source")

# Select variables of interest
selected <- c("h1", "h5", "d2", "n3", "l1", "l2", "f1", "n2p", "e11", "e30", "j7a", 
              "j30f", "c5", "c14", "d5a", "d15a", "g4", "j5", "j15", "c22b", "m1a", 
              "j30e", "j30b", "d30b", "j30c", "l30a", "h7a", "j4", "BMj1a", "BMj1b",
              "b5", "a0", "a6b", "b2b", "d3c", "BMb6", "a4b", "a3a", "BMl1a", "j6a",
              "k4", "k21", "b7", "b1", "e6", "BMh2", "b7a", "l10", "b8","k8",
              "h30", "a3", "a16")

# Subset data
data.subset <- data[selected]
data.subset <- as.data.frame(data.subset)

# Rename variables
df<- data.subset %>% 
  rename(Truth = a16,
         LocalitySize = a3,
         Sector = a0,
         Size = a6b,
         Foreign = b2b,
         Year.0 = b5,
         Experience = b7,
         FemaleTMT = b7a, 
         Water.Bribe = c14,
         Electric.Bribe = c5,
         Sales.1 = d2,
         InformalCompetition = e11,
         InformalObstacle = e30,
         TechLicense = e6,
         CapacityUtil = f1,
         Construction.Bribe = g4,
         InnovationProduct = h1,
         InnovationProcess = h5,
         CourtFairness = h7a,
         Court.Obstacle = h30,
         Import.Bribe = d15a,
         Export.Bribe = d5a,
         OperatingLicense.Bribe = j15,
         Inspection.Bribe = j5,
         Corruption.Obstacle = j30f,
         Inspection.Frequency = j4,
         Gov.Contract = j6a,
         Bribes = j7a,
         Bank.Financing = k8,
         External.Audit = k21,
         Employees.1 = l1,
         Employees.0 = l2,
         UniversityDegree = BMl1a,
         Sales.0 = n3,
         Costofsales = n2p,
         Website = c22b,
         BiggestObstacle = m1a,
         PoliticalInstability = j30e,
         TaxAdmin = j30b,
         CustomTrade = d30b,
         BusinessPermit = j30c,
         LaborReg = l30a,
         TaxofficialProfessional = BMj1a,
         TaxofficialTransparent = BMj1b,
         Export = d3c,
         Network = BMb6,
         Industry = a4b,
         Region = a3a,
         Equipment = k4,
         LegalStatus = b1,
         RD = BMh2,
         TrainingEmployees = l10,
         QualityCertificate = b8
  )

# Recode variables
## Dependent variables
df <- df%>%
  mutate(InnovationProduct=case_when(
    InnovationProduct=="Yes" ~ 1,
    InnovationProduct=="No" ~ 0
  ))

df <- df%>%
  mutate(InnovationProcess=case_when(
    InnovationProcess=="Yes" ~ 1,
    InnovationProcess=="No" ~ 0
  ))

x <- 1
df$InnovationIndex <- as.numeric(df$InnovationProduct %in% x | df$InnovationProcess %in% x)

df$Sales.1 <- as.numeric(df$Sales.1)
df <- df %>% replace_with_na(replace = list(Sales.1 = -9))
df$Sales.0 <- as.numeric(df$Sales.0)
df <- df %>% replace_with_na(replace = list(Sales.0 = c(-9,-7)))
df$SalesGrowth <- 1/3*((df$Sales.1-df$Sales.0)/((df$Sales.1+df$Sales.0)/2))
#df$SalesGrowth <- ifelse(df$SalesGrowth < 0, NA, df$SalesGrowth)

df$Employees.1 <- as.numeric(df$Employees.1)
df <- df %>% replace_with_na(replace = list(Employees.1 = -9))
df$Employees.0 <- as.numeric(df$Employees.0)
df <- df %>% replace_with_na(replace = list(Employees.0 = c(-9,-7)))
df$EmploymentGrowth <- 1/3*((df$Employees.1-df$Employees.0)/((df$Employees.1+df$Employees.0)/2))
#df$logEmploymentGrowth <- 1/3*((log(df$Employees.1)-log(df$Employees.0))/((log(df$Employees.1)+log(df$Employees.0))/2))
#df$EmploymentGrowth <- ifelse (df$EmploymentGrowth < 0, NA, df$EmploymentGrowth)

df$LaborProductivityGrowth <- 1/3*(((df$Sales.1/df$Employees.1)-(df$Sales.0/df$Employees.0))/(((df$Sales.1/df$Employees.1)+(df$Sales.0/df$Employees.0))/2))
#df$LaborProductivityGrowth <- ifelse (df$LaborProductivityGrowth < 0, NA, df$LaborProductivityGrowth)

df$Costofsales <- as.numeric(df$Costofsales)
df <- df %>% replace_with_na(replace = list(Costofsales = -9))
df$Performance <- (df$Sales.1-df$Costofsales)/df$Employees.1
df$Performance <- ifelse (df$Performance < 0, NA, df$Performance)
df <- df %>% replace_with_na(replace = list(Performance = 0))
df$Performance <- log(df$Performance)

df$CapacityUtil <- as.numeric(df$CapacityUtil)
df <- df %>% replace_with_na(replace = list(CapacityUtil = -9))
df$CapacityUtil <- df$CapacityUtil/100

## Independent variables
df$Bribes <- as.numeric(df$Bribes)
df <- df %>% replace_with_na(replace = list(Bribes = c(-9,-8)))
df$Bribes <- df$Bribes/100

df <- df%>%
  mutate(InformalCompetition=case_when(
    InformalCompetition=="Yes" ~ 1,
    InformalCompetition=="No" ~ 0
  ))

df <- df%>%
  mutate(Inspection.Bribe=case_when(
    Inspection.Bribe=="Yes" ~ 1,
    Inspection.Bribe=="No" ~ 0
  ))
df <- df%>%
  mutate(Water.Bribe=case_when(
    Water.Bribe=="Yes" ~ 1,
    Water.Bribe=="No" ~ 0
  ))

df <- df%>%
  mutate(Electric.Bribe=case_when(
    Electric.Bribe=="Yes" ~ 1,
    Electric.Bribe=="No" ~ 0
  ))

df <- df%>%
  mutate(Export.Bribe=case_when(
    Export.Bribe=="Yes" ~ 1,
    Export.Bribe=="No" ~ 0
  ))

df <- df%>%
  mutate(Import.Bribe=case_when(
    Import.Bribe=="Yes" ~ 1,
    Import.Bribe=="No" ~ 0
  ))

df <- df%>%
  mutate(Construction.Bribe=case_when(
    Construction.Bribe=="Yes" ~ 1,
    Construction.Bribe=="No" ~ 0
  )) 

df <- df%>%
  mutate(OperatingLicense.Bribe=case_when(
    OperatingLicense.Bribe=="Yes" ~ 1,
    OperatingLicense.Bribe=="No" ~ 0
  ))

x <- 1
df$BribeIndex <- as.numeric(df$Water.Bribe %in% x | df$Electric.Bribe %in% x | df$Export.Bribe %in% x | df$Import.Bribe %in% x | df$Construction.Bribe %in% x | df$OperatingLicense.Bribe %in% x)

df <- df%>%
  mutate(Website=case_when(
    Website=="Yes" ~ 1,
    Website=="No" ~ 0
  ))

df <- df%>%
  mutate(Equipment=case_when(
    Equipment=="Yes" ~ 1,
    Equipment=="No" ~ 0
  ))

df <- df%>%
  mutate(PoliticalInstability=case_when(
    PoliticalInstability=="No obstacle" ~ 0,
    PoliticalInstability=="Minor obstacle" ~ 1,
    PoliticalInstability=="Moderate obstacle" ~2,
    PoliticalInstability=="Major obstacle" ~ 3,
    PoliticalInstability=="Very severe obstacle" ~ 4
  ))

df <- df%>%
  mutate(TaxAdmin=case_when(
    TaxAdmin=="No obstacle" ~ 0,
    TaxAdmin=="Minor obstacle" ~ 1,
    TaxAdmin=="Moderate obstacle" ~2,
    TaxAdmin=="Major obstacle" ~ 3,
    TaxAdmin=="Very severe obstacle" ~ 4
  ))
df$TaxAdmin <- as.numeric(df$TaxAdmin)

df <- df%>%
  mutate(CustomTrade=case_when(
    CustomTrade=="No obstacle" ~ 0,
    CustomTrade=="Minor obstacle" ~ 1,
    CustomTrade=="Moderate obstacle" ~2,
    CustomTrade=="Major obstacle" ~ 3,
    CustomTrade=="Very severe obstacle" ~ 4
  ))
df$CustomTrade <- as.numeric(df$CustomTrade)

df <- df%>%
  mutate(BusinessPermit=case_when(
    BusinessPermit=="No obstacle" ~ 0,
    BusinessPermit=="Minor obstacle" ~ 1,
    BusinessPermit=="Moderate obstacle" ~2,
    BusinessPermit=="Major obstacle" ~ 3,
    BusinessPermit=="Very severe obstacle" ~ 4
  ))
df$BusinessPermit <- as.numeric(df$BusinessPermit)

df <- df%>%
  mutate(LaborReg=case_when(
    LaborReg=="No obstacle" ~ 0,
    LaborReg=="Minor obstacle" ~ 1,
    LaborReg=="Moderate obstacle" ~2,
    LaborReg=="Major obstacle" ~ 3,
    LaborReg=="Very severe obstacle" ~ 4
  ))
df$LaborReg <- as.numeric(df$LaborReg)

#df$PolicyObstacle <- rowMeans(df[,23:26], na.rm=TRUE, na.action=na.pass)
df$PolicyObstacle <- with(df, ifelse(is.na(TaxAdmin), NA, ifelse(is.na(CustomTrade), NA, ifelse(is.na(BusinessPermit), NA, ifelse(is.na(LaborReg), NA, (TaxAdmin+CustomTrade+BusinessPermit+LaborReg)/4)))))
#df$PolicyObstacle <-aggregate(. ~ TaxAdmin+CustomTrade+BusinessPermit+LaborReg, df, mean, na.rm=TRUE, na.action=na.pass)

df <- df %>% replace_with_na(replace = list(Inspection.Frequency = c(-9,-7)))

df <- df%>%
  mutate(CourtFairness=case_when(
    CourtFairness=="Strongly agree" ~ 0,
    CourtFairness=="Tend to agree" ~ 0,
    CourtFairness=="Tend to disagree" ~ 1,
    CourtFairness=="Strongly disagree" ~ 1
  ))

df <- df%>%
  mutate(Court.Obstacle=case_when(
    Court.Obstacle=="No obstacle" ~ 0,
    Court.Obstacle=="Minor obstacle" ~ 1,
    Court.Obstacle=="Moderate obstacle" ~2,
    Court.Obstacle=="Major obstacle" ~ 3,
    Court.Obstacle=="Very severe obstacle" ~ 4
  ))

## Control variables
df <- df %>% replace_with_na(replace = list(Year.0 = -9))
df$Age <- 2019-df$Year.0
df$lnAge <- log(df$Age)

df <- df%>%
  mutate(Sector=case_when(
    Sector=="Manufacturing" ~ 1,
    Sector=="Retail services" ~ 0,
    Sector=="Other services" ~ 0
  )) 

df$Size <- as.numeric(df$Size)
df$Small <- ifelse(df$Size <= 2, 1, ifelse((df$Size <= 3) & (df$Size <= 4), 0, 0))
df$Medium <- ifelse(df$Size <= 2, 0, ifelse((df$Size <= 3) & (df$Size <= 4), 1, 0))
df$Large <- ifelse(df$Size <= 2, 0, ifelse((df$Size <= 3) & (df$Size <= 4), 0, 1))

df$Foreign <- as.numeric(df$Foreign)
df <- df %>% replace_with_na(replace = list(Foreign = c(-9,-7)))
df$Foreign <- df$Foreign/100
#############df$Foreign <- ifelse(df$Foreign )

df$Export <- as.numeric(df$Export)
df <- df %>% replace_with_na(replace = list(Export = c(-9,-7)))
df$Export <- df$Export/100
#df$Export <- ifelse(df$Export < 0.2, NA, df$Export)

df <- df%>% 
  mutate(Network=case_when(
    Network=="Yes" ~ 1,
    Network=="No" ~ 0
  ))

df$UniversityDegree <- as.numeric(df$UniversityDegree)
df <- df %>% replace_with_na(replace = list(UniversityDegree = -9))
df$UniversityDegree <- df$UniversityDegree/100

df <- df%>% 
  mutate(Gov.Contract=case_when(
    Gov.Contract=="Yes" ~ 1,
    Gov.Contract=="No" ~ 0
  ))

df <- df%>%
  mutate(External.Audit=case_when(
    External.Audit=="Yes" ~ 1,
    External.Audit=="No" ~ 0
  ))

df <- df%>% 
  mutate(FemaleTMT=case_when(
    FemaleTMT=="Yes" ~ 1,
    FemaleTMT=="No" ~ 0
  ))

df <- df%>%
  mutate(TechLicense=case_when(
    TechLicense=="Yes" ~ 1,
    TechLicense=="No" ~ 0
  ))

df <- df%>%
  mutate(Gov.Contract=case_when(
    Gov.Contract=="Yes" ~ 1,
    Gov.Contract=="No" ~ 0
  )) 

df <- df%>%
  mutate(Bank.Financing=case_when(
    Bank.Financing=="Yes" ~ 1,
    Bank.Financing=="No" ~ 0
  ))

df <- df%>%
  mutate(RD=case_when(
    RD=="Yes" ~ 1,
    RD=="No" ~ 0
  ))

df <- df%>%
  mutate(TrainingEmployees=case_when(
    TrainingEmployees=="Yes" ~ 1,
    TrainingEmployees=="No" ~ 0
  ))

df <- df%>%
  mutate(QualityCertificate=case_when(
    QualityCertificate=="Yes" ~ 1,
    QualityCertificate=="No" ~ 0
  ))

df$Experience <- as.numeric(df$Experience)
df <- df %>% replace_with_na(replace = list(Experience = -9))
df$lnExperience <- log(df$Experience)
df <- df %>% replace_with_na(replace = list(lnExperience = "NaN"))
###################################################################
# REGRESSION MODELS
###################################################################
# Descriptive Stats
descript <- c("SalesGrowth", "EmploymentGrowth", "LaborProductivityGrowth", "InnovationIndex",
              "Bribes", "Inspection.Bribe", "BribeIndex", "InformalCompetition", 
              "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", 
              "lnExperience", "Foreign", "Export", "TrainingEmployees", 
              "RD", "QualityCertificate")

df_descript <- df[descript]
df_descript <- df_descript[apply(df_descript, 1, function(x) !any(is.na(x))),]

df_descript <- df_descript[-c(238), ]

df_descript <- subset(df_descript,!(df_descript$Bribes > quantile(df_descript$Bribes, probs=c(.01, .99))[2] | df_descript$Bribes < quantile(df_descript$Bribes, probs=c(.01, .99))[1]) )

## Summary
summary(df_descript)
stargazer(df_descript)

##Correlation matrix
Modcor1 <- c("SalesGrowth", "Bribes", "RD",
             "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
             "Foreign", "Export", "TrainingEmployees", "InformalCompetition")

Modcor1 <- df[Modcor1]
Modcor1 <- Modcor1[apply(Modcor1, 1, function(x) !any(is.na(x))),]

Modcor2 <- c("InnovationIndex", "Bribes", "Inspection.Bribe", "BribeIndex", 
             "PolicyObstacle", "InformalCompetition", "RD", "TechLicense", 
             "QualityCertificate", "Sector", "Small", "Medium", "Large", 
             "lnAge", "lnExperience", "Foreign", "Export", "TrainingEmployees")

sds <- df_model3.3
sds[] <- lapply(sds,as.numeric)


cormatweek <- round(cor(sds, method = "spearman"),2)


### Get upper triangle of the correlation matrix


get_upper_tri_week <- function(cormatweek){
  cormatweek[lower.tri(cormatweek)]<- NA
  return(cormatweek)
}

upper_tri_week <- get_upper_tri_week(cormatweek)
melted_cormat_week <- melt(upper_tri_week, na.rm = TRUE)

ggheatmap <- ggplot(data = melted_cormat_week, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  theme(axis.text.y = element_text(vjust = 1, 
                                   size = 8, hjust = 1))
### add numbers
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.75),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

###################################################################
# MODEL 1: y=SalesGrowth, x=Bribes
model1 <- c("SalesGrowth", "Bribes", "InformalCompetition",
            "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
            "Foreign", "Export", "TrainingEmployees", "RD")

df_model1 <- df[model1]
df_model1 <- df_model1[apply(df_model1, 1, function(x) !any(is.na(x))),]
str(df_model1)

##Summary Stat.
stargazer(df_model3.3)

##Outliers
hist(df_model1$SalesGrowth)
ggplot(df_model3.3, aes(x=LaborProductivityGrowth)) + geom_histogram(binwidth=0.1)
hist(df_model1$Bribes)
ggplot(df_model3, aes(x=Bribes)) + geom_histogram(binwidth=0.05)
ggboxplot(df_model1, y = "SalesGrowth", width = 0.2)
ggboxplot(df_model1, y = "Bribes", width = 0.2)

###Bribes
df_model1 <- subset(df_model1,!(df_model1$Bribes > quantile(df_model1$Bribes, probs=c(.01, .99))[2] | df_model1$Bribes < quantile(df_model1$Bribes, probs=c(.01, .99))[1]) )
ggboxplot(df_model1, y = "Bribes", width = 0.2)

##Regression
ModelBase1 <- lm(SalesGrowth ~ Bribes, data=df_model1)
Model1.1.1 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD, data=df_model1)
Model1.1.2 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes, data=df_model1)
Model1.1.3 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes + PolicyObstacle, data=df_model1)
Model1.1.4 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes*PolicyObstacle, data=df_model1)
Model1.1.5 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes + InformalCompetition, data=df_model1)
Model1.1.6 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes*InformalCompetition, data=df_model1)

summary(Model1.1.6)
stargazer(Model1.1.1, Model1.1.2, Model1.1.3, Model1.1.4, Model1.1.5, Model1.1.6, title="Results of Model SB", align=TRUE, no.space=TRUE)

##Tests
###Multicollinearity
cor <- cor(df_model1$PolicyObstacle, df_model1$SalesGrowth, use = "complete.obs")
round(cor, digits=2)
correlation.matrix <- cor(df_model1[, ], use = "complete.obs")
stargazer(correlation.matrix, title="Correlation Matrix")

vif1 <- vif(Model1.1.6)
round(vif1, digits=2)

###Diagnostics
par(mfrow=c(2,2))
plot(Model1.1.1)
autoplot(Model1.1.2)

plot(Model1.1.5, 1) #Linearity
plot(df_model1$Bribes, df_model1$SalesGrowth)
plot(df_model1$Foreign, df_model1$SalesGrowth)
plot(df_model1$Export, df_model1$SalesGrowth)

plot(Model1.1.5, 2) #Normality
plot(Model1.1.5, 3) #Homoskedasticity
plot(Model1.1.5, 5) #Outliers

cooksd <- cooks.distance(Model1.1.2)
df_model1 %>%
  top_n(3, wt = cooksd)

###Heteroskedasticity
bptest(Model1.3.6)

###Normality
shapiro.test(resid(Model1.1.3))

# MODEL 1.2: y=SalesGrowth, x=BribeIndex
model1.2 <- c("SalesGrowth", "BribeIndex", "InformalCompetition",
              "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
              "Foreign", "Export", "TrainingEmployees", "RD")

df_model1.2 <- df[model1.2]
df_model1.2 <- df_model1.2[apply(df_model1.2, 1, function(x) !any(is.na(x))),]
##Summary Stats
stargazer(df_model1.2)

##Outliers
hist(df_model1$SalesGrowth)
ggboxplot(df_model1.2, y = "SalesGrowth", width = 0.2)

##Regression
ModelBase1.2 <- lm(SalesGrowth ~ BribeIndex, data=df_model1.2)
Model1.2.1 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD, data=df_model1.2)
Model1.2.2 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex, data=df_model1.2)
Model1.2.3 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex + PolicyObstacle, data=df_model1.2)
Model1.2.4 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex*PolicyObstacle, data=df_model1.2)
Model1.2.5 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex + InformalCompetition, data=df_model1.2)
Model1.2.6 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex*InformalCompetition, data=df_model1.2)

summary(ModelBase1.2)
summary(Model1.2.4)
stargazer(Model1.2.1, Model1.2.2, Model1.2.3, Model1.2.4, Model1.2.5, Model1.2.6, title="Results", align=TRUE, no.space=TRUE)

##Tests
###Multicollinearity
cor <- cor(df_model1.2$SalesGrowth, df_model1.2$InformalCompetition, use = "complete.obs")
round(cor, digits=2)
vif(Model1.2.6)
vif1.2 <-vif(Model1.2.6)
round(vif1.2, digits=2)


0###Diagnostics
par(mfrow=c(2,2))
autoplot(Model1.2.2) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model1.2.6)
df_model1.2 %>%
  top_n(3, wt = cooksd)

# MODEL 1.3: y=SalesGrowth, x=Inspection.Bribe
model1.3 <- c("SalesGrowth", "Inspection.Bribe", "InformalCompetition",
              "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
              "Foreign", "Export", "TrainingEmployees", "RD")

df_model1.3 <- df[model1.3]
df_model1.3 <- df_model1.3[apply(df_model1.3, 1, function(x) !any(is.na(x))),]

##Summary Stat.
stargazer(df_model1.3)

##Outliers
hist(df_model1.3$SalesGrowth)
ggboxplot(df_model1.3, y = "SalesGrowth", width = 0.2)

##Regression
ModelBase1.3 <- lm(SalesGrowth ~ Inspection.Bribe, data=df_model1.3)
Model1.3.1 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD, data=df_model1.3)
Model1.3.2 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe, data=df_model1.3)
Model1.3.3 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe + PolicyObstacle, data=df_model1.3)
Model1.3.4 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe*PolicyObstacle, data=df_model1.3)
Model1.3.5 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe + InformalCompetition, data=df_model1.3)
Model1.3.6 <- lm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe*InformalCompetition, data=df_model1.3)

summary(ModelBase1.3)
stargazer(Model1.3.1, Model1.3.2, Model1.3.3, Model1.3.4, Model1.3.5, Model1.3.6, title="Results", align=TRUE, no.space=TRUE)

##Tests
###Multicollinearity
cor <- cor(df_model1.3$PolicyObstacle, df_model1.3$Inspection.Bribe, use = "complete.obs")
round(cor, digits=2)
vif1.3 <- vif(Model1.3.6)
round(vif1.3, digits=2)


###Diagnostics
par(mfrow=c(2,2))
autoplot(Model1.3.2) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model1.3.6)
df_model1.3 %>%
  top_n(3, wt = cooksd)

###################################################################
# MODEL 2: y=EmploymentGrowth, x=Bribes
model2 <- c("EmploymentGrowth", "Bribes", "InformalCompetition", 
            "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
            "Foreign", "Export", "TrainingEmployees", "RD")

df_model2 <- df[model2]
df_model2 <- df_model2[apply(df_model2, 1, function(x) !any(is.na(x))),]

##Outliers
hist(df_model2$EmploymentGrowth)
ggboxplot(df_model2, y = "EmploymentGrowth", width = 0.2)
ggboxplot(df_model2, y = "Bribes", width = 0.2)

###Bribes
df_model2 <- subset(df_model2,!(df_model2$Bribes > quantile(df_model2$Bribes, probs=c(.01, .99))[2] | df_model2$Bribes < quantile(df_model2$Bribes, probs=c(.01, .99))[1]) )
ggboxplot(df_model2, y = "Bribes", width = 0.2)


##Regression
ModelBase2.1 <- lm(EmploymentGrowth ~ Bribes, data=df_model2)
Model2.1.1 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD, data=df_model2)
Model2.1.2 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes, data=df_model2)
Model2.1.3 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes + PolicyObstacle, data=df_model2)
Model2.1.4 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes*PolicyObstacle, data=df_model2)
Model2.1.5 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes + InformalCompetition, data=df_model2)
Model2.1.6 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes*InformalCompetition, data=df_model2)

summary(ModelBase2.1)
stargazer(Model2.1.1, Model2.1.2, Model2.1.3, Model2.1.4, Model2.1.5, Model2.1.6, title="Results", align=TRUE, no.space=TRUE)

##Tests
###Multicollinearity
cor <- cor(df_model2$Bribes, df_model2$PolicyObstacle, use = "complete.obs")
round(cor, digits=2)

vif2 <-vif(Model2.1.6)
round(vif2, digits=2)

###Diagnostics
par(mfrow=c(2,2))
autoplot(Model2.1.2) #or autoplot(model)
plot(df_model2$EmploymentGrowth, df_model2$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model2.1.2)
df_model2 %>%
  top_n(3, wt = cooksd)

# MODEL 2.2: y=EmploymentGrowth, x=BribeIndex
model2.2 <- c("EmploymentGrowth", "BribeIndex", "InformalCompetition", 
              "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
              "Foreign", "Export", "TrainingEmployees", "RD")

df_model2.2 <- df[model2.2]
df_model2.2 <- df_model2.2[apply(df_model2.2, 1, function(x) !any(is.na(x))),]

##Outliers
hist(df_model2.2$EmploymentGrowth)
ggboxplot(df_model2.2, y = "EmploymentGrowth", width = 0.2)

##Regression
ModelBase2.2 <- lm(EmploymentGrowth ~ BribeIndex, data=df_model2.2)
Model2.2.1 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD, data=df_model2.2)
Model2.2.2 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex, data=df_model2.2)
Model2.2.3 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex + PolicyObstacle, data=df_model2.2)
Model2.2.4 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex*PolicyObstacle, data=df_model2.2)
Model2.2.5 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex + InformalCompetition, data=df_model2.2)
Model2.2.6 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex*InformalCompetition, data=df_model2.2)

summary(Model2.2.3)
stargazer(Model2.2.1, Model2.2.2, Model2.2.3, Model2.2.4, Model2.2.5, Model2.2.6, title="Results", align=TRUE, no.space=TRUE)

##Tests
###Multicollinearity
cor <- cor(df_model2.2$BribeIndex, df_model2.2$PolicyObstacle, use = "complete.obs")
round(cor, digits=2)
vif2.2 <-vif(Model2.2.6)
round(vif2.2, digits=2)


###Diagnostics
par(mfrow=c(2,2))
autoplot(Model2.2.2) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model2.1.6)
df_model1 %>%
  top_n(3, wt = cooksd)

# MODEL 2.3: y=EmploymentGrowth, x=Inspection.Bribe
model2.3 <- c("EmploymentGrowth", "Inspection.Bribe", "InformalCompetition",
            "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
            "Foreign", "Export", "TrainingEmployees", "RD")

df_model2.3 <- df[model2.3]
df_model2.3 <- df_model2.3[apply(df_model2.3, 1, function(x) !any(is.na(x))),]

##Outliers
hist(df_model2.3$EmploymentGrowth)
ggboxplot(df_model2.3, y = "EmploymentGrowth", width = 0.2)

##Regression
ModelBase2.3 <- lm(EmploymentGrowth ~ Inspection.Bribe, data=df_model2.3)
Model2.3.1 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD, data=df_model2.3)
Model2.3.2 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe, data=df_model2.3)
Model2.3.3 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe + PolicyObstacle, data=df_model2.3)
Model2.3.4 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe*PolicyObstacle, data=df_model2.3)
Model2.3.5 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe + InformalCompetition, data=df_model2.3)
Model2.3.6 <- lm(EmploymentGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe*InformalCompetition, data=df_model2.3)

summary(Model2.3.3)
stargazer(Model2.3.1, Model2.3.2, Model2.3.3, Model2.3.4, Model2.3.5, Model2.3.6, title="Results", align=TRUE, no.space=TRUE)

##Tests
###Multicollinearity
cor <- cor(df_model2.3$PolicyObstacle, df_model2.3$Inspection.Bribe, use = "complete.obs")
round(cor, digits=2)
vif2.3 <-vif(Model2.3.6)
round(vif2.3, digits=2)


###Diagnostics
par(mfrow=c(2,2))
autoplot(Model2.3.2) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model2.1.6)
df_model1 %>%
  top_n(3, wt = cooksd)

#####################################################################
#MODEL 3: y=LaborProductivity
model3 <- c("LaborProductivityGrowth", "Bribes", "InformalCompetition",
            "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
            "Foreign", "Export", "TrainingEmployees", "RD")

df_model3 <- df[model3]
df_model3 <- df_model3[apply(df_model3, 1, function(x) !any(is.na(x))),]

##Outliers
hist(df_model3$LaborProductivityGrowth)
ggboxplot(df_model3, y = "LaborProductivityGrowth", width = 0.2)
ggboxplot(df_model3, y = "Bribes", width = 0.2)

###Bribes
df_model3 <- subset(df_model3,!(df_model3$Bribes > quantile(df_model3$Bribes, probs=c(.01, .99))[2] | df_model3$Bribes < quantile(df_model3$Bribes, probs=c(.01, .99))[1]) )
ggboxplot(df_model1, y = "Bribes", width = 0.2)

##Regression
ModelBase3.1 <- lm(LaborProductivityGrowth ~ Bribes, data=df_model3)
Model3.1.1 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD, data=df_model3)
Model3.1.2 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes, data=df_model3)
Model3.1.3 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes + PolicyObstacle, data=df_model3)
Model3.1.4 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes*PolicyObstacle, data=df_model3)
Model3.1.5 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes + InformalCompetition, data=df_model3)
Model3.1.6 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes*InformalCompetition, data=df_model3)

summary(Model3.1.2)
stargazer(Model3.1.1, Model3.1.2, Model3.1.3, Model3.1.4, Model3.1.5, Model3.1.6, title="Results", align=TRUE, no.space=TRUE)

##Tests
###Multicollinearity
cor <- cor(df_model3$Bribes, df_model3$PolicyObstacle, use = "complete.obs")
round(cor, digits=2)
vif(Model3.1.5)
vif3 <-vif(Model3.1.6)
round(vif3, digits=2)


###Diagnostics
par(mfrow=c(2,2))
autoplot(Model3.1.2) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model2.1.6)
df_model1 %>%
  top_n(3, wt = cooksd)

# Model 3.2:
model3.2 <- c("LaborProductivityGrowth", "BribeIndex", "InformalCompetition",
            "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
            "Foreign", "Export", "TrainingEmployees", "RD")

df_model3.2 <- df[model3.2]
df_model3.2 <- df_model3.2[apply(df_model3.2, 1, function(x) !any(is.na(x))),]

##Outliers
hist(df_model3.2$LaborProductivityGrowth)
ggboxplot(df_model3.2, y = "LaborProductivityGrowth", width = 0.2)

##Regression
ModelBase3.2 <- lm(LaborProductivityGrowth ~ BribeIndex, data=df_model3.2)
Model3.2.1 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD, data=df_model3.2)
Model3.2.2 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex, data=df_model3.2)
Model3.2.3 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex + PolicyObstacle, data=df_model3.2)
Model3.2.4 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex*PolicyObstacle, data=df_model3.2)
Model3.2.5 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex + InformalCompetition, data=df_model3.2)
Model3.2.6 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + BribeIndex*InformalCompetition, data=df_model3.2)

summary(Model3.2.2)
stargazer(Model3.2.1, Model3.2.2, Model3.2.3, Model3.2.4, Model3.2.5, Model3.2.6, title="Results", align=TRUE, no.space=TRUE)

##Tests
###Multicollinearity
cor <- cor(df_model3.2$BribeIndex, df_model3.2$PolicyObstacle, use = "complete.obs")
round(cor, digits=2)
vif3.2 <-vif(Model3.2.6)
round(vif3.2, digits=2)
vif1.2

###Diagnostics
par(mfrow=c(2,2))
autoplot(Model3.2.2) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model2.1.6)
df_model1 %>%
  top_n(3, wt = cooksd)

# Model 3.3:
model3.3 <- c("LaborProductivityGrowth", "Inspection.Bribe", "InformalCompetition",
            "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
            "Foreign", "Export", "TrainingEmployees", "RD")

df_model3.3 <- df[model3.3]
df_model3.3 <- df_model3.3[apply(df_model3.3, 1, function(x) !any(is.na(x))),]

##Outliers
hist(df_model3.3$LaborProductivityGrowth)
ggboxplot(df_model3.3, y = "LaborProductivityGrowth", width = 0.2)

##Regression
ModelBase3.3 <- lm(LaborProductivityGrowth ~ Inspection.Bribe, df_model3.3)
Model3.3.1 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD, data=df_model3.3)
Model3.3.2 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe, data=df_model3.3)
Model3.3.3 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe + PolicyObstacle, data=df_model3.3)
Model3.3.4 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe*PolicyObstacle, data=df_model3.3)
Model3.3.5 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe + InformalCompetition, data=df_model3.3)
Model3.3.6 <- lm(LaborProductivityGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Inspection.Bribe*InformalCompetition, data=df_model3.3)

summary(Model3.2.2)
stargazer(Model3.3.1, Model3.3.2, Model3.3.3, Model3.3.4, Model3.3.5, Model3.3.6, title="Results", align=TRUE, no.space=TRUE)

##Tests
###Multicollinearity
cor <- cor(df_model3.3$Inspection.Bribe, df_model3.3$PolicyObstacle, use = "complete.obs")
round(cor, digits=2)
vif3.3 <- vif(Model3.3.6)
round(vif3.3, digits=2)


###Diagnostics
par(mfrow=c(2,2))
autoplot(Model3.3.2) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model2.1.6)
df_model1 %>%
  top_n(3, wt = cooksd)
#####################################################################

#####################################################################
#MODEL 4: y=Innovation
model4 <- c("InnovationIndex", "Bribes", "InformalCompetition", 
            "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
            "Foreign", "Export", "TrainingEmployees", "RD", "QualityCertificate")

df_model4 <- df[model4]
df_model4 <- df_model4[apply(df_model4, 1, function(x) !any(is.na(x))),]

##Outliers
hist(df_model4$InnovationIndex)
ggboxplot(df_model4, y = "Bribes", width = 0.2)

###Bribes
df_model4 <- subset(df_model4,!(df_model4$Bribes > quantile(df_model4$Bribes, probs=c(.01, .99))[2] | df_model4$Bribes < quantile(df_model4$Bribes, probs=c(.01, .99))[1]) )
ggboxplot(df_model4, y = "Bribes", width = 0.2)

##Regression
ModelBase4.1 <- glm(InnovationIndex ~ Bribes, data=df_model4, family = binomial(link = "logit"))
Model4.1.1 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate, data=df_model4, family = binomial(link = "logit"))
Model4.1.2 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Bribes, data=df_model4, family = binomial(link = "logit"))
Model4.1.3 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Bribes + PolicyObstacle, data=df_model4, family = binomial(link = "logit"))
Model4.1.4 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Bribes*PolicyObstacle, data=df_model4, family = binomial(link = "logit"))
Model4.1.5 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Bribes + InformalCompetition, data=df_model4, family = binomial(link = "logit"))
Model4.1.6 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Bribes*InformalCompetition, data=df_model4, family = binomial(link = "logit"))

stargazer(Model4.1.1, coef = list(NULL, Model4.1.1_margin[,1]), se = list(NULL, Model4.1.1_margin[,2]))
stargazer(Model4.1.1)

summary(ModelBase4.1)
summary(Model4.1.6)
stargazer(Model4.1.1, Model4.1.2, Model4.1.3, Model4.1.4, Model4.1.5, Model4.1.6, title="Results", align=TRUE, no.space=TRUE)

##Correlation
cor <- cor(df_model4$Bribes, df_model4$PolicyObstacle, use = "complete.obs")
round(cor, digits=2)

###confinterval
logistic <- Model4.1.6
confint(logistic)
plot(logistic)

#### odds
exp(logistic$coefficients)

#### Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2 
ll.proposed <- logistic$deviance/-2

#### McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

#### chi-square value = 2*(LL(Proposed) - LL(Null))
#### p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)

##Tests
###Multicollinearity
vif(Model4.1.6)
vif1.2 <-vif(Model4.1.6)
vif1.2 <-round(vif1.2, digits=2)
vif1.2

###Diagnostics
par(mfrow=c(2,2))
plot(Model4.1.2) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model2.1.6)
df_model1 %>%
  top_n(3, wt = cooksd)

# Model 4.2
model4.2 <- c("InnovationIndex", "BribeIndex", "InformalCompetition",
            "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
            "Foreign", "Export", "TrainingEmployees", "RD", "QualityCertificate")

df_model4.2 <- df[model4.2]
df_model4.2 <- df_model4.2[apply(df_model4.2, 1, function(x) !any(is.na(x))),]

##Outliers

##Regression
ModelBase4.2 <- glm(InnovationIndex ~ BribeIndex, data=df_model4.2, family = binomial(link = "logit"))
Model4.2.1 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate, data=df_model4.2, family = binomial(link = "logit"))
Model4.2.2 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + BribeIndex, data=df_model4.2, family = binomial(link = "logit"))
Model4.2.3 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + BribeIndex + PolicyObstacle, data=df_model4.2, family = binomial(link = "logit"))
Model4.2.4 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + BribeIndex*PolicyObstacle, data=df_model4.2, family = binomial(link = "logit"))
Model4.2.5 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + BribeIndex + InformalCompetition, data=df_model4.2, family = binomial(link = "logit"))
Model4.2.6 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + BribeIndex*InformalCompetition, data=df_model4.2, family = binomial(link = "logit"))

summary(ModelBase4.2)
stargazer(Model4.2.1, Model4.2.2, Model4.2.3, Model4.2.4, Model4.2.5, Model4.2.6, title="Results", align=TRUE, no.space=TRUE)


##Correlation
cor <- cor(df_model4.2$BribeIndex, df_model4.2$PolicyObstacle, use = "complete.obs")
round(cor, digits=2)

###confinterval
logistic <- Model4.2.6
confint(logistic)
plot(logistic)

#### odds
exp(logistic$coefficients)

#### Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

#### McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

#### chi-square value = 2*(LL(Proposed) - LL(Null))
#### p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)

##Tests
###Multicollinearity
vif(Model4.2.6)
vif1.2 <-vif(Model1.2.6)
vif1.2 <-round(vif1.2, digits=2)
vif1.2

###Diagnostics
par(mfrow=c(2,2))
plot(Model4.2.5) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model2.1.6)
df_model1 %>%
  top_n(3, wt = cooksd)

# Model 4.3
model4.3 <- c("InnovationIndex", "Inspection.Bribe", "InformalCompetition",
            "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
            "Foreign", "Export", "TrainingEmployees", "RD", "QualityCertificate")

df_model4.3 <- df[model4.3]
df_model4.3 <- df_model4.3[apply(df_model4.3, 1, function(x) !any(is.na(x))),]

##Outliers

##Regression
ModelBase4.3 <- glm(InnovationIndex ~ Inspection.Bribe, data=df_model4.3, family = binomial(link = "logit"))
Model4.3.1 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate, data=df_model4.3, family = binomial(link = "logit"))
Model4.3.2 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Inspection.Bribe, data=df_model4.3, family = binomial(link = "logit"))
Model4.3.3 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Inspection.Bribe + PolicyObstacle, data=df_model4.3, family = binomial(link = "logit"))
Model4.3.4 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Inspection.Bribe*PolicyObstacle, data=df_model4.3, family = binomial(link = "logit"))
Model4.3.5 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Inspection.Bribe + InformalCompetition, data=df_model4.3, family = binomial(link = "logit"))
Model4.3.6 <- glm(InnovationIndex ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + QualityCertificate + Inspection.Bribe*InformalCompetition, data=df_model4.3, family = binomial(link = "logit"))

summary(ModelBase4.3)
stargazer(Model4.3.1, Model4.3.2, Model4.3.3, Model4.3.4, Model4.3.5, Model4.3.6, title="Results", align=TRUE, no.space=TRUE)

##Correlation
cor <- cor(df_model4.3$Inspection.Bribe, df_model4.3$PolicyObstacle, use = "complete.obs")
round(cor, digits=2)

###confinterval
logistic <- Model4.3.6
confint(logistic)
plot(logistic)

#### odds
exp(logistic$coefficients)

#### Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

#### McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

#### chi-square value = 2*(LL(Proposed) - LL(Null))
#### p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)

##Tests
###Multicollinearity
vif1.2 <-vif(Model1.2.6)
vif1.2 <-round(vif1.2, digits=2)
vif1.2

###Diagnostics
par(mfrow=c(2,2))
plot(Model4.3.5) #or autoplot(model)
plot(df_model1$SalesGrowth, df_model1$Bribes)

plot(Model2.1.6, 1)
plot(Model1.1.5, 2)
plot(Model1.1.5, 4)
plot(Model1.1.5, 5)

cooksd <- cooks.distance(Model2.1.6)
df_model1 %>%
  top_n(3, wt = cooksd)

#####################################################################
stargazer(Model1.1.5, Model2.1.5, Model3.1.5, Model4.1.4, title="Results", align=TRUE, no.space=TRUE)

stargazer(Model1.2.5, Model2.2.5, Model3.2.5, Model4.2.4, title="Results", align=TRUE, no.space=TRUE)

stargazer(Model1.3.5, Model2.3.5, Model3.3.5, Model4.3.4, title="Results", align=TRUE, no.space=TRUE)

stargazer(Model1.4.3, Model2.4.3, Model3.1.3, Model4.1.3, title="Results", align=TRUE, no.space=TRUE)
#####################################################################
# Instrumental Variable Approach
library(AER)
library(ivpack)

modelX <- c("SalesGrowth", "InformalCompetition", "BribeIndex", "Bribes",
              "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
              "Foreign", "Export", "RD", "TrainingEmployees", "Network")

df_modelX <- df[modelX]
df_modelX <- df_modelX[apply(df_modelX, 1, function(x) !any(is.na(x))),]

###SalesGrowth
outliers <- boxplot(df_modelX$SalesGrowth, plot=FALSE)$out
df_modelX <- df_modelX[-which(df_modelX$SalesGrowth %in% outliers),]
ggboxplot(df_modelX, y = "SalesGrowth", width = 0.2)

###Bribes
df_modelX <- subset(df_modelX,!(df_modelX$Bribes > quantile(df_modelX$Bribes, probs=c(.01, .99))[2] | df_modelX$Bribes < quantile(df_modelX$Bribes, probs=c(.01, .99))[1]) )
df_model1 <- subset(df_model1,!(df_model1$Bribes > quantile(df_model1$Bribes, probs=c(.1, .9))[2]) )
ggboxplot(df_model1, y = "Bribes", width = 0.2)

iv1 = ivreg(SalesGrowth ~ Bribes + Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + PolicyObstacle + InformalCompetition | Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + PolicyObstacle + InformalCompetition + BribeIndex, 
            data=df_modelX)

iv1 = ivreg(SalesGrowth ~ Bribes | BribeIndex, data=df_modelX)

summary(iv1, vcov = sandwich, diagnostics = TRUE)
#####################################################################
#Sure Regression
install.packages("systemfit")
library("systemfit")
Y1 <- df$SalesGrowth
Y2 <- df$EmploymentGrowth
Y3 <- df$LaborProductivityGrowth

X1 <- df$Bribes
X2 <- df$BribeIndex
X3 <- df$Inspection.Bribe

eq1 <-  Y1 ~ X1
eq2 <- Y2 ~ X2

system <- list(eq1 = eq1, eq2 = eq2)

sur <- systemfit(system, method="SUR", data=df)
summary(sur)

restriction <- "eq1_femalefemale- mathreg_femalefemale"
linearHypothesis(fitsur, restriction, test = "Chisq")

summary(rr.huber <- rlm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes, data=df_model1))
rr.bisquare <- rlm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes, data=df_model1, psi = psi.bisquare)
summary(rr.bisquare)

library(lmtest)
library(sandwich)
coeftest(Model1.3.6, vcov = vcovHC(Model1.3.6, type="HC0"))

heavy <- heavyLm(SalesGrowth ~ Sector + Small + Medium + lnAge + lnExperience + Foreign + Export + TrainingEmployees + RD + Bribes, data=df_model1, family = Student(df = 4))
summary(heavy)
#####################################################################
library(sjPlot)
library(sjmisc)
theme_set(theme_sjplot())

plot_model(Model1.2.4, type = "pred", terms = c("Bribes", "PolicyObstacle"))

tips %>% 
  ggplot() +
  aes(x = Bribes, color = PolicyObstacle, group = PolicyObstacle, y = SalesGrowth, data=df_model1) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

#####################################################################
plot(predictorEffects(Model4.1.4),axes=list(grid=TRUE, x=list(rug=FALSE, rotate=35)))

e1_glm <- predictorEffect("Bribes", Model4.1.4)
plot(e1_glm, main="type='response'",
     axes=list(y=list(type="response",
     lab="probability scale, probability labels"),
     x=list(rotate=30),
     grid=TRUE))

plot(e1_glm, main="type='link'",
     axes=list(y=list(type="link",
                      lab="logit scale, logit labels"),
               x=list(rotate=30),
               grid=TRUE))

plot(e1_glm, main="type='rescale'",
     axes=list(y=list(type="rescale",
                      lab="logit scale, probability labels"),
               x=list(rotate=30),
               grid=TRUE))


#####################################################################
##########################
library(effects)

#Run the interaction 
Inter.HandPick <- effect('Bribes*PolicyObstacle', Model4.1.4,
                         xlevels=list(PolicyObstacle = c(0, 1, 2, 3, 4),
                                      Bribes = c(0, 0.2)),
                         se=TRUE, confidence.level=.95, typical=mean)

#Put data in data frame 
Inter.HandPick <- as.data.frame(Inter.HandPick)

#Check out what the "head" (first 6 rows) of your data looks like
head(Inter.HandPick)

#Create a factor of the IQ variable used in the interaction                   
Inter.HandPick$PolicyObstacle <- factor(Inter.HandPick$PolicyObstacle,
                            levels=c(0, 1, 2, 3, 4),
                            labels=c("No", "Minor", "Moderate", "Major", "Very severe"))

#Create a factor of the Work Ethic variable used in the interaction 
Inter.HandPick$Inspection.Bribe <- factor(Inter.HandPick$Bribes,
                                    levels=c(0, 0.2),
                                    labels=c("No", "Yes"))

Plot.HandPick<-ggplot(data=Inter.HandPick, aes(x=Bribes, y=fit, group=PolicyObstacle))+
  geom_line(size=1, aes(color=PolicyObstacle))+
  #scale_color_manual(values=wes_palette(n=5, name="Darjeeling2"))+
  scale_color_brewer(palette="RdYlGn", direction = -1)+
  ylim(0,1)+
  ylab("Innovation Index (probability scale)")+
  xlab("Bribes")+
  ggtitle("Interaction Effect (Model IB)")+
  theme_stata(base_size = 10.7)


Plot.HandPick 
#################################
#Run the interaction 
Inter.HandPick2 <- effect('Bribes*InformalCompetition', Model4.1.6,
                         xlevels=list(InformalCompetition = c(1, 0),
                                      Bribes = c(0, 0.2)),
                         se=TRUE, confidence.level=.95, typical=mean)

#Put data in data frame 
Inter.HandPick2 <- as.data.frame(Inter.HandPick2)

#Check out what the "head" (first 6 rows) of your data looks like
head(Inter.HandPick2)

#Create a factor of the IQ variable used in the interaction                   
Inter.HandPick2$InformalCompetition <- factor(Inter.HandPick2$InformalCompetition,
                                        levels=c(1, 0),
                                        labels=c("Yes", "No"))

#Create a factor of the Work Ethic variable used in the interaction 
Inter.HandPick2$Bribes <- factor(Inter.HandPick2$Bribes,
                                levels=c(0, 0.2),
                                labels=c("No", "Yes"))

Plot.HandPick2<-ggplot(data=Inter.HandPick2, aes(x=Bribes, y=fit, group=InformalCompetition))+
  geom_line(size=1, aes(color=InformalCompetition))+
  scale_color_manual(name = "Informal Competition", values=c("red", "darkgreen"), labels = c("Yes", "No"))+
  ylim(0,1)+
  ylab("Innovation Index (probability scale)")+
  xlab("Bribes")+
  ggtitle("Interaction Effect (Model IB)")+
  theme_stata(base_size = 10.7)

Plot.HandPick2

#################################
e <- effects::effect("Inspection.Bribe*InformalCompetition", Model4.3.6)
e <- as.data.frame(e)
ggplot2::ggplot(e, ggplot2::aes(Inspection.Bribe, fit, color=InformalCompetition, group = InformalCompetition)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_line() +
  theme_stata()
#################################
