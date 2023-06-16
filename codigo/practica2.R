knitr::opts_chunk$set(echo = TRUE)

library("gridExtra")
library(dplyr)
library(rminer)
library(pROC)
library(corrplot)
library(RColorBrewer)
require(ggplot2)
if (!require('GGally')) install.packages('GGally')
library(GGally)
library(knitr)
library(kableExtra)
library(car)
library(caret)
library("DescTools")

# carga de datos
heart <- read.csv(file = 'heart.csv')

structure = str(heart)

head(heart, 5)

heart.original<-heart
heart$sex<-as.factor(heart$sex)
levels(heart$sex)<-c("Female","Male")

heart$exng<-as.factor(heart$exng)
levels(heart$exng)<-c("No","Yes")

heart$cp<-as.factor(heart$cp)
levels(heart$cp)<-c("Tipical angina","Atypical angina","Non-anginal pain","Asymptomatic")

heart$fbs<-as.factor(heart$fbs)
levels(heart$fbs)<-c("False","True")

heart$restecg<-as.factor(heart$restecg)
levels(heart$restecg)<-c("Normal","ST-T wave abnormality","Left ventricular hypertrophy")

heart$slp<-as.factor(heart$slp)
levels(heart$slp)<-c("unsloping","flat","downsloping")

heart$caa<-as.factor(heart$caa)

heart$thall<-as.factor(heart$thall)
levels(heart$thall)<-c("null","fixed defect","normal","reversable defect")

heart$trtbps<-as.numeric(heart$trtbps)
heart$chol<-as.numeric(heart$chol)
heart$thalachh<-as.numeric(heart$thalachh)

#Output es la variable que vamos a utilizar para comparar si nuestro
heart$output<-as.factor(heart$output)
levels(heart$output)<-c("no disease","disease")


tabla<-data.frame(variable = names(heart),
           Original = sapply(heart.original, class),
           Final = sapply(heart, class),
           row.names = NULL)
names(tabla)<-c("Variable","Clase original","Clase modificada")
kable(tabla,format="html") %>% kable_styling(bootstrap_options = "striped")

summary(heart)

print(sum(is.na(heart)))

sum(duplicated(heart))
heart<-heart[!duplicated(heart),]

#heart.old<-heart

numericas<-c("age","trtbps","chol","thalachh","oldpeak")

rows2Delete<-c()

for (columnName in numericas) {
  q1<-quantile(heart[[columnName]],0.25)
  q3<-quantile(heart[[columnName]],0.75)
  iqr<-q3-q1

  lower<-q1-iqr*1.5
  higher<-q3+iqr*1.5  
  
  outliers_lower<-which(heart[[columnName]]<lower)
  outliers_higher<-which(heart[[columnName]]>higher)
  if (length(outliers_higher) | length(outliers_higher)){
    rows2Delete<-append(rows2Delete,outliers_lower)
    rows2Delete<-append(rows2Delete,outliers_higher)
  }
}

#heart.todrop<-heart[rows2Delete,]
#heart.todrop<-heart[-rows2Delete,]
#View(heart.todrop)

heart<-heart[-rows2Delete,]


par(mfrow=c(1,2), oma=c(0,0,2,0))
boxplot(heart.original$trtbps,main="Con outliers")
boxplot(heart$trtbps,main="Sin outliers")
mtext("Variable trtbps",line=0, side=3, outer=TRUE,cex=2)

par(mfrow=c(1,2), oma=c(0,0,2,0))
boxplot(heart.original$chol, main="Con outliers")
boxplot(heart$chol, main="Sin outliers")
mtext("Variable chol",line=0, side=3, outer=TRUE,cex=2)

par(mfrow=c(1,2), oma=c(0,0,2,0))
boxplot(heart.original$oldpeak, main="Con outliers")
boxplot(heart$oldpeak, main="Sin outliers")
mtext("Variable oldpeak",line=0, side=3, outer=TRUE,cex=2)

table(heart$output)

ggplot(heart, aes(x=output, fill=output)) +
  geom_bar() +  
  labs(x='Result') + 
  ggtitle("Diagnóstico de enfermedad cardíaca") + 
  theme(legend.position = "none")

table(heart$output,heart$sex)

ggplot(heart, aes(x=output,fill=sex)) +
  geom_bar() +  
  labs(x='Result') + 
  scale_x_discrete(labels=c("No heart disease","Heart disease")) +
  scale_y_discrete(labels=c("Mujer","Hombre")) +
  ggtitle("Diagnóstico de enfermedad cardíaca por sexo")

componentesPrin <- prcomp(heart[,numericas], center = TRUE, scale = TRUE)
summary(componentesPrin)

ggplot(heart, aes(x=age,fill=output)) +
     geom_histogram(bins=10) +  
     labs(x='Edad') + 
     ggtitle("Diagnóstico de enfermedad cardíaca por edad")

#heart.disease<-heart[which(heart$output=="disease"),]

plot.hombres<-ggplot(heart[heart$sex=="Male",], aes(x=age,fill=output)) +
     geom_histogram(bins=10) +  
     labs(x='Edad') + 
     ggtitle("Sexo masculino")+
  scale_x_continuous(limits=c(25, 80)) +
  theme(legend.position="bottom")

plot.mujeres<-ggplot(heart[heart$sex=="Female",], aes(x=age,fill=output)) +
     geom_histogram(bins=10) +  
     labs(x='Edad') + 
     ggtitle("Sexo femenino")+
  scale_x_continuous(limits=c(25, 80)) +
  theme(legend.position="bottom")

require(gridExtra)
grid.arrange(plot.hombres, plot.mujeres, ncol=2, top = "Diagnostico enfermedad cardiaca por edad y sexo")

plot.hombres<-ggplot(heart[heart$sex=="Male",],aes(x=age,fill=output)) +
     geom_bar(position="fill") +
     labs(x='Edad',y='') +
  scale_fill_manual(values = c("#FFFFFF", "#6A51A3")) +
  scale_x_continuous(limits=c(25, 80)) +
     ggtitle("Sexo masculino") +
  theme(legend.position = "none",
        panel.background=element_rect(fill = '#FFFFFF'))

plot.mujeres<-ggplot(heart[heart$sex=="Female",], aes(x=age,fill=output)) +
     geom_bar(position="fill") +
     labs(x='Edad',y='') +
  scale_fill_manual(values = c("#FFFFFF", "#6A51A3")) +
  scale_x_continuous(limits=c(25, 80)) +
     ggtitle("Sexo femenino") + 
  theme(legend.position = "none",
        panel.background=element_rect(fill = '#FFFFFF'))
require(gridExtra)
grid.arrange(plot.hombres, plot.mujeres, ncol=2, top = "Porcentaje de enfermos cardiacos por edad y sexo")

par(mfrow=c(2,2))
for(i in 1:ncol(heart)) {
  if (is.numeric(heart[,i])){
    qqnorm(heart[,i],main = paste("Normal Q-Q Plot for ",colnames(heart)[i]))
    qqline(heart[,i],col="red")
    hist(heart[,i], 
      main=paste("Histogram for ", colnames(heart)[i]), 
      xlab=colnames(heart)[i], freq = FALSE)
  }
}

for(i in numericas) {
   cat("P-valor Shapiro test para variable",
       i,
       ": ",
       shapiro.test(heart[[i]])$p.value,"\n")
}

heart.norm<-heart
heart.norm$age<-BoxCox(heart$age,lambda = BoxCoxLambda(heart$age))
heart.norm$trtbps<-BoxCox(heart$trtbps,lambda = BoxCoxLambda(heart$trtbps))
heart.norm$thalachh<-BoxCox(heart$thalachh,lambda = BoxCoxLambda(heart$thalachh))
heart.norm$oldpeak<-BoxCox(heart$oldpeak,lambda = BoxCoxLambda(heart$oldpeak))

for(i in numericas) {
   cat("P-valor Shapiro test para variable",
       i,
       ": ",
       shapiro.test(heart.norm[[i]])$p.value,"\n")
}

heart$age<-heart.norm$age
heart$trtbps<-heart.norm$trtbps
heart$thalachh<-heart.norm$thalachh

table(heart$sex,heart$output)

chisq.test(table(heart$sex,heart$output))

leveneTest(y = heart$age, group = heart$sex, center = "median")

heart.disease<-heart[which(heart$output=="disease"),]
t.test(heart.disease$age[heart.disease$sex=="Male"],heart.disease$age[heart.disease$sex=="Female"],alternative="two.sided")

set.seed(9)
h<-holdout(heart$output,ratio=0.8)
heartTraining<-heart[h$tr,]
heartTest<-heart[h$ts,]

cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(heart.original)

M<-cor(heart.original)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
         )

Modelo=glm(
  formula=output~sex+age+cp+thalachh+exng+oldpeak+slp+caa+thall,
           data=heartTraining,
           family=binomial)

dev <- Modelo$deviance
nullDev <- Modelo$null.deviance
Chi_Obs <- nullDev - dev
Chi_Obs


plot(Modelo,which=1)

plot(Modelo,which=2)

output_predicted <- predict(Modelo,newdata=heartTest,type='response')

#Se asume que la predicción del modelo es 1 si el modelo de regresión logística es 0.8 o mayor.
output_predicted <- ifelse(output_predicted > 0.8,1,0)

matrix_confusion<-table(output_predicted,heartTest$output)
matrix_confusion

test_roc = roc(heartTest$output~output_predicted, plot = TRUE, print.auc = TRUE)

auc(test_roc)

#Se guarda el dataset trabajado
write.csv(heart, "heartFinal.csv", row.names=FALSE)

