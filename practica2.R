
library(rminer)
library(pROC)

# carga de datos
heart <- read.csv(file = 'heart.csv')

#---------Apartado 3.1 Limpieza de los datos. ¿Los datos contienen ceros o elementos vacíos? Gestiona cada uno de estos casos

#formateo de variables categoricas"
#https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset/discussion/329925

#Se desconoce cómo está codificado la variable sexo
heart$sex<-as.factor(heart$sex)

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

heart$thall<-as.factor(heart$thall)
levels(heart$thall)<-c("null","fixed defect","normal","reversable defect")

#Output es la variable que vamos a utilizar para comparar si nuestro modelo opina lo mismo
heart$output<-as.factor(heart$output)
levels(heart$output)<-c("Less chance","More chance")


#Chequeo de categorias
print("##Variables categóricas")
print("Variable sex: Sex of the patient")
print(levels(heart$sex))
print("Variable Exng: exercise induced angina")
print(levels(heart$exng))
print("Variable cp: Chest Pain type")
print(levels(heart$cp))
print("Variable fbs: fasting blood sugar higher than 120 mg/dl")
print(levels(heart$fbs))
print("Variable restecg: resting electrocardiographic results")
print(levels(heart$restecg))
print("Variable slp: the slope of the peak exercise ST segment")
print(levels(heart$slp))
print("Variable output: diagnosis of heart disease (angiographic disease status)")
print(levels(heart$output))

#no hay registros con columnas vacías
print("##Chequeo de columnas vacias")
print(sum(is.na(heart)))

#---------Apartado 3.2. Identifica y gestiona los valores extremos

print("##Chequeo de outliers")
#No hay valores extremos en age
print("Variable age: Age of the patient")
print(summary(heart$age))
#No hay valores extremos en trtbps
print("Variable trtbps : resting blood pressure (in mm Hg)")
print(summary(heart$trtbps))

print("Variable chol : cholesterol in mg/dl fetched via BMI sensor")
print(summary(heart$chol))

print("Variable oldpeak : ST depression induced by exercise relative to rest")
print(summary(heart$oldpeak))

print("Variable caa : number of major vessels (0-3)")
print(summary(heart$caa))

#---------4. Análisis de los datos

#Separacion

set.seed(9)
h<-holdout(heart$output,ratio=0.8)
heartTraining<-heart[h$tr,]
heartTest<-heart[h$ts,]

#---------4.1 Selección de los grupos de datos que se quieren analizar/comparar

Modelo=glm(formula=output~sex+cp+trtbps+thalachh+exng+oldpeak+caa,
           data=heartTraining,
           family=binomial)

summary(Modelo)$coefficients





#---------4.2 Comprobación de la normalidad y homogeneidad de la varianza

#---------4.3 Aplicación de pruebas estadísticas para comparar los grupos de datos.
# En función de los datos y el objetivo del estudio, aplicar pruebas de
#contraste de hipótesis, correlaciones, regresiones, etc. Aplicar al menos
#tres métodos de análisis diferentes.


output_predicted <- predict(Modelo,newdata=heartTest,type='response')

#Se asume que la predicción del modelo es 1 si el modelo de regresión logística es 0.8 o mayor.
output_predicted <- ifelse(output_predicted > 0.8,1,0)

matrix_confusion<-table(output_predicted,heartTest$output)

print(matrix_confusion)

test_roc = roc(heartTest$output~output_predicted, plot = TRUE, print.auc = TRUE)

auc(test_roc)

