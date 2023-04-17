
# Kód célja: Tudományos Diákköri Konferencia dolgozat (melléklet)
# Év: 2023.
# Dolgozat cím: Ügyfelek elvándorlási viselkedésének vizsgálata egy fiktív távközlési szolgáltató nyomán 
#               – statisztikai és gépi tanuló modellek segítségével
# Név: Sumicz Benedek Máté
# Szak: Gazdaságinformatikus BSc, IV. évfolyam
# Egyetem: Budapesti Corvinus Egyetem 

getwd()
setwd("C:/Users/sumbe/Desktop/Suli/8. félév/Szakszeminárium II")


### Adatok beolvasása --------------------------------------------------------
library(readxl)
df <- read_excel("telco_churn.xlsx", sheet = "Data")
str(df)
ncol(df) # 21 változó 
nrow(df) # 7043 megfigyelés

### Kategorikus változók faktorizálása ------------------------------------------
# Ügyfél egyedi azonosítója
df$customerID <- as.factor(df$customerID)

# Ügyfél neme
df$gender <- as.factor(df$gender)

# Szenior állampolgár (igen, ha az ügyfél 65 éves vagy annál idősebb) (0 = Nem, 1 = Igen)
df$SeniorCitizen <- as.factor(df$SeniorCitizen)

# Partner (ügyfél házas-e) (0 = Nem, 1 = Igen)
df$Partner <- as.factor(df$Partner)

# Eltartott (ügyfél "rendelkezik-e" eltartottal -> eltartottak lehetnek gyerekek, szülők, nagyszülők stb.) 
# (0 = Nem, 1 = Igen)
df$Dependents <- as.factor(df$Dependents)

# Otthoni telefon szolgáltatás előfizetés (0 = Nincs, 1 = Van)
df$PhoneService <- as.factor(df$PhoneService)

# Több telefonos előfizetés (0 = No phone service, 1 = Nincs, 2 = Van)
df$MultipleLines <- as.factor(df$MultipleLines)

# Internet szolgáltatás előfizetés (No, DSL, Fiber optic)
df$InternetService <- as.factor(df$InternetService)

# Addicionális online biztonsági szolgáltatás igénybevétele 
# (0 = No phone service, 1 = Nincs, 2 = Van)
df$OnlineSecurity <- as.factor(df$OnlineSecurity)

# Addicionális online biztonsági mentés szolgáltatás igénybevétele
# (0 = No phone service, 1 = Nincs, 2 = Van)
df$OnlineBackup <- as.factor(df$OnlineBackup)

# Addicionális eszközvédelmi terv előfizetés az internetes berendezésre
# (0 = No phone service, 1 = Nincs, 2 = Van)
df$DeviceProtection <- as.factor(df$DeviceProtection)

# Addicionális technikai support igénybevétele csökkentett várakozási idővel
# (0 = No phone service, 1 = Nincs, 2 = Van)
df$TechSupport <- as.factor(df$TechSupport)

# Internet szolgáltatás 3rd party TV műsorok streameléséhez (nincs extra díja!)
# (0 = No phone service, 1 = Nincs, 2 = Van)
df$StreamingTV <- as.factor(df$StreamingTV)

# Internet szolgáltatás 3rd party filmek streameléséhez (nincs extra díja!)
# (0 = No phone service, 1 = Nincs, 2 = Van)
df$StreamingMovies <- as.factor(df$StreamingMovies)

# Ügyfél szerződési konstrukciója
df$Contract <- as.factor(df$Contract)

# Papírmentes számla
df$PaperlessBilling <- as.factor(df$PaperlessBilling)

# Fizetési mód
df$PaymentMethod <- as.factor(df$PaymentMethod)

# Ügyfél lechurnölt-e az aktuális negyedévben
df$Churn <- as.factor(df$Churn)

str(df) # sikeresen faktorizálva minden kategorikus változó

# Hiányzó adatok kezelése
df <- na.omit(df)


### Leíró statisztika ---------------------------------------------------------
summary(df)

psych::describe(df[,c(6,19,20)]) # numerikus változók
library("ggplot2")

# Néhány numerikus változó leíró jellemzése
#1. Tenure leíró mutatói és hisztogramja (5. ábra)
psych::describe(df$tenure)

ggplot(df) + 
  geom_histogram(aes(x = tenure), color = "darkblue", fill = "skyblue", bins = 12L) +
  ggtitle("Ügyfél-hűségidő gyakorisági eloszlása", subtitle = "Hónapok száma szerint") +
  xlab("Ügyfél-hűségidő") +
  ylab("Ügyfelek száma")

# Sűrűségfüggvény
ggplot(df, aes(x = tenure)) + 
  geom_histogram(aes(y=..density..), color = "darkblue", fill = "skyblue", bins = 12L) +
  geom_density(alpha=0.2, fill="#FF6666") +
  ggtitle("Ügyfél-hűségidő gyakorisági eloszlása", subtitle = "Hónapok száma szerint") +
  xlab("Ügyfél-hűségidő") +
  ylab("Ügyfelek száma")
  
#2. MonthlyCharges leíró mutatói és hisztogramja (6. ábra)
psych::describe(df$MonthlyCharges)

ggplot(df) + 
  geom_histogram(aes(x = MonthlyCharges), color = "darkblue", fill = "skyblue", bins = 12L) +
  ggtitle("Havi díjak gyakorisági eloszlása", subtitle = "Hónapok száma szerint") +
  xlab("Havi díj (USD)") +
  ylab("Ügyfelek száma")


### Túléléselemzés Kaplan-Meier becslővel--------------------------------------

#1. Szükséges könyvtárak
library(survival)
library(survminer)
library(dplyr)
library(ggfortify)
library(ggsurvfit)

#2. Túlélések becslése Kaplan-Meier módszerrel
glimpse(df)
surv_object <- Surv(time = df$tenure, event = df$Churn)
surv_object 

fit1 <- survfit(surv_object ~ 1, data = df)
summary(fit1)

# Összegző tábla szöveges elemzésbe (5. táblázat)
fit1_table <- data.frame(
  ti = c(6,12,18,24,30,36,42,48,54,60,66,72),
  ni = fit1$n.risk[c(6,12,18,24,30,36,42,48,54,60,66,72),1],
  di = fit1$n.event[c(6,12,18,24,30,36,42,48,54,60,66,72),2],
  "S(t)" = round(fit1$pstate[c(6,12,18,24,30,36,42,48,54,60,66,72),1],3),
  lower =  round(fit1$lower[c(6,12,18,24,30,36,42,48,54,60,66,72),1],3),
  upper = round(fit1$upper[c(6,12,18,24,30,36,42,48,54,60,66,72),1],3)
)
View(fit1_table)

# Segédtábla 7. ábrához
km <- data.frame(
  t = 0:72,
  surv = round(c(1,fit1$pstate[,1]),3),
  lower =  round(c(1,fit1$lower[,1]),3),
  upper =  round(c(1,fit1$upper[,1]),3)
)

# Túlélési görbe (7. ábra)
ggplot(km, aes(x = t, y = surv)) +
  geom_step(color = "#00bf7d", size = 0.75) +
  geom_step(aes(x = t, y = lower), color = "#004d40", linetype = "dashed") +
  geom_step(aes(x = t, y = upper), color = "#004d40", linetype = "dashed") +
  geom_point(color = "#00bf7d") +
  ggtitle("Kaplan-Meier túlélési görbe", subtitle = "Túlélések modellezésére a vizsgált időtartományban") +
  xlab("Eltelt hónapok száma") +
  ylab("Túlélési valószínűség")


#3. Bontás1: Contract type szerinti túlélési valószínűségek becslése
fit2 <- survfit(surv_object ~ Contract, data = df)
summary(fit2)

# Segédtábla - Contract type szerinti bontás (8. ábrához)
km2 <- data.frame(
  t = 0:72,
  mtm = round(c(1,fit2$pstate[1:72,1]),3),
  oy =  round(c(1,fit2$pstate[73:144,1]),3),
  ty =  round(c(1,fit2$pstate[145:216,1]),3)
)

# Szerződéstípusok túlélési görbéi (8.ábra)
ggplot(km2, aes(x = t)) +
  geom_step(aes(y = mtm, colour = "Month-to-month"), size = 0.75) +
  geom_step(aes(y = oy, colour = "One-year"), size = 0.75) +
  geom_step(aes(y = ty, colour = "Two-year"), size = 0.75) +
  scale_color_manual(name = 'Contract type', 
                     values = c('Month-to-month' = "#388e3c", 
                                'One-year' = "#1976d2",
                                'Two-year' = "#d32f2f")) +
  labs(color = 'Contract type') +
  geom_vline(xintercept = 24, colour="#212121", linetype = "dashed") +
  ggtitle("Kaplan-Meier túlélési görbe", subtitle = "A túlélési valószínűség szerződési konstrukciók szerinti modellezésére") +
  xlab("Eltelt hónapok száma") +
  ylab("Túlélési valószínűség") +
  theme(legend.position="bottom", legend.box = "horizontal")


#4. Bontás2: SeniorCitizen szerinti túlélési valószínűségek becslése
fit3 <- survfit(surv_object ~ SeniorCitizen, data = df)
summary(fit3)

# Segédtábla - SeniorCitizen szerinti bontás (9. ábrához)
km3 <- data.frame(
  t = 0:72,
  notsenior = round(c(1,fit3$pstate[1:72,1]),3),
  notsenior_lower = round(c(1,fit3$lower[1:72,1]),3),
  notsenior_upper = round(c(1,fit3$upper[1:72,1]),3),
  senior =  round(c(1,fit3$pstate[73:144,1]),3),
  senior_lower = round(c(1,fit3$lower[73:144,1]),3),
  senior_upper = round(c(1,fit3$upper[73:144,1]),3)
)

# SeniorCitizen túlélési görbéi (9.ábra)
ggplot(km3, aes(x = t)) +
  geom_step(aes(y = notsenior, colour = "Not senior"), size = 0.75) +
  geom_step(aes(y = notsenior_lower), linetype = "dotted", size = 0.25) +
  geom_step(aes(y = notsenior_upper), linetype = "dotted", size = 0.25) +
  geom_step(aes(y = senior, colour = "Senior"), size = 0.75) +
  geom_step(aes(y = senior_lower), linetype = "dotted", size = 0.25) +
  geom_step(aes(y = senior_upper), linetype = "dotted", size = 0.25) +
  ggtitle("Kaplan-Meier túlélési görbe", subtitle = "A túlélési valószínűség szenior státusz szerinti modellezésére") +
  xlab("Eltelt hónapok száma") +
  ylab("Túlélési valószínűség") +
  labs(color = 'SeniorCitizen')


### ML algoritmusok 

### Logisztikus regresszió ----------------------------------------------------

#1: Példa reprodukálhatósága
set.seed(1)

#2: Adatkészlet felosztása tanuló- és teszthalmazra 
sample <- sample(c(TRUE,FALSE), nrow(df), replace = TRUE, prob = c(0.8,0.2))
train <- df[sample,]
test <- df[!sample,]

#3: Logit modell elkészítése
#    a.) Logit1 modell: túlélés elemzés változóiból (tenure, Contract, SeniorCitizen)
logit1 <- glm(Churn ~ tenure 
              + Contract 
              + SeniorCitizen, data=train, family = binomial(link = "logit"))
summary(logit1)


#    b.) Logit2 (bővített) modell: Logit1 + MonthlyCharges
logit2 <- glm(Churn ~ tenure + MonthlyCharges
              + Contract 
              + SeniorCitizen, data=train, family = binomial(link = "logit"))
summary(logit2)


#    c.) Logit3 (bővített) modell: alapszolgáltatások relevanciája (Logit2 + PhoneService + InternetService) 
df$PhoneService <- relevel(df$PhoneService, ref = "0")
df$InternetService <- relevel(df$InternetService, ref= "No")

logit3 <- glm(Churn ~ tenure + MonthlyCharges
              + Contract 
              + SeniorCitizen
              + PhoneService
              + InternetService, data=train, family = binomial(link = "logit"))
summary(logit3)


#    d.) Logit4 (bővített) modell: fizetős kieg.szolgáltatás relevanciája (Logit2 + DeviceProtection)
df$DeviceProtection <- relevel(df$DeviceProtection, ref= "0")

logit4 <- glm(Churn ~ tenure + MonthlyCharges
              + Contract 
              + SeniorCitizen
              + PhoneService
              + DeviceProtection, data=train, family = binomial(link = "logit"))
summary(logit4)


#    e.) Logit5 (bővített) modell: ingyenes kieg.szolgáltatás relevanciája (Logit2 + StreamingTV)
df$StreamingTV <- relevel(df$StreamingTV, ref= "0")
logit5 <- glm(Churn ~ tenure + MonthlyCharges
              + Contract 
              + SeniorCitizen
              + PhoneService
              + StreamingTV, data=train, family = binomial(link = "logit"))
summary(logit5)


#    f.) Legjobb illeszkedésű modell - információs kritériumok (7. táblázat)
IC <- data.frame(AIC = AIC(logit1, logit2, logit3, logit4, logit5)["AIC"],
                 BIC = BIC(logit1, logit2, logit3, logit4, logit5)["BIC"])
print(IC)

#    g.) Logit modell regressziós együtthatóinak értelmezése, illeszkedés értékelése, változófontosság
# Együtthatók:
exp(logit3$coefficients)

# Illeszkedés: McFadden féle pszeudo R^2
library(DescTools)
PseudoR2(logit3, "McFadden")  # 25.87%

# Változófontosság
library(caret)
caret::varImp(logit3) # tenure és ContractType a két legjobb prediktor


# Random forest ---------------------------------------------------------------
#1: Szükséges csomag
library(randomForest)
library(dplyr)

#2: Példa reprodukálhatósága
set.seed(1)

#3: Adatkészlet felosztása tanuló- és teszthalmazra (csak ha logitot nem futtatta korábban!)
sample <- sample(c(TRUE,FALSE), nrow(df), replace = TRUE, prob = c(0.8,0.2))
train <- df[sample,]
test <- df[!sample,]

#4: Véletlen erdő elkészítése - output értelmezése
forest <- randomForest(formula = as.factor(train$Churn) ~ tenure + MonthlyCharges 
                       + Contract + SeniorCitizen + PhoneService + InternetService, data = train)
print(forest)
RandomNum <- sort(sample.int(nrow(train), 3))
print(forest$votes)[c(RandomNum),]

#5: Változófontosság (grafikusan is, 10. ábra)
importance(forest)

imp <- varImpPlot(forest)
imp <- as.data.frame(imp)
imp$X <- rownames(imp)

ggplot(imp, aes(x = MeanDecreaseGini, y = reorder(X, MeanDecreaseGini))) +
  geom_point()+
  geom_segment(aes(x=0,xend=MeanDecreaseGini,y=X,yend=X))+
  ylab("Változó neve") +
  xlab("Gini-index átlagos csökkenése")+
  ggtitle("Változófontosság vizsgálata", subtitle = "Random forest modell alapján")+
  theme_gray()

  
# Modell diagnosztika a teszthalmazon------------------------------------------
View(test)

#1: Logit3 modell futtatása a teszthalmazon
test$Prob_logit3 <- predict(logit3, newdata = test, type = "response")
test$Pred_churn_logit <- 0
test$Pred_churn_logit[test$Prob_logit3 > 0.5] <- 1

#2: Forest modell futtatása a teszthalmazon
test$Pred_churn_forest <- predict(forest, newdata = test, type = "response")

#3: Konfúziós mátrixok és a pontosság mutatók
Confusion_logit <- data.frame(table(test[,c("Churn","Pred_churn_logit")]))
Confusion_logit
Confusion_forest <- data.frame(table(test[,c("Churn","Pred_churn_forest")]))
Confusion_forest

# Pontossági mutatók:
perf_indicators <- data.frame(
  "Logit" = c(
    round((Confusion_logit[1,3]+Confusion_logit[4,3])/nrow(test),3),
    round(Confusion_logit[4,3]/(Confusion_logit[4,3]+ Confusion_logit[3,3]),3),
    round(Confusion_logit[1,3]/(Confusion_logit[1,3]+ Confusion_logit[2,3]),3),
    round(Confusion_logit[4,3]/(Confusion_logit[4,3]+ Confusion_logit[2,3]),3),
    round(Confusion_logit[1,3]/(Confusion_logit[1,3]+ Confusion_logit[3,3]),3)
  ),
  "Forest" = c(
    round((Confusion_forest[1,3]+Confusion_forest[4,3])/1474, 3),
    round(Confusion_forest[4,3]/(Confusion_forest[4,3]+ Confusion_forest[3,3]),3),
    round(Confusion_forest[1,3]/(Confusion_forest[1,3]+ Confusion_forest[2,3]),3),
    round(Confusion_forest[4,3]/(Confusion_forest[4,3]+ Confusion_forest[2,3]),3),
    round(Confusion_forest[1,3]/(Confusion_forest[1,3]+ Confusion_forest[3,3]),3)
  )
)
rownames(perf_indicators) <- c("Accuracy", "Precision(1)", "Precision(0)", "Recall(1)", "Recall(0)")
perf_indicators

#4: ROC görbe és AUC mutató a két modellre
library(ggplot2)
library(pROC)
ROC_logit <- roc(test$Churn, test$Prob_logit3)
ROC_logit$auc #0.8446

predict_rf <- predict(forest, test, "prob")
ROC_forest <- roc(test$Churn, predict_rf[,2])
ROC_forest$auc #0.8314

groc <- ggroc(ROC_logit,legacy.axes = T,color="red")
groc

# ROC görbe a logit modellre (11. ábra)
groc + xlab("FPR")+ylab("TPR")+
  geom_segment(aes(x=0, xend=1, y=0, yend=1),color="darkgrey",linetype="dashed")+
  theme_minimal()+ggtitle("ROC görbe logit modellre")


# Modell pontosság növelése - optimális cut-off
ROC <- plot.roc(test$Churn,test$Prob_logit3,main="ROC görbe logit modellre")
bestcutoff<- coords(ROC,"best",best.method="closest.topleft")
bestcutoff$sensitivity #0.7862
1-bestcutoff$specificity #0.2340
