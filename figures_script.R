
# Kód célja: Tudományos Diákköri Konferencia dolgozat (saját szerkesztésű ábrák melléklet)
# Év: 2023.
# Dolgozat cím: Ügyfelek elvándorlási viselkedésének vizsgálata egy fiktív távközlési szolgáltató nyomán 
#               – statisztikai és gépi tanuló modellek segítségével
# Név: Sumicz Benedek Máté
# Szak: Gazdaságinformatikus BSc, IV. évfolyam
# Egyetem: Budapesti Corvinus Egyetem 

setwd("C:/Users/sumbe/Desktop/Suli/8. félév/Szakszeminárium II/Ábrák")
getwd()

# package-k 
install.packages("readxl")
library(readxl)
install.packages("ggplot")
library(ggplot2)


#1. Mobilelőfizetők száma (1993-2021) -----------------------------------------
# forrás: https://www.statista.com/statistics/262950/global-mobile-subscriptions-since-1993/

df <- read_excel("no_of_global_subscriptions.xlsx")
str(df)

ggplot(df, aes(x = Year, y= `Subscriptions in million`)) + 
  geom_line(color = "slateblue", size = 1) +
  geom_point(color = "slateblue") +
  geom_step(size = 0.5, linetype = "dotted") +
  ggtitle("Mobilelőfizetések számának globális alakulása (1993-2021)") +
  xlab("Év") +
  ylab("Előfizetések száma (millió db)")


#2. Dolgok internetéhez (IoT) csatlakoztatott eszközök száma világszerte ------
# forrás: https://www.statista.com/statistics/1194701/iot-connected-devices-use-case/

library(ggplot2)
df2 <- read_excel("number_of_iot_devices_by_usecase.xlsx", sheet = "data")
str(df2)
df2$Year <- as.factor(df2$Year)
df2$Usecase <- as.factor(df2$Usecase)

ggplot(df2, aes(fill = Usecase, y= Devices, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("IoT-hez csatlakoztatott eszközök száma és előrejelzése világszerte", 
          subtitle = "Felhasználás szerint (2019-2030)") +
  scale_fill_manual(name = 'Felhasználás', 
                     values = c('Biztonsági kamerák' = "#66Bb6a", 
                                'Egyéb' = "#42a5f5",
                                'Épületautomatizálás' = "#c0ca33",
                                'Eszközkövetés és felügyelet' = "#006064",
                                'Fogyasztói internetes és médiaeszközök' = "#00acc1",
                                'Irodai felszerelés' = "#827717",
                                'IT infrastruktúra' = "#00796b",
                                'Készletgazdálkodás és monitoring' = "#5c6bc0",
                                'Okoshálózat' = "#4dd0e1",
                                'Összekapcsolt járművek' = "#33691e",
                                'Világítás' = "#546e7a")) +
  labs(color = 'Felhasználás')+
  xlab("Év") +
  ylab("Eszközök száma (millió db)")


#3. Az 5G fontossági szempontjai Európában, 2020-ban --------------------------
# forrás: https://www.statista.com/statistics/1246858/5g-europe-importance-viewpoints/

df_5g <- read_excel("importance_of_5g.xlsx", sheet = "data1")
df_5g$View <- as.factor(df_5g$View)
df_5g$Importance <- as.factor(df_5g$Importance)
str(df_5g)

ggplot(df_5g, aes(fill = Importance, y = Share, x = View)) +
  geom_bar(position = "fill", stat = "identity") +
  ggtitle("5G fontossági szempontjai Európában (2020)") +
  scale_fill_manual(name = 'Relevancia', 
                    values = c('1 - Extremely important' = "#81c784", 
                               '2 - Very important' = "#64b5f6",
                               '3 - Important' = "#d4e157",
                               '4 - Not important' = "#0097a7",
                               '5 - Not important at all' = "#9fa8da")) +
  labs(color = 'Relevancia')+
  xlab("Szempontok") +
  ylab("Válaszok megoszlása (%)")

