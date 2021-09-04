
#ECON413 - Group J
#GRAPHS

#Libraries----
library(CBRT)
library(WDI)
library(ggplot2)
library(forecast)
library(data.table)

#Graph 1 - Agriculture, Forestry, and Fishing, Value Added (% of GDP)----
GDP_share <- WDI(country = c("TR", "XP") , indicator = "NV.AGR.TOTL.ZS", start = 1997, end = 2018)
Turkey <- GDP_share[c(23:44), ]
Middle <- GDP_share[c(1:22),]

ggplot(Turkey, aes(x= year, y=NV.AGR.TOTL.ZS, color= "Turkey")) + 
  geom_line(size=1) +
  geom_line(data= Middle, aes(y = NV.AGR.TOTL.ZS, color= "Middle"), size=1) + theme_minimal() + 
  labs(title= "Agriculture, Forestry, and Fishing, Value Added (% of GDP) ", x = "Year", y = "Value Added (%)", caption = "Source : WorldBank, National Accounts") +
  guides(colour = guide_legend(""))

#Graph 2 - Frequency Graph

#Graph 3 - Agriculture Credit----
Credit <- getDataSeries(c("TP.KM.B33","TP.KREDI.L015"), CBRTKey = "t1BeysCQGf", 8)
Credit <- Credit[, list(time,TP.KM.B33,TP.KREDI.L015)]
names(Credit) <- c("Years","Total_Credit","Agriculture_Credit")
Governmentsupport <- getDataSeries(c("TP.KB.GEN26"), CBRTKey = "t1BeysCQGf", 8)
Governmentsupport <- Governmentsupport[, list(time,TP.KB.GEN26)]
names(Governmentsupport) <- c("Years","Agricultural_Support")
clabsu1 <- labs(title = "The Share of Agriculture Credit on The Total Credit",
                x= "Years", y = "Percentage of Agriculture Credit",
                caption = "Source: Central Bank of the Republic of Turkey")
ggplot(Credit[Credit$Years >= 2005 & Credit$Years <= 2019], aes(x=Years)) + 
  geom_line(aes(y = (Agriculture_Credit)/(Total_Credit), color= "Agriculture Credit"), size=1) + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(2005,2019,1))+
  clabsu1 + guides(colour = guide_legend(""))

#Graph 4
clabsu2 <- labs(title = "Agriculture Credit",
                x= "Years", y = "Agriculture(Million TL)",caption = "Source: Central Bank of the Republic of Turkey")
ggplot(Credit[Credit$Years >= 2005 & Credit$Years <= 2019], aes(x = Years, y = Agriculture_Credit/1000, label = Agriculture_Credit, ))+
  geom_bar(stat = "identity", color= "white", fill = "#b3de69") + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(2005,2019,1))+
  clabsu2

#Graph 5 - Frequency Graph

#Graph 6
clabsu3 <- labs(title = "Agricultural Support",
                x= "Years", y = "Agricultural Support(Million TL)",
                caption = "Source: Central Bank of the Republic of Turkey")
ggplot(Governmentsupport[Governmentsupport$Years >= 2006 & Governmentsupport$Years <= 2019], aes(x = Years, y = Agricultural_Support/1000, label = Agricultural_Support, ))+
  geom_bar(stat = "identity", color= "white", fill = "#b3de69") + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(2006,2019,1))+
  clabsu3

#Graph 7 - Frequency Graph

#Graph 8 - Frequency Graph

#Graph 9 - Frequency Graph

#Graph 10 - Tobacco Production in Turkey
dt <- fread("https://bin.jvnv.net/file/0fEHZ.csv")
dt <- dt[, list(Year,Value)]
Turkey <- dt[c(1:24),]
World <- dt[c(25:48),]
data_1 <- data.frame(Turkey, World)
data_1$ Year.1 <- NULL
names(data_1) <- c("Years","Tobacco_Turkey","Tobacco_World")
clabsg1 <- labs(title = "Tobacco Production in Turkey, % of Total World Production",
                x= "Years", y = "Quantity Tobacco Production,  Turkey", Source= "Food and Agriculture Databse (2021)")
ggplot(data_1, aes(x=Years)) + 
  geom_line(aes(y = (Tobacco_Turkey)/(Tobacco_World), colour="Tobacco Production"), size=1) + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(1996,2019,1))+
  clabsg1 + guides(colour = guide_legend("")) + scale_color_manual(values= "skyblue2")

#Graph 11 - Frequency Graph

#Graph 12 - Rural Population
library(CBRT)
library(forecast)
library(WDI)
myCBRTKey <- "ha20CXJ1Qe"
Rural_population <- WDI(country = "TR", indicator = "SP.RUR.TOTL.ZS", start = 1996, end = 2020)
names(Rural_population)[3] <- "rural_population"
title1 <- labs(title = "Rural Population Percentage of Total Population",
               x = "", y = "rural population",
               caption = "Source: World Bank")
ruralpopulation <- ggplot(Rural_population, aes(x = year, y = rural_population, fill = rural_population)) + 
  geom_bar(position = position_dodge(), stat = "identity") + theme_minimal()+ title1
title2 <- labs(title = "Urban Population percentage of Total Population",
               x = "year", y = "urban population",
               caption = "Source : World Bank")
#Graph 13 - Urban Population
WDIsearch("urban population")
Urban_population <- WDI(country = "TR", indicator = "SP.URB.TOTL.IN.ZS", start = 1996, end = 2020)
names(Urban_population)[3] <- "urban_pop"
urbanpop <- ggplot(Urban_population, aes(x = year, y = urban_pop, fill = urban_pop)) + geom_bar(positin=position_dodge(), stat = "identity") +
  title2 + theme_minimal()

#Graph 14 - Agricultural Credits for Rural Development
kırsal_kalkınma <- getDataSeries("TP.KB.GID075", start = "1996-01-01", end = "2020-01-01", freq = 8)
names(kırsal_kalkınma)[2] <- "ruraldevelop"
title3 <- labs(title = "Agricultural Credits for Rural Development",
               x = "year", y = "rural development credits",
               caption = "Source : Central Bank of the Republic of Turkey")
rural_credit <- ggplot(kırsal_kalkınma, aes(x = time, y = ruraldevelop/1000)) + 
  geom_line(size = 0.5, color = "blue") + scale_y_log10() + title3 + theme_minimal() + 
  geom_vline(xintercept = c(2009,2015), color = "light blue", size = 1) + 
  annotate("text", x=2009, y = 21180.906, label="IPARD-I") + 
  annotate("text", x=2015, y=20, label="IPARD-II")


#Graph 15 - Agricultural Employment vs Non-Agricultural
tarim_ist <- getDataSeries("TP.RS.YBB02.TOP", start = "1996-01-01", end = "2020-01-01", freq = 8)
names(tarim_ist)[2] <- "agricultural_employment"
tarim_disi_istihdam <- getDataSeries("TP.TIG08", start = "1996-01-01", end = "2020-01-01", freq = 8)
names(tarim_disi_istihdam)[2] <- "non_agri"
non_agri_total <- merge(tarim_disi_istihdam,tarim_ist, by = "time")
title4 <- labs(title = "Agricultural vs Non-Agricultural Employment",
               x = "year", y = "agricultural employment",
               caption = "Source : Central Bank of the Republic Turkey")
agrivsnon <- ggplot(non_agri_total, aes(x = time)) + geom_bar(aes(y = agricultural_employment, fill = "agri"), 
                                                              stat = "identity", position=position_dodge()) + 
  scale_fill_manual(values = "light blue") + geom_line(aes(y = non_agri*150, color = "non_agri"), size = 2) + 
  scale_color_manual(values = "#154267") + 
  theme_minimal() + theme(legend.title = element_blank()) + 
  title4 + scale_y_continuous(sec.axis = sec_axis(~ ./150 , name = "non-agricultural employment(%)")) + 
  theme(legend.position = "bottom")

#Graph 16 - Food Production
WDIsearch("food production index")
Food_production <- WDI(country = "TR", indicator = "AG.PRD.FOOD.XD", start = 1996, end = 2020)
names(Food_production)[3] <- "foodprod" 
title5 <- labs(title = "Food Production", 
               x = "year", y = "food production",
               caption = "Source : World Bank")

foodproduction <- ggplot(Food_production, aes(x = year, y = foodprod)) + geom_line(size = 1.5, color = "#80b1d3") + theme_minimal() +
  geom_vline(xintercept = c(2009,2014), color = "#152099", size = 1) + annotate("text", x=2009, y = 108.8, label="IPARD-I") + 
  annotate("text", x = 2014, y = 133.66, label = "IPARD-II") + title5

#Graph 17 - Women Employment
women_agr <- getDataSeries("TP.RS.YBB02.AIL", start = "1996-01-01", end = "2020-01-01", freq = 8)
names(women_agr)[2] <- "women_emp"
title6 <- labs(title = "Women Employment in Agriculture",
               x = "year", y = "women employment",
               caption = "Source : Central Bank of the Republic of Turkey")
women_ <- ggplot(women_agr, aes(x = time, y = women_emp, color = women_emp)) + geom_line(size = 1.5) + title6 + theme_minimal()

#Graph 18 - Frequency Graph

#Graph 19 - Frequency Graph

#Graph 20 - Tariff Rate, Applied, Primary Products (%)
Tariffs <- WDI(country = "TR", indicator = "TM.TAX.TCOM.WM.AR.ZS", start = 1996, end = 2011)

clabsi2 <- labs(title = "Tariff Rate, Applied, Primary Products (%)",
                x = "Year", y = "Tariff Rate",
                caption = "Source: World Bank")
ggplot(Tariffs, aes(x= year, y=TM.TAX.TCOM.WM.AR.ZS, color= "Tariff Rate")) + 
  geom_line(size=1) + theme_minimal() + clabsi2 + scale_color_manual(values = "royalblue2") + 
  guides(colour = guide_legend("")) + geom_vline(xintercept = 1999, color = "light blue", size = 1) + 
  annotate("text", x=1999, y = 5.45, label="         1999")

#Graph 21 - Agricultural Export-Import Quantity (M$)
Export <- getDataSeries("TP.DT.TAR.IH.I", start = "1989-01-01", end = "2020-01-01", freq = 8)
Import <- getDataSeries("TP.DT.TAR.IT.I", start = "1989-01-01", end = "2020-01-01", freq = 8)
clabsi3 <- labs(title = "Agricultural Export-Import Quantity (Million USD)",
                x = "Year", y = "",
                caption = "Source: Central Bank of the Republic of Turkey")
ggplot(Export, aes(x=time, y=TP.DT.TAR.IH.I, color= "Export Quantity")) + geom_line(size=1) + 
  geom_line(data =Import, aes(y=TP.DT.TAR.IT.I, color= "Import Quantity"), size=1) + 
  guides(colour = guide_legend("")) + theme_minimal() + clabsi3 + scale_color_manual(values = c("olivedrab4", "orange2"))


