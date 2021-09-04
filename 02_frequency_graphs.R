
#ECON413 - Group J
#Frequency Graphs

#Agricultural Credits & Supports----
wordlist2 <- c("sulama", "gap", "dogrudan gelir destegi", "ziraat bankasi", "tarimsal destekleme")
WordDataKredi <- WordCountTable(wordlist2)
FreqDataKredi <- WordDataKredi[ , .(FreqSul = sulama/sum(sulama)*100,
                                    FreqGAP = gap/sum(gap)*100,
                                    FreqDGD = `dogrudan gelir destegi`/sum(`dogrudan gelir destegi`)*100,
                                    FreqZB = `ziraat bankasi`/sum(`ziraat bankasi`)*100,
                                    FreqTD = `tarimsal destekleme`/sum(`tarimsal destekleme`)*100)][ , Year := c(1996:2021)]
ggplot(FreqDataKredi, aes(x= Year, y=FreqTD, color= "Tarım Destekleme")) + geom_line(size =1) +
  guides(colour = guide_legend("")) + theme_minimal() + labs(title = "Frequency of Words by Years(%)",
                                                             x = "Year", y = "Frequency of Words(%)",
                                                             caption = "Source: TC. Strateji ve Bütçe Başkanlığı")
ggplot(FreqDataKredi, aes(x= Year, y=FreqZB, color= "Ziraat Bankası")) + geom_line(size =1) +
  guides(colour = guide_legend("")) + theme_minimal() + labs(title = "Frequency of Words by Years(%)",
                                                             x = "Year", y = "Frequency of Words(%)",
                                                             caption = "Source: TC. Strateji ve Bütçe Başkanlığı")
ggplot(FreqDataKredi, aes(x=Year, y=FreqSul, color= "Sulama")) + geom_line(size= 1) + geom_line(aes(y= FreqGAP, color= "GAP"), size = 1) +
  theme_minimal() + guides(colour = guide_legend("")) + labs(title = "Frequency of Words by Years(%)",
                                                             x = "Year", y = "Frequency of Words(%)",
                                                             caption = "Source: TC. Strateji ve Bütçe Başkanlığı")
ggplot(FreqDataKredi, aes(x=Year, y= FreqDGD, color= "Doğrudan Gelir Desteği")) + geom_line(size = 1) + theme_minimal() + 
  guides(colour = guide_legend("")) + labs(title = "Frequency of Words by Years(%)",
                                           x = "Year", y = "Frequency of Words(%)",
                                           caption = "Source: TC. Strateji ve Bütçe Başkanlığı")
csig <- (Word_Count("tarim sigorta"))
Freqsig <- csig[, .(fresig = `tarim sigorta`/ sum(`tarim sigorta`)*100)]
Freqsig <- Freqsig[, Year := c(1996:2021)]
ggplot(Freqsig, aes(x= Year, y=fresig, color= "Tarım Sigorta")) + guides(colour = guide_legend("")) + 
  geom_line(size=1) + labs(title = "Frequency of Words by Years(%)", x = "Year", y = "Frequency of Words(%)",
                           caption = "Source: TC. Strateji ve Bütçe Başkanlığı") + theme_minimal()

#Market-Orientation & Privatization in Agriculture----------
aa <- c("tekel", "tutun", "seker") 
tt <- WordCountTable(aa)
FreqData <- tt[, .(freqtek = tekel / sum(tekel)*100,
                   freqsek = seker / sum(seker)*100,
                   frtut = tutun/sum(tutun)*100)]
FreqData <- FreqData[, Year := c(1996:2021)]
ggplot(FreqData, aes(x= Year)) +
  geom_line(aes(y = freqtek), color = "darkred") + 
  geom_line(aes(y = frtut), color="steelblue") +
  geom_line(aes(y = freqsek), color="orange") + theme_minimal() + labs(title = "Frequency of Words by Years (%)",
                                                                       x = "year", y = "Frequency of words (%)",
                                                                       caption = "Source : TC. Strateji ve Bütçe Başkanlığı")

#Rural Development-------
wordlistt <- c("kirsal kalkinma", "tarimsal istihdam", "kentlesme", "kurumsal kapasite") 
word_data <- WordCountTable(wordlistt)
Freq_Data <- word_data[, .(FreqKir = `kirsal kalkinma`/sum(`kirsal kalkinma`)*100,
                           FreqTar = `tarimsal istihdam`/sum(`tarimsal istihdam`)*100,
                           FreqKent = kentlesme / sum(kentlesme)*100,
                           FreqKur = `kurumsal kapasite`/sum(`kurumsal kapasite`)*100)]
Freq_Data <- Freq_Data[, year := c(1996:2021)]
clabs_kir <- labs(title = "Frequency of Words by Years (%)",
                  x = "year", y = "Frequency of words (%)",
                  caption = "Source : TC. Strateji ve Bütçe Başkanlığı")
MyColors <- c("olivedrab4", "violetred3", "royalblue2", "orange2")
Kirkalfreq <- ggplot(Freq_Data, aes(x = year)) + geom_line(aes(y = FreqKir, color = "Kirsal Kalkinma"), size = 1) + 
  geom_line(aes(y = FreqTar, color = "Tarimsal İstihdam"), size = 1) +
  geom_line(aes(y = FreqKent, color = "Kentlesme"), size = 1) + 
  geom_line(aes(y = FreqKur, color = "Kurumsal Kapasite"), size = 1) + 
  theme_minimal() + scale_color_manual(values = MyColors) + guides(colour = guide_legend("")) + clabs_kir

#International Agreements on Agricultural Trade Liberalization----
wordlist=c("serbestlesme", "serbest ticaret anlasma", "gumruk vergi", "tarife")
WordData=WordCountTable(wordlist)

FreqData1 <- WordData[ , .(FreqSer = serbestlesme/sum(serbestlesme)*100,
                           FreqSTA = `serbest ticaret anlasma`/sum(`serbest ticaret anlasma`)*100,
                           FreqGV = `gumruk vergi`/sum(`gumruk vergi`)*100,
                           FreqTar = tarife/sum(tarife)*100)]
FreqData1 <- FreqData1[ , Year := c(1996:2021)]

clabsi <- labs(title = "Frequency of Words by Years(%)",
               x = "Year", y = "Frequency of Words(%)",
               caption = "Source: TC. Strateji ve Bütçe Başkanlığı") 
MyColors <- c("olivedrab4", "violetred3", "royalblue2", "orange2")
ggplot(FreqData1, aes(x=Year, y=FreqSer, color = "Serbestleşme")) + geom_line(size = 1) + 
  geom_line(data = FreqData1, aes(y= FreqSTA, color= "Serbest Ticaret Anlaşma"), size= 1) +
  geom_line(data = FreqData1, aes(y = FreqGV, color= "Gümrük Vergi"), size = 1) + 
  geom_line(data = FreqData1, aes(y = FreqTar, color= "Tarife"), size= 1) + 
  theme_minimal() + clabsi + guides(colour = guide_legend("")) + scale_color_manual(values = MyColors)

