library(data.table)
library(ggplot2)
library(tidyr)
library(quantmod)
library(stringr)
library(reshape2)
library(ineq)
setwd("ST309/yelp-dataset")
test = readr::read_csv('HousingPrices1973.csv')
test = test[,-c(1,13:16)]
setDT(test)[, c("Quarter","Year") := tstrsplit(Year, " ")]
hali = read.csv(file.choose())
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
try = test %>% gather(regi, pri, -Quarter, -Year)
try1 = try[try$Year<1983,]
try2 = try[try$Year>=1983,]
setDT(try1)
setDT(try2)
pre1983d = try1[,sd(pri), by = try1$Year]
pre1983d$Year = as.numeric(pre1983d$Year)
lloyds1983d = try2[,sd(pri), by = try2$Year]
colnames(pre1983d)<- c("Year", "Standard Deviation")
colnames(lloyds1983d)<- c("Year", "Standard Deviation")
qplot(data = pre1983d, x=Year, y=`Standard Deviation`) + geom_smooth(colour = "green")
qplot(data = lloyds1983d[Year<1995,], x=Year, y=`Standard Deviation`) + geom_smooth(colour = "green")

# write.csv(pre1983d, file = "pre1983.csv")
# to write the csv file for the data

hali = hali[,-c(1:3,5,6,9,11:15)]
filter = hali$RegionIDDesc %in% c("NIrelandUK","ScotlandUK","WalesUK")
hali = hali[!filter,]
summary(hali$RegionIDDesc)
hali$Year = as.numeric(hali$Year)
hali$RegionIDDesc = droplevels(hali$RegionIDDesc)
setDT(hali)
hali1983d = hali[,sd(Standard_Average_Price), by = hali$Year]
hali1983d$Adjusted = hali1983d$`Standard Deviation` * Inflation_Sheet1[c(11:46),2]
colnames(hali1983d)<- c("Year", "Standard Deviation")
qplot(data = hali1983d[Year<1995,], x=Year, y=`Standard Deviation`) + geom_smooth(colour = "green")

#during the merging we noticed that the lloyds data set inclueded an additional region classification
#so we will remove this region during the merging to keep the data as similar as possible
#notably both the data sets exhibit similar trends which implies that merging will not
#yield significantly different results

try2$regi = as.factor(try2$regi)
try2$Quarter = as.factor(try2$Quarter)
try2 = try2[!try2$regi == "OUTER MET",]
levels(try2$regi)
try2$regi = droplevels(try2$regi)
levels(hali$RegionIDDesc) = levels(try2$regi)
levels(hali$Period) = levels(try2$Quarter)
try2$Year = as.numeric(try2$Year)
mset = merge(try2,hali, by.x = c("Year","regi","Quarter"), by.y = c("Year","RegionIDDesc","Period"))
mset1 = cbind(mset, Difference = mset$pri - mset$Standard_Average_Price)
summary(mset1$Difference)

#Exploration of differences in indicies shows that Halifax prices are higher than Nationwide

fulset = mset %>% gather(Index,Price, -Quarter, -Year,-regi)
levels(fulset$Index) = c("Nationwide","Halifax")
setDT(fulset)
fullp1983d = fulset[,sd(Price),by = Year]
colnames(fullp1983d)<- c("Year", "Standard Deviation")
fullp1983d$Adjusted = fullp1983d$`Standard Deviation` * Inflation_Sheet1[c(11:46),2]
qplot(data = fullp1983d[Year<1995,], x=Year, y=`Standard Deviation`) + geom_smooth(colour = "green")

#Exploration of land registry data

PPLV = readr::read_csv('PPLV.csv')
library(DataExplorer)
plot_missing(PPLV)
PPLV = PPLV[,-c(3:5)] #remove all columns besides price,date and county
setDT(PPLV)[, c("Year","Month", "Day") := tstrsplit(Date, "-")]
PPLV = PPLV[,-c(2,3,5,6)] #remove all columns besides year and price
Dev1995p = PPLV[,sd(Price), by = Year]
colnames(Dev1995p)<- c("Year", "Standard Deviation")
Dev1995p$Year = as.numeric(Dev1995p$Year)
lloyds1983d$Year = as.numeric(lloyds1983d$Year)
qplot(data = Dev1995p[Year<2011,], x=Year, y=`Standard Deviation`) + geom_smooth(colour = "green")
qplot(data = lloyds1983d[Year>=1995,], x=Year, y=`Standard Deviation`) + geom_smooth(colour = "green")
qplot(data = fullp1983d[Year>=1995,], x=Year, y=`Standard Deviation`) + geom_smooth(colour = "green")

#Further exploration

MeanPop = PPLV[,mean(Price), by = Year]
MeanPop$Year = as.numeric(MeanPop$Year)
colnames(MeanPop)<- c("Year", "Standard Deviation")
qplot(data = MeanPop, x=Year, y=`Standard Deviation`) + geom_smooth(colour = "green") 

#After careful consideration properties falling under the Other category have been removed
#This is mostly due to them accounting for some large land transactions
#These skew the data and invalidate our results, hence they will not be included in
#this analysis
landreg = readr::read_csv('landreg.csv')
landreg1 = landreg[,-c(1,3:6,8,9)]
LR1995 = landreg1[,sd(Price), by = landreg1$Year]
colnames(LR1995)<- c("Year", "Standard Deviation")
qplot(data = LR1995, x=Year, y=`Standard Deviation`) + geom_smooth(colour = "green")
landreg$Region <- NA
EastAnglia <- landreg$County %in% c("NORFOLK","SUFFOLK","CAMBRIDGESHIRE","ESSEX","THURROCK","HERTFORDSHIRE","BEDFORDSHIRE","SOUTHEND-ON-SEA","LUTON") 
landreg$Region[EastAnglia] <- "EAST ANGLIA"
EastMids <- landreg$County %in% c("DERBYSHIRE", "LEICESTERSHIRE", "LINCOLNSHIRE", "NORTHAMPTONSHIRE", "NOTTINGHAMSHIRE", "RUTLAND","LEICESTER")
landreg$Region[EastMids] <- "EAST MIDS"
London <- landreg$County %in% c("GREATER LONDON","MIDDLESEX","KENT","SURREY")
landreg$Region[London] <- "LONDON"
NorthEast <- landreg$County %in% c("NORTHUMBERLAND", "COUNTY DURHAM", "TYNE AND WEAR","DURHAM","STOCKTON-ON-TEES","DARLINGTON","CLEVELAND")
landreg$Region[NorthEast] <- "NORTH EAST"
NorthWest <- landreg$County %in% c("CHESHIRE","CUMBRIA","GREATER MANCHESTER","LANCASHIRE","MERSEYSIDE","MANCHESTER","LIVERPOOL","WARRINGTON","PRESTON","BLACKPOOL")
landreg$Region[NorthWest] <- "NORTH WEST"
SouthEast <- landreg$County %in% c("BERKSHIRE","BUCKINGHAMSHIRE","EAST SUSSEX", "HAMPSHIRE","ISLE OF WIGHT","KENT","OXFORDSHIRE","SURREY","WEST SUSSEX","SUSSEX","SLOUGH","BRACKNELL FOREST","SOUTHAMPTON","READING","MILTON KEYNES","WINDSOR AND MAIDENHEAD","PORTSMOUTH","WOKINGHAM")
landreg$Region[SouthEast] <- "SOUTH EAST"
SouthWest <- landreg$County %in% c("GLOUCESTERSHIRE","BRISTOL","WILTSHIRE","SOMERSET","DORSET","DEVON","CORNWALL","ISLES OF SCILLY","SCILLY","PLYMOUTH","SWINDON","GLOUCESTER","CHELTENHAM","EXETER","BATH","TORBAY","AVON","POOLE","BOURNEMOUTH","THAMESDOWN")
landreg$Region[SouthWest] <- "SOUTH WEST"
WestMids <- landreg$County %in% c("BIRMINGHAM","WEST MIDLANDS","CITY OF COVENTRY","COVENTRY","WARWICKSHIRE","GREATER BIRMINGHAM","SHROPSHIRE","HEREFORDSHIRE","WORCESTERSHIRE","WORCESTER","STAFFORDSHIRE","STOKE-ON-TRENT","HEREFORD AND WORCESTER")
landreg$Region[WestMids] <- "WEST MIDS"
YorkHside <- landreg$County %in% c("SOUTH YORKSHIRE","WEST YORKSHIRE","EAST RIDING OF YORKSHIRE","EAST RIDING", "HULL","NORTH YORKSHIRE", "CITY OF YORK","YORK", "NORTH LINCOLNSHIRE","NORTH EAST LINCOLNSHIRE","LEEDS","SHEFFIELD","BRADFORD","HUDDERSFIELD","HUMBERSIDE","MIDDLESBROUGH")
landreg$Region[YorkHside] <- "YORK & HSIDE"
RegionLandReq = na.omit(landreg)
dim(landreg)[1] - dim(RegionLandReq)[1] #loss of 2,777,190 observations

setDT(RegionLandReq)
EAmean = RegionLandReq[RegionLandReq$Region == "EAST ANGLIA",mean(Price), by = Year]
colnames(EAmean)[2] = "Mean"

EMmean = RegionLandReq[RegionLandReq$Region == "EAST MIDS",mean(Price), by = Year]
colnames(EMmean)[2] = "Mean"

Lmean = RegionLandReq[RegionLandReq$Region == "LONDON",mean(Price), by = Year]
colnames(Lmean)[2] = "Mean"

NEmean = RegionLandReq[RegionLandReq$Region == "NORTH EAST",mean(Price), by = Year]
colnames(NEmean)[2] = "Mean"

NWmean = RegionLandReq[RegionLandReq$Region == "NORTH WEST",mean(Price), by = Year]
colnames(NWmean)[2] = "Mean"

SEmean = RegionLandReq[RegionLandReq$Region == "SOUTH EAST",mean(Price), by = Year]
colnames(SEmean)[2] = "Mean"

SWmean = RegionLandReq[RegionLandReq$Region == "SOUTH WEST",mean(Price), by = Year]
colnames(SWmean)[2] = "Mean"

WMmean = RegionLandReq[RegionLandReq$Region == "WEST MIDS",mean(Price), by = Year]
colnames(WMmean)[2] = "Mean"

YORKmean = RegionLandReq[RegionLandReq$Region == "YORK & HSIDE",mean(Price), by = Year]
colnames(YORKmean)[2] = "Mean"

ggplot(fullset, aes(x = Year, y = `EAST ANGLIA`/1000, col = "East Anglia"))+
  geom_line() +
  geom_line(aes(x=Year, y=LONDON/1000, col="London")) +
  geom_line(aes(x=Year, y=`NORTH WEST`/1000, col="North West")) +
  geom_line(aes(x=Year, y=`EAST MIDS`/1000, col="East Midlands")) +
  geom_line(aes(x=Year, y=`NORTH EAST`/1000, col="North East")) +
  geom_line(aes(x=Year, y=`SOUTH EAST`/1000, col="South East")) +
  geom_line(aes(x=Year, y=`SOUTH WEST`/1000, col="South West")) +
  geom_line(aes(x=Year, y=`WEST MIDS`/1000, col="West Midlands")) +
  geom_line(aes(x=Year, y=`YORK & HSIDE`/1000, col="York&H")) +
  ggtitle("Mean Housing Prices per Region 1995-2018") +
  ylab("Price in £ Thousands ") +
  theme(
    plot.title = element_text(colour = "black"),
    axis.title.x = element_text(colour = "light blue", size = 15),
    axis.title.y = element_text(colour = "light blue", size = 15),
    axis.text.x = element_text(colour = "light blue", size = 10),
    axis.text.y = element_text(colour = "light blue", size = 10),
    plot.background = element_rect("white"),
    panel.background = element_rect("dark grey","light blue"),
    panel.grid = element_line(colour = "light grey"),
    legend.title = element_blank(),
    legend.text = element_text(colour = "black",size = 8),
    legend.background = element_rect(fill = "light blue"),
    legend.key = element_rect(fill = "light grey", color = NA)
  )
lr = fullset %>% gather(Region, Price, -Year)
setDT(lr)
lr1995d = lr[,sd(Price), by = Year]
colnames(lr1995d)[2] = "Deviation"
ggplot(lr1995d, aes(x = Year, y = Adjusted, col = "LandRegistry"))+
  geom_line() +
  geom_line(aes(x = Year, y = Secondary, col ="MergedSets"))

ggplot(Nationwide1973, aes(x = Year, y = Adjusted/10000, col = "Nationwide"))+
  geom_line() +
  geom_line(aes(x = Year, y = LR/10000, col ="Land Registry"))+
  geom_line(aes(x = Year, y = RealGrowth, col ="Real GDP Growth"))

for (i in 1:63){
  b <- ((YBHA_240419[i+1,2] - YBHA_240419[i,2])/YBHA_240419[i,2])*100
  a = c(a,b)
}

RegionLandReq = readr::read_csv('RLandR.csv')
RegionLandReq = RegionLandReq[,-1]

setDT(RegionLandReq)
EAd = RegionLandReq[RegionLandReq$Region == "EAST ANGLIA",sd(Price), by = Year]
colnames(EAd)[2] = "Deviation"

EMd = RegionLandReq[RegionLandReq$Region == "EAST MIDS",sd(Price), by = Year]
colnames(EMd)[2] = "Deviation"

Ld = RegionLandReq[RegionLandReq$Region == "LONDON",sd(Price), by = Year]
colnames(Ld)[2] = "Deviation"

NEd = RegionLandReq[RegionLandReq$Region == "NORTH EAST",sd(Price), by = Year]
colnames(NEd)[2] = "Deviation"

NWd = RegionLandReq[RegionLandReq$Region == "NORTH WEST",sd(Price), by = Year]
colnames(NWd)[2] = "Deviation"

SEd = RegionLandReq[RegionLandReq$Region == "SOUTH EAST",sd(Price), by = Year]
colnames(SEd)[2] = "Deviation"

SWd = RegionLandReq[RegionLandReq$Region == "SOUTH WEST",sd(Price), by = Year]
colnames(SWd)[2] = "Deviation"

WMd = RegionLandReq[RegionLandReq$Region == "WEST MIDS",sd(Price), by = Year]
colnames(WMd)[2] = "Deviation"

YORKd = RegionLandReq[RegionLandReq$Region == "YORK & HSIDE",sd(Price), by = Year]
colnames(YORKd)[2] = "Deviation"

ggplot(EAd, aes(x = Year, y = EastAnglia/1000, col = "East Anglia"))+
  geom_line() +
  geom_line(aes(x=Year, y=London/1000, col="London")) +
  geom_line(aes(x=Year, y=YorkHside/1000, col="York&H")) +
  geom_line(aes(x=Year, y=NorthWest/1000, col="North West")) +
  geom_line(aes(x=Year, y=EastMidlands/1000, col="East Midlands")) +
  geom_line(aes(x=Year, y=NorthEast/1000, col="North East")) +
  geom_line(aes(x=Year, y=SouthEast/1000, col="South East")) +
  geom_line(aes(x=Year, y=SouthWest/1000, col="South West")) +
  geom_line(aes(x=Year, y=WestMidlands/1000, col="West Midlands")) +
  ylab("Deviations in £ Thousands ")

#---------------------------------------------- Gini calcs

ineq(test$NORTH,type="Gini")
EAatk = RegionLandReq[RegionLandReq$Region == "EAST ANGLIA",ineq(Price,type="Atkinson"), by = Year]
colnames(EAatk)[2] = "Atkinson"

EMgini = RegionLandReq[RegionLandReq$Region == "EAST MIDS",ineq(Price,type="Gini"), by = Year]
colnames(EMgini)[2] = "Gini"

Lgini = RegionLandReq[RegionLandReq$Region == "LONDON",ineq(Price,type="Gini"), by = Year]
colnames(Lgini)[2] = "Gini"

NEgini = RegionLandReq[RegionLandReq$Region == "NORTH EAST",ineq(Price,type="Gini"), by = Year]
colnames(NEgini)[2] = "Gini"

NWgini = RegionLandReq[RegionLandReq$Region == "NORTH WEST",ineq(Price,type="Gini"), by = Year]
colnames(NWgini)[2] = "Gini"

SEgini = RegionLandReq[RegionLandReq$Region == "SOUTH EAST",ineq(Price,type="Gini"), by = Year]
colnames(SEgini)[2] = "Gini"

SWgini = RegionLandReq[RegionLandReq$Region == "SOUTH WEST",ineq(Price,type="Gini"), by = Year]
colnames(SWgini)[2] = "Gini"

WMgini = RegionLandReq[RegionLandReq$Region == "WEST MIDS",ineq(Price,type="Gini"), by = Year]
colnames(WMgini)[2] = "Gini"

YORKgini = RegionLandReq[RegionLandReq$Region == "YORK & HSIDE",ineq(Price,type="Gini"), by = Year]
colnames(YORKgini)[2] = "Gini"

Nationalgini = RegionLandReq[,ineq(Price,type="Gini"), by = Year]
colnames(Nationalgini)[2] = "Gini"

Sgini = RegionLandReq[RegionLandReq$Region == "EAST ANGLIA" || RegionLandReq$Region == "SOUTH EAST" || RegionLandReq$Region == "SOUTH WEST",ineq(Price,type="Gini"), by = Year]
colnames(EAgini)[2] = "Gini"

Ngini = RegionLandReq[RegionLandReq$Region == "NORTH WEST" || RegionLandReq$Region == "NORTH EAST" || RegionLandReq$Region == "YORK & HSIDE",ineq(Price,type="Gini"), by = Year]
colnames(NEgini)[2] = "Gini"

colnames(EAgini)[2] <- "EastAnglia"
EAgini$EastMidlands = EMgini$Gini
EAgini$London = Lgini$Gini
EAgini$NorthEast = NEgini$Gini
EAgini$NorthWest = NWgini$Gini
EAgini$SouthEast = SEgini$Gini
EAgini$SouthWest = SWgini$Gini
EAgini$WestMidlands = WMgini$Gini
EAgini$YorkH = YORKgini$Gini
EAgini$National = Nationalgini$Gini

ggplot(EAgini, aes(x = Year, y = EastAnglia, col = "East Anglia"))+
  geom_smooth(se = F) +
  geom_smooth(aes(x=Year, y=London, col="London"),se = F) +
  #geom_smooth(aes(x=Year, y=YorkH, col="York&H"),se = F) +
  #geom_smooth(aes(x=Year, y=NorthWest, col="North West"),se = F) +
  #geom_smooth(aes(x=Year, y=EastMidlands, col="East Midlands"),se = F) +
  #geom_smooth(aes(x=Year, y=NorthEast, col="North East"),se = F) +
  geom_smooth(aes(x=Year, y=SouthEast, col="South East"),se = F) +
  geom_smooth(aes(x=Year, y=SouthWest, col="South West"),se = F) +
  #geom_smooth(aes(x=Year, y=WestMidlands, col="West Midlands"),se = F) +
  geom_smooth(aes(x=Year, y=National, col="National"),se = F) +
  ylab("Gini")

d = ggplot(NationalMean, aes(x = Year, y = Mean)) +
  geom_line()


c = ggplot(InterRegionalDeviations, aes(x = Year, y = NChange)) +
  geom_line()
b = ggplot(NationalMean, aes(x = Year, y = Gini)) +
  geom_line()
grid.arrange(d,b, ncol = 2)

for (i in 1:23){
  a = c(a,(NationalMean$Mean[i+1] - NationalMean$Mean[i])/NationalMean$Mean[i])
}

ggplot(RegionLandReq[Price <= 500000], aes(x=Price)) + geom_density()

for(i in 2:8){
  for(j in i+1:8){
    print("---------------------")
    print(paste("Correlation",colnames(EAgini[i]),colnames(EAgini[j])))
    print(cor(EAgini[i],EAgini[j]))
  }
}

#------------------------------------------Atkinson Calcs

EAatk = RegionLandReq[RegionLandReq$Region == "EAST ANGLIA",ineq(Price,type="Atkinson"), by = Year]
colnames(EAatk)[2] = "Atkinson"

EMatk = RegionLandReq[RegionLandReq$Region == "EAST MIDS",ineq(Price,type="Atkinson"), by = Year]
colnames(EMatk)[2] = "Atkinson"

Latk = RegionLandReq[RegionLandReq$Region == "LONDON",ineq(Price,type="Atkinson"), by = Year]
colnames(Latk)[2] = "Atkinson"

NEatk = RegionLandReq[RegionLandReq$Region == "NORTH EAST",ineq(Price,type="Atkinson"), by = Year]
colnames(NEatk)[2] = "Atkinson"

NWatk = RegionLandReq[RegionLandReq$Region == "NORTH WEST",ineq(Price,type="Atkinson"), by = Year]
colnames(NWatk)[2] = "Atkinson"

SEatk = RegionLandReq[RegionLandReq$Region == "SOUTH EAST",ineq(Price,type="Atkinson"), by = Year]
colnames(SEatk)[2] = "Atkinson"

SWatk = RegionLandReq[RegionLandReq$Region == "SOUTH WEST",ineq(Price,type="Atkinson"), by = Year]
colnames(SWatk)[2] = "Atkinson"

WMatk = RegionLandReq[RegionLandReq$Region == "WEST MIDS",ineq(Price,type="Atkinson"), by = Year]
colnames(WMatk)[2] = "Atkinson"

YORKatk = RegionLandReq[RegionLandReq$Region == "YORK & HSIDE",ineq(Price,type="Atkinson"), by = Year]
colnames(YORKatk)[2] = "Atkinson"

Nationalatk = RegionLandReq[,ineq(Price,type="Atkinson"), by = Year]
colnames(Nationalatk)[2] = "Atkinson"


#------------------------Large regions

Southgini = RegionLandReq[RegionLandReq$Group == 0,ineq(Price,type="Gini"), by = Year]
Southatk = RegionLandReq[RegionLandReq$Group == 0,ineq(Price,type="Atkinson"), by = Year]
Northgini = RegionLandReq[RegionLandReq$Group == 1,ineq(Price,type="Gini"), by = Year]
Northatk = RegionLandReq[RegionLandReq$Group == 1,ineq(Price,type="Atkinson"), by = Year]
Midgini = RegionLandReq[RegionLandReq$Group == 2,ineq(Price,type="Gini"), by = Year]
Midatk = RegionLandReq[RegionLandReq$Group == 2,ineq(Price,type="Atkinson"), by = Year]

ggplot(Southgini, aes(x = Year, y = SouthATK, col = "South-London"))+
  geom_smooth(se = F)+
  geom_smooth(aes(x = Year, y = NorthATK, col = "North"),se = F)+
  geom_smooth(aes(x = Year, y = MidATK, col = "Midlands"),se = F)+
  #geom_smooth(aes(x = Year, y = LondonGini, col = "London"),se = F, linetype = "longdash")+
  #geom_smooth(aes(x = Year, y = NationalGini, col = "National"),se = F, linetype = "longdash")+
  ylab("Atkinson Coefficient") +
  ggtitle("Expanded Regional Atkinson Coefficients")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, margin(t = 3, r = 3, b = 3, l = 3, unit = "pt"))
  )

for (i in 1:23){
  b = ((LR1995$Adjusted[i+1] - LR1995$Adjusted[i])/LR1995$Adjusted[i])*100
  a = c(a,b)
}

for (i in 0:3){
  b = (mean(RegionLandReq$Price[RegionLandReq$Group  == i & RegionLandReq$Year == 2015]) - mean(RegionLandReq$Price[RegionLandReq$Group  == i & RegionLandReq$Year == 2014]))/mean(RegionLandReq$Price[RegionLandReq$Group  == i & RegionLandReq$Year == 2015])
  c = c(c,b)
}

ggplot(RegionalGinis, aes(x = Year, y = National, col = "National"))+
  geom_smooth(se = F) +
  