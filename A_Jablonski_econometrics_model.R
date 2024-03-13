#Project title: 
##"Who votes left? Who votes right? 
##Econometric analysis of political ideology in France"

#Data available at (ESS10.csv): https://www.europeansocialsurvey.org/data

library('lmtest')
library('tseries')
library('car')

#Database & data cleaning----
base = read.csv('ESS10.csv')

#Variable selection
ESS = base[,c('idno', 'cntry','agea','hinctnta','mbtru','eduyrs','gndr',
              'implvdm','imprich','ipeqopt','gvctzpv', 'rlgdgr', 'imbgeco',
              'lrscale','polintr','gincdif','wrclmch','freehms',
              'ipstrgv','ipfrule','ipsuces','imptrad')]
#Age
table(ESS$agea)
ESS = ESS[ESS$agea<100,]

#Income
table(ESS$hinctnta)
ESS = ESS[ESS$hinctnta<11,]
boxplot(ESS$hinctnta, main = 'Income (centiles)')

#Union (membership)
table(ESS$mbtru)
ESS = ESS[ESS$mbtru<4,]
ESS$mbtru[ESS$mbtru == 2] = 1 # = YES
ESS$mbtru[ESS$mbtru == 3] = 0 # = NO
barplot(table(ESS$mbtru),
        main='Union memebership (0 - no, 1 - yes)',
        ylim=c(0,20000))

#Education (years)
table(ESS$eduyrs)
ESS = ESS[ESS$eduyrs<66,]

#Rest of the datac leaning
ESS = ESS[ESS$implvdm<11,]#Important: democracy
ESS$gndr[ESS$gndr == 2] = 0 #Female = 0, Male = 1
ESS = ESS[ESS$ipeqopt<7,] #Important: equal treatment and opportunities
ESS = ESS[ESS$gvctzpv<11,] #Important: goverment protect against poverty
ESS = ESS[ESS$rlgdgr<11,] #Religiosity (degree)
ESS = ESS[ESS$imbgeco<11,] #Immigration bad or good for country's economy
ESS = ESS[ESS$lrscale<11,] #Left-Right scale
ESS = ESS[ESS$polintr<5,] #Politicial interest
ESS = ESS[ESS$gincdif<6,] #Government should reduce differences in income levels
ESS = ESS[ESS$wrclmch<6,] #Worried about the climate change
ESS = ESS[ESS$freehms<6,] #LGBT freedom
ESS = ESS[ESS$ipstrgv<7,] #Important that government is strong and ensures safety
ESS = ESS[ESS$ipfrule<7,] #Important to obey the rules
ESS = ESS[ESS$ipsuces<7,] #Important to be successful
ESS = ESS[ESS$imptrad<7,] #Important: tradition
ESS$lrscale = ESS$lrscale+1


#Standardize variable names
new_names = c('id','cntry','Age','Income','Union','Education','Male',
              'Democracy','Rich','Egalitarianism','Benevolence','Religiosity','Immigration',
              'LR','Sophistication','Inequality','Climate','LGBT',
              'Authoritarianism', 'Obedience','Ambition','Tradition')
colnames(ESS) = new_names

#Limit to France
FR = ESS[ESS$cntry == 'FR',] #1507
barplot(table(FR$Income),main = 'Income distribution for France')

#Model specification----
m1 = lm(LR ~ Income+Age+I(Age^2)+Education+Income*Age+Income*Education+Male+
          Union+Ambition+Authoritarianism+Benevolence+Egalitarianism+Inequality+
          Obedience+Tradition+Climate+Democracy+Immigration+LGBT, data = FR)
summary(m1)
reset(m1) #p-value = 0.006356

#Chow (political sophistication)-----
FR_soph = FR[FR$Sophistication < 2,]
FR_nsoph = FR[FR$Sophistication > 1,]

m1_soph = lm(LR ~ Income+Age+I(Age^2)+Education+Income*Age+Income*Education+Male+
               Union+Ambition+Authoritarianism+Benevolence+Egalitarianism+Inequality+
               Obedience+Religiosity+Tradition+Climate+Democracy+Immigration+LGBT, data = FR_soph)
m1_nsoph = lm(LR ~ Income+Age+I(Age^2)+Education+Income*Age+Income*Education+Male+
               Union+Ambition+Authoritarianism+Benevolence+Egalitarianism+Inequality+
               Obedience+Religiosity+Tradition+Climate+Democracy+Immigration+LGBT, data = FR_nsoph)
RSS_S = sum(m1_soph$residuals^2)
RSS_nS = sum(m1_nsoph$residuals^2)
RSS = sum(m1$residuals^2)

N = nrow(FR)
m=2
K = length(m1$coefficients)
Ft = ((RSS-(RSS_S+RSS_nS))/(K*(m-1)))/((RSS_S+RSS_nS)/(N-m*K))
Ft > qf(0.95,K,N-K) #TRUE >>> split by Sophistication
1-pf(Ft,K,N-K) #p-value = 0.0005073778

#Sophisticated----
m1_soph = lm(LR ~ Income+Age+I(Age^2)+Education+Income*Age+Income*Education+Male+
               Union+Ambition+Authoritarianism+Benevolence+Egalitarianism+Inequality+
               Obedience+Religiosity+Tradition+Climate+Democracy+Immigration+LGBT, data = FR_soph)
summary(m1_soph)
reset(m1_soph) #p-value = 0.04246 >>> We stil need to fix the function form

#1st model in the table 2
m2_soph = lm(LR ~ Income+I(Income^2)+Age+I(Age^2)+Education+Income*Age+Income*Education+Male+
               Union+Ambition+Authoritarianism+Benevolence+Egalitarianism+Inequality+
               Obedience+Religiosity+Tradition+Climate+Democracy+Immigration+LGBT, data = FR_soph)
reset(m2_soph)
bptest(m2_soph)
summary(m2_soph)
jarque.bera.test(m2_soph$residuals)
dwtest(m2_soph)

#2nd model in the table 2
m3_soph = lm(LR ~ Income+I(Income^2)+Age+Education+Income*Age+Income*Education+Male+
               Authoritarianism+Inequality+
               Religiosity+Climate+Immigration, data = FR_soph)
reset(m3_soph)
summary(m3_soph)
bptest(m3_soph)
jarque.bera.test(m3_soph$residuals)

#3rd model in the table 2
m4_soph = lm(LR ~ Income+I(Income^2)+Male+
               Authoritarianism+Inequality+
               Religiosity+Climate+Democracy+Immigration, data = FR_soph)
reset(m4_soph)
summary(m4_soph)
bptest(m4_soph)

table2 <- stargazer(m2_soph,m3_soph,m4_soph, type = 'latex', align = TRUE)

#Non-sophisticated----
m2_nsoph = lm(LR ~ Income++Age+Education+Income*Age+Income*Education+Male+
                Union+Ambition+Authoritarianism+Benevolence+Egalitarianism+Inequality+
                Obedience+Religiosity+Tradition+Climate+Democracy+Immigration+LGBT+Male*Religiosity+
                Tradition*Religiosity+Male*Benevolence, data = FR_nsoph)
summary(m2_nsoph)
reset(m2_nsoph) #p-value = 0.0724
bptest(m2_nsoph)#p-value = 0.02104 >>> hetero
bptest(LR ~ Age, data = FR_nsoph) #p-value = 0.0729

#Weighted Least Squares
WLS = lm(LR ~ Income++Age+Education+Income*Age+Income*Education+Male+
           Union+Ambition+Authoritarianism+Benevolence+Egalitarianism+Inequality+
           Obedience+Religiosity+Tradition+Climate+Democracy+Immigration+LGBT+Male*Religiosity+
           Tradition*Religiosity+Male*Benevolence-1, data = FR_nsoph, weights = 1/Age)
bptest(WLS)
reset(WLS)
jarque.bera.test(WLS$residuals)
summary(WLS)

WLS2 = lm(LR ~ Income+Age+Male+
            Union+Authoritarianism+Benevolence+Egalitarianism+Inequality+
            Religiosity+Tradition+Climate+Democracy+Immigration+Male*Religiosity, data = FR_nsoph, weights = 1/Age)
summary(WLS2)
reset(WLS2)
bptest(WLS2)

#Generalized Least squares
e = m3_nsoph$residuals
e2 = e^2
ln_e2 = log(e2)
aux = lm(ln_e2~FR_nsoph$Age)
ln_e2_hat = fitted(aux)
e2_hat = exp(ln_e2_hat)
GLS = lm(LR ~ Income+Age+Education+Male+
           Union+Ambition+Authoritarianism+Benevolence+Egalitarianism+Inequality+
           Obedience+Religiosity+Tradition+Climate+Democracy+Immigration+LGBT+Male*Religiosity+
           Tradition*Religiosity+Male*Benevolence, data = FR_nsoph,weights = 1/e2_hat)
reset(GLS)
bptest(GLS)

#Robust std. errors
robust1 = coeftest(m2_nsoph, vcov = vcovHC(m2_nsoph, type = "HC3"))
robust1

rbst2 = lm(LR ~ Income++Age+Male+
             Union+Authoritarianism+Egalitarianism+Inequality+
             Religiosity+Tradition+Climate+Democracy+Immigration+Male*Religiosity,data = FR_nsoph)
reset(rbst2)
robust2 = coeftest(rbst2, vcov = vcovHC(rbst2, type = "HC3"))
robust2

#Tables----
table2 <- stargazer(m2_soph,m3_soph,m4_soph, type = 'latex', align = TRUE)


table3 <- stargazer(m2_nsoph,WLS,WLS2,GLS,robust1,robust2, type = 'text', 
                    align = TRUE, column.labels = c('OLS', 'WLS1', 'WLS2','GLS'),
                    covariate.labels=FALSE)


table(FR$Income)
355/1507