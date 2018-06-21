library(R.utils) # Utils
library(readr) # Reading text tables
library(readxl) # Reading excel

# TODO: Process data with Rcpp instead of Fortran library execution

wd = "/Volumes/Data/Work/_Kireeva/2018/"
# wd = "X:/Work/_Kireeva/2018/"

# dyn.load("DllGrWatnotab.dll") # Windows
dyn.load("DllGrWatnotab.so") # Linux or macOS
stopifnot(is.loaded("maingrwat")) # Stop execution if subroutine is not loaded. 

use.excel = TRUE # Initialize parameters from excel file
params.file = "params.xlsx"

# FILES -------------------------------------------------------------------

infilehydro <- 'in.txt'
nlines<- countLines('in.txt')
lenin <- nchar(infilehydro)

infilemeteo <- 'inmeteo.txt'
leninmeteo <- nchar(infilemeteo)

outfileAllGrWat <- 'AllGrWat.txt'
lenallGrWat <- nchar(outfileAllGrWat)

outfileTotal <- 'Total.txt'
lenTotal <- nchar(outfileTotal)

outfileparameters <- 'parameters2.txt'
lenparameters <- nchar(outfileparameters)

# MANUAL PARAMETERS --------------------------------------------------------------

# All these parameters will be overwritten if use.excel == TRUE

polmon <- vector("integer", 2)
polkol <- vector("integer", 3)
polgrad <- vector("numeric", 2)

mome <- as.integer(11) # месяц, с которого начинается зимняя межень
grad <- 1.5  # интенсивность спада/подъема расхода воды за счет грунтовой составляющей, выраженная в долях от расхода предшествующего дня
grad1 <- 2.0  # интенсивность спада/подъема расхода воды на спаде половодья за счет грунтовой составляющей, выраженная в долях от расхода предшествующего дня
kdQgr1 <-as.double(150)  # максимально допустимое повышение грунтовой составляющей по сравнению с начальным значением [уже после спада половодья] в процентах###
polmon[1] <- as.integer(1) # самый ранний месяц, когда может наблюдаться начало половодья
polmon[2] <- as.integer(5) # самый поздний месяц, когда может наблюдаться начало половодья
polkol[1] <- as.integer(15) # количество дней с начала половодья с устойчивым увеличением в среднем  <- установленному градиенту [polgrad[1]]
polkol[2] <- as.integer(25) # количество дней с начала половодья с устойчивым увеличением в среднем на > <- 0 %
polkol[3] <- as.integer(30) # количество дней с начала половодья, за которое проходит основная волна половодья
polgrad[1] <- 7 # значения градиента устойчивого увеличения расхода в начале половодья
polgrad[2] <- 6 # во сколько раз средний расход воды за половодье [polkol[3]] гарантированно превышает предшествующий половодью меженный уровень
prodspada <- as.integer(90) # кол-во дней с даты max расхода деяствия grad, после этого переходит на grad1
nPav <- as.integer(5) #количество дней, за которое анализируется сумма осадков и сумма температур
nZam <- as.integer(15) #колическтво дней за которое анализируются температуры воздуха для определения заморозков
nWin <- as.integer(5) #колическтво дней за которое анализируются температуры воздуха для определения начала змней межени
Pcr <- 30.0 #мм осадков - критическая сумма, которая вызывает значимый паводок
Tcr1 <- 0.0 #C средняя температура за nPav, < <-  которой - снег, >  тогда паводки 
Tzam <- -30.0 #— средняя суточная температура события "заморозок"
Twin <- -1.0 #— средняя суточная температура не более Twin к ряду nWin дней для перехода к зимнему сезону
SignDelta <- 0.01#значимый перепад расходов [для выбора значимых спадов/подъемов при отделении паводков] в долях от максимального расхода воды этого года
SignDelta1 <- SignDelta*0.15 #значимый перепад для поиска лок.максимума
FlagGaps <- as.integer(-999) # каким образом обозначаются пропуски в рядах
PavRate <- 0.001 # доля от объема половодья когда паводок считается стоющим внимания 
InterpolStep <- as.integer(1)  #предельное количество ячеек с пропусками подряд, которые заполяются интерполяцией 

polmoni<-as.integer(2)
polkoli<-as.integer(3)
polgradi<-as.integer(2)

# EXCEL PARAMETERS --------------------------------------------------------

if (use.excel){
  params <- read_excel(params.file, 2)
  
  values <- params$VALUE
  
  mome <- as.integer(values[1])
  grad <- as.double(values[2])
  grad1 <- as.double(values[3])
  kdQgr1 <-as.double(values[4])  
  polmon[1] <- as.integer(values[5])
  polmon[2] <- as.integer(values[6])
  polkol[1] <- as.integer(values[7])
  polkol[2] <- as.integer(values[8])
  polkol[3] <- as.integer(values[9])
  polgrad[1] <- as.double(values[10])
  polgrad[2] <- as.double(values[11])
  prodspada <- as.integer(values[12])
  nPav <- as.integer(values[13])
  nZam <- as.integer(values[14])
  nWin <- as.integer(values[15])
  Pcr <- as.double(values[16])
  Tcr1 <- as.double(values[17])
  Tzam <- as.double(values[18])
  Twin <- as.double(values[19])
  SignDelta <- as.double(values[20])
  SignDelta1 <- as.double(values[21])
  FlagGaps <- as.integer(values[22])
  PavRate <- as.double(values[23])
  InterpolStep <- as.integer(values[24])
}

# RUN -------------------------------------------------------------------

.Fortran("maingrwat", nlines, mome, polmon, polmoni, polkol, polkoli, prodspada, nPav, nZam, nWin, FlagGaps, InterpolStep, grad, grad1, kdQgr1, polgrad, polgradi, Pcr, Tcr1, Tzam, Twin, SignDelta, SignDelta1, PavRate, infilehydro, lenin, infilemeteo, leninmeteo, outfileAllGrWat, lenallGrWat, outfileTotal, lenTotal, outfileparameters, lenparameters)