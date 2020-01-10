integer polmon(2), polkol(3), prodspada, nPav, nZam, nWin, FlagGaps, InterpolStep, polkolMount(2), mome
real grad, grad1, kdQgr1, polgrad(2), Pcr, Tzam, Twin, SignDelta, SignDelta1, PavRate, Tcr1, pgrad, polgradMount, gradabs
character infilehydro*19, infilemeteo*24, outfileAllGrWat*26, outfileTotal*22, outfileparameters*28, outfilepavs*21
integer lenin/19/, leninmeteo/24/, lenallGrWat/26/,  lenTotal/22/, lenparameters/28/, lenpavs/21/
logical ModeMountain

write (infilehydro, '(a<lenin>)') 'files\Chi-Ob\in.txt'
write (infilemeteo, '(a<leninmeteo>)') 'files\Chi-Ob\inmeteo.txt'
write (outfileAllGrWat, '(a<lenallGrWat>)') 'files\Chi-Ob\AllGrWat.txt'
write (outfileTotal, '(a<lenTotal>)') 'files\Chi-Ob\Total.txt'
write (outfileparameters, '(a<lenparameters>)') 'files\Chi-Ob\parameters.txt'
write (outfilepavs, '(a<lenpavs>)')'files\Chi-Ob\pavs.txt'

! калибруемые параметры

mome=11 ! месяц, с которого начинается зимняя межень
grad=1 ! интенсивность спада/подъема расхода воды за счет грунтовой составляющей, выраженная в долях от расхода предшествующего дня
grad1=13! интенсивность спада/подъема расхода воды на спаде половодья за счет грунтовой составляющей, выраженная в долях от расхода предшествующего дня
kdQgr1=1500  ! максимально допустимое повышение грунтовой составляющей по сравнению с начальным значением (уже после спада половодья)
polmon(1)=1 ! самый ранний месяц, когда может наблюдаться начало половодья
polmon(2)=5 ! самый поздний месяц, когда может наблюдаться начало половодья
polkol(1)=5 ! количество дней с начала половодья с устойчивым увеличением в среднем =установленному градиенту (polgrad(1))
polkol(2)=15 ! количество дней с начала половодья с устойчивым увеличением в среднем на >=0 %
polkol(3)=25 ! количество дней с начала половодья, за которое проходит основная волна половодья
polgrad(1)=10 ! значения градиента устойчивого увеличения расхода в начале половодья
polgrad(2)=6 ! во сколько раз средний расход воды за половодье (polkol(3)) гарантированно превышает предшествующий половодью меженный уровень
prodspada=90 ! кол-во дней с даты max расхода деяствия grad, после этого переходит на grad1
nPav=2 !количество дней, за которое анализируется сумма осадков и сумма температур! НЕЧЕТНОЕ ЧИСЛО!
nZam=5 !колическтво дней за которое анализируются температуры воздуха для определения заморозков! НЕЧЕТНОЕ ЧИСЛО!
nWin=5 !колическтво дней за которое анализируются температуры воздуха для определения начала змней межени
Pcr=0.2 !мм осадков - критическая сумма, которая вызывает значимый паводок
Tcr1=0 !C средняя температура за nPav, <= которой - снег, >  тогда паводки 
Tzam=-5 !С средняя суточная температура события "заморозок"
Twin=-1 !С средняя суточная температура не более Twin к ряду nWin дней для перехода к зимнему сезону
SignDelta=0.01!значимый перепад расходов (для выбора значимых спадов/подъемов при отделении паводков) в долях от максимального расхода воды этого года
SignDelta1=SignDelta*0.15 !значимый перепад для поиска лок.максимума
FlagGaps=-999 ! каким образом обозначаются пропуски в рядах
PavRate=0.001 ! доля от объема половодья когда паводок считается стоющим внимания 
InterpolStep=15 !предельное количество ячеек с пропусками подряд, которые заполяются интерполяцией 
Tcr2=2 !C средняя температура за nPav когда наблюдается талый паводок
gradabs=1000 ! интенсивность спада/подъема расхода воды за счет грунтовой составляющей, выраженная в м3/с/день
!--------  РЕЖИМ ГОРНЫХ РЕК
ModeMountain=.false.! .true. если горная, иначе - .false.
!--------
pgrad=2 !в варианте горном - градиент базисной составляющей половодья в м3/с/день (!!!!)
polkolMount(1)=30 !продолжительность периода в течение которого, средний расход расход воды за polgradMount(2) дней не меньше чем в polgradMount раз больше предыдущего расхода
polkolMount(2)=5 !количество дней, средний расход за которые в период половодья не меньше чем в polgradMount раз больше предыдущего расхода
polgradMount=1.5 !во сколько раз средний расход воды за каждые polgradMount(2) дней в течение половодья превышает предполоводный уровень




call mainprog(polmon, 2, polkol,3, prodspada, nPav, nZam, nWin, FlagGaps, InterpolStep,grad, grad1, kdQgr1, polgrad,2, Pcr, Tcr1, Tcr2, Tzam, Twin, SignDelta, SignDelta1, PavRate, infilehydro, lenin, infilemeteo, leninmeteo, outfileAllGrWat, lenallGrWat, outfileTotal, lenTotal, outfileparameters, lenparameters, ModeMountain, pgrad, outfilepavs, lenpavs, polkolMount, 2, polgradMount, gradabs, mome)
! подпрограммы:

contains

subroutine mainprog (polmon, ss1, polkol,ss2, prodspada, nPav, nZam, nWin, FlagGaps, InterpolStep,grad, grad1, kdQgr1, polgrad,ss3, Pcr, Tcr1, Tcr2, Tzam, Twin, SignDelta, SignDelta1, PavRate, infilehydro, lenin, infilemeteo, leninmeteo, outfileAllGrWat, lenallGrWat, outfileTotal, lenTotal, outfileparameters, lenparameters, ModeMountain, pgrad, outfilepavs, lenpavs, polkolMount, ss4, polgradMount, gradabs, mome)

integer ss1, ss2, ss3, ss4
integer lenin, leninmeteo, lenallGrWat,  lenTotal, lenparameters
character (len=lenin)::infilehydro
character (len=leninmeteo)::infilemeteo
character (len=lenallGrWat)::outfileAllGrWat
character (len=lenTotal)::outfileTotal
character (len=lenparameters)::outfileparameters
character (len=lenpavs)::outfilepavs
integer i, l, nmax, qua, n, nlast, k, kk, nn, NF, Day1(50000), Year1(50000), Mon1(50000), Day(1000), Year(1000), Mon(1000), mome,  krat, polmon(ss1),donep(4), polkol(ss2), iy(100) , iy1, in, ny(100), polend(100), ngrpor, nmaxpavs, nmaxpavw, prodspada ! iy - строка в исходном массиве - последний член данного года, iy1 - строка в исходном массиве - последний член ряда хвостика перед началом первого года, in - строка начала половодья (не массив!все время заново поределяется!!), polend - номер члена массива Qin конца половодья, ngrpor -  порядковый номер назначенного Qgr из массива Qin
real Qin1(50000),Pin1(50000), Tin1(50000), Qin(1000),Pin(1000),Tin(1000), Qgr(1000), dQ, dQgr, Qgrlast, Qgrlast1, dQgr1, dQgr2, Qygr(100),Qy(100),  Vm(1001), MM(4), Q30s(100), Q30w(100), i30sn(100), i30wn(100), Q10s(100), Q10w(100), i10sn(100), i10wn(100), Res(4), Q5s(100), Q5w(100), i5sn(100), i5wn(100), grad, polgrad(ss3), Wpol(100,3), Wpavs(100,2), Wpavw(100,2), Qmaxpavs(100), Qmaxpavw(100), Wy(100), Wgr(100), WgrS(100), WgrW(100), WW(100), Qpb(1000), Qpblast, dQpb2
real Qpol(1000), Qpav(1000), Qthaw(1000), deltaQ(1000), NegDeltaCum, gradQ(1000), kdQgr1
character nme*6(99), nmout*10(99), xr*2, dchar*10,dchar1*10, dchar2*10, dchar3*10, date30s(2)*10,date30w(2)*10, date10s(2)*10,date10w(2)*10, date5s(2)*10,date5w(2)*10, datestart*10, datemax*10, datemaxpavs*10, datemaxpavw*10, datepolend*10, DateTotal*10  ! 99 - максимальное количество файлов
integer nPav, nZam,  HalfSt, HalfStZ, nWin, HalfStW !nPav- количество дней, за которое анализируется сумма осадков, nZam - колическтво дней за которое анализируются температуры воздуха для определения заморозков, HalfSt - половина nPav-1, HalfStZ -  половина nZam-1
integer FlagsPcr(1000), FlagsPlusTemp(1000), FlagsMinusTemp(1000)! FlagsPcr - флаги событий критических осадков,  FlagsPlusTemp - флаги событий оттепелей по температурам, FactMinusTemp - флаги событий замороков по температурам
real Pcr, Psumi, Tcr1, Tcr2, Tsri, SignDelta, z, Qo, afunc, bfunc, SignDelta1, Twin, pgrad ! Pcr - критическая сумма осадков, которая вызывает значимый паводок, Psumi - сумма осадков за nPav дней, Tcr1 - средняя температура за nPav, <= которой - снег, >  тогда паводки Tcr2 - средняя температура за nPav когда наблюдается талый паводок, SignDelta - значимый пеерпад расходов (для выбора значимых спадов/подъемов при отделении паводков) в долях от максимального расхода воды этого года, z - коэффициент затухания экспаненты, Qo - параметр экспаненциальной функции
integer FactPcr(100), PcrDates(100,100), FactPlusTemp(100), PlusTempDates(100,100), FactMinusTemp(100), MinusTempDates(100,100)
integer Flex1, startPol(100), LocMax1, Bend1, Flex2, Bend2, ecycle, kkk, p, pp! Flex1 - точка перегиба -/+, startPol - точка начала половодья, LocMax1 - номер члена ряда локального максимума паводка, Bend1 - начало дождевого паводка на подъеме, Flex2 - точка перегиба -/+, Bend2 - конец паводка на спаде
integer YGaps(100), FlagGaps, NumGaps, SummerEnd(100), DaysPavsSum(100), DaysThawWin(100), SumProd(100), WinProd(100), FactGapsin1(50000), FactGapsin2(50000) ! YGaps - маркеры годов в которых наблюдаются пробелы, FlagGaps - каким образом обозначаются пробелы, NumGaps - число пропусков, SummerEnd - ное член ряда - конец летнего периода, DaysPavsSum - количество дней с паводками летов, DaysThawWin(100) - количество дней с оттепелями зимой, SumProd, WinProd - продолжительность летней и зимней межени соответственно, FactGapsin1(50000) - сколько пропусков в исходных данных после этого срока, включая этот срок
integer cgaps, InterpolStep, YGapsRow(100), MarkWarm, YearStartGaps, CountPavs(100), CountThaws(100), MarkUpPavs, FlexPrev !InterpolStep - предельное количество ячеек с пропусками подряд, которые заполяются интерполяцией 
real, allocatable :: Awin(:), Asum(:) !  Входной  массив для расчета Cv ряда
real CvWin(100), CvSum(100), PavRate, VolPav, VolPav1, SingleWpav(100,50), SingleWthaw(100,50), polgradMount, dQabs, dQgr2abs, gradabs, Qport(1000), Psums(1000), Tsrs(1000)
real, allocatable :: Qcrop(:), Qinpol(:), Qcroprize(:)
integer, allocatable ::  Moncrop(:), Yearcrop(:), Daycrop(:)
integer lengthcrop, NumGapsY(100), kstart, kfirst, kmax, polkolMount(ss4)
logical ModeMountain


open (1, file=infilehydro)
open (2, file=infilemeteo)


open(3, file=outfileAllGrWat) ! файл, в который записываются значения всех составляющих посуточно подряд, не разделяя на года
open(4, file=outfileTotal) ! файл с выходной статистикой по годам
open (5, file=outfileparameters)
open(6,file=outfilepavs) !выходной файл с данными по паводкам
open(7, file='logGW.txt')

write(3,'(1x, a150)') 'Date  Qin Qgr Qpol Qpav Qthaw Qpb Tin Pin' 
write(4,'(1x, a460)') 'year_number Year1 Year2 datestart datepolend Qy Qmax datemax Qygr Qmmsummer monmmsummer Qmmwin nommwin &
             Q30s date30s(1) date30s(2) &
			 Q30w date30w(1) date30w(2) &
             Q10s date10s(1) date10s(2) &
			 Q10w date10w(1), date10w(2) &
			 Q5s date5s(1) date5s(2) &
			 Q5w date5w(1), date5w(2) &
			 Wy Wgr Wpol1(km3) Wpol2 Wpol3 Wpavs1 &
			 Wpavs2  Wpavthaw1 Wpavthaw2 WgrS WS WgrW WW Qmaxpavs datemaxpavs Qmaxpavthaw datemaxpavthaw &
			 SumProd DaysPavsSum WinProd DaysThawWin CvWin CvSum CountPavs CountThaws' 


write(5,'(1x, a150)')'Year polgrad(1) polgrad(2) polkol(1) polkol(2) polkol(3) polkolMount(1) polkolMount(2) polgradMount'

write(6,'(1x, a500)') 'Type CountPavs Year dstart dend dmax maxQ VolPav VolPavFULL &
				FlagsPcr_start FlagsPcr_start_minus_HalfSt Psums_start Psums_start_minus_HalfSt &
				Pin_start Pin_start_minus_1 Pin_max FlagsPlusTemp_start FlagsPlusTemp_start_minus_HalfSt &
				Tsrs_start Tsrs_start_minus_HalfSt Tin_max FlagsMinusTemp_end FlagsMinusTemp_end_minus_HalfStZ &
				Tsrs_end Tsrs_end_minus_HalfSt Tin_end Tin_max_plus Qin_start max_start end_start Qin_max_minus_Qin_start dmax_rel'


do ii=1, 100
	 iy(ii)=-99
	 ny(ii)=0
	 YGaps(ii)=0
end do

 do ii=1,50000
	  Qin1(ii)=0
	  Day1(ii)=0
	  Mon1(ii)=0
	  Year1(ii)=0
	  Tin1(ii)=0
	  Pin1(ii)=0
 end do



do while (.not.eof(1)) !считывание исходных данных
	 w=w+1
	 read(1,*) Day1(w),Mon1(w),Year1(w), Qin1(w)
	 read(2,*) Tin1(w), Pin1(w)

end do


!определене наличия пропусков в исходных данных

do l=1,w
 cgaps=0
 if (Qin1(l)==FlagGaps) then
    cgaps=1
	do ll=l+1,w
		if (Qin1(ll).ne.FlagGaps) exit
		cgaps=cgaps+1
	end do
	FactGapsin1(l)=cgaps
  end if
end do

!заполнение пропусков

do l=2,w
 if (FactGapsin1(l)>=1) then
   if (FactGapsin1(l)<=InterpolStep)then 
	   do ll=l,l+FactGapsin1(l)-1
	    if (Qin1(l-1).ne.FlagGaps.and.Qin1(l+FactGapsin1(l)).ne.FlagGaps) then
			Qin1(ll)=Qin1(l-1)*(FactGapsin1(l)-(ll-l+1)+1)/(FactGapsin1(l)+1)+Qin1(l+FactGapsin1(l))*(FactGapsin1(l)+1-(FactGapsin1(l)-(ll-l+1)+1))/(FactGapsin1(l)+1)
		end if
	   end do
	else
		FactGapsin2(l)=FactGapsin1(l)
	end if
 end if
end do 


!определение начала половодья, границ годов

ng=1
donep(4)=Year1(1)

do ii=1,100
	NumGapsY(Year1(ii))=0
end do
NumGaps=0
l=0




do while(l<=w)
 l=l+1
 polQsum=0
 do tt=1,3
  donep(tt)=-1
 end do

  !проверка пропусков в исходных рядах
  if (Qin1(l)==FlagGaps) then
	NumGapsY(Year1(l))=NumGapsY(Year1(l))+1
	NumGaps=NumGaps+1
  end if
  
  if(Year1(l)>donep(4)) then !перескачили через 1 год - так и не нашли половодье (из-за неправильных параметров или пропусков)
   
    if (NumGapsY(Year1(l)-1)>=30) then
		if (ng>1) iy(ng-1)=-99 ! неизвестен номер последнего члена ряда предыдущего года в общем массиве
		YGaps(ng)=1
		YGaps(ng-1)=1
		ng=ng+1
		NumGaps=0
		donep(4)=Year1(l)
    else 
        hh=0
		do while (iy(ng-hh)<0) !сколько лет назад последний раз iy было
			hh=hh+1
		end do
		lengthcrop=l-(iy(ng-hh)+300)+1
		allocate(Qcrop(lengthcrop),Moncrop(lengthcrop), Yearcrop(lengthcrop), Daycrop(lengthcrop))    
		do jk=1,lengthcrop
			Qcrop(jk)=Qin1(iy(ng-hh)+300+jk-1)
			Moncrop(jk)=Mon1(iy(ng-hh)+300+jk-1)
			Yearcrop(jk)=Year1(iy(ng-hh)+300+jk-1)
			Daycrop(jk)=Day1(iy(ng-hh)+300+jk-1)
		end do
		print*, Year1(l)-1
		call Polfinder (Qcrop, lengthcrop, Moncrop, lengthcrop, Yearcrop, lengthcrop, Daycrop, lengthcrop, polmon, 2, &
						FlagGaps, polkol, 3, polgrad, 2, donep, 4, iy(ng-1), ng, iy(ng-hh)+300, polkolMount, 2, polgradMount, ModeMountain)
					  
		write(7,*) 'polfind done'
		deallocate (Qcrop, Moncrop, Yearcrop, Daycrop)
   end if
  end if 
 
 if(Year1(l)==donep(4)) then 
  
  if(Mon1(l)>=polmon(1).and.Mon1(l)<=polmon(2).and.Qin1(l).ne.FlagGaps) then
  
  dQ=0
  do ff=1,polkol(1) !проверка критерия начала половодья №1 - устойчивое увеличение со средним градиентом больше или равном заданному (polgrad(1))
   if(Qin1(l+ff)==FlagGaps.or.Qin1(l+ff-1)==FlagGaps) goto 8787
   dQ=dQ+(Qin1(l+ff-1)-Qin1(l+ff))/Qin1(l+ff-1)*100/polkol(1)
  end do
   if (dQ>-polgrad(1)) then
    donep(1)=-1
	else
	donep(1)=1 ! критерий 1 удовлетворен
	write(7,*) Year1(l),Mon1(l), Day1(l), 'd1'
   end if

  dQ=0
  do ff=1,polkol(2)  !проверка критерия начала половодья №2 - устойчивое увеличение не заканчивается polkol(2) дней
   dQ=dQ+(Qin1(l+ff-1)-Qin1(l+ff))/Qin1(l+ff-1)*100/polkol(2)
  end do
   if (dQ>0) then
    donep(2)=-1
	else
    donep(2)=1  ! критерий 2 удовлетворен
	write(7,*) Year1(l),Mon1(l), Day1(l), 'd2'
   end if

	if (ModeMountain) then

		  donep(3)=1 
		  do ff=1, polkolMount(1) !критерий 3 для гор превышение в течении polkolMount(1) каждые polkolMount(2) дней расхода воды в polgradMount раз
		    polQsum=0
			do fff=ff,ff+polkolMount(2)
			    !print*, fff, Qin1(l+fff), polQsum
				polQsum=polQsum+Qin1(l+fff)
			end do
			!print*, polQsum, Qin1(l)*polkolMount(2), Qin1(l), polkolMount(2)
			if (polQsum/(Qin1(l)*polkolMount(2))<polgradMount) then
				donep(3)=-1
				!print*, 'donep(3)=-1'
			end if
		  end do
		  if (donep(3)==1) then
		      write(7,*) Year1(l),Mon1(l), Day1(l), 'd3', ng 
		   end if


	else

		  do ff=1,polkol(3) !критерий 3 - превышение среднего расхода воды за polkol(3) дней расхода дня начала половодья (Qin(l)) в  polgrad(2) раз
		   polQsum=polQsum+Qin1(l+ff-1)
		  end do
		  if (polQsum/(Qin1(l)*polkol(3))<polgrad(2)) then
		   donep(3)=-1
		  else
		   donep(3)=1 ! критерий 3 удовлетворен
		   write(7,*) Year1(l),Mon1(l), Day1(l), 'd3', ng 
		  end if
	end if


  8787 if (donep(1)==1.and.donep(2)==1.and.donep(3)==1) then !все 3 критерия удовлетворены
	if(ng>1) then
			iy(ng-1)=l-1 ! номер последнего члена ряда предыдущего года в общем массиве
	else
			iy1=l-1
	end if
    donep(4)=Year1(l)+1  !следующее начало половодья уже искать в следующем году
    write(7,*) ng, l, Year1(l),donep(4), iy(ng-1)
    if (l>1.and.NumGaps>0) then
		YGaps(ng-1)=1 !в предыдущем году есть пропуски в значениях
		NumGaps=0
    end if 
    ng=ng+1 ! число лет
  end if

  end if 
 
 end if

end do

if(NumGaps>0) then
	YGaps(ng-1)=1
end if


ng=ng-2

!определение сколько лет подряд были пропуски

do l=1,ng
 cgaps=0
 if (YGaps(l)==1) then
    cgaps=1
	do ll=l+1,ng
		if (YGaps(ll).ne.1) exit
		cgaps=cgaps+1
	end do
	YGapsRow(l)=cgaps
  end if
end do



print*, 'kolichestvo let', ng


do NF=1,ng
!print*, NF; pause
! Обнуление массивов
 Qy(NF)=0
 Qygr(NF)=0
 Qmax=0
 i=0
 srtr=10000
 ngrpor=0
 Wpol(NF,1)=0
 Wpol(NF,2)=0
 Wpavs(NF,1)=0
 Wpavs(NF,2)=0
 Wpavw(NF,1)=0
 Wpavw(NF,2)=0
 WgrS(NF)=0
 WgrW(NF)=0
 WW(NF)=0
 Qmaxpavs(NF)=0
 Qmaxpavw(NF)=0
 FactPcr(NF)=0
 FactPlusTemp(NF)=0
 do jjj=1,100
	PcrDates(NF,jjj)=0
	PlusTempDates(NF,jjj)=0
 end do

 
 do ii=1,1000
	  Qgr(ii)=-999
	  if (ModeMountain) then
		Qpb(ii)=-999
	  else
		Qpb(ii)=0
	  end if
	  Qin(ii)=0
	  Day(ii)=0
	  Mon(ii)=0
	  Year(ii)=0
	  Vm(ii)=0
	  Pin(ii)=0
	  Tin(ii)=0
	  FlagsPcr(ii)=0
	  FlagsPlusTemp(ii)=0
	  FlagsMinusTemp(ii)=0
	  Qpav(ii)=0
	  Qthaw(ii)=0
	  CountPavs(ii)=0
	  CountThaws(ii)=0
 end do



 if (YGaps(NF)==1) then !если год с пропусками - не считаем вообще

	if (YGapsRow(NF-1)<1) then
		YearStartGaps=Year1(iy(NF-1))
		
		do pp=0,YGapsRow(NF)-1
			write(4,'(1x, i6, 2i10)')NF+pp, YearStartGaps+pp, YearStartGaps+pp+1
		end do	
		do pp=iy(NF-1), iy(NF+YGapsRow(NF)-1)
			call Datechar (Day1(pp), Mon1(pp), Year1(pp), dchar)
			write(3,'(1x, a15, f10.2, a30, 2f10.2)') dchar, Qin1(pp), '-999 -999 -999 -999 -999', Tin1(pp), Pin1(pp)
		end do
    end if
    cycle
 end if
!назначение массива Qin на данный год
 if (NF==1) in=iy1+1
 if (NF>1) in=iy(NF-1)+1
 print*, 'in', in, iy(NF)


 do ii=in, iy(NF)
      !print*, Year1(ii), Mon1(ii), Day1(ii), Qin1(ii), Pin1(ii), Tin1(ii);pause
	  Qin(ii-in+1)=Qin1(ii)
	  Day(ii-in+1)=Day1(ii)
	  Mon(ii-in+1)=Mon1(ii)
	  Year(ii-in+1)=Year1(ii)
	  Pin(ii-in+1)=Pin1(ii)
	  Tin(ii-in+1)=Tin1(ii)
 end do

 ny(NF)=iy(NF)-in+1  ! количество дней в данном водхоз году




 allocate (Qinpol(prodspada*2))
 do ii=1, prodspada*2
   Qinpol(ii)=Qin(ii)
 end do

 
 nmax=MAXLOC(Qinpol,1)
 Qmax=Qinpol(nmax)

 deallocate (Qinpol)
 write(7,*) '1st interation max', Qmax, Day(nmax), Mon(nmax), Year(nmax)
 print*, NF, Year(1), YGaps(NF), ny(NF), nmax, Qmax, Day(nmax), Mon(nmax), Year(nmax)

 do n=1,ny(NF) !нахождение точек грунтового стока в гидрогрфе за данный вх год
 !print*,'n', n, nmax, Qin(n)

     deltaQ(n)=Qin(n+1)-Qin(n)
     gradQ(n)=deltaQ(n)/Qin(n)*100

     
     
	 if (n==1.or.n==ny(NF)) then
	      
		  Qgr(n)=Qin(n)
		  Qgrlast1=Qin(n)
		  Qgrlast=Qin(n)
		  nlast=n
	 else

		  if(.not.ModeMountain) then
			if (n==nmax) then
				Qgr(n)=0
				write(7,*) 'Qgr(n)=0', Year(n), Mon(n), Day(n)
			end if
		  end if

		  dQ=abs(Qin(n-1)-Qin(n))/Qin(n-1)*100 !модуль градиента между текущим и предыдущим днем [в % от предыдущего дня]
		  dQabs=abs(Qin(n-1)-Qin(n)) !модуль градиента между текущим и предыдущим днем [в м3/с/день]
		  dQgr=-(Qgrlast-Qin(n))/Qgrlast*100 !разница в процентах между текущим днем и ближайшим предыдущим днем выделенным как грунтовый [в % от предыдущего дня] 
		  dQgr1=-(Qgrlast1-Qin(n))/Qgrlast1*100 !градиент между текущим днем и первым днем выделенным как грунтовый
		  dQgr2=abs((Qgrlast-Qin(n))/(n-nlast+1)/Qgrlast*100) !модуль градиента между текущим днем и ближайшим предыдущим днем выделенным как грунтовый
		  dQgr2abs=abs((Qgrlast-Qin(n))/(n-nlast+1)) !модуль градиента между текущим днем и ближайшим предыдущим днем выделенным как грунтовый

			!print*, dQ, dQabs, dQgr2, dQgr1
		  if (Qin(n)>Qgrlast) then ! чтобы не рисовал прямых линий
		  !print*, 'Qin(n)>Qgrlast', Qin(n), Qgrlast
			   if((n-nmax)>prodspada) then  !  длина периода с начала половодья, когда градиенты спада и подъема повышены
					if(dQ>grad.or.dQgr1>kdQgr1.or.dQgr2>grad.or.dQabs>gradabs.or.dQgr2abs>gradabs) then !фильтр для дней сильно после половодья
   						cycle
					end if
			   else
					if(dQ>grad1.or.dQgr2>grad1.or.dQgr1>kdQgr1.or.dQabs>gradabs.or.dQgr2abs>gradabs) then !фильтр для дней сразу же после половодья
						cycle
					end if
			   end if
		  else
		  !print*, 'Qin(n)<Qgrlast', Qin(n), Qgrlast
			   if(dQ>20*grad.or.dQgr1>20*kdQgr1.or.dQgr2>20*grad) then !фильтр чтобы пропускал косяки с резким выпаданием точки вниз
				   if(abs(Qin(n+1)-Qin(n-1))<abs(Qin(n+1)-Qin(n)))then
						cycle
				   end if
			   end if
			   if (n>nlast+20) then  !!почему 20????проверить на практике
					if((n-nmax)>prodspada) then  
						 if(dQ>grad.or.dQgr1>kdQgr1.or.dQgr2>grad.or.dQabs>gradabs.or.dQgr2abs>gradabs) then !фильтр для дней сильно после половодья
							cycle
						 end if
					else
						 if(dQ>grad1.or.dQgr2>grad1.or.dQgr1>kdQgr1.or.dQabs>gradabs.or.dQgr2abs>gradabs) then !фильтр для дней сразу же после половодья
							cycle
						 end if
					end if
			   end if

		  end if
	      	
	      	

		  Qgr(n)=Qin(n)
		  if (n>nmax) ngrpor=ngrpor+1 
		  if (ngrpor==1) polend(NF)=n ! определение даты конца половодья
!		  print*, n, Qgr(n), ngrpor
		  Qgrlast=Qin(n)
		  nlast=n

	 end if
 
 
 end do

 print*, 'polend(NF)', polend(NF), Mon(polend(NF)), Day(polend(NF))
 write(7,*) 'polend(NF)', polend(NF), Mon(polend(NF)), Day(polend(NF))
 if (polend(NF)>0) then
	allocate (Qinpol(polend(NF)))
 else
	print*, 'polend(NF)=', polend(NF);pause
	allocate (Qinpol(prodspada*4))
 end if
   print*, 'Qinpol allocated'
 do ii=1, polend(NF)

	Qinpol(ii)=Qin(ii)
 end do
 
 nmax=MAXLOC(Qinpol,1)
 Qmax=Qinpol(nmax) !ИЗ Макс расход за половодье
 
 write(7,*) 'max', nmax, Qmax

 do k=1,ny(NF) ! линейная интерполяция грунтовой составляющей в точки без значений

	  if(Qgr(k)<0) then
			do kk=k,ny(NF)
				 if(Qgr(kk)>=0) then
					  Qgr(k)=Qgr(k-1)+(Qgr(kk)-Qgr(k-1))/(kk-k+1+1) 
					  exit
				 end if
			end do
			dQ=abs(Qin(k-1)-Qin(k))/Qin(k-1)*100
		   
			if(Qgr(k)>Qin(k)) then
				 if(dQ>20*grad) then !фильтр чтобы пропускал косяки с резким выпаданием точки вниз
					  if(abs(Qin(k+1)-Qin(k-1))<abs(Qin(k+1)-Qin(k)))then
						goto 88
					  else
						Qgr(k)=Qin(k)
					  end if
				 end if
				 Qgr(k)=Qin(k)
			 end if
	  end if
	  88  Qygr(NF)=Qygr(NF)+Qgr(k)/ny(NF)  ! ИЗ среднегодовой расход грунтовых вод  
      
   
	  Qy(NF)=Qy(NF)+Qin(k)/ny(NF)   ! ИЗ среднегодовой расход воды  
 end do
 
 print*, 'Qgr interpolated'
 !print*, Qpb; pause

 ! проверка, были ли дождевые паводки и температурные оттепели по массиву осадков и температуры 
 HalfSt=(nPav-1)/2 !отрезок назад и вперед от данной точки (=пол периода за который анализируется интенсивность осадков минус один) 
 do m=(HalfSt+1),ny(NF)-HalfSt !проходится массив расходов за данный год от точки начала половодья + HalfSt до конца года - HalfSt
    Psumi=0
    Tsri=0
	do u=m-HalfSt,m+HalfSt
		Psumi=Psumi+Pin(u)
		Tsri=Tsri+Tin(u)/nPav
	end do
		Psums(m)=Psumi
	if (Psumi>=Pcr.and.Tsri>=Tcr1) then !прошли критической интенсивнсти осадки в дождевой фазе
		FactPcr(NF)=FactPcr(NF)+1
	    !PcrDates(NF,FactPcr(NF))=m
		FlagsPcr(m)=1
	end if
		Tsrs(m)=Tsri
	if (Tsri>=Tcr2) then ! nPav-дневки устойчивых температурных плюсов
		FactPlusTemp(NF)=FactPlusTemp(NF)+1
		FlagsPlusTemp(m)=1
		!PlusTempDates(NF,FactPlusTemp(NF))=m
	end if
 end do

print*, 'meteo checked1'

 ! проверка, были ли заморозки по массиву температуры 
 HalfStZ=(nZam-1)/2 !отрезок назад и вперед от данной точки (=пол периода за который анализируется температура воздуха минус один) 
 do m=(HalfStZ+1),ny(NF)-HalfStZ !проходится массив расходов за данный год от точки начала половодья + HalfSt до конца года - HalfSt
    Tsri=0
    do u=m-HalfStZ,m+HalfStZ
		Tsri=Tsri+Tin(u)/nZam
	end do
	if (Tsri<Tzam) then ! nPav-дневки устойчивых температурных минусов
		FactMinusTemp(NF)=FactMinusTemp(NF)+1
		FlagsMinusTemp(m)=1
		!MinusTempDates(NF,FactMinusTemp(NF))=m
	end if

 end do

print*, 'meteo checked2'

 if (.not.ModeMountain) then
	 ! нахождение волн паводков на подъеме половодья
	 
		startPol(NF)=1 !номер члена ряда - начала половодья начальный, до проверки на оттепели
		LocMax1=0 !обнуление номер члена ряда локального максимума паводка
		Flex1=0 !обнуление точки перегиба -/+
		Bend1=nmax  !обнуление точки начала паводка на подъеме
		p=nmax-2
	  do p=nmax-2,startPol(Nf),-1 !ракоход от максимума -2
	   FlexPrev=0 !обнуление точки перегиба -/+ предшествующего до найденного паводка
	   if (p<Bend1) then
		 if((deltaQ(p)<=-Qin(nmax)*SignDelta).or.((deltaQ(p)+deltaQ(p-1))<=-Qin(nmax)*SignDelta)) then !отрицательный перепад между текущим днем и следующим или суммарный за этот день+день до больше критической доли максимального расхода - считаем значимым спадом паводка
			do pp=p,nmax-2
				if (deltaQ(pp)>0) then
					Flex1=pp ! pp - точка перепада -/+
					exit
				end if
			end do
		 end if
		 if(Flex1>0) then !если мы нашли спад ракоходя
		 write(7,*)'Found peregib in', Flex1, Year(Flex1), Mon(Flex1), Day(Flex1)
		    !Проверяем, были ли еще перегибы -/+ - один ли пик у паводков или их было несколько
		    do u=Flex1,startPol(Nf),-1
				if((deltaQ(u)<=-Qin(nmax)*SignDelta*0.5).or.((deltaQ(u)+deltaQ(u-1))<=-Qin(nmax)*SignDelta*0.5)) then !отрицательный перепад между текущим днем и следующим или суммарный за этот день+день до больше критической доли максимального расхода - считаем значимым спадом паводка
					do pp=u,Flex1-1
						if (deltaQ(pp)>0) then
							FlexPrev=pp ! pp - точка перепада -/+, был еще паводок до - это важно для нахождения локального максимума
							exit
						end if
					end do
				end if		
			end do		
			!находим ближайший локальный максимум
			if (FlexPrev>0) then
				LocMax1=MAXLOC(Qin(FlexPrev:Flex1),1)+FlexPrev-1
			else
				LocMax1=MAXLOC(Qin(1:Flex1),1)
			end if
			!как до этого выглядео этот блок:
!			do pp=Flex1, startPol(NF), -1
!				if (deltaQ(pp)<-Qin(nmax)*SignDelta1.and.deltaQ(pp-1)>Qin(nmax)*SignDelta1) then 
!					LocMax1=pp !номер члена ряда локального максимума паводка
!		     	   exit
!				end if
!			end do
            write(7,*), 'FlexPrev',Year(FlexPrev), Mon(FlexPrev), Day(FlexPrev)
            write(7,*), 'LocMax1=', Mon(LocMax1), Day(LocMax1), Qin(LocMax1) 
            
			!проверяем на заморозки
			do pp=LocMax1-HalfStZ, Flex1
				if(FlagsMinusTemp(pp)==1)then !заморозок был по данным температур
					 write(7,*), 'podyem, zamorozok detected', Year(pp), Mon(pp), Day(pp), Tsrs(pp)
					!все, что было от начала в/х года до точки Flex1 - оттепель. 
					startPol(NF)=Flex1 ! настоящая дата начала половодья - двигаем
!					  do rr=Flex1-1,LocMax1, -1 
!						 if ((Qin(rr)-Qin(Flex1))>=Qin(nmax)*SignDelta) then !есть нормальный отрицательный перепад чтобы высчитать экспаненту
!							z=-log(Qin(Flex1)/Qin(rr))/(Flex1-rr) !вычисляем показатель экспаненты, по этой и предыдущей точке
!							Qo=Qin(rr)/exp(-z*(rr)) !вычисляем Qo, по этой и предыдущей точке
							z=-log(Qin(Flex1)/Qin(LocMax1))/(Flex1-LocMax1) !вычисляем показатель экспаненты, по этой и предыдущей точке
							Qo=Qin(LocMax1)/exp(-z*(LocMax1)) !вычисляем Qo, по этой и предыдущей точке
							do qq=1,Flex1 !все расходы, превышающие грунтовые считаются оттепельно-паводочными
								Qthaw(qq)=Qin(qq)-Qgr(qq)
							end do
							do qq=Flex1, nmax*2  !остальная чать оттепельного паводка моделируется по кривой спада
								if (Qo*exp(-z*qq)<=Qgr(qq)) exit
								Qthaw(qq)=Qo*exp(-z*qq)-Qgr(qq)
							end do
!							exit
!						  end if
!					  end do
					goto 667 !выходим из поиска паводков на подъеме, так как уже все отрезали от начала оттепельным паводком
				end if
			end do	
			!не было заморозков, это либо спад дождевого паводка, либо многопиковой половодье
		
			!ищем точку подъема графически
			do pp=LocMax1,2, -1
				if (Qin(pp)<Qin(Flex1).and.(deltaQ(pp-1)<=((Qin(Flex1)-Qin(pp))/(Flex1-pp))))then !точка начала паводка ниже и градиент перед ей меньше или равен перепаду между началом и концом паводка, тк на подъем половодья накладывается подъем паводка
					Bend1=pp
					exit
				end if
			end do
	 
			!проверяем на наличие дождевых осадков стокоформирующих чтобы подтвердить дождевой паводок
		   do pp=Bend1-2*HalfSt,LocMax1, 1	
				if(FlagsPcr(pp)==1)then !был дождевой паводок	
					 print*, 'podyem, pavodok detected'
					 !считаем линейную функцию, прохождящую через точки Bend1 и Flex1
					  afunc=(Qin(Flex1)-Qin(Bend1))/(Flex1-Bend1)
					  bfunc=Qin(Flex1)-afunc*Flex1	
					  do qq=Bend1, Flex1 !отрезаем по линейной функции дождевой паводок  
						  Qpav(qq)=Qin(qq)-(afunc*qq+bfunc)
					  end do
					  exit
				end if	
		   end do		
			
		 end if
	   end if

	  end do		

	667 print*, 'podyem done'; write(7,*), 'podyem done'

	 ! нахождение волн паводков на спуске
		Flex2=0 !обнуление точки перегиба -/+
		Bend2=0  !обнуление точки конца паводка на спуске 
	  do p=nmax, polend(NF)-1
	   if(p>Bend2) then
		if((deltaQ(p)>=Qin(nmax)*SignDelta).or.((deltaQ(p)+deltaQ(p+1))>=Qin(nmax)*SignDelta)) then !положительный перепад между текущим днем и следующим или суммарный за этот день+день до больше критической доли максимального расхода - считаем значимым спадом паводка
			do pp=p+1,nmax, -1
				if (deltaQ(pp)<0) then
					Flex2=pp+1 ! pp+1 - точка перепада
					write(7,*), 'Flex2', Year(Flex2), Mon(Flex2), Day(Flex2)
					exit
				end if
			end do
		!теперь ищем точку конца паводка
			do pp=Flex2+1, polend(NF)
				if ((Qin(pp)<Qin(Flex2).and.(min(deltaQ(pp),deltaQ(pp-1))>=(Qin(pp)-Qin(Flex2))/(pp-Flex2))).or.(pp==polend(NF))) then
					Bend2=pp !точка конца паводка
					write(7,*), 'Bend2', Year(Bend2), Mon(Bend2), Day(Bend2)
				   !проверяем на наличие дождевых осадков стокоформирующих чтобы подтвердить дождевой паводок
				   do ppp=Bend2-HalfSt,Flex2-2*HalfSt,-1	
				   if(FlagsPcr(ppp)==1)then !был дождевой паводок
						z=-log(Qin(Bend2)/Qin(Flex2))/(Bend2-Flex2) !вычисляем показатель экспаненты, по точкам начала и конца паводка
						Qo=Qin(Flex2)/exp(-z*(Flex2)) !вычисляем Qo
						do qq=Flex2, Bend2
							Qpav(qq)=Qin(qq)-Qo*exp(-z*qq)
						end do
				   end if
				   end do
				   exit
				end if
			end do
		 end if
	   end if
	  end do
 end if
 

 
 
 if (ModeMountain) then !распил горного половодья на основную волну и накладывающиеся паводки
	do n=1,polend(NF) 
	!print*, polend(NF), Day(n), Mon(n), Qpb(n); pause

	 if (n==1.or.n==polend(NF)) then
		  Qpb(n)=Qin(n)
		  Qpblast=Qin(n)
		  nlast=n
	 else

          dQabs=abs((Qin(n-1)-Qin(n))/Qin(n-1))!модуль разницы текущим и предыдущим днем [м3/с/день]
		  dQpb2=abs((Qpblast-Qin(n))/(n-nlast+1)) !модуль градиента между текущим днем и ближайшим предыдущим днем выделенным как половодно-базисный [м3/с/день]

		  if(dQabs<=pgrad.and.dQpb2<=pgrad) then !если изменеия в этот-предшествующий день и этот-последний базисный день идут с базовой интенсивностью					 						
   			!print*, '111'
   					Qpb(n)=Qin(n)
					Qpblast=Qin(n)
					nlast=n
   		  end if
   		  
   		  if (deltaQ(n)>0.and.deltaQ(n-1)<0.and.dQpb2<=pgrad) then !точка перегиба -/+ и изменения этот-последний базисный день идут с базовой интенсивностью
     		!print*, '-/+'
     				Qpb(n)=Qin(n)
					Qpblast=Qin(n)
					nlast=n
   		  end if 	
	  	  		
	 end if
		!print*, Qpb(n), Qin(n)
    end do
    
    
	  do k=1,ny(NF) ! линейная интерполяция половодной базисной составляющей в точки без значений
          
          if (k>polend(NF))then
			Qpb(k)=0
			cycle
          end if
          
		  if(Qpb(k)<0) then
				do kk=k,polend(NF)
					 if(Qpb(kk)>=0) then
						  Qpb(k)=Qpb(k-1)+(Qpb(kk)-Qpb(k-1))/(kk-k+1+1) 
						  exit
					 end if
				end do
			   
				if(Qpb(k)>Qin(k)) then
					 Qpb(k)=Qin(k)
				 end if
		  end if

		!print*, Day(k), Mon(k), Qpb(k);pause
	  end do
		
   ! вычитание из половодной составляющей грунтовой
   
   do k=1,polend(NF)
	Qpb(k)=Qpb(k)-Qgr(k)
   end do
     
  end if
 
 if (ModeMountain) then !застраховываемся от возможных косяков в следующем блоке
	SummerEnd(NF)=polend(NF)+31
 end if
 
 if (ModeMountain) then
		!в горном варианте летне-осенняя переходит на зимнюю всегда в "mome" месяце в конце паводка
		
		do pp=polend(NF)+1, ny(NF)
			

			if (Mon(pp)==mome) then
				if (Qin(pp)==Qgr(pp)) then
					SummerEnd(NF)=pp
				else
					ppp=pp
					do while (Qin(ppp)>Qgr(ppp))

						ppp=ppp+1
					end do
					SummerEnd(NF)=ppp				
				end if
				exit
			end if
		end do
 
 else
 
	!нахождение даты перехода с летней на зимнюю межень по критическим температурам
	 
	 HalfStW=(nWin-1)/2 !отрезок назад и вперед от данной точки (=пол периода за который анализируется температура воздуха минус один) 

	 do pp=polend(NF)+1, ny(NF)
		MarkWarm=0
		do u=pp,pp+nWin
			if (Tin(u)>Twin) MarkWarm=1
		end do    
		if(MarkWarm==0)then !заморозок был
			print*, 'zamorozok', Year(1), pp, Day(pp), Mon(pp), Year(pp), Tin(pp), Tin(pp+1)
			if (Qin(pp+HalfStW)==Qgr(pp+HalfStW)) then
				SummerEnd(NF)=pp+HalfStW
			else
				ppp=pp
				do while (Qin(ppp)>Qgr(ppp))
					ppp=ppp+1
					!print*, ppp, Qin(ppp), Qgr(ppp)
				end do
				SummerEnd(NF)=ppp				
			end if

			goto 512
		end if
		
	 end do
 end if   
 
!летне-осенние паводки с разделением на дождевые и талые
 
512 if (ModeMountain) then
        k=1  !в горном режиме паводки за летний период включают павдки во время половодья
    else
        k=polend(NF)
    end if

!формирование массива Qpav, Qthaw, подсчет числа дней с паводками
 do while (k<ny(NF)) 
	 if(Qin(k)>Qgr(k)) then ! идет паводок
 		if(k<=SummerEnd(NF)) then                    !лето
			if (k>polend(NF)) then
				Qpav(k)=Qin(k)-Qgr(k)
			else
				Qpav(k)=Qin(k)-Qgr(k)-Qpb(k)
			end if
			DaysPavsSum(NF)=DaysPavsSum(NF)+1			
		end if
		if(k>SummerEnd(NF)) then                     !зима
		   Qthaw(k)=Qin(k)-Qgr(k)  
		   DaysThawWin(NF)=DaysThawWin(NF)+1
		end if
	 end if
     k=k+1
 end do

 
 
  ! расчет Cv межени
 
 SumProd(NF)=SummerEnd(NF)-1-polend(NF)+1
 WinProd(NF)=ny(NF)-SummerEnd(NF)+1
 
 print*, Year(1),'SumProd=',SumProd(NF), SummerEnd(NF)
 
 if (SumProd(NF)<=0) then
	print*, Year(1),'SumProd=',SumProd(NF);pause
 end if
 
 if (WinProd(NF)<=0) then
	print*, Year(1),'WinProd=',WinProd(NF);pause
 end if
 
 allocate (Awin(WinProd(NF)))
 allocate (Asum(SumProd(NF)))
 
 Awin(1:WinProd(NF))=Qin(SummerEnd(NF):ny(NF))
 Asum(1:SumProd(NF))=Qin(polend(NF):SummerEnd(NF)-1)

 CvWin(NF)=Cv(Awin,WinProd(NF))
 CvSum(NF)=Cv(Asum,SumProd(NF))

 deallocate (Awin)
 deallocate (Asum)
 

 !Определение объема половодья и запись всего в файл, расчет всех объемных
 do p=1,ny(NF)
	Qpol(p)=Qin(p)-Qgr(p)-Qthaw(p)-Qpav(p)
 	
 	call Datechar (Day(p), Mon(p), Year(p), dchar)
 	
 	write(3,'(1x, a15, 8f10.2)') dchar, Qin(p), Qgr(p), Qpol(p), Qpav(p), Qthaw(p), Qpb(p), Tin(p), Pin(p) 
    
    Wy(NF)=Wy(NF)+Qin(p)*86400/1000000000 
    Wgr(NF)=Wgr(NF)+Qgr(p)*86400/1000000000 
    
    if (p>polend(NF)) then
		if (p<=SummerEnd(NF).and.p>polend(NF)) then !летняя межень
			WgrS(NF)=WgrS(NF)+Qgr(p)*86400/1000000000
		else !зимняя межень
			WgrW(NF)=WgrW(NF)+Qgr(p)*86400/1000000000
			WW(NF)=WW(NF)+Qin(p)*86400/1000000000 !зимняя межень с паводками целиком 
		end if
	end if
		
    
    if (Qpol(p)>0) then
		Wpol(NF,1)=Wpol(NF,1)+(Qpol(p)+Qgr(p))*86400/1000000000                 ! расчет объёма половодья вместе с грунтом
		Wpol(NF,2)=Wpol(NF,2)+Qpol(p)*86400/1000000000        ! расчет объема половодья за вычетом грунта
		Wpol(NF,3)=Wpol(NF,3)+(Qpol(p)+Qpav(p))*86400/1000000000   !расчет оъема половодья в сумме с дождевыми паводками
	end if
    !дождевые паводки
    if(ModeMountain) then
		if (Qpav(p)>0) then
			Wpavs(NF,1)=Wpavs(NF,1)+(Qpav(p)+Qgr(p))*86400/1000000000             ! расчет объёма дождевых паводков вместе с грунтом
			Wpavs(NF,2)=Wpavs(NF,2)+Qpav(p)*86400/1000000000    ! расчет объёма дождевых  паводков за вычетом грунта
		end if
	else
		if (Qpav(p)>0.and.p>polend(NF)) then
			Wpavs(NF,1)=Wpavs(NF,1)+(Qpav(p)+Qgr(p))*86400/1000000000             ! расчет объёма дождевых паводков вместе с грунтом
			Wpavs(NF,2)=Wpavs(NF,2)+Qpav(p)*86400/1000000000    ! расчет объёма дождевых  паводков за вычетом грунта
		end if
	end if
	
	 if(Qin(p)>Qmaxpavs(NF).and.Qpav(p)>0) then                                ! определение максимального расхода воды дождевых паводков
	   Qmaxpavs(NF)=Qin(p)
	   nmaxpavs=p
	 end if
     !оттепельные паводки
    if (Qthaw(p)>0) then 
		Wpavw(NF,1)=Wpavw(NF,1)+(Qthaw(p)+Qgr(p))*86400/1000000000             ! расчет объёма оттепельных паводки паводков вместе с грунтом
		Wpavw(NF,2)=Wpavw(NF,2)+Qthaw(p)*86400/1000000000    ! расчет объёма оттепельных паводков за вычетом грунта 
	end if
	 if(Qin(p)>Qmaxpavw(NF).and.Qthaw(p)>0) then                ! определение максимального расхода воды оттепельных паводков
	   Qmaxpavw(NF)=Qin(p)
	   nmaxpavw=p
	 end if
	
 
 end do
 

 
 !Расчет количества паводков

 MarkUpPavs=0
 VolPav=0
 VolPav1=0
 kstart=0
 
 if (ModeMountain) then
        kfirst=1  !в горном режиме паводки за летний период включают павдки во время половодья
  else
        kfirst=polend(NF)
  end if
  
  do k=kfirst, SummerEnd(NF)  !лето
  !print*, 'k', k
	 if(Qpav(k)>0) then ! идет паводок     
	   !print*, 'Qpav(k)>0'           
			if (MarkUpPavs==1) then
				VolPav=VolPav+Qpav(k)*86400/1000000000
				VolPav1=VolPav1+(Qpav(k)+Qgr(k)+Qpb(k))*86400/1000000000
			else
				MarkUpPavs=1
    			VolPav=VolPav+Qpav(k)*86400/1000000000
				VolPav1=VolPav1+(Qpav(k)+Qgr(k)+Qpb(k))*86400/1000000000
				kstart=k
			end if
	 else
			!print*, 'Qpav(k)<=0'
			MarkUpPavs=0
			if (VolPav>=PavRate*Wpol(NF,2)) then
				CountPavs(NF)=CountPavs(NF)+1
				SingleWpav(NF,CountPavs(NF))=VolPav1
				Qport=0
				do yy=kstart-1, k
					Qport(yy)=Qin(yy)
				end do
				kmax=MAXLOC(Qport,1)
				call Datechar (Day(kstart-1),Mon(kstart-1),Year(kstart-1),dchar)
				call Datechar (Day(k),Mon(k),Year(k),dchar1)
				call Datechar (Day(kmax),Mon(kmax),Year(kmax),dchar2)
				call Datechar (Day(kmax), Mon(kmax), 2000, dchar3)
				
				write(6,'(1x, a1, i5, i6, 3a15, f10.2, 2f10.3, 2i3, 5f10.1, 2i3, 3f10.1, 2i3, 5f10.1, 2i4, f10.1, a15)') &
				'1', CountPavs(NF), Year(k), dchar, dchar1, dchar2, Qin(kmax), VolPav, VolPav1, &
				FlagsPcr(kstart-1), FlagsPcr(kstart-HalfSt-1), Psums(kstart-1), Psums(kstart-1-HalfSt),&
				Pin(kstart-1), Pin(kstart-2), Pin(kmax), FlagsPlusTemp(kstart-1), FlagsPlusTemp(kstart-1-HalfSt),&
				Tsrs(kstart-1), Tsrs(kstart-1-HalfSt), Tin(kmax), FlagsMinusTemp(k), FlagsMinusTemp(k-HalfStZ), &
				Tsrs(k), Tsrs(k-HalfSt),Tin(k), Tin(kmax+1),&
				Qin(kstart-1), kmax-(kstart-1), k-(kstart-1), Qin(kmax)-Qin(kstart-1), dchar3
					
			end if
			VolPav=0
			VolPav1=0
	 end if
	end do		
	
 MarkUpPavs=0
 VolPav=0
 VolPav1=0	
 
  do k=SummerEnd(NF)+1, ny(NF)  !зима
	 if(Qthaw(k)>0) then ! идет паводок                
			if (MarkUpPavs==1) then
				VolPav=VolPav+Qthaw(k)*86400/1000000000
				VolPav1=VolPav1+(Qthaw(k)+Qgr(k))*86400/1000000000
			else
				MarkUpPavs=1
				VolPav=VolPav+Qthaw(k)*86400/1000000000
				VolPav1=VolPav1+(Qthaw(k)+Qgr(k))*86400/1000000000
				kstart=k
			end if
	 else
			MarkUpPavs=0

			if (VolPav>=PavRate*Wpol(NF,2)) then
				CountThaws(NF)=CountThaws(NF)+1
				SingleWthaw(NF,CountThaws(NF))=VolPav1
				Qport=0
				do yy=kstart-1, k
					Qport(yy)=Qin(yy)
				end do
				kmax=MAXLOC(Qport,1)
				call Datechar (Day(kstart-1),Mon(kstart-1),Year(kstart-1),dchar)
				call Datechar (Day(k),Mon(k),Year(k),dchar1)
				call Datechar (Day(kmax),Mon(kmax),Year(kmax),dchar2)
				call Datechar (Day(kmax), Mon(kmax), 2000, dchar3)
				
				write(6,'(1x, a1, i5, i6, 3a15, f10.2, 2f10.3, 2i3, 5f10.1, 2i3, 3f10.1, 2i3, 5f10.1, 2i4, f10.1, a15)') &
				'2', CountThaws(NF), Year(k), dchar, dchar1, dchar2, Qin(kmax), VolPav, VolPav1, &
				FlagsPcr(kstart-1), FlagsPcr(kstart-HalfSt-1), Psums(kstart-1), Psums(kstart-1-HalfSt),&
				Pin(kstart-1), Pin(kstart-2), Pin(kmax), FlagsPlusTemp(kstart-1), FlagsPlusTemp(kstart-1-HalfSt),&
				Tsrs(kstart-1), Tsrs(kstart-1-HalfSt), Tin(kmax), FlagsMinusTemp(k), FlagsMinusTemp(k-HalfStZ), &
				Tsrs(k), Tsrs(k-HalfSt),Tin(k), Tin(kmax+1),&
				Qin(kstart-1), kmax-kstart-1, k-kstart-1, Qin(kmax)-Qin(kstart-1), dchar3
			end if
			VolPav=0
			VolPav1=0
	 end if
	end do	


 !Расчет минимального месячного расхода воды л и з

  if (ModeMountain) then
	do yy=1,ny(NF)
		if(Mon(yy)==10.and.Day(yy)==30) then
			SummerEnd(NF)=yy
		end if	
	end do
	call Mmes (Qin, 1000, Day, 1000, Mon, 1000, MM,4,ny(NF),Mon(SummerEnd(NF)), Mon(polend(NF)),Mon(ny(NF)), Mon(1), Day(1), Day(ny(NF)), SummerEnd(NF), Year(ny(NF)))
  else
  	call Mmes (Qin, 1000, Day, 1000, Mon, 1000, MM,4,ny(NF),Mon(SummerEnd(NF)), Mon(polend(NF)),Mon(ny(NF)), Mon(1), Day(1), Day(ny(NF)), SummerEnd(NF), Year(ny(NF)))
  end if 

  !Расчет минимального 30-суточного расхода воды л и з
  krat=30

  call Mkrat (Qin,1000, Day, 1000, Year, 1000, Mon, 1000,ny(NF), krat, SummerEnd(NF), polend(NF), Res, 4)

  Q30s(NF)=Res(1)
  i30sn(NF)=Res(2)
  Q30w(NF)=Res(3)
  i30wn(NF)=Res(4)
  
  
  call Datechar (Day(i30sn(NF)),Mon(i30sn(NF)),2000, dchar)
  date30s(1)=dchar
  call Datechar (Day(i30sn(NF)+29),Mon(i30sn(NF)+29),2000,dchar)
  date30s(2)=dchar
  call Datechar (Day(i30wn(NF)),Mon(i30wn(NF)),2000,dchar)
  date30w(1)=dchar
  call Datechar (Day(i30wn(NF)+29),Mon(i30wn(NF)+29),2000,dchar)
  date30w(2)=dchar



  krat=10

  call Mkrat (Qin,1000, Day, 1000, Year, 1000, Mon, 1000,ny(NF), krat, SummerEnd(NF), polend(NF), Res, 4)


  Q10s(NF)=Res(1)
  i10sn(NF)=Res(2)
  Q10w(NF)=Res(3)
  i10wn(NF)=Res(4)

  call Datechar (Day(i10sn(NF)),Mon(i10sn(NF)),2000,dchar)
  date10s(1)=dchar
  call Datechar (Day(i30sn(NF)+9),Mon(i30sn(NF)+9),2000,dchar)
  date10s(2)=dchar
  call Datechar (Day(i10wn(NF)),Mon(i10wn(NF)),2000,dchar)
  date10w(1)=dchar
  call Datechar (Day(i10wn(NF)+9),Mon(i10wn(NF)+9),2000,dchar)
  date10w(2)=dchar


  krat=5

  call Mkrat (Qin,1000, Day, 1000, Year, 1000, Mon, 1000,ny(NF), krat, SummerEnd(NF), polend(NF), Res, 4)


  Q5s(NF)=Res(1)
  i5sn(NF)=Res(2)
  Q5w(NF)=Res(3)
  i5wn(NF)=Res(4)

  call Datechar (Day(i5sn(NF)),Mon(i5sn(NF)),2000,dchar)
  date5s(1)=dchar
  call Datechar (Day(i5sn(NF)+4),Mon(i5sn(NF)+4),2000,dchar)
  date5s(2)=dchar
  call Datechar (Day(i5wn(NF)),Mon(i5wn(NF)),2000,dchar)
  date5w(1)=dchar
  call Datechar (Day(i5wn(NF)+4),Mon(i5wn(NF)+4),2000,dchar)
  date5w(2)=dchar

! вывод в текстовом виде хараткерных дат
  if(ModeMountain) then
	  call Datechar (Day(1), Mon(1),2000, dchar)
	  datestart=dchar
  else
	  call Datechar (Day(startPol(NF)), Mon(startPol(NF)),2000, dchar)
	  datestart=dchar  
  end if 

  call Datechar (Day(nmax), Mon(nmax),2000, dchar)
  datemax=dchar

  
  call Datechar (Day(polend(NF)), Mon(polend(NF)),2000,dchar)
  datepolend=dchar

  call Datechar (Day(nmaxpavs), Mon(nmaxpavs),2000,dchar)
  datemaxpavs=dchar

  call Datechar (Day(nmaxpavw), Mon(nmaxpavw),2000,dchar)
  datemaxpavw=dchar


print*, 'done'

 write(4,'(1x, i6, 2i10, 2a15, 2f10.2, a15, 6f10.2, 2a15, f10.2, 2a15, f10.2, 2a15, f10.2, 2a15, f10.2, 2a15, f10.2, 2a15, &
             13f13.5, f10.2, a15, f10.2, a15, 4i10, 2f7.3, 2i10)') &
             NF, Year(1), Year(ny(NF)), datestart, datepolend, Qy(NF), Qmax, datemax, Qygr(NF), (MM(j), j=1,4),&
             Q30s(NF), date30s(1), date30s(2),&
			 Q30w(NF), date30w(1), date30w(2),&
             Q10s(NF), date10s(1), date10s(2), &
			 Q10w(NF),  date10w(1), date10w(2),&
			 Q5s(NF), date5s(1), date5s(2), &
			 Q5w(NF),  date5w(1), date5w(2), &
             Wy(NF), Wgr(NF), Wpol(NF,1),Wpol(NF,2), Wpol(NF,3), Wpavs(NF,1), Wpavs(NF,2), Wpavw(NF,1), Wpavw(NF,2), &
             WgrS(NF), WgrS(NF)+Wpavs(NF,2), WgrW(NF), WW(NF),& 
			 Qmaxpavs(NF), datemaxpavs, Qmaxpavw(NF), datemaxpavw, SumProd(NF), DaysPavsSum(NF), WinProd(NF), &
			 DaysThawWin(NF), CvWin(NF), CvSum(NF), CountPavs(NF), CountThaws(NF)

print*, '____'

 deallocate (Qinpol)

end do

end subroutine mainprog


subroutine Mmes (qin,nm,day,nnm, mon, nnmm, MM,ll,Tot,mome, monpolend, mok, mon1, day1, dayEnd, SummerEnd, Year) !мин месяц
! call Mmes (Qin, 1000, Day, 1000, Mon, 1000, MM,4,ny(NF),Mon(SummerEnd(NF)), Mon(polend(NF)),Mon(ny(NF)), Mon(1), Day(1), Day(ny(NF)), SummerEnd(NF), Year(ny(NF)))

integer nm, ll, Tot, mome, monpolend, mok, mon1, day1, dayEnd, SummerEnd, Year, nnm, nnmm, day(nnm), mon(nnmm)
real qin(nm), MM(ll)
real Qsrm(12), Qsumm(12), SRavn
integer CH(12), m, t, i, h, nomer, s, v1, v2
data CH/31,28,31,30,31,30,31,31,30,31,30,31/

SRavn=10000
v1=Year
v2=Year/4
v2=v2*4
s=0
if(v1==v2) then
print*, 'visokos'

 CH(2)=29
 else
 CH(2)=28
end if

do j=1,12
 Qsrm(j)=-999
 Qsumm(j)=0
end do

if (day(SummerEnd)<=(CH(mome)/2)) then !если к концу летней межени прошло меньше половины месяца - он веь относится к зимней межени
	mome=mome-1!последний месяц летней межени
end if

do i=mon1, monpolend-1
	s=s+CH(i) 
end do

s=s-(day1-1)+1  !стартовый номер дня - номер первого дня поседнего месяца половодья

m=s-1

!Летне-осенняя межень
do t=monpolend,mome !monpolend - месяц когда кончилось половодье, mome - последний месяц летней межени
 Do i=1,CH(t) !суммируем по месяцам расходы воды от месяца конца половодья до последнего месяца летней межени
  m=m+1
  Qsumm(t)=Qsumm(t)+qin(m)
 end do
 Qsrm(t)=Qsumm(t)/CH(t)
end do

do h=monpolend,mome  !перебираем - сравниваем чтобы найти самый маленький
 if (Qsrm(h)>-99.and.Qsrm(h)<SRavn) then
 SRavn=Qsrm(h)
 MM(1)=Qsrm(h)
 MM(2)=h
end if
end do

!Зимняя межень

SRavn=10000


if (mome<12) then
	do t=mome+1,12  !суммируем по месяцам расходы воды от начала зимней межени до конца календарного года
	 Do i=1,CH(t)
	  m=m+1  ! в начальном значении m для этого цикла сохранен последний номер дня летней межени
	  Qsumm(t)=Qsumm(t)+qin(m)
	 end do
	 Qsrm(t)=Qsumm(t)/CH(t)
	end do
end if 



do t=1,mok-1 !суммируем по месяцам расходы воды от янаря до месяца перед половодьем
 Do i=1,CH(t)
  m=m+1
  Qsumm(t)=Qsumm(t)+qin(m)
 end do
 s=m
 Qsrm(t)=Qsumm(t)/CH(t)
end do

if (dayEnd>28) then ! если половодье началось позже 28 числа
 
 Do i=1,dayEnd
  m=m+1
  Qsumm(mok)=Qsumm(mok)+qin(m)
 end do
 s=m
 Qsrm(mok)=Qsumm(mok)/dayEnd

end if

if (mome<12) then !перебираем - сравниваем чтобы найти самый маленький сначала до нового года
	do h=mome,12
	 if (Qsrm(h)>-99.and.Qsrm(h)<SRavn) then
	  SRavn=Qsrm(h)
	  MM(3)=Qsrm(h)
	  MM(4)=h
	 end if
	end do
end if

do h=1,mok !перебираем - сравниваем чтобы найти самый маленький с нового года до месяца начала половодья
 if (Qsrm(h)>-99.and.Qsrm(h)<SRavn) then
  SRavn=Qsrm(h)
  MM(3)=Qsrm(h)
  MM(4)=h
 end if
end do


end subroutine Mmes


! минимальный n-суточный по усовершенствованной методе



subroutine Mkrat (Qin,qq, Day, qd, Year, qy, Mon, qm,lastday, krat, SummerEnd, polend, Res, qr)
!  call Mkrat (Qin,1000, Day, 1000, Year, 1000, Mon, 1000,ny(NF), krat, SummerEnd(NF), Res, 4)
!  call Mkrat (Qin,1000, Day, 1000, Year, 1000, Mon, 1000,ny(NF), krat, SummerEnd(NF), polend(NF), Res, 4)

integer qq, qd, qy, qm, lastday, krat, SummerEnd, qr, polend
integer Day(qd), Year(qy), Mon(qm), s(qq), w(qq)
real Qin(qq), Res(qr), Sumw(qq), Sums(qq), srs, srw

srs=999999
srw=999999

 do nn=1,qq
  Sumw(nn)=0
  Sums(nn)=0
  w(nn)=-999
  s(nn)=-999
 end do 

 Do ii=polend,lastday-(krat+1)
 
  if (Year(ii)==Year(1).and.Mon(ii)<Mon(SummerEnd).or.Mon(ii)==Mon(SummerEnd).and.Day(ii)<=(Day(SummerEnd)-krat)) then !летняя межень
   s(ii)=1 !числ посчитанных n-дневок
   do j=1,krat
    Sums(ii)=Sums(ii)+Qin(ii+j)/krat
   end do 
  
  else
   w(ii)=1
   do j=1,krat
    Sumw(ii)=Sumw(ii)+Qin(ii+j)/krat
   end do 
  end if
 end do


 Do uu=1,lastday-(krat+1)
  if(Sums(uu)<srs.and.s(uu)>0) then
   srs=Sums(uu)
   Res(1)=Sums(uu)
   Res(2)=uu
  end if
  if(Sumw(uu)<srw.and.w(uu)>0) then
   srw=Sumw(uu)
   Res(3)=Sumw(uu)
   Res(4)=uu
  end if

 end do


end subroutine Mkrat

subroutine Datechar (Day,Mon,Year, dcahr)
  
integer Day, Mon, Year
character dcahr*10, r*1, xr*2, xm*2, m*1, yearfikt*5, yearfakt*4

!yearfikt='.2000'
  
if (Day<10) then
 dcahr(1:1)='0'
 write(r,'(i1)') Day
 dcahr(2:2)=r(1:1)
 dcahr(3:3)='.'
else

 write(xr,'(i2)') Day
 dcahr(1:2)=xr(1:2)
 dcahr(3:3)='.'
end if

if (Mon<10) then
 dcahr(4:4)='0'
 write(m,'(i1)') Mon
 dcahr(5:5)=m(1:1)
else
 write(xm,'(i2)') Mon
 dcahr(4:5)=xm(1:2)
end if

write (yearfakt,'(i4)') Year
dcahr(6:6)='.'
dcahr(7:10)=yearfakt(1:4)

end subroutine Datechar



!расчет Cv

function Cv(a,q)

integer q, i
real a(q), Valu, Sr, SumSqDev


Sr=0
SumSqDev=0

do i=1, q
	Sr=Sr+a(i)/q
end do

do i=1,q
	SumSqDev=SumSqDev+(a(i)-Sr)**2
end do

Cv=((SumSqDev**0.5)/q)/Sr

end function Cv

subroutine Polfinder (Qin1, p, Mon1, p1, Year1, p2, Day1, p3, polmon, z, FlagGaps, polkol, y, polgrad, z1, donep, x, iyY, ng, lstart, polkolMount, p4, polgradMount, ModeMountain)

integer p, l, p1, p2, p3, z, z1, y, x, Mon1(p1), Year1(p2), Day1(p3), FlagGaps, polmon(z), polkol(y), donep(x), iyY, inipolkol(3), ng, p4, polkolMount(p4), inipolkolMount(2)
real Qin1(p), polgrad(z1), inipolgrad(2) , inipolgradMount, polgradMount
integer tt, mm, Sumdonep(4), seed/80000/, vsebylo, Smallestdonep, lstart
real polQsum, choose
logical ModeMountain

!print*, 'startPolfinder', Year1(1), Qin1(1);pause

inipolgrad(1)=polgrad(1)
inipolgrad(2)=polgrad(2)
inipolkol(1)=polkol(1)
inipolkol(2)=polkol(2)
inipolkol(3)=polkol(3)
inipolkolMount(1)=inipolkolMount(1)
inipolkolMount(12)=inipolkolMount(2)
inipolgradMount=polgradMount


do mm=1,100

    print*, mm
    
    do iii=1,4
		Sumdonep(iii)=0
	end do
    vsebylo=0
    
	do l=1,p

	 polQsum=0

	 do tt=1,3
	  donep(tt)=-1
	 end do

	if(Year1(l)==donep(4)) then 
	  
	  if(Mon1(l)>=polmon(1).and.Mon1(l)<=polmon(2).and.Qin1(l).ne.FlagGaps) then
	  
	  dQ=0
	  do ff=1,polkol(1) !проверка критерия начала половодья №1 - устойчивое увеличение со средним градиентом больше или равном заданному (polgrad(1))
	   dQ=dQ+(Qin1(l+ff-1)-Qin1(l+ff))/Qin1(l+ff-1)*100/polkol(1)
	  end do
	   if (dQ>-polgrad(1)) then
		  donep(1)=-1
	   else 
		donep(1)=1 ! критерий 1 удовлетворен
		Sumdonep(1)=Sumdonep(1)+1
		print*, Year1(l), Mon1(l), Day1(l), 'd1'
	   end if

	  dQ=0
	  do ff=1,polkol(2)  !проверка критерия начала половодья №2 - устойчивое увеличение не заканчивается polkol(2) дней
	   dQ=dQ+(Qin1(l+ff-1)-Qin1(l+ff))/Qin1(l+ff-1)*100/polkol(2)
	  end do
	   if (dQ>0) then
		 donep(2)=-1
	   else
		 donep(2)=1  ! критерий 2 удовлетворен
		 Sumdonep(2)=Sumdonep(2)+1
		 print*, Year1(l), Mon1(l), Day1(l), 'd2'
	   end if



	if (ModeMountain) then

		  donep(3)=1 
		  do ff=1, polkolMount(1) !критерий 3 для гор превышение в течении polkolMount(1) каждые polkolMount(2) дней расхода воды в polgradMount раз
			polQsum=0
			do fff=ff,ff+polkolMount(2)
				polQsum=polQsum+Qin1(l+fff)
			end do
			if (polQsum/(Qin1(l)*polkolMount(2))<polgradMount) then
				donep(3)=-1
			end if
		  end do
		  if (donep(3)==1) then
		   	  Sumdonep(3)=Sumdonep(3)+1
		  end if
		  

	else

		  do ff=1,polkol(3) !критерий 3 - превышение среднего расхода воды за polkol(3) дней расхода дня начала половодья (Qin(l)) в  polgrad(2) раз
		   polQsum=polQsum+Qin1(l+ff-1)
		  end do
		  if (polQsum/(Qin1(l)*polkol(3))<polgrad(2)) then
		   donep(3)=-1
		  else
		   donep(3)=1 ! критерий 3 удовлетворен
		   Sumdonep(3)=Sumdonep(3)+1
		   print*, Year1(l),Mon1(l), Day1(l), 'd3'
		  end if
	end if



		if (donep(1)==1.and.donep(2)==1.and.donep(3)==1) then !все 3 критерия удовлетворены

			iyY=lstart+l-1 ! номер последнего члена ряда предыдущего года в общем массиве
			donep(4)=Year1(l)+1  !следующее начало половодья уже искать в следующем году
            print*, 'polgrad(1)=', polgrad(1), 'polgrad(2)=', polgrad(2) 
            print*, 'polkol(1)=', polkol(1)
            print*, 'polkol(2)=',polkol(2)
            print*, 'polkol(3)=', polkol(3)
            write(5,'(i8, 2f4.1, 5i6, f4.1)') Year1(l), polgrad(1), polgrad(2), polkol(1), polkol(2), polkol(3), polkolMount(1), polkolMount(2), polgradMount
			ng=ng+1 ! число лет
			polgrad(1)=inipolgrad(1)
			polgrad(2)=inipolgrad(2)
			polkol(1)=inipolkol(1)
			polkol(2)=inipolkol(2)
			polkol(3)=inipolkol(3)
		end if
		
	  end if
	  if (donep(1)==1.and.donep(2)==1.and.donep(3)==1.and.donep(4)==Year1(p2)) then 
		 exit
	  end if
	 end if

	end do ! l - цикл

	if (donep(1)==1.and.donep(2)==1.and.donep(3)==1.and.donep(4)==Year1(p2)) then 
	 exit
	end if
	
	! уменьшение параметров в зависимости от того, какой donep не срабатывает
	print*, Sumdonep(1), Sumdonep(2), Sumdonep(3)
	if (Sumdonep(1)>0.and.Sumdonep(2)>0.and.Sumdonep(3)>0) then
	    vsebylo=1
	else
		vsebylo=0
	end if
	print*, vsebylo
	Smallestdonep=min(Sumdonep(1),Sumdonep(2),Sumdonep(3))
	print*, Smallestdonep
	
	if (Sumdonep(1)==0.or.vsebylo==1.and.Sumdonep(1)==Smallestdonep) then
		print*, 'no d1'
		choose=ran(seed) 
		print*, 'choose', choose
		if (choose<=0.5) then
			polgrad(1)=polgrad(1)-0.1*polgrad(1)
		else
			if (polkol(1)>2) then
				polkol(1)=polkol(1)-1
			else 
				polgrad(1)=polgrad(1)-0.1*polgrad(1)
			end if
		end if
	end if
	if (Sumdonep(2)==0.or.vsebylo==1.and.Sumdonep(2)==Smallestdonep) then
		print*, 'no d2'	
		polkol(2)=polkol(2)-1
	end if
	if (Sumdonep(3)==0.or.vsebylo==1.and.Sumdonep(3)==Smallestdonep) then
	    print*, 'no d3'	
	    if (ModeMountain) then
			choose=ran(seed)
			if (choose<=0.2) then
				 polkolMount(1)=polkolMount(1)-5
			end if
			if (choose>0.2.and.choose<=0.6) then
			    if (polkolMount(2)>1) then
					polkolMount(1)=polkolMount(1)-1
				else
					polgradMount=polgradMount-0.1*polgradMount
				end if
			end if
			if (choose>0.6)then
				polgradMount=polgradMount-0.1*polgradMount
			end if
			
			
			
	    else
			choose=ran(seed) 
			if (choose<=0.5) then
				polgrad(2)=polgrad(2)-0.1*polgrad(2)
			else
				if (polkol(3)>inipolkol(3)/2) then
					polkol(3)=polkol(3)-5
				else 
					polgrad(2)=polgrad(2)-0.1*polgrad(2)
				end if
			end if
		end if
	end if
	

	if (mm==100) then
		print*, 'Polovodye was not found', Year1(1);pause
	end if
end do ! mm - цикл
 






end subroutine Polfinder

end


