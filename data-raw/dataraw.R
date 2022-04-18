# DATA PREPARATION

params_out = readxl::read_excel('data-raw/params_out.xlsx')
params_in_desc = readxl::read_excel('data-raw/params_in.xlsx', 1)
params_in = readxl::read_excel('data-raw/params_in.xlsx', 2)

spas = readr::read_delim(
  'http://carto.geogr.msu.ru/grwat/spas-zagorye-meteo.txt', 
  col_names = c('Date', 'Q', 'Temp', 'Prec'),
  col_types = 'Dddd', delim = ' '
)

grlabs = list(
  'EN' = list(
    subtitle = 'Water-resources year',
    bartitle.win = "Month of a minimum monthly runoff during winter",
    bartitle.sum = "Month of a minimum monthly runoff during summer",
    monthtitle = "Month",
    periodtitle = "Period",
    beforetitle = "before ",
    aftertitle = "after ",
    label.p = 'p',
    student.t = 'Student: t',
    fisher.f = 'Fisher: F',
    pettitt.u = 'Pettitt: U*',
    kendall.z = 'Mann-Kendall: z',
    theil.i = 'Theil-Sen: i',
    student.df = 'df',
    clipped.remark = "(clipped by the end of the year)",
    discharge = "Runoff",
    year = "Year",
    date = "Date",
    day = "days",
    m3s = bquote(m^3/s),
    pheader = "p-values of statistical criteria",
    rain = 'Rain',
    seasonal =  'Seasonal',
    season = 'Season of runoff',
    components = 'Component of runoff',
    thaw = 'Thaw',
    ground = 'Ground',
    discharge.type = 'Flow',
    discharge.value = 'Runoff',
    year.density = 'Change year distribution density',
    temp = 'Temperature, \u00b0C',
    prec = 'Precipitation, mm',
    preccum = 'Cum. precipitation, mm',
    wraplength = 60 # row wrap length for long titles
  ),
  'RU' = list(
    subtitle = 'Начало водохозяйственного года',
    bartitle.win = "Месяц минимального месячного расхода за зиму",
    bartitle.sum = "Месяц минимального месячного расхода за лето",
    monthtitle = "Месяц",
    periodtitle = "Период",
    beforetitle = "до ",
    aftertitle = "с ",
    label.p = 'p',
    student.t = 'Стьюдент: t',
    fisher.f = 'Фишер: F',
    pettitt.u = 'Петтитт: U*',
    kendall.z = 'Манн-Кендалл: z',
    theil.i = 'Тейл-Сен: i',
    student.df = 'df',
    clipped.remark = "(обрезано по концу календарного года)",
    discharge = "Суммарный расход",
    year = "Год",
    date = "Дата",
    day = "дней",
    m3s = bquote(м^3/с),
    pheader = "p-значения статистических критериев",
    rain = 'Дождевой',
    seasonal =  'Половодье',
    season = 'Сезон стока',
    components = 'Компонента стока',
    thaw = 'Талый',
    ground = 'Грунтовый',
    discharge.type = 'Сток',
    discharge.value = 'Величина стока',
    year.density = 'Плотность распределения значения переломного года',
    temp = 'Температура, \u00b0C',
    prec = 'Осадки, мм',
    preccum = 'Осадки накопл., мм',
    wraplength = 50
  )
)

usethis::use_data(params_out,
                  params_in,
                  params_in_desc,
                  grlabs,
                  internal = TRUE,
                  overwrite = TRUE)

usethis::use_data(spas,
                  overwrite = TRUE)
