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
    subtitle = stringi::stri_escape_unicode('Начало водохозяйственного года'),
    bartitle.win = stringi::stri_escape_unicode("Месяц минимального месячного расхода за зиму"),
    bartitle.sum = stringi::stri_escape_unicode("Месяц минимального месячного расхода за лето"),
    monthtitle = stringi::stri_escape_unicode("Месяц"),
    periodtitle = stringi::stri_escape_unicode("Период"),
    beforetitle = stringi::stri_escape_unicode("до "),
    aftertitle = stringi::stri_escape_unicode("с "),
    label.p = stringi::stri_escape_unicode('p'),
    student.t = stringi::stri_escape_unicode('Стьюдент: t'),
    fisher.f = stringi::stri_escape_unicode('Фишер: F'),
    pettitt.u = stringi::stri_escape_unicode('Петтитт: U*'),
    kendall.z = stringi::stri_escape_unicode('Манн-Кендалл: z'),
    theil.i = stringi::stri_escape_unicode('Тейл-Сен: i'),
    student.df = stringi::stri_escape_unicode('df'),
    clipped.remark = stringi::stri_escape_unicode("(обрезано по концу календарного года)"),
    discharge = stringi::stri_escape_unicode("Суммарный расход"),
    year = stringi::stri_escape_unicode("Год"),
    date = stringi::stri_escape_unicode("Дата"),
    day = stringi::stri_escape_unicode("дней"),
    m3s = expression(stringi::stri_escape_unicode('м')^3/stringi::stri_escape_unicode('с')),
    pheader = stringi::stri_escape_unicode("p-значения статистических критериев"),
    rain = stringi::stri_escape_unicode('Дождевой'),
    seasonal =  stringi::stri_escape_unicode('Половодье'),
    season = stringi::stri_escape_unicode('Сезон стока'),
    components = stringi::stri_escape_unicode('Компонента стока'),
    thaw = stringi::stri_escape_unicode('Талый'),
    ground = stringi::stri_escape_unicode('Грунтовый'),
    discharge.type = stringi::stri_escape_unicode('Сток'),
    discharge.value = stringi::stri_escape_unicode('Величина стока'),
    year.density = stringi::stri_escape_unicode('Плотность распределения значения переломного года'),
    temp = stringi::stri_escape_unicode('Температура, \u00b0C'),
    prec = stringi::stri_escape_unicode('Осадки, мм'),
    preccum = stringi::stri_escape_unicode('Осадки накопл., мм'),
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
