# Загрузка данные с WDI и Портала открытых данных РФ
library('httr')
library('jsonlite')
library('XML')
library('RCurl')
library('WDI')
library('data.table')
# Загрузка с WDI, показатель Доступ к электричеству (% населения)
indicator.code <- 'EG.ELC.ACCS.ZS'

dat <- WDI(indicator = indicator.code, start = 2018, end = 2018)

# Загружаем данные
data <- data.table(dat)

# Загружаем данные в .csv файл
write.csv(data, file = './data/WDI_data_2018.csv', row.names = F)

# Загружаем данные с портала открытых данных РФ

API.key <- 'adadc480cabbad83d80d945dc996b3ca'
URL.base <- 'http://data.gov.ru/api/'


# Функция для работы с API портала открытых данных РФ
getOpenDataRF <- function(api.params, url.base = URL.base, api.key = API.key){
  par <- paste0(api.params, collapse = '/')
  url <- paste0(url.base, par, '/?access_token=', api.key)
  message(paste0('Загружаем ', url, ' ...'))
  resp <- GET(url)
  fromJSON(content(resp, 'text'))
}

# id: Адресный перечень многоквартирных домов, 
# вошедших в региональную программу Капитальный ремонт общего имущества в многоквартирных домах
dataset_id <- '3460012716-zhkhregistryoverhaul'

# Задаем параметры и получаем данные
params <- c('dataset', dataset_id)
dataset <- getOpenDataRF(params)

# Количество версий таблицы
params <- c(params, 'version')
versions <- getOpenDataRF(params)

nrow(versions)

# Загружаем последнюю версию в объект doc
mrv <- versions[nrow(versions), 1]
params <- c(params, mrv)
content <- c(params, 'content')
doc <- getOpenDataRF(content)

# Оставляем только те данные в которых присутствует поселок Пурпе
doc <- doc[grep('г. Калач-на-Дону', doc$Address), c('Municipality', 'Year2',
                                                    'Wall', 'TotalArea1', 'Address')]

head(doc)
## ------------------------------------------------------------

## Yandex.Карты -----------------------------------------------

API.key <- '13ae988e-8470-4547-b9c0-41dd74d8cc5e'
URL.base <- 'https://geocode-maps.yandex.ru/1.x/'

# Функция для работы с API Yandex Карт
getYandexMaps <- function(api.params, url.base = URL.base, api.key = API.key){
  par <- paste0(api.params, collapse = '&')
  url <- paste0(url.base, '?format=xml&apikey=', api.key, par)
  message(paste0('Загружаем ', url, ' ...'))
  doc.ya <- content(GET(url), 'text', encoding = 'UTF-8')
  
  rootNode <- xmlRoot(xmlTreeParse(doc.ya, useInternalNodes = TRUE))
  coords <- xpathSApply(rootNode, "//*[name()='Envelope']/*", xmlValue)
  coords <- lapply(strsplit(coords, ' '), as.numeric)
  coords <- c((coords[[1]][1] + coords[[2]][1])/2, (coords[[1]][2] + coords[[2]][2])/2)
  names(coords) <-c('lat', 'long')
  coords
}

# Задаем параметры
params <-paste0('&geocode=', gsub(pattern =' ', replacement ='+',
                                  curlEscape(doc$Address[1])))


# Парсим координаты
coords <- sapply(as.list(doc$Address), function(x){
  params <- paste0('&geocode=', gsub(curlEscape(x), pattern = ' ',
                                     replacement = '+'))
  try(getYandexMaps(params))
})

df.coords <- as.data.frame(t(coords))
colnames(df.coords) <- c('long', 'lat')

#Добавляем координаты в основной фрейм данных
doc <- cbind(doc, df.coords)
# Сохраняем данные в файл
write.csv2(doc, file = './data/RF.csv', row.names = F)
