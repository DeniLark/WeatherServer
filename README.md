# Промежуточный кеш-сервер для погоды

## Параметры приложения

Параметры приложения указываются в переменных окружения и в конфигурационном файле config.yaml. Пример конфигурационного файла присутствует в репозитории.

### Переменные окружения

- ключ API(обязательный) - имя переменной WEATHER_API_KEY

- API root(необязательный) - имя переменной WEATHER_API_ROOT

### Конфигурационный файл

- port(обязательный) - порт на котором запустится сервер

- locations(необязательный) - координаты для которых необходимо автоматическое кеширование

  - указываются в виде:

```
- lat: широта
  lon: долгота
```

- updatePeriod(необязательный) - период автоматических обновлений, в минутах. По умолчанию - 10 минут

- offsetLocations(необязательный) - допустимые погрешности для координат локаций. Число типа Double

- offsetTime(необязательный) - допустимые погрешности для времени. В минутах

## Эндпойнты

```
/weather - получение информации о погоде. Обязательные GET параметры: lat, lon
/swagger-ui/ - документация по API
```

## Запуск

Выполнить в терминале

`cabal run`

## Структура

- app/Main.hs - точка входа приложения, описание и запуск сервера
- src/Server/ - функции для работы сервера
- src/Weather/ - описание типа описывающего информацию о погоде
- src/Config.hs - модуль для загрузки параметров из config.yaml
- src/Weather.hs - модуль для работы с API сервиса погоды OpenWeatherMap API (https://openweathermap.org/api)
- config.yaml - конфигурационный файл
