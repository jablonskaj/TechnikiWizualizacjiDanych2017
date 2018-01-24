# aplikacja shiny - praca domowa 08

Aplikacja dotyczy interaktywnej wizualizacji danych dot. ocen piłkarzy Legii Warszawa w meczach
Ekstraklasy w sezonie 2017/2018.

Oceny wystawiał portal `Weszło!`, jeden z najpopularniejszych portali internetowych
dot. polskiej piłki nożnej.

Ponieważ `Weszło!` nie udostępnia żadnej bazy danych, dane należało zescrapować ze stron internetowych
za pomocą języka `Python` i pakietów `BeautifulSoup` oraz `pandas` (program 'scrap_data.ipynb')

### zakres pracy aplikacji

Aplikacja pozwala na porównanie formy całych formacji (obrona, pomoc, atak) oraz poszczególnych zawodników
na przestrzeni wszystkich lub wybranych spotkań sezonu 2017/2018.

# plik z danymi

programy `ui.R` oraz `server.R` korzystają z pliku `dane.RDS`
