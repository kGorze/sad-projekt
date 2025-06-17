# Sprawozdanie SAD, wersja 0.0.38
Gorzelańczyk Konrad, 159909, Bioinformatyka 4 semestr

### Struktura programu i funkcjonalności

Program składa się z **modularnej architektury obejmującej cztery główne moduły analityczne**, każdy odpowiedzialny za inny aspekt analizy statystycznej. Wszystkie moduły współpracują ze sobą, tworząc kompleksowy system analizy danych.

- **Moduł przygotowania danych**
```bash
# Automatyczne przygotowanie danych z domyślnymi ustawieniami i różnymi plikami wejściowymi
Rscript main.R --input dane.csv --export
```
Program automatycznie wczytuje dane, rozpoznaje typy zmiennych i kolumnę grupującą, klasyfikuje braki (MCAR, MAR, MNAR) i dobiera optymalną imputację: regresję dla MAR, MICE przy licznych brakach lub proste techniki dla MCAR, z analizą wrażliwości. Outliery wykrywa IQR-em, Z-score i zmodyfikowanym Z-score, następnie winsoryzuje, usuwa lub log-transformuje je, a gdy > 15 % obserwacji to odstające, automatycznie rekomenduje testy nieparametryczne. Wszystkie działania są dokładnie udokumentowane w raporcie końcowym.

- **Moduł statystyk opisowych**
```bash
# Generowanie statystyk opisowych z raportem HTML
Rscript main.R --descriptive_stats --input dane.csv --report --export
```
Drugi moduł generuje kompleksowe charakterystyki dla każdej grupy badawczej. Program automatycznie oblicza podstawowe miary tendencji centralnej (średnia, mediana), miary rozproszenia (odchylenie standardowe, rozstęp międzykwartylowy), oraz miary kształtu rozkładu (skośność, kurtoza). Wyniki są prezentowane oprócz konsoli w przejrzystych tabelach HTML z kolorowym kodowaniem ułatwiającym interpretację. Program automatycznie dostosowuje liczbę miejsc dziesiętnych do charakteru zmiennej i generuje dodatkowe statystyki jak współczynnik zmienności dla lepszego zrozumienia danych.

- **Moduł analizy porównawczej**
```bash
# Analiza porównawcza z raportem HTML
Rscript main.R --comparative_analysis --input dane.csv --report --export
```
Moduł 3 najpierw testuje normalność (Shapiro-Wilk < 50, Anderson-Darling ≥ 50) i równość wariancji (Levene, Bartlett, Fligner), po czym automatycznie wybiera ANOVA, ANOVA Welcha, Kruskala-Wallisa lub X^2, oblicza wielkości efektu i post-hoc z korekcją wielokrotną. Równocześnie imputuje braki: średnia/mediana przy nielicznych MCAR, regresja przy zależnościach, MICE przy licznych złożonych wzorcach, a raport pokazuje, jak te decyzje wpływają na wnioski.

- **Moduł analizy korelacji**
```bash
# Analiza korelacji z raportem HTML
Rscript main.R --correlation_analysis --input dane.csv --report --export
```
Czwarty moduł bada wzajemne relacje między zmiennymi. Program automatycznie wybiera między korelacją Pearsona (dla danych normalnych) a korelacją Spearmana (dla danych nienormalnych). System implementuje korekcję FDR (False Discovery Rate) dla kontroli błędu I rodzaju przy wielokrotnych porównaniach. Wyniki są prezentowane w formie macierzy korelacji z kolorowym kodowaniem siły związków oraz szczegółowych tabel pokazujących istotne korelacje z interpretacją siły związku.



- **Moduł zaawansowanej analizy inferencjalnej** 
```bash
# Zaawansowana analiza inferential z raportem
Rscript main.R --enhanced_inferential --input dane.csv --report --export
```
Moduł 5 wyszukuje zmienne towarzyszące o umiarkowanej korelacji z grupą (około 5-80%) i współczynniku zmienności przekraczającym 5%. Te zmienne są centrowane wokół własnej średniej, a następnie porównuje się modele zawierające tylko informację o grupie z modelami rozszerzonymi o kowariancje, wykorzystując regresję wielokrotną i analizę kowariancji. Obliczane są średnie marginalne i kontrasty parami. Jeżeli próba liczy co najmniej 75 obserwacji (oraz co najmniej 20 przypadków na każdą zmienną towarzyszącą), moduł bada także interakcję między grupą a kowariancją. Optymalny model wybiera przy pomocy kryterium informacyjnego Akaike’a, kryterium Bayesa oraz skorygowanego współczynnika determinacji. W końcu raportuje wielkość efektu jako częściowe eta kwadrat dla analiz kowariancji i współczynnik f kwadrat dla regresji.
## Raporty wyjściowe

Istnieje opcja zrobienia kompleksowej analizy danych poprzez zjednoczony raport, który składa się z wszystkich innych:
```bash
Rscript main.R --unified_dashboard
```

Program wtedy generuje cztery szczegółowe raporty HTML:
- **Statystyki opisowe** z charakterystykami grup i testami założeń
- **Analiza porównawcza** z testami różnic międzygrupowych 
- **Analiza korelacji** ze wszystkimi istotnymi związkami i kontrolą FDR
- **Zaawansowana analiza inferential** z modelowaniem ANCOVA, regresją wielokrotną i analizą interakcji
i łączy je poprzez indeks. **Wszystkie wykresy są zapisywane również jako osobne pliki PNG.**

## Przykładowe dane z labolatoriów - raporty w output/reports
1. Raport statystyczny automatycznie diagnozuje dane biomedyczne z trzech grup (CHOR1, CHOR2, KONTROLA, po 25 przypadków). Narzędzie rozpoznaje typy zmiennych, brakujące wartości oraz rozkłady - wskazało przede wszystkim nienormalność hsCRP, PLT i MON oraz graniczne odstępstwa dla wieku, HGB, HCT i LEU. Mimo to wariancje są jednorodne, dlatego algorytm proponuje Welch-ANOVA przy nierównych wariancjach albo Kruskal-Wallis, gdy rozkład jest wyraźnie odchylony. Wykresy gęstości i słupki płci ułatwiają wizualną ocenę. Analiza skrajnych obserwacji podkreśliła MON i hsCRP jako potencjalne outliery.


2. Analiza korelacji pokazuje mapę wzajemnych zależności między dziewięcioma zmiennymi krwi w 75 obserwacjach i trzech grupach. Globalnie dominują dodatnie związki erytrocytowe: HGB-HCT (r = 0,85), ERY-HGB (0,70) i ERY-HCT (0,67). Umiarkowane zależności to HGB-MCHC (+0,49) oraz ujemne PLT-MCHC (-0,32) i wiek-MCHC/LEU (-0,31). W CHOR1 wyróżnia się hsCRP-MON (+0,58) oraz mocny klaster HGB-HCT-MCHC. CHOR2 potwierdza silne trójkowe powiązania erytrocytowe, lecz bez istotnych związków zapalnych. W grupie kontrolnej wiek dodatnio koreluje z ERY i ujemnie z LEU, a ERY sprzęga się zarówno z PLT, jak i odwrotnie z MCHC. Macierze i histogram pokazują, że większość współczynników oscyluje wokół zera, podkreślając selektywność odkrytych relacji statystycznych.


3. W porównawczej analizie trzech równolicznych grup (CHOR1, CHOR2, KONTROLA) przetestowano dziewięć zmiennych ciągłych i jedną kategorialną, każdorazowo dobierając test na podstawie normalności i homogeniczności wariancji. Większość parametrów nie różniła się istotnie między grupami (wiek, hsCRP, ERY, PLT, MON, LEU, płeć). Statystycznie znaczące różnice ujawniono dla hemoglobiny, hematokrytu i MCHC (Welch-ANOVA, p ≤ 0,013), efekty były duże (d 0,87-1,07) i miały wysoką moc (> 85 %). Transformacje log lub metody nieparametryczne zabezpieczały modele z nienormalnymi resztami.

4. Analiza wpływów testuje dla każdej z dziewięciu zmiennych dwa proste modele: sam wyraz wolny („intercept”) versus „group-only”. Kryterium AIC/BIC i skorygowane R² wskazało, że dla większości parametrów (wiek, hsCRP, ERY, PLT, MON, LEU) najlepszy jest model bez efektu grupy. Różnice między CHOR1, CHOR2 i kontrolą nie poprawiają dopasowania. Jedynie HGB (Adj R² $\approx$ 0,17), HCT (0,07) i MCHC (0,13) korzystają z modelu „group-only”, potwierdzając wcześniejsze wyniki ANOVA. Nie stwierdzono interakcji grupa $\times$ kowariaty, co oznacza jednolity efekt grupy w całej próbie. Łącznie wskazuje to na selektywny, lecz stabilny wpływ choroby na parametry erytrocytarne.


## Architektura projektu

### Główny skrypt main.r

Plik main.r ładuje wszystkie wymienione moduły i definiuje interfejs wiersza poleceń, dzięki któremu można uruchamiać poszczególne analizy:

```R
source("modules/utils/config.R")
source("modules/utils/logging.R")
source("modules/utils/statistical_helpers.R")
source("modules/data/fetch_dataset.R")
source("modules/data/inspect_data.R")
source("modules/data/validate_data.R")
source("modules/data/repair_dataset.R")
source("modules/analysis/assumptions_dashboard.R")
source("modules/analysis/master_descriptive_summary.R")
source("modules/analysis/descriptive_stats.R")
source("modules/analysis/comparative_analysis.R")
source("modules/analysis/correlation_analysis.R")
source("modules/reporting/generate_report.R")
source("modules/reporting/export_results.R")
```


### Struktura modułów

modules/
- analysis/     implementacja głównych analiz statystycznych
- data/         wczytywanie, inspekcja i naprawa danych
- reporting/    generowanie raportów i eksport wyników
- utils/        konfiguracja, logowanie i funkcje pomocnicze


### Pliki w modules/analysis
descriptive_stats.R, comparative_analysis.R, correlation_analysis.R – główne moduły analityczne realizujące odpowiednio statystyki opisowe, porównania grup oraz analizy korelacji.

enhanced_inferential_framework.R – moduł rozszerzonej analizy inferencyjnej (ANCOVA, regresja, interakcje).

assumptions_dashboard.R, master_descriptive_summary.R, enhanced_posthoc.R, residual_transformation.R – wspierające funkcje dotyczące testów założeń, podsumowania opisowego, analizy post‑hoc oraz diagnostyki reszt.

### Pliki w modules/data
fetch_dataset.R – wczytuje plik CSV, wykrywa kodowanie i konwertuje typy danych

inspect_data.R, validate_data.R – sprawdzają strukturę i poprawność zbioru.

repair_dataset.R – wykonuje analizę braków, imputację, wykrywanie/outlierów i generuje raport z czyszczenia danych

### Pliki w modules/reporting
generate_report.R – tworzy raporty HTML dla dowolnego modułu analizy, łącząc wyniki i wykresy

export_results.R – zapisuje tabele i dane wyjściowe do katalogu output/tables.

### Pliki w modules/utils
config.R – przechowuje globalne ustawienia analizy (poziom istotności, parametry wykresów itp.)

logging.R – obsługa logów wykonywanych kroków i błędów podczas analizy

plotting_utils.R – narzędzia do zarządzania urządzeniami graficznymi oraz bezpiecznego zapisu wykresów

statistical_helpers.R – funkcje statystyczne wykorzystywane w analizach (np. liczenie skośności, kurtozy, z-score)
