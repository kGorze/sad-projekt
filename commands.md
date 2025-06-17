# Wszystkie możliwe kombinacje użycia programu analizy statystycznej

## **WZORCE KOMEND (SZABLONY)**

### Podstawowy wzorzec komendy
```bash
Rscript main.R [--input PLIK] [ANALIZA] [--report] [--export] [OPCJE_PRZETWARZANIA]
```

**Gdzie:**
- `[--input PLIK]` - opcjonalnie, domyślnie `dane.csv`
- `[ANALIZA]` - jeden z typów analizy (patrz niżej)
- `[--report]` - opcjonalnie, generuje raport HTML
- `[--export]` - opcjonalnie, eksportuje wyniki do CSV
- `[OPCJE_PRZETWARZANIA]` - opcjonalnie, `--repair_data=FALSE` i/lub `--validate_data=FALSE`

## **TYPY ANALIZ**

### Wszystkie dostępne analizy
```bash
--comparative_analysis      # Analiza porównawcza między grupami
--correlation_analysis       # Analiza korelacji między zmiennymi
--descriptive_stats          # Statystyki opisowe
--statistical_tests          # Testy statystyczne (placeholder)
--enhanced_inferential       # Analiza inferencji rozszerzonej (ANCOVA, regresja)
--unified_dashboard          # Zintegrowany dashboard (wszystkie analizy)
```

## **PODSTAWOWE KOMBINACJE**

### Pojedyncze analizy (z domyślnym plikiem dane.csv)
```bash
# Tylko analiza
Rscript main.R --comparative_analysis
Rscript main.R --correlation_analysis
Rscript main.R --descriptive_stats
Rscript main.R --enhanced_inferential
Rscript main.R --statistical_tests

# Analiza + raport HTML
Rscript main.R --comparative_analysis --report
Rscript main.R --correlation_analysis --report
Rscript main.R --descriptive_stats --report
Rscript main.R --enhanced_inferential --report

# Analiza + eksport CSV
Rscript main.R --comparative_analysis --export
Rscript main.R --correlation_analysis --export
Rscript main.R --descriptive_stats --export
Rscript main.R --enhanced_inferential --export

# Analiza + raport + eksport
Rscript main.R --comparative_analysis --report --export
Rscript main.R --correlation_analysis --report --export
Rscript main.R --descriptive_stats --report --export
Rscript main.R --enhanced_inferential --report --export
```

### Dashboard (specjalny przypadek)
```bash
# Dashboard (zawiera wszystkie analizy, nie potrzebuje --report)
Rscript main.R --unified_dashboard

# Dashboard + eksport
Rscript main.R --unified_dashboard --export
```

## **Z NIESTANDARDOWYMI PLIKAMI DANYCH**

### Szablon z własnym plikiem
```bash
# Zastąp TWÓJ_PLIK.csv właściwą nazwą pliku
Rscript main.R --input TWÓJ_PLIK.csv --[ANALIZA] [--report] [--export]
```

### Przykłady z różnymi plikami
```bash
# Z dostępnymi plikami danych
Rscript main.R --input dane2.csv --descriptive_stats --report
Rscript main.R --input dane3.csv --correlation_analysis --export
Rscript main.R --input dane4.csv --unified_dashboard

# Z niestandardowymi plikami
Rscript main.R --input custom_data.csv --comparative_analysis --report
Rscript main.R --input clinical_data.xlsx --unified_dashboard
Rscript main.R --input research_data.csv --enhanced_inferential --report --export
```

## **OPCJE PRZETWARZANIA DANYCH**

### Szablony z opcjami przetwarzania
```bash
# Bez naprawy danych
Rscript main.R [--input PLIK] --[ANALIZA] --repair_data=FALSE [--report] [--export]

# Bez walidacji danych
Rscript main.R [--input PLIK] --[ANALIZA] --validate_data=FALSE [--report] [--export]

# Bez naprawy i walidacji (dla już oczyszczonych danych)
Rscript main.R [--input PLIK] --[ANALIZA] --repair_data=FALSE --validate_data=FALSE [--report] [--export]
```

### Przykłady z opcjami przetwarzania
```bash
# Dla już oczyszczonych danych
Rscript main.R --descriptive_stats --repair_data=FALSE --validate_data=FALSE

# Z niestandardowym plikiem i bez naprawy
Rscript main.R --input clean_data.csv --comparative_analysis --repair_data=FALSE --report

# Test z surowymi danymi bez walidacji
Rscript main.R --input raw_data.csv --descriptive_stats --validate_data=FALSE
```

## **TYLKO EKSPORT DANYCH (bez analiz)**

```bash
# Tylko eksport oczyszczonych danych (domyślny plik)
Rscript main.R --export

# Z niestandardowym plikiem
Rscript main.R --input TWÓJ_PLIK.csv --export

# Przykłady
Rscript main.R --input dane2.csv --export
Rscript main.R --input raw_data.csv --export
```

## **NAJCZĘŚCIEJ UŻYWANE KOMBINACJE**

### Top 10 najpraktyczniejszych komend
```bash
# 1. Kompletny dashboard (najlepszy start)
Rscript main.R --unified_dashboard

# 2. Szybki przegląd danych z raportem
Rscript main.R --descriptive_stats --report

# 3. Dashboard z własnym plikiem
Rscript main.R --input TWÓJ_PLIK.csv --unified_dashboard

# 4. Analiza porównawcza z pełną dokumentacją
Rscript main.R --comparative_analysis --report --export

# 5. Korelacje z eksportem
Rscript main.R --correlation_analysis --export

# 6. Zaawansowana analiza z raportem
Rscript main.R --enhanced_inferential --report --export

# 7. Tylko eksport oczyszczonych danych
Rscript main.R --export

# 8. Dashboard z eksportem wyników
Rscript main.R --unified_dashboard --export

# 9. Test bez przetwarzania danych
Rscript main.R --descriptive_stats --repair_data=FALSE --validate_data=FALSE

# 10. Analiza własnych danych z pełną dokumentacją
Rscript main.R --input TWÓJ_PLIK.csv --enhanced_inferential --report --export
```

## **WORKFLOW BADAWCZY (KROK PO KROKU)**

### Rekomendowany przepływ pracy
```bash
# KROK 1: Sprawdzenie jakości danych
Rscript main.R --input TWOJE_DANE.csv --export

# KROK 2: Szybka analiza opisowa
Rscript main.R --input TWOJE_DANE.csv --descriptive_stats --report

# KROK 3: W zależności od potrzeb:
# - Porównania między grupami:
Rscript main.R --input TWOJE_DANE.csv --comparative_analysis --report --export

# - Badanie zależności:
Rscript main.R --input TWOJE_DANE.csv --correlation_analysis --report --export

# - Zaawansowane modelowanie:
Rscript main.R --input TWOJE_DANE.csv --enhanced_inferential --report --export

# KROK 4: Kompletny raport (opcjonalnie)
Rscript main.R --input TWOJE_DANE.csv --unified_dashboard
```

## **SPECJALNE PRZYPADKI UŻYCIA**

### Batch processing (analiza wielu plików)
```bash
# Szablon dla batch processing
for file in dane*.csv; do
    Rscript main.R --input "$file" --descriptive_stats --export
done

# Lub pojedynczo:
Rscript main.R --input dane.csv --descriptive_stats --export
Rscript main.R --input dane2.csv --descriptive_stats --export
Rscript main.R --input dane3.csv --descriptive_stats --export
Rscript main.R --input dane4.csv --descriptive_stats --export
```

### Porównanie metod analizy (te same dane, różne analizy)
```bash
# Szablon
Rscript main.R --input TWÓJ_PLIK.csv --descriptive_stats --report
Rscript main.R --input TWÓJ_PLIK.csv --correlation_analysis --report
Rscript main.R --input TWÓJ_PLIK.csv --comparative_analysis --report
Rscript main.R --input TWÓJ_PLIK.csv --enhanced_inferential --report
```

### Debugowanie i rozwiązywanie problemów
```bash
# Dla już oczyszczonych danych
Rscript main.R --input CZYSTE_DANE.csv --descriptive_stats --repair_data=FALSE --validate_data=FALSE

# Test z minimalnymi zmianami
Rscript main.R --input PROBLEM_DANE.csv --descriptive_stats --repair_data=FALSE

# Szybka walidacja
Rscript main.R --input TEST_DANE.csv --export
```

### Najważniejsze wzorce:
1. **`Rscript main.R --unified_dashboard`** - kompletna analiza (START TUTAJ)
2. **`Rscript main.R --input PLIK.csv --[ANALIZA] --report --export`** - pełna analiza z dokumentacją
3. **`Rscript main.R --descriptive_stats --report`** - szybki przegląd
4. **`Rscript main.R --export`** - tylko eksport danych