skrypt .r
sprawozdanie w formacie .pdf - jako dokumentacja produktu czyli tej analizy, sprawozdanie ma byc napisane jak manual do zaproponowanego narzędzie.
- trzeba opisać jakie testy użyliśmy
- jak intepretować przykładowe wyniki
- jak interpretować generowane wykresy
podstawowa analiza statystyczna dla danych medycznych
skrypt
badane grupy są niezależne(więcej niż dwie grupy)


# Zdefiniowanie problemu badawczego
Musimy określić cel oraz przedstawić hipoteze badawczą.
- Jakie mamy badane parametry?
- Jaka jest skala pomiarowa?
- Czy próba jest odpowiednio duża?
    - Jak możemy zwiększyć odpowiednio próbe? Czy jeżeli generujemy próby to również zmniejszamy błąd? 
- Czym jest niska moc testu?
    - Czy da się i tak jakoś znaleść istotne efekty?
- Czy możemy jakoś zweryfikować dane?


# Charakterystyka zmiennych
Musze odpowiedzieć na te pytania i później do nich dobrać testy statystyczne.
- Czy nasze zmienne wszystkie są danymi ilościowymi?
- Czy nasze zmienne mają tylko i wyłącznie charakter ciągły?
- Czy mamy jakieś skale nominalne? 
- Czy są jakieś skale porządkowe?
- Czy jest jakaś skala interwałowa?
- Czy jest jakaś skala ilorazowa?

# Ocena danych wejściowych
- Lepiej usunąć niepełne dane czy jednak wstawić sztuczny pomiar?
- Użyć srednią, mediane czy może zrobić regresje liniową?

# Ocena zgodności danych z rozkładem normalnym
- Czym jest test Kołogomorowa-Smirnowa?
- Czym jest test Shapiro-Wilka?
- Plots quantiles of data vs. expected quantiles of rnomal distribution with same mi and sigma.
- Graficzna oceny zgodności z rozkładem normalnym.
- Ocena homogeniczności(jedrorodności) wariancji - test FIshera, test Levne'a
- porównywanie grup niezależnych i zależnych
- Jakie są testy nieparametryczne?
- Czym ją testy post hoc?

# Ocena zależności pomiędzy parametrami
- Ocena istnienia siły i korelacji pomiędzy wybranami parametrami
- Ocena kształtu i kierunku tej zależności
- Zależność nie zawsze oznacza ciąg przyczynowo-skutkowy
- Jak policzyć 


# Hipotezy zerowe


## Uruchamianie skryptu w trybie wsadowym
z argumentem wejściowym w formie .csv


## Co to znaczy, że grupy są niezależne?
W kontekście tego zadania „grupy niezależne” oznaczają, że każda z badanych grup składa się z różnych, niepowiązanych ze sobą obserwacji (np. różnych osób). Innymi słowy:
- Każdy „próba” (obserwacja, pacjent) należy dokładnie do jednej grupy i nie występuje w żadnej innej.
- Nie ma parowania ani powtarzanych pomiarów na tych samych obiektach między grupami (np. nie porównujemy wyników tego samego pacjenta przed/po zabiegu – bo to byłyby grupy zależne/parami sparowane).
- Wyniki w jednej grupie nie wpływają bezpośrednio na wyniki w drugiej grupie.
Dzięki temu możemy założyć, że obserwacje pomiędzy grupami są losowo od siebie niezależne. Przykładowo:
1. Grupa A = pacjenci leczeni lekiem X.
2. Grupa B = pacjenci leczeni lekiem Y.
3. Grupa C = pacjenci placebo.
    
Każdy pacjent jest przypisany tylko do jednej z tych trzech grup (nie ma takiej samej osoby w Grupie A i jednocześnie w Grupie B itd.). Jeżeli chcemy porównać średnie, odsetki czy inny parametr pomiędzy tymi grupami, używamy testów dla prób niezależnych (np. **testu t-Studenta** dla dwóch grup lub **analizy wariancji ANOVA/Kruskal-Wallisa** dla więcej niż dwóch).

Jeśli założenie niezależności jest spełnione, wyniki testów są poprawne (nie dochodzi do „podwójnego liczenia” czy korelacji między pomiarami tych samych osób).

# Rodzaj danych jakie mamy


# Elementy analizy statystycznej
### 1.Przygotowanie danych wejściowych przez poradzenie sobie z brakami danych. 
Zaraportowanie wszystkich wprowadzonych zmian. Dodatkowo zaraportowanie informacji o wartościach odstających dla wybranych parame trów.

### 2.Wykonanie charakterystyki dla badanych grup, zapisanie wyników w czytelnej formie.
Polecana struktura tabelaryczna

### 3. Wykonanie analizy porównawczej pomiędzy grupami, określenie czy istenija istotne statystyczne różnice. 
Jeśli istnieją istotne statystyczne różnice pomiędzy grupami to zaraportowanie pomiędzy którymi grupami wystę pują i jak istotne są to różnice.

### 4. Wykonanie analizy korelacji. 
Zaraportowanie pomiędzy którymi parametrami w obrębie jakich grup występują istotne statystycznie korelacje oraz określenie siły i kierunku korelacji.

# Wizaualizacja danych

# Generowanie raportu podsumowującego










## To do 

### 🗒️ Comprehensive To-Do List

**A. Remove Redundancy**

* [X] Build one “Assumptions dashboard” that holds all normality + variance tests; delete the duplicates now in Descriptive Stats and Comparative Analysis
* [X] Merge repeated group-wise descriptive tables into a single master summary; link to it instead of re-printing the full table everywhere&#x20;

**B. Strengthen Inferential Framework**

* [X] Add multiple linear regression / ANCOVA models to measure group effects after adjusting for covariates (e.g., age, hsCRP)&#x20;
* [X] Test interaction terms (e.g., group × age) and keep those that improve model fit

**C. Deepen Association Analysis**

* [X] Compute separate correlation matrices for CHOR1, CHOR2, KONTROLA
* [X] Provide partial correlations that adjust for group in an “overall” matrix

**D. Control for Multiple Testing**

* [X] Apply Benjamini–Hochberg (FDR) correction to every correlation p-value
* [X] Use Holm (or BH) adjustment for all post-hoc comparisons (Tukey, Dunn)&#x20;

**E. Report Effect Sizes Fully**

* [X] Calculate rank-biserial *r* for each Mann-Whitney result
* [X] Calculate ε² (or η²) with 95 % CI for every Kruskal-Wallis; fill “Effect Size: Not calculated” gaps&#x20;
* [X] Add 95 % CI to η² in every ANOVA and Cohen’s *d* to all two-group t-tests

**F. Fix Model Residual Issues**

* [X] For each model flagged “Residuals Non-normal” (e.g., *wiek \~ grupa*, *hsCRP \~ grupa*) try log/sqrt transforms and re-check assumptions
* [X] If transformation fails, refit with robust regression (Huber/M-estimator)

**G. Handle Missing Data**

* [ ] Review missing-value summary (4 % overall) and decide on imputation route
* [ ] Run mean/median and multiple-imputation sensitivity analyses; document impact

**H. Automate Assumption Flags**

* [ ] Code an automatic flag for borderline normality (e.g., MON Shapiro p = 0.0435) that feeds directly into test-selection rules&#x20;

**I. Visual & UX Enhancements**

* [ ] Replace static correlation table with an interactive heat-map (hover shows *r*, CI, FDR-adjusted p)
* [ ] Swap boxplots for violin/rain-cloud plots to convey distribution shape
* [ ] Combine the three HTML files into one dashboard with tab navigation and sticky sidebar

**J. Standardize Nomenclature & Formatting**

* [ ] Harmonize variable names (e.g., always “hsCRP”) across all outputs
* [ ] Format all p-values consistently (e.g., three decimals, *p* < 0.001 in sci-notation)
* [ ] Add a small glossary for variable abbreviations and units

**K. Document Statistical Choices**

* [ ] Insert footnotes explaining each test/effect-size choice (per decision matrix)&#x20;
* [ ] Explicitly describe the chosen multiple-testing correction in the methods section

**L. Summarize Key Findings Up Front**

* [ ] Create a two-line “Executive summary” at the very top of the report highlighting main significant differences and effect sizes

**M. Dashboard Polish**

* [ ] Add quick filters (group select) and download buttons for plots/tables
* [ ] Ensure responsive layout works on tablets and phones

**N. Quality Assurance**

* [ ] Unit-test every analysis step with representative data slices
* [ ] Cross-check that table totals and *N*’s match the raw dataset counts

*(Tick off each box as you implement—feel free to expand any item into subtasks as needed.)*
