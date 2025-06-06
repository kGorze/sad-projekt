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

