# Projekt 4: Narzędzie sterujące mocą sygnału karty bezprzewodowej (wersja Haskell)

## Nazwa i opis projektu

**Nazwa:** `Wireless Power Control Tool (Haskell Edition)`

**Opis:**
Narzędzie wiersza poleceń napisane w funkcyjnym języku Haskell, które pozwala na zarządzanie mocą nadawania (Tx Power) bezprzewodowych kart sieciowych w systemie operacyjnym GNU/Linux. Program, podobnie jak wersje w innych językach, komunikuje się z systemem za pomocą standardowych narzędzi `iw` oraz `ip`.

Projekt demonstruje zarządzanie stanem i efektami ubocznymi (operacje I/O, wywoływanie procesów) w czysto funkcyjnym paradygmacie przy użyciu monady `IO`.

## Zawartość plików źródłowych i zasady ich kompilacji

### Pliki źródłowe:

* `WirelessPowerTool.hs`: Główny i jedyny plik źródłowy projektu. Zawiera całą logikę aplikacji w języku Haskell.

### Zasady kompilacji:

Projekt należy skompilować przy użyciu kompilatora GHC (Glasgow Haskell Compiler).

1.  **Instalacja GHC:**
    Jeśli nie masz GHC, zainstaluj go używając menedżera pakietów swojej dystrybucji.
    ```bash
    # Dla systemów bazujących na Debianie/Ubuntu
    sudo apt-get install ghc

    # Dla systemów bazujących na Arch Linux
    sudo pacman -S ghc
    ```

2.  **Instalacja zależności:**
    Program wymaga pakietu `unix`, który dostarcza powiązania do funkcji systemowych POSIX (w tym `geteuid`).
    ```bash
    # Zainstaluj narzędzie cabal-install, jeśli go nie masz
    sudo apt-get install cabal-install # Debian/Ubuntu
    sudo pacman -S cabal-install      # Arch Linux

    # Zaktualizuj bazę pakietów Cabal i zainstaluj zależność
    cabal update
    cabal install unix
    ```

3.  **Kompilacja programu:**
    Otwórz terminal w katalogu z plikiem `WirelessPowerTool.hs` i wykonaj polecenie:
    ```bash
    ghc --make WirelessPowerTool.hs -o wireless_power_tool
    ```
    Polecenie to stworzy plik wykonywalny o nazwie `wireless_power_tool`.

### Zależności systemowe:

Narzędzie do poprawnego działania wymaga zainstalowanych w systemie pakietów `iw` oraz `iproute2`.
```bash
# Dla systemów bazujących na Debianie/Ubuntu
sudo apt-get update && sudo apt-get install iw iproute2

# Dla systemów bazujących na Arch Linux
sudo pacman -S iw iproute2