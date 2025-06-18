-- Plik: WirelessPowerTool.hs
-- Opis: Narzędzie do sterowania mocą karty bezprzewodowej w Haskellu.
-- Kompilacja: ghc --make WirelessPowerTool.hs -o wireless_power_tool

import System.Process         (readProcessWithExitCode, shell)
import System.Exit            (ExitCode(..), exitFailure)
import System.IO              (hFlush, stdout)
import System.Posix.User      (getEffectiveUserID)
import Data.List              (isPrefixOf, find)
import Text.Read              (readMaybe)
import Control.Monad          (when, forM, forM_)

-- Typ danych reprezentujący interfejs bezprzewodowy
data WirelessInterface = WirelessInterface {
    ifaceName :: String,
    txPower   :: String
} deriving (Show)

-- Główna funkcja programu
main :: IO ()
main = do
    putStrLn "--- Narzędzie do sterowania mocą karty bezprzewodowej (Haskell) ---"

    -- Sprawdzenie uprawnień administratora
    checkRootPrivileges

    -- Krok 1: Znajdź interfejsy
    putStrLn "\n1. Wyszukiwanie interfejsów bezprzewodowych..."
    maybeInterfaces <- findWirelessInterfaces
    
    case maybeInterfaces of
        Nothing -> do
            putStrLn "Błąd: Nie można było wykonać polecenia 'iw'. Czy jest zainstalowane?"
            exitFailure
        Just [] -> do
            putStrLn "Nie znaleziono żadnych interfejsów bezprzewodowych."
            exitFailure
        Just interfaces -> do
            -- Krok 2: Pobierz szczegóły i pokaż menu
            detailedInterfaces <- getInterfacesDetails interfaces
            presentMenuAndGetInput detailedInterfaces

-- Sprawdza, czy program został uruchomiony z uprawnieniami roota.
checkRootPrivileges :: IO ()
checkRootPrivileges = do
    euid <- getEffectiveUserID
    when (euid /= 0) $ do
        putStrLn "Błąd: Ten program wymaga uprawnień administratora (root)."
        putStrLn "Uruchom go używając 'sudo ./wireless_power_tool'"
        exitFailure

-- Wykonuje polecenie systemowe i zwraca jego kod wyjścia, stdout i stderr.
runCommand :: String -> IO (ExitCode, String, String)
runCommand cmd = readProcessWithExitCode "sh" ["-c", cmd] ""

-- Znajduje nazwy interfejsów bezprzewodowych przez parsowanie wyniku 'iw dev'.
findWirelessInterfaces :: IO (Maybe [String])
findWirelessInterfaces = do
    (exitCode, stdoutStr, _) <- runCommand "iw dev"
    case exitCode of
        ExitSuccess -> return $ Just (parseInterfaces stdoutStr)
        _           -> return Nothing

-- Funkcja czysta: parsuje output polecenia 'iw dev'.
parseInterfaces :: String -> [String]
parseInterfaces output =
    [word | line <- lines output, "Interface" `isPrefixOf` line, let ws = words line, length ws > 1, let word = ws !! 1]

-- Dla listy nazw interfejsów, pobiera ich szczegóły (aktualną moc).
getInterfacesDetails :: [String] -> IO [WirelessInterface]
getInterfacesDetails names = forM names $ \name -> do
    power <- getCurrentTxPower name
    return $ WirelessInterface name power

-- Pobiera aktualną moc Tx dla danego interfejsu.
getCurrentTxPower :: String -> IO String
getCurrentTxPower name = do
    (exitCode, stdoutStr, _) <- runCommand ("iw dev " ++ name ++ " link")
    case exitCode of
        ExitSuccess -> return $ parseTxPower stdoutStr
        _           -> return "Nie można odczytać"

-- Funkcja czysta: parsuje moc z wyniku polecenia 'iw'.
parseTxPower :: String -> String
parseTxPower output =
    case find (== "txpower") (words output) of
        Just _  -> unwords . take 2 . drop 1 . dropWhile (/= "txpower") $ words output
        Nothing -> "Nieznana"

-- Prezentuje menu, pobiera wybór użytkownika i uruchamia proces zmiany mocy.
presentMenuAndGetInput :: [WirelessInterface] -> IO ()
presentMenuAndGetInput interfaces = do
    putStrLn "\nDostępne interfejsy bezprzewodowe:"
    forM_ (zip [1..] interfaces) $ \(i, iface) ->
        putStrLn $ show i ++ ". " ++ ifaceName iface ++ " (Aktualna moc: " ++ txPower iface ++ ")"

    -- Pobranie wyboru interfejsu
    putStr "\nWybierz interfejs (podaj numer): "
    hFlush stdout
    choiceStr <- getLine
    let maybeChoice = readMaybe choiceStr :: Maybe Int

    -- Pobranie nowej wartości mocy
    putStr "Podaj nową moc nadawania (w dBm): "
    hFlush stdout
    powerStr <- getLine
    let maybePower = readMaybe powerStr :: Maybe Int

    case (maybeChoice, maybePower) of
        (Just choice, Just power) | choice > 0 && choice <= length interfaces -> do
            let selectedIface = interfaces !! (choice - 1)
            setTxPower (ifaceName selectedIface) power
        _ -> do
            putStrLn "\nBłąd: Nieprawidłowy numer interfejsu lub wartość mocy."
            exitFailure

-- Ustawia nową moc nadawania wykonując sekwencję poleceń.
setTxPower :: String -> Int -> IO ()
setTxPower name power = do
    putStrLn $ "\nKonfigurowanie interfejsu '" ++ name ++ "'..."
    
    let commands = [
            ("1. Wyłączanie interfejsu...", "ip link set " ++ name ++ " down"),
            ("2. Ustawianie mocy na " ++ show power ++ " dBm...", "iw dev " ++ name ++ " set txpower fixed " ++ show power ++ "mBm"),
            ("3. Włączanie interfejsu...", "ip link set " ++ name ++ " up")
            ]

    -- Wykonaj polecenia sekwencyjnie, zatrzymaj w razie błędu
    success <- executeCommandsSequentially commands
    
    when success $ do
        putStrLn "\nKonfiguracja zakończona pomyślnie!"
        putStrLn "Sprawdzanie nowej konfiguracji..."
        newPower <- getCurrentTxPower name
        putStrLn $ "Nowa moc dla interfejsu '" ++ name ++ "': " ++ newPower

executeCommandsSequentially :: [(String, String)] -> IO Bool
executeCommandsSequentially [] = return True -- Wszystkie polecenia wykonane pomyślnie
executeCommandsSequentially ((message, cmd):cmds) = do
    putStrLn message
    (exitCode, _, stderrStr) <- runCommand cmd
    case exitCode of
        ExitSuccess -> executeCommandsSequentially cmds
        _           -> do
            putStrLn $ "Błąd podczas wykonywania polecenia: " ++ cmd
            putStrLn $ "Stderr: " ++ stderrStr
            -- Próba przywrócenia interfejsu do stanu 'up' w razie błędu
            _ <- runCommand ("ip link set " ++ takeWhile (/=' ') (words cmd !! 3) ++ " up")
            return False