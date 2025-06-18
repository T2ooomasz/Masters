#!/bin/bash

echo "🔧 DISTRIBUTED MONITOR - ETAP 3 - POPRAWKI"
echo "=========================================================="
echo "Uruchamianie poprawionych testów monitora"
echo "=========================================================="

# Sprawdź czy pliki istnieją
if [ ! -f "monitor_server.py" ] || [ ! -f "monitor_client.py" ]; then
    echo "❌ Brakuje plików monitor_server.py lub monitor_client.py"
    echo "💡 Skopiuj zawartość artifacts do plików:"
    echo "   - Artifact 'fixed_monitor_server' → monitor_server.py"
    echo "   - Artifact 'fixed_monitor_client' → monitor_client.py"
    echo "   - Artifact 'improved_test_monitor' → test_monitor_fixed.py"
    exit 1
fi

# Sprawdź zależności
echo "📦 Sprawdzanie zależności..."
python3 -c "import zmq; print('✅ pyzmq zainstalowane')" || {
    echo "❌ Brak pyzmq. Zainstaluj: pip install pyzmq"
    exit 1
}

# Uruchom testy
echo ""
echo "🚀 Uruchamianie testów..."
echo ""

python3 test_monitor_fixed.py

echo ""
echo "📊 PODSUMOWANIE:"
echo "  - Jeśli testy przeszły pomyślnie: ✅ Etap 3 ukończony!"
echo "  - Jeśli nadal są błędy: 🔍 Analiza logów powyżej"
echo ""
echo "🎯 NASTĘPNE KROKI (Etap 4):"
echo "  1. Implementacja BoundedBuffer jako test case"
echo "  2. Porównanie z lokalnym monitorem"
echo "  3. Testy wydajności"
echo "=========================================================="