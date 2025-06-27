import asyncio
import websockets
import json
import random
import struct
import time
import logging
from typing import Dict, List, Optional

# Konfiguracja logowania
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

BALL_SPEED = 2.0
PADDLE_SPEED = 10.0
CANVAS_WIDTH = 800
CANVAS_HEIGHT = 600
PADDLE_HEIGHT = 50
PADDLE_WIDTH = 10

# Definicje typów wiadomości binarnych
# Klient -> Serwer
C2S_JOIN = 1
C2S_INPUT = 2
# Serwer -> Klient
S2C_WAITING_FOR_PLAYER = 10
S2C_GAME_JOINED = 11
S2C_GAME_STARTED = 12
S2C_GAME_STATE = 13
S2C_ERROR_GAME_FULL = 14

class Player:
    def __init__(self, player_id: str, websocket, player_number: int):
        self.player_id = player_id
        self.websocket = websocket
        self.player_number = player_number  # 1 lub 2
        self.connected = True
        self.last_seen = time.time()
    
    async def send_binary(self, data: bytes):
        """Wysyła dane binarne do gracza."""
        if self.connected and self.websocket:
            try:
                await self.websocket.send(data)
            except websockets.exceptions.ConnectionClosed:
                self.connected = False

class Game:
    def __init__(self, game_id: int):
        self.id = game_id
        self.players: Dict[str, Player] = {}  # player_id -> Player
        self.created_at = time.time()
        self.last_update = time.time()
        self.running = False
        self.task = None
        
        # Stan gry
        self.state = {
            "ball_x": CANVAS_WIDTH / 2,
            "ball_y": CANVAS_HEIGHT / 2,
            "paddle1_y": (CANVAS_HEIGHT - PADDLE_HEIGHT) / 2,
            "paddle2_y": (CANVAS_HEIGHT - PADDLE_HEIGHT) / 2,
            "score1": 0,
            "score2": 0,
            "ball_dx": BALL_SPEED if random.random() > 0.5 else -BALL_SPEED,
            "ball_dy": BALL_SPEED if random.random() > 0.5 else -BALL_SPEED
        }

    def add_player(self, player_id: str, websocket) -> int:
        """Dodaje gracza do gry. Zwraca numer gracza (1 lub 2)."""
        if player_id in self.players:
            # Gracz powraca - aktualizuj połączenie
            self.players[player_id].websocket = websocket
            self.players[player_id].connected = True
            self.players[player_id].last_seen = time.time()
            logger.info(f"Player {player_id} reconnected to game {self.id}")
            return self.players[player_id].player_number
        else:
            # Nowy gracz
            player_number = len(self.players) + 1
            if player_number > 2:
                return None  # Gra jest pełna
            
            player = Player(player_id, websocket, player_number)
            self.players[player_id] = player
            logger.info(f"Player {player_id} joined game {self.id} as player {player_number}")
            return player_number

    def remove_player(self, player_id: str):
        """Oznacza gracza jako rozłączonego."""
        if player_id in self.players:
            self.players[player_id].connected = False
            logger.info(f"Player {player_id} disconnected from game {self.id}")

    def get_connected_players_count(self) -> int:
        """Zwraca liczbę połączonych graczy."""
        return sum(1 for p in self.players.values() if p.connected)

    def should_be_removed(self) -> bool:
        """Sprawdza, czy gra powinna być usunięta (brak graczy przez długi czas)."""
        if not self.players:
            return True
        
        # Usuń grę jeśli wszyscy gracze są rozłączeni przez ponad 5 minut
        all_disconnected_time = 5 * 60  # 5 minut
        current_time = time.time()
        
        for player in self.players.values():
            if player.connected or (current_time - player.last_seen) < all_disconnected_time:
                return False
        
        return True

    async def update_game_state(self):
        """Aktualizuje stan gry (fizyka)."""
        # Aktualizacja pozycji piłki
        self.state["ball_x"] += self.state["ball_dx"]
        self.state["ball_y"] += self.state["ball_dy"]

        # Odbicia od górnej i dolnej ściany
        if self.state["ball_y"] <= 5 or self.state["ball_y"] >= CANVAS_HEIGHT - 5:
            self.state["ball_dy"] *= -1

        # Ograniczenia paletek
        self.state["paddle1_y"] = max(0, min(CANVAS_HEIGHT - PADDLE_HEIGHT, self.state["paddle1_y"]))
        self.state["paddle2_y"] = max(0, min(CANVAS_HEIGHT - PADDLE_HEIGHT, self.state["paddle2_y"]))

        # Kolizje z paletkami
        ball_left = self.state["ball_x"] - 5
        ball_right = self.state["ball_x"] + 5
        ball_top = self.state["ball_y"] - 5
        ball_bottom = self.state["ball_y"] + 5

        # Kolizja z lewą paletką (gracz 1)
        if (ball_left <= PADDLE_WIDTH and 
            self.state["paddle1_y"] <= self.state["ball_y"] <= self.state["paddle1_y"] + PADDLE_HEIGHT and
            self.state["ball_dx"] < 0):
            self.state["ball_dx"] *= -1
            # Dodaj efekt "spin" w zależności od miejsca uderzenia
            hit_pos = (self.state["ball_y"] - self.state["paddle1_y"]) / PADDLE_HEIGHT
            self.state["ball_dy"] += (hit_pos - 0.5) * 2

        # Kolizja z prawą paletką (gracz 2)
        elif (ball_right >= CANVAS_WIDTH - PADDLE_WIDTH and 
              self.state["paddle2_y"] <= self.state["ball_y"] <= self.state["paddle2_y"] + PADDLE_HEIGHT and
              self.state["ball_dx"] > 0):
            self.state["ball_dx"] *= -1
            # Dodaj efekt "spin"
            hit_pos = (self.state["ball_y"] - self.state["paddle2_y"]) / PADDLE_HEIGHT
            self.state["ball_dy"] += (hit_pos - 0.5) * 2

        # Punkty
        if self.state["ball_x"] < 0:
            self.state["score2"] += 1
            await self.reset_ball()
        elif self.state["ball_x"] > CANVAS_WIDTH:
            self.state["score1"] += 1
            await self.reset_ball()

        self.last_update = time.time()

    async def reset_ball(self):
        """Resetuje piłkę na środek po zdobyciu punktu."""
        self.state["ball_x"] = CANVAS_WIDTH / 2
        self.state["ball_y"] = CANVAS_HEIGHT / 2
        self.state["ball_dx"] = BALL_SPEED if random.random() > 0.5 else -BALL_SPEED
        self.state["ball_dy"] = BALL_SPEED if random.random() > 0.5 else -BALL_SPEED
        
        # Krótka pauza po golu
        await asyncio.sleep(1)

    async def send_state_to_players(self):
        """Wysyła stan gry do wszystkich połączonych graczy."""
        # Pakowanie stanu gry do formatu binarnego
        buffer = struct.pack("<Bffffhh",
                           S2C_GAME_STATE,
                           self.state["ball_x"], 
                           self.state["ball_y"],
                           self.state["paddle1_y"], 
                           self.state["paddle2_y"],
                           self.state["score1"], 
                           self.state["score2"])
        
        # Wysyłanie do wszystkich połączonych graczy
        for player in self.players.values():
            if player.connected:
                await player.send_binary(buffer)

    def handle_input(self, player_id: str, direction: str):
        """Obsługuje input od gracza."""
        if player_id not in self.players:
            return
        
        player = self.players[player_id]
        if not player.connected:
            return
        
        move_amount = PADDLE_SPEED if direction == "down" else -PADDLE_SPEED # up: 0, down: 1
        
        if player.player_number == 1:
            self.state["paddle1_y"] += move_amount
        elif player.player_number == 2:
            self.state["paddle2_y"] += move_amount
        
        player.last_seen = time.time()

class GameManager:
    def __init__(self):
        self.games: Dict[int, Game] = {}
        self.next_game_id = 1
        self.player_to_game: Dict[str, int] = {}  # player_id -> game_id
        
    def find_or_create_game(self, player_id: str) -> Game:
        """Znajduje grę dla gracza lub tworzy nową."""
        # Sprawdź czy gracz już ma przypisaną grę
        if player_id in self.player_to_game:
            game_id = self.player_to_game[player_id]
            if game_id in self.games:
                return self.games[game_id]
            else:
                # Gra została usunięta, usuń mapowanie
                del self.player_to_game[player_id]
        
        # Znajdź grę oczekującą na gracza
        for game in self.games.values():
            if len(game.players) < 2:
                return game
        
        # Utwórz nową grę
        game = Game(self.next_game_id)
        self.games[self.next_game_id] = game
        self.next_game_id += 1
        logger.info(f"Created new game {game.id}")
        return game

    async def add_player_to_game(self, player_id: str, websocket) -> Game:
        """Dodaje gracza do gry."""
        game = self.find_or_create_game(player_id)
        player_number = game.add_player(player_id, websocket)
        
        if player_number is None:
            return None  # Gra jest pełna
        
        self.player_to_game[player_id] = game.id
        
        # Powiadom gracza o dołączeniu
        if player_id in game.players and len(game.players) == 1:
            # Gracz 1, czeka na przeciwnika
            msg = struct.pack("<Bh", S2C_WAITING_FOR_PLAYER, game.id)
            await game.players[player_id].send_binary(msg)
        else:
            # Gracz 2 dołączył
            msg = struct.pack("<BhB", S2C_GAME_JOINED, game.id, player_number)
            await game.players[player_id].send_binary(msg)
        
        # Jeśli gra ma 2 graczy i nie jest uruchomiona, uruchom ją.
        # Drugi gracz, który dołączył, wywoła start gry.
        if len(game.players) == 2 and not game.running:
            await self.start_game(game)
        
        return game

    async def start_game(self, game: Game):
        """Uruchamia pętlę gry."""
        if game.running:
            return
        
        game.running = True
        # Powiadom obu graczy o starcie gry
        msg = struct.pack("<B", S2C_GAME_STARTED)
        for player in game.players.values():
            if player.connected:
                await player.send_binary(msg)
        
        async def game_loop():
            try:
                while game.running and game.get_connected_players_count() > 0:
                    await game.update_game_state()
                    await game.send_state_to_players()
                    await asyncio.sleep(1/60)  # 60 FPS
            except Exception as e:
                logger.error(f"Error in game loop for game {game.id}: {e}")
            finally:
                game.running = False
                logger.info(f"Game {game.id} stopped")
        
        game.task = asyncio.create_task(game_loop())

    def remove_player(self, player_id: str):
        """Usuwa gracza z gry."""
        if player_id in self.player_to_game:
            game_id = self.player_to_game[player_id]
            if game_id in self.games:
                self.games[game_id].remove_player(player_id)

    async def cleanup_old_games(self):
        """Usuwa stare, nieaktywne gry."""
        games_to_remove = []
        for game_id, game in self.games.items():
            if game.should_be_removed():
                games_to_remove.append(game_id)
                if game.task and not game.task.done():
                    game.task.cancel()
        
        for game_id in games_to_remove:
            del self.games[game_id]
            # Usuń mapowania graczy
            players_to_remove = [pid for pid, gid in self.player_to_game.items() if gid == game_id]
            for player_id in players_to_remove:
                del self.player_to_game[player_id]
            logger.info(f"Removed old game {game_id}")

# Globalny manager gier
game_manager = GameManager()

async def handle_client(websocket, path):
    """Obsługuje połączenia WebSocket od klientów."""
    player_id = None
    game = None
    try:
        async for message in websocket:
            if not isinstance(message, bytes) or len(message) == 0:
                continue

            opcode = message[0]

            if opcode == C2S_JOIN:
                if player_id is not None: # Gracz próbuje dołączyć ponownie na tym samym połączeniu
                    continue
                
                try:
                    # Reszta wiadomości to player_id
                    pid = message[1:].decode('utf-8')
                    game = await game_manager.add_player_to_game(pid, websocket)
                    if game is None:
                        # Gra jest pełna
                        await websocket.send(struct.pack("<B", S2C_ERROR_GAME_FULL))
                        break
                    else:
                        player_id = pid # Pomyślnie dołączono, zapisz player_id dla tego połączenia
                except UnicodeDecodeError:
                    logger.warning("Otrzymano nieprawidłową wiadomość 'join'.")
                    break

            elif opcode == C2S_INPUT:
                if player_id is None or len(message) != 2: # Musi najpierw dołączyć
                    continue
                
                # Rozpakuj kierunek (0: góra, 1: dół)
                _, direction_code = struct.unpack("<BB", message)
                direction = "down" if direction_code == 1 else "up"

                if game:
                    game.handle_input(player_id, direction)
    
    except websockets.exceptions.ConnectionClosed:
        pass
    except Exception as e:
        logger.error(f"Error handling client {player_id}: {e}")
    finally:
        if player_id:
            game_manager.remove_player(player_id)

async def cleanup_task():
    """Okresowe czyszczenie starych gier."""
    while True:
        await asyncio.sleep(60)  # Co minutę
        await game_manager.cleanup_old_games()

async def main():
    """Główna funkcja serwera."""
    logger.info("Starting Pong server on localhost:8765")
    
    # Uruchom serwer WebSocket
    server = await websockets.serve(handle_client, "localhost", 8765)
    
    # Uruchom zadanie czyszczenia
    cleanup = asyncio.create_task(cleanup_task())
    
    try:
        await server.wait_closed()
    except KeyboardInterrupt:
        logger.info("Shutting down server...")
    finally:
        cleanup.cancel()
        server.close()

if __name__ == "__main__":
    asyncio.run(main())