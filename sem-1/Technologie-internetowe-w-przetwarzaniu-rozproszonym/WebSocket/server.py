import asyncio
import websockets
import json
import random
import struct

# Lista aktywnych gier
games = []
next_game_id = 0

class Game:
    def __init__(self, game_id):
        self.id = game_id
        self.players = []  # Lista graczy: {ws, player_id, connected}
        self.state = {
            "ball_x": 400.0,   # Pozycja X piłki
            "ball_y": 300.0,   # Pozycja Y piłki
            "paddle1_y": 275.0, # Pozycja Y paletki gracza 1
            "paddle2_y": 275.0, # Pozycja Y paletki gracza 2
            "score1": 0,       # Wynik gracza 1
            "score2": 0,       # Wynik gracza 2
            "ball_dx": 5.0 if random.random() > 0.5 else -5.0, # Prędkość X piłki
            "ball_dy": 5.0 if random.random() > 0.5 else -5.0  # Prędkość Y piłki
        }

    def add_player(self, ws, player_id):
        """Dodaje gracza do gry."""
        self.players.append({"ws": ws, "player_id": player_id, "connected": True})

    async def update(self):
        """Aktualizuje stan gry (fizyka gry)."""
        # Aktualizacja pozycji piłki
        self.state["ball_x"] += self.state["ball_dx"]
        self.state["ball_y"] += self.state["ball_dy"]

        # Kolizje z górą i dołem
        if self.state["ball_y"] <= 0 or self.state["ball_y"] >= 600:
            self.state["ball_dy"] *= -1

        # Kolizje z paletkami
        if self.state["ball_x"] <= 10 and self.state["paddle1_y"] <= self.state["ball_y"] <= self.state["paddle1_y"] + 50:
            self.state["ball_dx"] *= -1
        elif self.state["ball_x"] >= 790 and self.state["paddle2_y"] <= self.state["ball_y"] <= self.state["paddle2_y"] + 50:
            self.state["ball_dx"] *= -1

        # Punkty
        if self.state["ball_x"] < 0:
            self.state["score2"] += 1
            self.reset_ball()
        elif self.state["ball_x"] > 800:
            self.state["score1"] += 1
            self.reset_ball()

    def reset_ball(self):
        """Resetuje piłkę na środek po zdobyciu punktu."""
        self.state["ball_x"] = 400.0
        self.state["ball_y"] = 300.0
        self.state["ball_dx"] = 5.0 if random.random() > 0.5 else -5.0
        self.state["ball_dy"] = 5.0 if random.random() > 0.5 else -5.0

    async def send_state(self):
        """Wysyła stan gry do wszystkich graczy w formacie binarnym."""
        buffer = struct.pack("<Bfffffhh", 2, self.state["ball_x"], self.state["ball_y"],
                             self.state["paddle1_y"], self.state["paddle2_y"],
                             self.state["score1"], self.state["score2"])
        for player in self.players:
            if player["connected"]:
                await player["ws"].send(buffer)

async def handler(websocket, path):
    """Obsługuje połączenia WebSocket od klientów."""
    global games, next_game_id
    try:
        async for message in websocket:
            data = json.loads(message)
            if data["type"] == "join":
                player_id = data["player_id"]
                # Szukamy gry z wolnym miejscem lub tworzymy nową
                game = next((g for g in games if len(g.players) < 2), None)
                if not game:
                    game = Game(next_game_id)
                    next_game_id += 1
                    games.append(game)
                game.add_player(websocket, player_id)
                if len(game.players) == 2:
                    asyncio.create_task(game_loop(game))
            elif data["type"] == "input":
                player_id = data["player_id"]
                direction = data["direction"]
                for game in games:
                    for player in game.players:
                        if player["player_id"] == player_id:
                            if player == game.players[0]:
                                game.state["paddle1_y"] += 10 if direction == "down" else -10
                            else:
                                game.state["paddle2_y"] += 10 if direction == "down" else -10
                            break
    except websockets.exceptions.ConnectionClosed:
        pass

async def game_loop(game):
    """Pętla gry aktualizująca stan i wysyłająca go do klientów."""
    while True:
        await game.update()
        await game.send_state()
        await asyncio.sleep(1 / 60)  # 60 FPS

# Uruchomienie serwera
start_server = websockets.serve(handler, "localhost", 8765)
asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()