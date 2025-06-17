import zmq
import json
import logging
from collections import deque

# Configure logging for the server module
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - SERVER - %(message)s')

class MonitorState:
    """
    Klasa reprezentująca stan pojedynczego monitora
    
    Atrybuty:
    - mutex_owner: str | None - ID procesu posiadającego mutex lub None
    - condition_queues: Dict[str, deque[str]] - kolejki oczekujących dla każdego warunku (ID procesów)
    - enter_queue: deque[str] - Kolejka procesów (ich ID) czekających na zdobycie muteksu.
                              Procesy trafiają tu po początkowym żądaniu 'enter' (jeśli muteks jest zajęty)
                              lub po zasygnalizowaniu z kolejki warunku.
    """    
    def __init__(self):
        """Inicjalizacja stanu monitora"""
        self.mutex_owner: str | None = None
        self.enter_queue: deque[str] = deque()
        self.condition_queues: dict[str, deque[str]] = {}


class MonitorServer:
    """
    Główny serwer monitora obsługujący wiele instancji monitorów
    
    Atrybuty:
    - monitors: Dict[str, MonitorState] - mapa nazw monitorów do ich stanów
    - zmq_context: zmq.Context
    - zmq_socket: zmq.Socket - socket REP do komunikacji
    - bind_address: str - adres, na którym serwer nasłuchuje
    """
    
    def __init__(self, port: int = 5555):
        """
        Inicjalizacja serwera.
        
        Args:
            port: Port, na którym serwer będzie nasłuchiwał.
        """
        self.monitors: dict[str, MonitorState] = {}
        self.zmq_context = zmq.Context()
        self.zmq_socket = self.zmq_context.socket(zmq.REP)
        self.bind_address = f"tcp://*:{port}"
        logging.info(f"MonitorServer inicjalizowany.")

    def _get_monitor_state(self, monitor_name: str) -> MonitorState:
        """Pobiera stan monitora, tworząc go jeśli nie istnieje."""
        if monitor_name not in self.monitors:
            self.monitors[monitor_name] = MonitorState()
            logging.info(f"Utworzono nowy monitor: '{monitor_name}'")
        return self.monitors[monitor_name]

    def start(self):
        """Główna pętla serwera obsługująca żądania."""
        self.zmq_socket.bind(self.bind_address)
        logging.info(f"MonitorServer nasłuchuje na {self.bind_address}")

        while True:
            try:
                message_bytes = self.zmq_socket.recv()
                request = json.loads(message_bytes.decode('utf-8'))
                logging.debug(f"Otrzymano żądanie: {request}")

                action = request.get("action")
                monitor_name = request.get("monitor_name")
                process_id = request.get("process_id") # Essential for all state-changing ops
                
                response = {"status": "error", "message": "Invalid request"}

                if not monitor_name: # monitor_name is crucial for routing
                    response = {"status": "error", "message": "Missing monitor_name"}
                elif not action:
                     response = {"status": "error", "message": "Missing action"}
                elif not process_id and action in ["ENTER", "EXIT", "WAIT", "SIGNAL", "BROADCAST", "CHECK_GRANT", "CHECK_AWAKENED"]:
                    # process_id is needed for most actions
                    response = {"status": "error", "message": f"Missing process_id for action {action}"}
                else:
                    if action == "ENTER":
                        response = self.handle_enter_monitor(monitor_name, process_id)
                    elif action == "EXIT":
                        response = self.handle_exit_monitor(monitor_name, process_id)
                    elif action == "WAIT":
                        condition_name = request.get("condition_name")
                        if condition_name:
                            response = self.handle_wait_condition(monitor_name, condition_name, process_id)
                        else:
                            response = {"status": "error", "message": "Missing condition_name for WAIT"}
                    elif action == "SIGNAL":
                        condition_name = request.get("condition_name")
                        if condition_name:
                            response = self.handle_signal_condition(monitor_name, condition_name, process_id)
                        else:
                            response = {"status": "error", "message": "Missing condition_name for SIGNAL"}
                    elif action == "BROADCAST":
                        condition_name = request.get("condition_name")
                        if condition_name:
                            response = self.handle_broadcast_condition(monitor_name, condition_name, process_id)
                        else:
                            response = {"status": "error", "message": "Missing condition_name for BROADCAST"}
                    
                    # --- Polling actions (client would need to implement these) ---
                    elif action == "CHECK_GRANT": 
                         monitor = self._get_monitor_state(monitor_name)
                         if monitor.mutex_owner == process_id:
                             response = {"status": "granted", "monitor_name": monitor_name}
                         else:
                             response = {"status": "queued", "monitor_name": monitor_name}
                    elif action == "CHECK_AWAKENED":
                        monitor = self._get_monitor_state(monitor_name)
                        condition_name = request.get("condition_name") # Client should send this for context
                        if monitor.mutex_owner == process_id:
                            response = {"status": "granted_after_wait", "monitor_name": monitor_name}
                        elif process_id in monitor.enter_queue:
                            response = {"status": "queued_after_signal", "monitor_name": monitor_name}
                        elif condition_name and process_id in monitor.condition_queues.get(condition_name, deque()):
                            response = {"status": "still_waiting_on_condition", "monitor_name": monitor_name, "condition_name": condition_name}
                        else: 
                            response = {"status": "pending_unknown_state", "monitor_name": monitor_name} # Fallback
                    # --- End Polling actions ---

                self.zmq_socket.send_json(response)
                logging.debug(f"Wysłano odpowiedź: {response}")

            except json.JSONDecodeError:
                logging.error("Błąd dekodowania JSON.")
                self.zmq_socket.send_json({"status": "error", "message": "Invalid JSON format"})
            except zmq.ZMQError as e:
                logging.error(f"Błąd ZMQ: {e}")
                break 
            except Exception as e:
                logging.error(f"Nieoczekiwany błąd serwera: {e}", exc_info=True)
                try:
                    self.zmq_socket.send_json({"status": "error", "message": f"Internal server error: {str(e)}"})
                except Exception as send_e:
                    logging.error(f"Nie udało się wysłać odpowiedzi o błędzie: {send_e}")
        
        self.zmq_socket.close()
        self.zmq_context.term()
        logging.info("MonitorServer zakończył działanie.")

    def handle_enter_monitor(self, monitor_name: str, process_id: str) -> dict:
        monitor = self._get_monitor_state(monitor_name)
        
        if monitor.mutex_owner == process_id:
            logging.warning(f"Monitor '{monitor_name}': Proces {process_id} już posiada mutex (żądanie ENTER).")
            return {"status": "error", "message": "Already in monitor", "monitor_name": monitor_name}

        can_grant_immediately = monitor.mutex_owner is None and \
                                (not monitor.enter_queue or monitor.enter_queue[0] == process_id)

        if can_grant_immediately:
            if monitor.enter_queue and monitor.enter_queue[0] == process_id:
                monitor.enter_queue.popleft() 
            monitor.mutex_owner = process_id
            logging.info(f"Monitor '{monitor_name}': Przyznano mutex procesowi {process_id}.")
            return {"status": "granted", "monitor_name": monitor_name}
        else:
            if process_id not in monitor.enter_queue:
                 monitor.enter_queue.append(process_id)
            logging.info(f"Monitor '{monitor_name}': Proces {process_id} dodany do kolejki wejścia (enter_queue).")
            return {"status": "queued", "monitor_name": monitor_name}
        
    def handle_exit_monitor(self, monitor_name: str, process_id: str) -> dict:
        monitor = self._get_monitor_state(monitor_name)

        if monitor.mutex_owner != process_id:
            logging.warning(f"Monitor '{monitor_name}': Proces {process_id} próbował zwolnić mutex, którego nie posiada.")
            return {"status": "error", "message": "Not mutex owner", "monitor_name": monitor_name}

        logging.info(f"Monitor '{monitor_name}': Proces {process_id} zwalnia mutex.")
        monitor.mutex_owner = None
        
        next_owner_pid = self._grant_mutex_to_next(monitor_name)
        
        return {"status": "success", "monitor_name": monitor_name, "next_granted_to": next_owner_pid}
        
    def handle_wait_condition(self, monitor_name: str, condition_name: str, process_id: str) -> dict:
        monitor = self._get_monitor_state(monitor_name)

        if monitor.mutex_owner != process_id:
            logging.warning(f"Monitor '{monitor_name}', Warunek '{condition_name}': Proces {process_id} próbował czekać bez posiadania muteksu.")
            return {"status": "error", "message": "Not mutex owner for wait", "monitor_name": monitor_name}

        logging.info(f"Monitor '{monitor_name}', Warunek '{condition_name}': Proces {process_id} zwalnia mutex i czeka.")
        monitor.mutex_owner = None 

        if condition_name not in monitor.condition_queues:
            monitor.condition_queues[condition_name] = deque()
        monitor.condition_queues[condition_name].append(process_id)
        
        self._grant_mutex_to_next(monitor_name) 

        return {"status": "waiting_on_condition", "monitor_name": monitor_name, "condition_name": condition_name}
        
    def handle_signal_condition(self, monitor_name: str, condition_name: str, process_id: str) -> dict:
        monitor = self._get_monitor_state(monitor_name)
        
        if monitor.mutex_owner != process_id:
            logging.warning(f"Monitor '{monitor_name}', Warunek '{condition_name}': Proces {process_id} próbował sygnalizować bez posiadania muteksu.")
            return {"status": "error", "message": "Not mutex owner for signal", "monitor_name": monitor_name}

        woken_count = 0
        if condition_name in monitor.condition_queues and monitor.condition_queues[condition_name]:
            woken_pid = monitor.condition_queues[condition_name].popleft()
            monitor.enter_queue.append(woken_pid) 
            woken_count = 1
            logging.info(f"Monitor '{monitor_name}', Warunek '{condition_name}': Zasygnalizowano proces {woken_pid}. Przeniesiono do kolejki wejścia (enter_queue).")
        else:
            logging.info(f"Monitor '{monitor_name}', Warunek '{condition_name}': Brak procesów oczekujących na sygnał.")
            
        return {"status": "success", "woken_processes": woken_count, "monitor_name": monitor_name}
        
    def handle_broadcast_condition(self, monitor_name: str, condition_name: str, process_id: str) -> dict:
        monitor = self._get_monitor_state(monitor_name)

        if monitor.mutex_owner != process_id:
            logging.warning(f"Monitor '{monitor_name}', Warunek '{condition_name}': Proces {process_id} próbował broadcastować bez posiadania muteksu.")
            return {"status": "error", "message": "Not mutex owner for broadcast", "monitor_name": monitor_name}

        woken_count = 0
        if condition_name in monitor.condition_queues and monitor.condition_queues[condition_name]:
            while monitor.condition_queues[condition_name]:
                woken_pid = monitor.condition_queues[condition_name].popleft()
                monitor.enter_queue.append(woken_pid)
                woken_count += 1
            if woken_count > 0:
                 logging.info(f"Monitor '{monitor_name}', Warunek '{condition_name}': Rozgłoszono do {woken_count} procesów. Przeniesiono do kolejki wejścia (enter_queue).")
            else: # Should not happen if the outer if was true and inner while loop ran
                logging.info(f"Monitor '{monitor_name}', Warunek '{condition_name}': Brak procesów oczekujących na broadcast (kolejka pusta).")
        else:
            logging.info(f"Monitor '{monitor_name}', Warunek '{condition_name}': Brak procesów oczekujących na broadcast (kolejka nie istnieje lub pusta).")

        return {"status": "success", "woken_processes": woken_count, "monitor_name": monitor_name}
        
    def _grant_mutex_to_next(self, monitor_name: str) -> str | None:
        """
        Przyznaje mutex następnemu w kolejce (enter_queue), jeśli mutex jest aktualnie wolny.
        Ta metoda tylko aktualizuje stan serwera. Nie wysyła wiadomości.
        Klient dowie się o przyznaniu muteksu poprzez polling (np. CHECK_GRANT).
        Jest wywoływana po zwolnieniu muteksu (exit, wait).
        
        Returns:
            process_id następnego procesu, któremu przyznano mutex, lub None jeśli nikomu nie przyznano
            (lub mutex nie był wolny, co nie powinno się zdarzyć jeśli logika wywołania jest poprawna).
        """
        monitor = self._get_monitor_state(monitor_name)
        
        if monitor.mutex_owner is not None:
            # This case should ideally not be hit if _grant_mutex_to_next is called
            # only after mutex is confirmed to be free by the caller.
            logging.warning(f"Monitor '{monitor_name}': _grant_mutex_to_next wywołane, gdy mutex jest wciąż zajęty przez {monitor.mutex_owner}.")
            return None 
        
        if monitor.enter_queue:
            next_process_id = monitor.enter_queue.popleft()
            monitor.mutex_owner = next_process_id
            logging.info(f"Monitor '{monitor_name}': Mutex przyznany (przez _grant_mutex_to_next) procesowi {next_process_id} z kolejki wejścia (enter_queue).")
            return next_process_id
        else:
            # Mutex jest wolny (monitor.mutex_owner is None) i brak procesów w enter_queue.
            logging.info(f"Monitor '{monitor_name}': Mutex wolny, brak procesów w kolejce wejścia (enter_queue) do przyznania.")
            # Ensure mutex_owner remains None (it should already be None from the caller)
            monitor.mutex_owner = None 
            return None

if __name__ == '__main__':
    server = MonitorServer(port=5555)
    try:
        server.start()
    except KeyboardInterrupt:
        logging.info("Serwer zatrzymywany przez użytkownika (Ctrl+C)...")
    finally:
        # Basic cleanup, actual socket/context cleanup is in start()
        logging.info("Serwer definitywnie zakończył działanie.")

