import pika
import sys

# Nawiązanie połączenia z serwerem RabbitMQ
connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

# Deklaracja wymiany typu direct
channel.exchange_declare(exchange='direct_logs', exchange_type='direct')

# Ustalenie klucza routingu i wiadomości
severity = sys.argv[1] if len(sys.argv) > 1 else 'info'
message = ' '.join(sys.argv[2:]) or 'Hello World!'

# Wysłanie wiadomości
channel.basic_publish(
    exchange='direct_logs',
    routing_key=severity,
    body=message.encode()
)

print(f" [x] Wysłano {severity}: {message}")

# Zamknięcie połączenia
connection.close()
