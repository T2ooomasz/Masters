import pika
import sys

# Nawiązanie połączenia z serwerem RabbitMQ
connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

# Deklaracja wymiany typu topic
channel.exchange_declare(exchange='topic_logs', exchange_type='topic')

# Ustalenie klucza routingu i wiadomości
routing_key = sys.argv[1] if len(sys.argv) > 2 else 'anonymous.info'
message = ' '.join(sys.argv[2:]) or 'Hello World!'

# Wysłanie wiadomości
channel.basic_publish(
    exchange='topic_logs',
    routing_key=routing_key,
    body=message.encode()
)

print(f" [x] Wysłano {routing_key}: {message}")

# Zamknięcie połączenia
connection.close()
