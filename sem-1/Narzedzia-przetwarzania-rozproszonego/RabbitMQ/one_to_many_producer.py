import pika

# Nawiązanie połączenia z serwerem RabbitMQ
connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

# Deklaracja wymiany typu fanout
channel.exchange_declare(exchange='fanout_exchange', exchange_type='fanout')

# Wysłanie wiadomości
for i in range(5):
    message = f"Wiadomość rozgłoszeniowa {i}"
    channel.basic_publish(exchange='fanout_exchange', routing_key='', body=message.encode())
    print(f"Wysłano: {message}")

# Zamknięcie połączenia
connection.close()