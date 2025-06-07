import pika

# Nawiązanie połączenia z serwerem RabbitMQ
connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

# Deklaracja kolejki
channel.queue_declare(queue='point_to_point_queue')

# Wysłanie wiadomości
for i in range(5):
    message = f"Wiadomość {i}"
    channel.basic_publish(exchange='', routing_key='point_to_point_queue', body=message.encode())
    print(f"Wysłano: {message}")

# Zamknięcie połączenia
connection.close()