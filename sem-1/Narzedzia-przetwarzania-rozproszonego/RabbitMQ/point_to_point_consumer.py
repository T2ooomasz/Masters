import pika

# Funkcja obsługująca odebrane wiadomości
def callback(ch, method, properties, body):
    print(f"Odebrano: {body.decode()}")
    # Potwierdzenie przetworzenia wiadomości
    ch.basic_ack(delivery_tag=method.delivery_tag)

# Nawiązanie połączenia z serwerem RabbitMQ
connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

# Deklaracja kolejki
channel.queue_declare(queue='point_to_point_queue')

# Ustawienie konsumenta
channel.basic_consume(queue='point_to_point_queue', on_message_callback=callback)

print('Oczekiwanie na wiadomości. Aby zakończyć, naciśnij CTRL+C')
channel.start_consuming()