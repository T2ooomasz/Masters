import pika

# Funkcja obsługująca odebrane wiadomości
def callback(ch, method, properties, body):
    print(f"Odebrano: {body.decode()}")

# Nawiązanie połączenia z serwerem RabbitMQ
connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

# Deklaracja wymiany typu fanout
channel.exchange_declare(exchange='fanout_exchange', exchange_type='fanout')

# Deklaracja tymczasowej kolejki
result = channel.queue_declare(queue='', exclusive=True)
queue_name = result.method.queue

# Powiązanie kolejki z wymianą
channel.queue_bind(exchange='fanout_exchange', queue=queue_name)

# Ustawienie konsumenta
channel.basic_consume(queue=queue_name, on_message_callback=callback, auto_ack=True)

print('Oczekiwanie na wiadomości. Aby zakończyć, naciśnij CTRL+C')
channel.start_consuming()