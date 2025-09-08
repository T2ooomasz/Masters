import pika
import sys

# Nawiązanie połączenia z serwerem RabbitMQ
connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

# Deklaracja wymiany
channel.exchange_declare(exchange='direct_logs', exchange_type='direct')

# Deklaracja kolejki o losowej nazwie
result = channel.queue_declare(queue='', exclusive=True)
queue_name = result.method.queue

# Bindowanie kolejki
channel.queue_bind(exchange='direct_logs', queue=queue_name, routing_key='info')

print(' [*] Oczekiwanie na logi. Aby zakończyć, naciśnij CTRL+C')

def callback(ch, method, properties, body):
    print(f" [x] Otrzymano {method.routing_key}: {body.decode()}")

channel.basic_consume(
    queue=queue_name,
    on_message_callback=callback,
    auto_ack=True
)

channel.start_consuming()
