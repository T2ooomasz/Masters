import pika
import sys

# Nawiązanie połączenia z serwerem RabbitMQ
connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

# Deklaracja wymiany
channel.exchange_declare(exchange='topic_logs', exchange_type='topic')

# Deklaracja kolejki o losowej nazwie
result = channel.queue_declare(queue='', exclusive=True)
queue_name = result.method.queue

# Bindowanie kolejki
binding_keys = sys.argv[1:]
if not binding_keys:
    sys.stderr.write(f"Użycie: {sys.argv[0]} [klucz_bindowania]...\n")
    sys.exit(1)

for binding_key in binding_keys:
    channel.queue_bind(
        exchange='topic_logs',
        queue=queue_name,
        routing_key=binding_key
    )

print(' [*] Oczekiwanie na logi. Aby zakończyć, naciśnij CTRL+C')

def callback(ch, method, properties, body):
    print(f" [x] Otrzymano {method.routing_key}: {body.decode()}")

channel.basic_consume(
    queue=queue_name,
    on_message_callback=callback,
    auto_ack=True
)

channel.start_consuming()
