# Krok 4.1: Importujemy wygenerowane klasy
import student_pb2

# === TWORZENIE DANYCH ===

# Tworzymy główny obiekt, który będzie przechowywał listę studentów
students_list = student_pb2.StudentsList()

# --- Student 1: Jan Kowalski (identyfikator: numer albumu) ---
student1 = students_list.studenci.add()
student1.nr_albumu = 12345
student1.imie = "Jan"
student1.nazwisko = "Kowalski"
# Dodajemy oceny do listy, używając wartości z naszego Enum
student1.oceny.extend([student_pb2.BDB, student_pb2.DB, student_pb2.DST_P])

# --- Student 2: Anna Nowak (identyfikator: e-mail) ---
student2 = students_list.studenci.add()
student2.email = "anna.nowak@example.com"
student2.imie = "Anna"
student2.nazwisko = "Nowak"
student2.oceny.extend([student_pb2.DST, student_pb2.NDST])


print("--- Dane przed serializacją ---")
print(students_list)


# === SERIALIZACJA (ZAPIS DO PLIKU) ===

# Krok 4.2: Serializujemy obiekt do postaci binarnej i zapisujemy w pliku
file_path = "students.db"
with open(file_path, "wb") as f:
    f.write(students_list.SerializeToString())

print(f"\n✅ Dane zostały zapisane do pliku: {file_path}")


# === DESERIALIZACJA (ODCZYT Z PLIKU) ===

# Krok 4.3: Wczytujemy dane binarne z pliku i parsujemy je do nowego obiektu
new_students_list = student_pb2.StudentsList()

with open(file_path, "rb") as f:
    new_students_list.ParseFromString(f.read())

print("\n--- Dane po deserializacji (odczytane z pliku) ---")
# Krok 4.4: Wyświetlamy odczytane dane, aby sprawdzić poprawność
for student in new_students_list.studenci:
    print(f"\nStudent: {student.imie} {student.nazwisko}")
    
    # Sprawdzamy, który identyfikator jest ustawiony
    if student.HasField("nr_albumu"):
        print(f"  ID: Numer albumu - {student.nr_albumu}")
    elif student.HasField("email"):
        print(f"  ID: Email - {student.email}")
        
    # Odczytujemy oceny - dostajemy liczby, musimy je zmapować na nazwy
    grade_names = [student_pb2.Grade.Name(g) for g in student.oceny]
    print(f"  Oceny: {grade_names}")