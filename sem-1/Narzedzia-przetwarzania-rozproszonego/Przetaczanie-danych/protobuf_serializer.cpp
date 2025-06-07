#include <iostream>
#include <fstream>
#include <string>
#include "student_data.pb.h"  // Generowany z pliku .proto przez protoc

/*
 * Program demonstracyjny serializacji danych studentów za pomocą Protocol Buffers
 * 
 * Cel: Pokazać jak używać Protobuf do serializacji strukturalnych danych
 * z wykorzystaniem oneof (union-like), repeated fields i enums
 */

using namespace std;
using namespace student_data;

int main() {
    // Utworzenie głównego kontenera dla listy studentów
    ListaStudentow lista_studentow;
    
    /*
     * Student 1: identyfikator jako numer albumu
     * Pokazuje użycie oneof z wartością liczbową
     */
    Student* student1 = lista_studentow.add_studenci();
    student1->set_nr_albumu(123456);  // ustawia pole nr_albumu w oneof
    student1->set_nazwisko("Kowalski");
    student1->set_imie("Jan");
    
    // Dodanie ocen dla studenta 1: DST, DB, BDB
    student1->add_oceny(OcenaTyp::DST);   // dostateczny
    student1->add_oceny(OcenaTyp::DB);    // dobry
    student1->add_oceny(OcenaTyp::BDB);   // bardzo dobry
    
    /*
     * Student 2: identyfikator jako email
     * Pokazuje użycie oneof z wartością string
     */
    Student* student2 = lista_studentow.add_studenci();
    student2->set_email("anna.nowak@student.edu.pl");  // ustawia pole email w oneof
    student2->set_nazwisko("Nowak");
    student2->set_imie("Anna");
    
    // Dodanie ocen dla studenta 2: DB_P, BDB, BDB
    student2->add_oceny(OcenaTyp::DB_P);  // dobry plus
    student2->add_oceny(OcenaTyp::BDB);   // bardzo dobry
    student2->add_oceny(OcenaTyp::BDB);   // bardzo dobry
    
    /*
     * Student 3: identyfikator jako numer albumu
     * Pokazuje większą liczbę ocen w tablicy repeated
     */
    Student* student3 = lista_studentow.add_studenci();
    student3->set_nr_albumu(789012);
    student3->set_nazwisko("Wiśniewski");
    student3->set_imie("Piotr");
    
    // Dodanie ocen dla studenta 3: NDST, DST, DST_P, DB
    student3->add_oceny(OcenaTyp::NDST);   // niedostateczny
    student3->add_oceny(OcenaTyp::DST);    // dostateczny
    student3->add_oceny(OcenaTyp::DST_P);  // dostateczny plus
    student3->add_oceny(OcenaTyp::DB);     // dobry
    
    /*
     * Serializacja do pliku binarnego
     * Protocol Buffers automatycznie koduje dane w kompaktowym formacie binarnym
     */
    ofstream output_file("studenci.pb", ios::binary);
    if (!output_file) {
        cerr << "Błąd: Nie można otworzyć pliku do zapisu" << endl;
        return 1;
    }
    
    // SerializeToOstream() zapisuje dane w formacie binarnym Protobuf
    if (!lista_studentow.SerializeToOstream(&output_file)) {
        cerr << "Błąd serializacji danych" << endl;
        return 1;
    }
    
    output_file.close();
    
    cout << "Dane zostały zserializowane do pliku studenci.pb" << endl;
    cout << "Zserializowano " << lista_studentow.studenci_size() << " studentów" << endl;
    
    /*
     * Demonstracja deserializacji - odczyt z pliku
     * Pokazuje, że dane można poprawnie odczytać
     */
    ifstream input_file("studenci.pb", ios::binary);
    ListaStudentow odczytana_lista;
    
    if (odczytana_lista.ParseFromIstream(&input_file)) {
        cout << "\nVeryfikacja - odczytano z pliku:" << endl;
        
        for (int i = 0; i < odczytana_lista.studenci_size(); i++) {
            const Student& s = odczytana_lista.studenci(i);
            cout << "Student " << (i+1) << ": " << s.imie() << " " << s.nazwisko() << endl;
            
            // Sprawdzenie typu identyfikatora (oneof)
            if (s.has_nr_albumu()) {
                cout << "  Nr albumu: " << s.nr_albumu() << endl;
            } else if (s.has_email()) {
                cout << "  Email: " << s.email() << endl;
            }
            
            cout << "  Liczba ocen: " << s.oceny_size() << endl;
        }
    }
    
    input_file.close();
    
    // Protobuf automatycznie zarządza pamięcią - nie ma potrzeby jawnego zwalniania
    return 0;
}

/*
 * Instrukcje kompilacji:
 * 1. Wygeneruj kod C++ z definicji Protobuf: protoc --cpp_out=. student_data.proto
 * 2. Kompiluj: g++ -o protobuf_serializer protobuf_serializer.cpp student_data.pb.cc -lprotobuf
 * 3. Uruchom: ./protobuf_serializer
 * 
 * Uwaga: Wymagana instalacja biblioteki protobuf-dev
 */