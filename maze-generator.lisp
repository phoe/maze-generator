;; Michał Herda
;; ASD2
;; Generator labiryntów

;; #############################################################
;; ## ODCINEK I
;; ##
;; ## Kolejka priorytetowa
;; #############################################################

#| Zdefiniujmy sobie w tym miejscu klasy. Sterta będzie miała dwa pola - tablicę z wpisami
oraz liczbę elementów w stercie, natomiast wpis sterty liczbę będącą priorytetem
oraz dane. 
Zdefiniujemy tutaj od razu funkcje-akcesory, by nie musieć definiować ich dalej. |#

(defclass heap ()
  ((array :initarg :array
          :accessor array-of)
   (elements :initform 0
             :accessor elements
             :type integer)))

(defclass entry ()
  ((data :initarg :data
         :accessor data)
   (priority :initarg :priority
             :accessor priority
             :type integer)))

#| W konstruktorze stosu chcemy stworzyć tablicę o podanym przez użytkownika rozmiarze.|#
(defmethod initialize-instance :after ((heap heap) &key size)
  (setf (array-of heap) (make-array size :element-type 'entry)))

#| Stwórzmy ładnie brzmiące funkcje będące konstruktorami. |#
(defun make-heap (size)
  (make-instance 'heap :size size))

(defun make-entry (priority data)
  (make-instance 'entry :priority priority :data data))

#| Na potrzeby testów zdefiniujmy sobie prosty błąd sterty i konstruktor dla niego. |#
(define-condition heap-error (error)
  ((message :initarg :message
            :reader heap-error-message
            :type string))
  (:report (lambda (condition stream)
             (format stream (heap-error-message condition)))))
(defun heap-error (message)
  (check-type message string)
  (error 'heap-error :message message))

#| Określmy interfejs naszej sterty za pomocą generyków. Będziemy chcieli wkładać, podglądać 
i wyjmować elementy oraz sprawdzać rozmiar sterty, czy jest pusta i czy jest pełna.
Ustawmy również ustawiarkę dla HEAP-PEEK - przyda się nam ona za chwilę, przy zdejmowaniu
elementu z kopca. |#
(defgeneric heap-push (heap data priority))
(defgeneric heap-pop (heap))
(defgeneric heap-peek (heap))
(defgeneric (setf heap-peek) (entry heap))
(defgeneric heap-empty-p (heap))
(defgeneric heap-full-p (heap))
(defgeneric heap-size (heap))

#| Teraz zdefiniujmy metody. Będziemy specjalizować je na obiektach klasy HEAP, stąd też zapis -
w notacji (HEAP HEAP) pierwsza wartość oznacza nazwę zmiennej w metodzie, a druga - typ 
tej zmiennej.
Rozmiar sterty to inaczej rozmiar jej tablicy, czyli pierwszego (licząc od zera) wymiaru 
tablicy.|#
(defmethod heap-size ((heap heap))
  (array-dimension (array-of heap) 0))

#| Sprawdzenie pusty/pełny są analogiczne - sprawdzamy liczbę elementów. |#
(defmethod heap-empty-p ((heap heap))
  (= (elements heap) 0))
(defmethod heap-full-p ((heap heap))
  (= (elements heap) (heap-size heap)))

#| Podglądanie elementu to sprawdzenie pierwszego wpisu z tablicy. |#
(defmethod heap-peek ((heap heap))
  (when (heap-empty-p heap)
    (error 'heap-error :message "Attempted to peek at an element from an empty heap."))
  (aref (array-of heap) 0))

#| Ustawiarka przyjmuje pojedynczy element, a mianowicie wpis.
Zgodnie z lispową konwencją wszelkiej maści SETFów, zwracamy SETFowany element. |#
(defmethod (setf heap-peek) ((entry entry) (heap heap))
  (setf (aref (array-of heap) 0)
        entry)
  entry)

#| Wstawienie do kopca to sprawdzenie jego rozmiaru, wstawienie elementu na koniec tablicy,
kopcowanie w górę oraz inkrementacja ilości elementów. 
Zgodnie z lispową konwencją oryginalnego PUSHa, zwracamy stertę. |#
(defmethod heap-push ((heap heap) data (priority integer))
  (when (heap-full-p heap)
    (error 'heap-error :message "Attempted to push an element into a full heap."))
  (let ((entry (make-entry priority data))) 
    (setf (aref (array-of heap) (elements heap)) entry) 
    (heapify-upwards heap (elements heap))
    (incf (elements heap))
    heap))

#| Wyjęcie z kopca to sprawdzenie jego rozmiaru, dekrementacja ilości elementów, zdjęcie 
elementu z początku, zastąpienie go elementem z końca oraz kopcowanie w dół. |#

(defmethod heap-pop ((heap heap))
  (when (heap-empty-p heap)
    (error 'heap-error :message "Attempted to pop an element from an empty heap.")) 
  (let ((entry (heap-peek heap)))
    (decf (elements heap))
    (setf (heap-peek heap) (aref (array-of heap) (elements heap)))
    (heapify-downwards heap)
    (setf (aref (array-of heap) (elements heap)) nil)
    (values (data entry)
            (priority entry))))

#| Tutaj zaczynają się algorytmy kopca.
Ze względu na to, że wysokość sterty jest logarytmiczna względem ilości elementów, pozwalam
sobie na bezczelną rekurencję. Stos mi się raczej nie wywali, nawet przy tablicy wielkości 
miliona elementów.

ROTATEF to makro - Lispowy odpowiednik funkcji swap. Nie możemy tam użyć zmiennych lokalnych,
gdyż to wartości zmiennych lokalnych zostaną zamienione; chcemy zamienić komórki w tablicy,
więc musimy wewnątrz ROTATEF użyć surowych AREFów. |#
(defun heapify-upwards (heap index)
  (when (< 0 index)
    (let* ((node (aref (array-of heap) index))
           (parent-index (truncate index 2))
           (parent (aref (array-of heap) parent-index)))
      (when (> (priority parent) (priority node))
        (rotatef (aref (array-of heap) index)
                 (aref (array-of heap) parent-index))
        (heapify-upwards heap parent-index)))))

#| Do kopcowania w dół nie jest wymagany indeks, bo przy wywołaniu funkcji z zewnątrz 
kopcujemy zawsze od zera. Na potrzeby rekurencji jednak będziemy chcieli przekazywać
indeks jako argument, stąd zdefiniowanie indeksu jako argumentu opcjonalnego. |#
(defun heapify-downwards (heap &optional (index 0))
  (when (< index (truncate (elements heap) 2))
    (let* ((node (aref (array-of heap) index)) 
           (left-child-index (1+ (* index 2)))
           (left-child (aref (array-of heap) left-child-index)) 
           (right-child-index (1+ left-child-index))
           (right-child (aref (array-of heap) right-child-index)))
      (cond ((and (<= left-child-index (elements heap))
                  (> (priority node) (priority right-child))
                  (>= (priority left-child) (priority right-child)))
             (rotatef (aref (array-of heap) index)
                      (aref (array-of heap) right-child-index))
             (heapify-downwards heap right-child-index))
            ((and (<= right-child-index (elements heap))
                  (> (priority node) (priority left-child))
                  (>= (priority right-child) (priority left-child)))
             (rotatef (aref (array-of heap) index)
                      (aref (array-of heap) left-child-index))
             (heapify-downwards heap left-child-index))))))

#| No to czas zrobić Właściwą Rzecz i napisać jakiś większy test jednostkowy. Będzie wykonywać
się za każdym razem, gdy ten plik zostaje skompilowany lub wczytany - więc, jeśli cokolwiek się
sypnie, ten program w ogóle dalej nie ruszy. 
Najpierw coś, w czym Lisp jest bardzo dobry - miniframework testowy do chwytania błędów,
napisany w dwóch linijkach. |#
(defmacro signals (condition-type &body body)
  `(handler-case (progn ,@body (error "Condition was not signaled.")) (,condition-type ())))

#| I sam test jednostkowy. Mam nadzieję, że jest czytelny sam z siebie.

Zadeklarowana w środku testu funkcja CHECK-POP sprawdza, czy kolejny element zdjęty ze stosu
ma wartość i priorytet równym zadanym jako argumenty. CHECK-PEEK robi to samo, lecz tylko 
podgląda element bez zdejmowania go ze sterty. 

ASSERT sprawdza, czy jego argument zwraca prawdę. Jeśli nie - zgłasza błąd.

MAPCAR pod spodem jest skrótem na sprawdzenie wielu wartości pod rząd - czy najpierw
zdejmuje się :ZERO 0, potem :ONE 1, i tak do końca listy :NINE 9 jest sprawdzane osobno,
gdyż pod koniec sprawdzamy jeszcze warunek brzegowy dla HEAP-EMPTY-P. |#
(let ((heap (make-heap 10))) 
    (labels ((check-peek (data priority)
               (let ((entry (heap-peek heap)))
                 (assert (equal data (data entry)))
                 (assert (equal priority (priority entry)))))
             (check-pop (data priority) 
               (check-peek data priority)
               (heap-pop heap)))
      
      (signals heap-error (heap-peek heap))
      (signals heap-error (heap-pop heap))
      
      (assert (heap-empty-p heap))
      (heap-push heap :two 2)
      (check-peek :two 2)
      (assert (not (heap-empty-p heap)))
      
      (heap-push heap :one 1)
      (check-peek :one 1)
      (heap-push heap :three 3)
      (check-peek :one 1)
      
      (heap-push heap :zero 0)
      (check-peek :zero 0)

      (heap-push heap :zero 0)
      (check-peek :zero 0) 
      (heap-push heap :zero 0)
      (check-peek :zero 0)
      (check-pop :zero 0)
      (check-peek :zero 0)
      (check-pop :zero 0)
      (check-peek :zero 0)
      
      (heap-push heap :eight 8)
      (check-peek :zero 0)
      
      (heap-push heap :four 4)
      (check-peek :zero 0)
      
      (heap-push heap :seven 7)
      (check-peek :zero 0)
      
      (heap-push heap :nine 9)
      (check-peek :zero 0)
      
      (heap-push heap :six 6)
      (check-peek :zero 0)
      
      (assert (not (heap-full-p heap)))
      (heap-push heap :five 5)
      (check-peek :zero 0)
      (assert (heap-full-p heap))
      
      (signals heap-error (heap-push heap :ten 10))

      (mapcar (lambda (x) (apply #'check-pop x))
              '((:zero 0) (:one 1) (:two 2)
                (:three 3) (:four 4) (:five 5)
                (:six 6) (:seven 7) (:eight 8)))
      
      (assert (not (heap-empty-p heap))) 
      (check-pop :nine 9) 
      (assert (heap-empty-p heap))
      
      (signals heap-error (heap-peek heap))
      (signals heap-error (heap-pop heap))))






;; #############################################################
;; ## ODCINEK II
;; ##
;; ## Generator tablic pseudolosowych
;; #############################################################

#| Nie będzie tu skomplikowane - piszemy funkcję, która tworzy tablicę, po czym
wypełnia ją liczbami pseudolosowymi.
Wyróżniam to jako osobny odcinek, bo jest tu pole do rozwoju, gdyby trzeba byłouwzględnić 
nieco bardziej regularne dane do generowania labiryntów. Jakieś lokalne zgrupowania, 
wzniesienia, detale geograficzne - można to zaimplementować właśnie w tym miejscu.|#
(defun make-randomized-array (y-size x-size &optional (maximum 255))
  (let ((array (make-array (list y-size x-size) :element-type 'integer)))
    (dotimes (y y-size)
      (dotimes (x x-size)
        (setf (aref array y x) (random (1+ maximum)))))
    array))






;; #############################################################
;; ## ODCINEK III
;; ##
;; ## Tworzenie labiryntu
;; #############################################################

#| Algorytm działa zgodnie z zasadą płynącej wody - ze sterty zawsze najpierw zdejmowany jest 
wierzchołek o najmniejszej wartości i to on jest przyłączany do grafu w losowym już dołączonym
kierunku (bo czasem jest więcej, niż jeden). Oznacza to, że cały labirynt jest drzewem.

Nasz algorytm zakłada, że dysponujemy tablicą z danymi z poprzedniego kroku. Jeżeli traktujemy 
te liczby jako wierzchołki grafu, to musimy również mieć sposób na oznaczenie krawędzi - możemy
to osiągnąć za pomocą trójwymiarowej tablicy informującej o tym, czy wierzchołek ma krawędzie 
w dół albo w prawo. Dwa pierwsze wymiary to wymiary oryginalnej tablicy, trzeci wymiar oznacza
dwa bity; pierwszy oznacza krawędź w dół, drugi - w prawo. |#
(defun make-maze (y-size x-size &key random-array starting-point)
  (let ((data (or random-array (make-randomized-array y-size x-size)))
        (edges (make-array (list y-size x-size 2) :element-type 'bit))
        (starting-point (or starting-point (list (random y-size) (random x-size)))))
    (generate-maze data edges starting-point)))

#| Pesymistycznie rzecz biorąc, w kolejce może znaleźć się 4/5 wszystkich wierzchołków, każdy w
maksymalnie czterech duplikatach - stąd rozmiar sterty. Nie usuwam duplikatów, gdyż sprawdzanie
całej sterty za każdym razem byłoby O(n) - lepiej sprawdzać to za każdym razem przy zdejmowaniu
wierzchołka ze sterty.

Najpierw inicjalizujemy struktury danych - oznaczamy punkt startowy jako odwiedzony i karmimy
nim kolejkę. Sprawdzamy i tworzymy pierwszą krawędź, od której zacznie się budowa labiryntu.
Potem - wchodzimy w pętlę główną.|#
(defun generate-maze (data edges starting-point)
  (let* ((heap (make-heap (ceiling (array-total-size data) 5/16))))
    (heap-push heap starting-point 0)
    (if (valid-vertex-p (get-neighbor starting-point :down) edges)
        (make-edge edges starting-point (get-neighbor starting-point :down))
        (make-edge edges starting-point (get-neighbor starting-point :up))) 
    (maze-loop heap data edges)))

#| Główna logika funkcji generującej labirynt. Najpierw zdejmujemy jeden wierzchołek ze sterty,
po czym, jeśli jeszcze nie jest w grafie, dołączamy go i wrzucamy jego niedołączonych sąsiadów
na stertę. Robimy tak do wyczerpania się sterty - gdy to nastąpi, zwracamy tablicę z
wygenerowanymi krawędziami.|#
(defun maze-loop (heap data edges)
  (loop for vertex = (heap-pop heap)
        do (unless (pathed-p vertex edges)
             (add-random-edge vertex edges)) 
           (push-neighbors heap vertex data edges)
        until (heap-empty-p heap)
        finally (return edges)))

#| Wierzchołek jest właściwy, gdy mieści się w granicach tablicy. |#
(defun valid-vertex-p (vertex edges)
  (let ((y (first vertex)) (x (second vertex)))
    (and (< -1 y (array-dimension edges 0))
         (< -1 x (array-dimension edges 1)))))

#| Ta funkcja dodaje krawędź między wskazanym wierzchołkiem a jednym z jego właściwych sąsiadów
należących do grafu. |#
(defun add-random-edge (vertex edges)
  (let ((neighbors (get-neighbors vertex)))
    (flet ((valid-p (x) (and (valid-vertex-p x edges) (pathed-p x edges))))
      (make-edge edges vertex (get-at-random (remove-if-not #'valid-p neighbors))))))

#| Funkcja pomocnicza - zwraca losowy element z listy; w przypadku listy pustej zwraca ją. |#
(defun get-at-random (list)
  (when list (nth (random (length list)) list)))

#| Tworzymy krawędź. Zakładamy, że pierwszy wierzchołek to właściwy wierzchołek, natomiast 
drugi różni się od niego o ±1 w którejś współrzędnej. |#
(defun make-edge (edges vertex-ori vertex-alt) 
  (when vertex-alt
    (let ((y-ori (first vertex-ori)) (x-ori (second vertex-ori))
          (y-alt (first vertex-alt)) (x-alt (second vertex-alt)))
      (flet ((mark (y x i) (setf (aref edges y x i) 1)))
        (cond
          ((= x-ori (1- x-alt)) (mark y-ori x-ori 1))
          ((= x-ori (1+ x-alt)) (mark y-ori x-alt 1))
          ((= y-ori (1- y-alt)) (mark y-ori x-ori 0))
          ((= y-ori (1+ y-alt)) (mark y-alt x-ori 0)))))))

#| Wrzucamy na stertę wszystkich właściwych nieościeżkowanych sąsiadów wskazanego wierzchołka. |#
(defun push-neighbors (heap vertex data edges)
  (flet ((push-vertices (x) (heap-push heap x (aref data (first vertex) (second vertex))))
         (properp (x) (and (valid-vertex-p x edges) (not (pathed-p x edges)))))
    (mapcar #'push-vertices (remove-if-not #'properp (get-neighbors vertex)))))

#| W tym miejscu bierzemy czterech sąsiadów wskazanego wierzchołka, nie patrząc na to, czy
są właściwi. |#
(defun get-neighbors (vertex)
  (mapcar (lambda (x) (get-neighbor vertex x)) '(:up :down :left :right)))

#| Funkcja produkująca pojedynczego sąsiada. |#
(defun get-neighbor (vertex direction)
  (let ((y (first vertex)) (x (second vertex)))
    (case direction
      (:up (list (1- y) x))
      (:down (list (1+ y) x))
      (:left (list y (1- x)))
      (:right (list y (1+ x))))))

#| Predykat odpowiadający na pytanie, czy wskazany wierzchołek jest już połączony z innymi. |#
(defun pathed-p (vertex edges) 
  (let* ((up (get-neighbor vertex :up))
         (down (get-neighbor vertex :down))
         (left (get-neighbor vertex :left))
         (right (get-neighbor vertex :right)))
    (or (when (valid-vertex-p up edges) 
          (= 1 (aref edges (first up) (second up) 0)))
        (when (valid-vertex-p down edges) 
          (= 1 (aref edges (first vertex) (second vertex) 0)))
        (when (valid-vertex-p left edges) 
          (= 1 (aref edges (first left) (second left) 1)))
        (when (valid-vertex-p right edges) 
          (= 1 (aref edges (first vertex) (second vertex) 1))))))






;; #############################################################
;; ## ODCINEK IV
;; ##
;; ## Drukowanie labiryntu na ekran
;; #############################################################

#| To jest w sumie najmniej interesująca część. |#
(defun print-maze (edges &optional return-nil-p) 
  (let ((width (array-dimension edges 1)))
    (flet ((print-solid-line ()
             (dotimes (i (+ 3 (* 2 width)))
               (princ "█"))
             (terpri)) 
           (print-rows (edges)
             (dotimes (y (array-dimension edges 0))
               (princ "██")
               (dotimes (x (array-dimension edges 1))
                 (if (= 1 (aref edges y x 1))
                     (princ "  ")
                     (princ " █")))
               (princ "█")
               (terpri) 
               (princ "██")
               (dotimes (x (array-dimension edges 1))
                 (if (= 1 (aref edges y x 0))
                     (princ " █")
                     (princ "██")))
               (princ "█")
               (terpri))))
      (print-solid-line)
      (print-solid-line)
      (print-rows edges)
      (print-solid-line)
      (unless return-nil-p edges))))

#| Testów jednostkowych nie ma - ludzkie oko najlepiej przetestuje taki labirynt. |#

#| Ta funkcja to interfejs publiczny. |#
(defun print-new-maze (y-size x-size)
  (print-maze (make-maze y-size x-size) t))

#| By uruchomić: sbcl --script heap.lisp
Na standardowe wejściu przyjmowane są: wysokość i szerokość labiryntu.
Na standardowy ANSI terminal polecam 10 x 38.|#
(defun main () 
  (print-new-maze (read) (read)))

(eval-when (:execute)
  (main))

