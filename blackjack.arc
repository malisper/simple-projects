;;;; A game for playing blackjack

(= ranks '(2 3 4 5 6 7 8 9 10 J K Q A))
(= suits '(H C S S))
(= num-decks 2)

(= deck nil)
(= dhand nil)
(= phand nil)

(deftem card rank -1 suit nil)

(def card-value (c)
  "gets the value of a card"
  (if (isa c!rank 'int) c!rank
      (is  c!rank 'A)   11
      'else             10))

(def hand-value (h)
  "calculates the total value of a hand"
  (withs (raw-value (reduce + (map card-value h))
	  actual-value raw-value
	  ace-count (count [is _!rank 'A] h))
     (until (or (<= actual-value 21) (<= ace-count 0))
	(-- actual-value 10)
	(-- ace-count))
     actual-value))
	 
(def top ()
  "takes off the top card of the deck"
  (pop deck))

(def start-game ()
  "deals and then starts the game"
  (when (len< deck (* num-decks 13)) ; when to reshuffle
    (= deck (shuffle:new-deck num-decks)))
  (deal)
  (players-turn)
  (dealers-turn)
  (result))

(def deal ()
  "deal the hands"
  (= dhand nil)
  (= phand nil)
  (push (top) phand)
  (push (top) dhand)
  (push (top) phand)
  (push (top) dhand))

(def players-turn ()
  "play out the players hand"
  (prn "You have ") (prn-hand phand)
  (prn "You see the dealer has ") (pr-card:car dhand) (prn)
  (unless (> (hand-value phand) 21)
     (prn "What do you want to do? ")
     (unless (is (read) 's)
	(push (top) phand)
	(players-turn))))

(def dealers-turn ()
  "dealers turn"
  (while (< (hand-value dhand) 17)
     (push (top) dhand)))

(def result ()
  "tell what the result is"
  (prn "The dealer had ") (prn-hand dhand)
  (if (> (hand-value phand) 21)
        (prn "You busted, dealer wins")
      (> (hand-value dhand) 21)
        (prn "Dealer busted, you win")
      (> (hand-value phand)(hand-value dhand))
        (prn "You have more than the dealer, you win")
      'else
        (prn "Dealer had more than you, dealer wins"))
  (prn "Would you like to play again?")
  (unless (is (read) 'n)
     (start-game))) 

(def new-deck ((o num 1))
  "create a given number of decks by default one
   they are not shuffled yet"
  (accum a
    (repeat num
      (each rank ranks
        (each suit suits
          (a:inst 'card 'rank rank 'suit suit))))))

(def prn-hand (h)
  "prints a hand"
  (each c h
    (pr-card c)
    (pr " "))
  (prn))

(def pr-card (c)
  "prints a card"
  (pr c!rank c!suit))
