(ns patience)

(def suits #{:hearts, :diamonds, :clubs, :spades})
(def names #{:two, :three, :four, :five, :six, :seven, :eight, :nine, :ten, :jack, :queen, :king, :ace})

(def deck
  (for [suit suits, name names]
       {:suit suit :name name}))

(defn stackable
  "Checks if two cards are stackable."
  [card_1 card_2]
  (or 
  (= (get card_1 :suit)(get card_2 :suit))
  (= (get card_1 :name)(get card_2 :name))))

(defn has-more-cards
  "Checks the card-deck for emptyness."
  [card-deck]
  (not (nil? (first card-deck))))

(defn new-stack
  "Creates a new stack containing the card at the end of the stacks"
  [card stack]
  (do
  (println "new-stack")
  (conj stack (list card))))

(defn add-to-last-stack [card stacks]
  (do
  (println "add-to-last-stack")
  (assoc stacks (- (count stacks) 1) (conj (last stacks) card))))

(defn third-from-the-end [stacks]
  (nth stacks (- (count stacks) 3)))

(defn add-to-three-from-last [card stacks]
  (do
  (println "add-to-three-from-last")
  (assoc stacks (- (count stacks) 3) (conj (third-from-the-end stacks) card))))

(defn is-stackable-on-last [card stacks]
  (stackable card (first (last stacks))))

(defn is-stackable-on-three-from-last [card stacks]
  (if-not (> (- (count stacks) 2) 0)
    false
    (stackable card (first (third-from-the-end stacks)))))

(defn stack-card
  "Places a card on a stack according to patience rules."
  [card stacks]
  (cond
    (is-stackable-on-last card stacks)(add-to-last-stack card stacks)
    (is-stackable-on-three-from-last card stacks)(add-to-three-from-last card stacks)
    :else (new-stack card stacks)))
  
(defn print-result[stacks]
  (do
    (println (count stacks))
    (println "end")
    (println stacks)))

(defn remove-stack
  "Removes a stack at a given index."
  [stacks index]
  (vec (concat (subvec stacks 0 index) (subvec stacks (+ index 1) (count stacks)))))

(defn copy-into-stack
  "Copies cards from one stack in to another."
  [stacks to from]
  (assoc stacks to (into (nth stacks to) (nth stacks from))))

(defn merge-stacks
  "Merges two stacks."
  [stacks to from]
  (do
  (println "Merge stacks")
  (remove-stack (copy-into-stack stacks to from) from)))

(defn do-restack
  "Finds stacks which can be merged."
  [stacks no]
  (if-not (stackable (first(nth stacks (- no 1)))(first(nth stacks no)))
    stacks
    (merge-stacks stacks (- no 1) no)))

(defn restack
  "Merges stacks which has its first card stackable with a neighbor stack."
  [stacks no]
  (if-not (> no 0)
    stacks
    (recur (do-restack stacks no) (- no 1))))

(defn patience
  "Draws cards from the deck and then places them on the stack. After a card has been placed the stacks are restacked."
  [card-deck stacks]
  (if-not (has-more-cards card-deck)
    (print-result stacks)
    ;stacks
    (recur (rest card-deck)(restack (stack-card (first card-deck) stacks) (- (count stacks) 1)))))

(def stacks [])
(patience (shuffle deck) stacks)