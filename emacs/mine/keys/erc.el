
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "KICK" "MODE")) ;; don't show "xxx joined the channel"
(setq erc-input-line-position -2)  ;; supposed to keep newest message at bottom of buffer, but it's not working...

(defvar first-names '("mary" "patricia" "linda" "barbara" "elizabeth" "sarah" "kimberly" "deborah" "jessica" "shirley"
                      "cynthia" "angela" "melissa" "brenda" "amy" "anna" "rebecca" "virginia" "kathleen" "pamela" "martha"
                      "veronica" "jill" "erin" "geraldine" "lauren" "cathy" "joann" "lorraine" "lynn" "sally" "regina"
                      "erica" "beatrice" "dolores" "bernice" "audrey" "yvonne" "annette" "june" "samantha" "marion" "dana"
                      "stacy" "ana" "renee" "ida" "vivian" "roberta" "holly" "brittany" "melanie" "loretta" "yolanda"
                      "jeanette" "laurie" "katie" "kristen" "james" "john" "robert" "michael" "william" "david" "richard"
                      "charles" "joseph" "thomas" "christopher" "daniel" "paul" "mark" "donald" "george" "kenneth"
                      "steven" "edward" "brian" "ronald" "anthony" "kevin" "jason" "matthew" "gary" "timothy" "jose"
                      "larry" "jeffrey" "frank" "scott" "eric" "stephen" "andrew" "raymond" "gregory" "joshua" "jerry"
                      "dennis" "walter" "patrick" "peter" "harold" "douglas" "henry" "carl" "arthur" "ryan" "roger" "joe"
                      "bernard" "mario" "leroy" "francisco" "marcus" "micheal" "theodore" "clifford" "miguel" "oscar"
                      "jay" "jim" "tom" "calvin" "alex" "jon" "ronnie" "bill" "lloyd" "tommy"))

(defvar last-names '("aquirre" "arscott" "bagent" "balin" "bernales" "bomkamp" "bouthillier" "cavendish" "clyatt"
                     "detienne" "dewbre" "dimuro" "dingledine" "dosh" "droney" "dunklee" "duyck" "emilio" "ence"
                     "eversley" "fetzner" "garofano" "gellis" "gemmer" "grealish" "haertel" "haik" "handyside"
                     "haroutunian" "hornburg" "hurtig" "jenniges" "juncaj" "kallhoff" "kanaan" "kleban" "klontz" "knier"
                     "kopischke" "kugelman" "kuri" "lacoss" "lamarque" "langsdorf" "latouche" "leabo" "lorette" "maracle"
                     "mathus" "mccamy" "merta" "meulemans" "montieth" "muoio" "neyens" "niccoli" "oberhaus"
                     "oborn" "osorto" "penkala" "podoll" "prenatt" "ramone" "romes" "roupp" "ruscitti" "santaella"
                     "scozzari" "siverling" "speigner" "spinnato" "stentz" "stocke" "sundt" "thorup" "tresch" "tripplett"
                     "uhls" "urdaneta" "uttech" "vosler" "werber" "wieand" "zacharia" "zeleznik" "zoucha" "zuch"))

(defun select-random (coll)
  (let ((selected (nth (random (length coll)) coll)))
    (either selected
            (capitalize selected))))

(defun either (a b)
  (if (= 1 (random 2)) a b))

(defun run-erc ()
  (interactive)
  (let ((nickname (concat (select-random first-names)
                          (either (concat "_" (select-random last-names))
                                  (number-to-string (+ 1000 (random 10000)))
                                  ))))
    (erc :server "irc.freenode.net"
         :port 6667
         :nick nickname
         )))
