;;; alt-codes.el --- Insert alt codes using meta key.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-06-23 00:27:18

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Insert alt codes using meta key.
;; Keyword: alt codes insertion meta
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/jcs090218/alt-codes

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Insert alt codes using meta key.
;;

;;; Code:


(require 'subr-x)


(defgroup alt-codes nil
  "Insert alt codes using meta key."
  :prefix "alt-codes-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/alt-codes"))


(defvar-local alt-codes--code ""
  "Recording current key code.")


(defun alt-codes--pre-command-hook ()
  "Hook run before every command."
  (when (symbolp last-input-event)
    (let* ((lie-str (symbol-name last-input-event))
           (key-id "")
           (insert-it nil))
      (if (and (stringp lie-str)
               (string-match-p "M-kp-" lie-str))
          (progn
            (setq key-id (substring lie-str (1- (length lie-str)) (length lie-str)))
            (if (string-match-p "[0-9]+" key-id)
                (progn
                  (setq-local alt-codes--code (concat alt-codes--code key-id))
                  (message "[Alt Code]: %s" alt-codes--code))
              (setq insert-it t)))
        (setq insert-it t))
      (when (and insert-it
                 (not (string= alt-codes--code "")))
        (let ((code (alt-codes--get-alt-codes alt-codes--code)))
          (when code
            (insert code)))
        (setq-local alt-codes--code "")))))


(defun alt-codes--get-alt-codes (code)
  "Return the alt code by CODE."
  (cond ((string= code "1") "☺") ((string= code "2") "☻") ((string= code "3") "♥")
        ((string= code "4") "♦") ((string= code "5") "♣") ((string= code "6") "♠")
        ((string= code "7") "•") ((string= code "8") "◘") ((string= code "9") "○")
        ((string= code "10") "◙") ((string= code "11") "♂") ((string= code "12") "♀")
        ((string= code "13") "♪") ((string= code "14") "♫") ((string= code "15") "☼")
        ((string= code "16") "►") ((string= code "17") "◄") ((string= code "18") "↕")
        ((string= code "19") "‼") ((string= code "20") "¶") ((string= code "21") "§")
        ((string= code "22") "▬") ((string= code "23") "↨") ((string= code "24") "↑")
        ((string= code "25") "↓") ((string= code "26") "→") ((string= code "27") "←")
        ((string= code "28") "∟") ((string= code "29") "↔") ((string= code "30") "▲")
        ((string= code "31") "▼") ((string= code "32") "spc") ((string= code "33") "!")
        ((string= code "34") "\"") ((string= code "35") "#") ((string= code "36") "$")
        ((string= code "37") "%") ((string= code "38") "&") ((string= code "39") "'")
        ((string= code "40") "(") ((string= code "41") ")") ((string= code "42") "*")
        ((string= code "43") "+") ((string= code "44") ",") ((string= code "45") "-")
        ((string= code "46") ".") ((string= code "47") "/") ((string= code "48") "0")
        ((string= code "49") "1") ((string= code "50") "2") ((string= code "51") "3")
        ((string= code "52") "4") ((string= code "53") "5") ((string= code "54") "6")
        ((string= code "55") "7") ((string= code "56") "8") ((string= code "57") "9")
        ((string= code "58") ":") ((string= code "59") ";") ((string= code "60") "<")
        ((string= code "61") "=") ((string= code "62") ">") ((string= code "63") "?")
        ((string= code "64") "@") ((string= code "65") "A") ((string= code "66") "B")
        ((string= code "67") "C") ((string= code "68") "D") ((string= code "69") "E")
        ((string= code "70") "F") ((string= code "71") "G") ((string= code "72") "H")
        ((string= code "73") "I") ((string= code "74") "J") ((string= code "75") "K")
        ((string= code "76") "L") ((string= code "77") "M") ((string= code "78") "N")
        ((string= code "79") "O") ((string= code "80") "P") ((string= code "81") "Q")
        ((string= code "82") "R") ((string= code "83") "S") ((string= code "84") "T")
        ((string= code "85") "U") ((string= code "86") "V") ((string= code "87") "W")
        ((string= code "88") "X") ((string= code "89") "Y") ((string= code "90") "Z")
        ((string= code "91") "[") ((string= code "92") "\\") ((string= code "93") "]")
        ((string= code "94") "^") ((string= code "95") "_") ((string= code "96") "`")
        ((string= code "97") "a") ((string= code "98") "b") ((string= code "99") "c")
        ((string= code "100") "d") ((string= code "101") "e") ((string= code "102") "f")
        ((string= code "103") "g") ((string= code "104") "h") ((string= code "105") "i")
        ((string= code "106") "j") ((string= code "107") "k") ((string= code "108") "l")
        ((string= code "109") "m") ((string= code "110") "n") ((string= code "111") "o")
        ((string= code "112") "p") ((string= code "113") "q") ((string= code "114") "r")
        ((string= code "115") "s") ((string= code "116") "t") ((string= code "117") "u")
        ((string= code "118") "v") ((string= code "119") "w") ((string= code "120") "x")
        ((string= code "121") "y") ((string= code "122") "z") ((string= code "123") "{")
        ((string= code "124") "|") ((string= code "125") "}") ((string= code "126") "~")
        ((string= code "127") "⌂") ((string= code "128") "Ç") ((string= code "129") "ü")
        ((string= code "130") "é") ((string= code "131") "â") ((string= code "132") "ä")
        ((string= code "133") "à") ((string= code "134") "å") ((string= code "135") "ç")
        ((string= code "136") "ê") ((string= code "137") "ë") ((string= code "138") "è")
        ((string= code "139") "ï") ((string= code "140") "î") ((string= code "141") "ì")
        ((string= code "142") "Ä") ((string= code "143") "Å") ((string= code "144") "É")
        ((string= code "145") "æ") ((string= code "146") "Æ") ((string= code "147") "ô")
        ((string= code "148") "ö") ((string= code "149") "ò") ((string= code "150") "û")
        ((string= code "151") "ù") ((string= code "152") "ÿ") ((string= code "153") "Ö")
        ((string= code "154") "Ü") ((string= code "155") "¢") ((string= code "156") "£")
        ((string= code "157") "¥") ((string= code "158") "₧") ((string= code "159") "ƒ")
        ((string= code "160") "á") ((string= code "161") "í") ((string= code "162") "ó")
        ((string= code "163") "ú") ((string= code "164") "ñ") ((string= code "165") "Ñ")
        ((string= code "166") "ª") ((string= code "167") "º") ((string= code "168") "¿")
        ((string= code "169") "⌐") ((string= code "170") "¬") ((string= code "171") "½")
        ((string= code "172") "¼") ((string= code "173") "¡") ((string= code "174") "«")
        ((string= code "175") "»") ((string= code "176") "░") ((string= code "177") "▒")
        ((string= code "178") "▓") ((string= code "179") "│") ((string= code "180") "┤")
        ((string= code "181") "╡") ((string= code "182") "╢") ((string= code "183") "╖")
        ((string= code "184") "╕") ((string= code "185") "╣") ((string= code "186") "║")
        ((string= code "187") "╗") ((string= code "188") "╝") ((string= code "189") "")
        ((string= code "190") "╛") ((string= code "191") "┐") ((string= code "192") "└")
        ((string= code "193") "┴") ((string= code "194") "┬") ((string= code "195") "├")
        ((string= code "196") "─") ((string= code "197") "┼") ((string= code "198") "╞")
        ((string= code "199") "╟") ((string= code "200") "╚") ((string= code "201") "╔")
        ((string= code "202") "╩") ((string= code "203") "╦") ((string= code "204") "╠")
        ((string= code "205") "═") ((string= code "206") "╬") ((string= code "207") "╧")
        ((string= code "208") "╨") ((string= code "209") "╤") ((string= code "210") "╥")
        ((string= code "211") "╙") ((string= code "212") "╘") ((string= code "213") "╒")
        ((string= code "214") "╓") ((string= code "215") "╫") ((string= code "216") "╪")
        ((string= code "217") "┘") ((string= code "218") "┌") ((string= code "219") "█")
        ((string= code "220") "▄") ((string= code "221") "▌") ((string= code "222") "▐")
        ((string= code "223") "▀") ((string= code "224") "α") ((string= code "225") "ß")
        ((string= code "226") "Γ") ((string= code "227") "π") ((string= code "228") "Σ")
        ((string= code "229") "σ") ((string= code "230") "µ") ((string= code "231") "τ")
        ((string= code "232") "Φ") ((string= code "233") "Θ") ((string= code "234") "Ω")
        ((string= code "235") "δ") ((string= code "236") "∞") ((string= code "237") "φ")
        ((string= code "238") "ε") ((string= code "239") "∩") ((string= code "240") "≡")
        ((string= code "241") "±") ((string= code "242") "≥") ((string= code "243") "≤")
        ((string= code "244") "⌠") ((string= code "245") "⌡") ((string= code "246") "÷")
        ((string= code "247") "≈") ((string= code "248") "°") ((string= code "249") "∙")
        ((string= code "250") "·") ((string= code "251") "√") ((string= code "252") "ⁿ")
        ((string= code "253") "²") ((string= code "254") "■") ((string= code "255") "spc")
        ((string= code "0128") "€") ((string= code "0129") "") ((string= code "0130") "‚")
        ((string= code "0131") "ƒ") ((string= code "0132") "„") ((string= code "0133") "…")
        ((string= code "0134") "†") ((string= code "0135") "‡") ((string= code "0136") "ˆ")
        ((string= code "0137") "‰") ((string= code "0138") "Š") ((string= code "0139") "‹")
        ((string= code "0140") "Œ") ((string= code "0141") "") ((string= code "0142") "Ž")
        ((string= code "0143") "") ((string= code "0144") "") ((string= code "0145") "‘")
        ((string= code "0146") "’") ((string= code "0147") "“") ((string= code "0148") "”")
        ((string= code "0149") "•") ((string= code "0150") "–") ((string= code "0151") "—")
        ((string= code "0152") "˜") ((string= code "0153") "™") ((string= code "0154") "š")
        ((string= code "0155") "›") ((string= code "0156") "œ") ((string= code "0157") "")
        ((string= code "0158") "ž") ((string= code "0159") "Ÿ") ((string= code "0160") "spc")
        ((string= code "0161") "¡") ((string= code "0162") "¢") ((string= code "0163") "£")
        ((string= code "0164") "¤") ((string= code "0165") "¥") ((string= code "0166") "¦")
        ((string= code "0167") "§") ((string= code "0168") "¨") ((string= code "0169") "©")
        ((string= code "0170") "ª") ((string= code "0171") "«") ((string= code "0172") "¬")
        ((string= code "0173") "") ((string= code "0174") "®") ((string= code "0175") "¯")
        ((string= code "0176") "°") ((string= code "0177") "±") ((string= code "0178") "²")
        ((string= code "0179") "³") ((string= code "0180") "´") ((string= code "0181") "µ")
        ((string= code "0182") "¶") ((string= code "0183") "·") ((string= code "0184") "¸")
        ((string= code "0185") "¹") ((string= code "0186") "º") ((string= code "0187") "»")
        ((string= code "0188") "¼") ((string= code "0189") "½") ((string= code "0190") "¾")
        ((string= code "0191") "¿") ((string= code "0192") "À") ((string= code "0193") "Á")
        ((string= code "0194") "Â") ((string= code "0195") "Ã") ((string= code "0196") "Ä")
        ((string= code "0197") "Å") ((string= code "0198") "Æ") ((string= code "0199") "Ç")
        ((string= code "0200") "È") ((string= code "0201") "É") ((string= code "0202") "Ê")
        ((string= code "0203") "Ë") ((string= code "0204") "Ì") ((string= code "0205") "Í")
        ((string= code "0206") "Î") ((string= code "0207") "Ï") ((string= code "0208") "Ð")
        ((string= code "0209") "Ñ") ((string= code "0210") "Ò") ((string= code "0211") "Ó")
        ((string= code "0212") "Ô") ((string= code "0213") "Õ") ((string= code "0214") "Ö")
        ((string= code "0215") "×") ((string= code "0216") "Ø") ((string= code "0217") "Ù")
        ((string= code "0218") "Ú") ((string= code "0219") "Û") ((string= code "0220") "Ü")
        ((string= code "0221") "Ý") ((string= code "0222") "Þ") ((string= code "0223") "ß")
        ((string= code "0224") "à") ((string= code "0225") "á") ((string= code "0226") "â")
        ((string= code "0227") "ã") ((string= code "0228") "ä") ((string= code "0229") "å")
        ((string= code "0230") "æ") ((string= code "0231") "ç") ((string= code "0232") "è")
        ((string= code "0233") "é") ((string= code "0234") "ê") ((string= code "0235") "ë")
        ((string= code "0236") "ì") ((string= code "0237") "í") ((string= code "0238") "î")
        ((string= code "0239") "ï") ((string= code "0240") "ð") ((string= code "0241") "ñ")
        ((string= code "0242") "ò") ((string= code "0243") "ó") ((string= code "0244") "ô")
        ((string= code "0245") "õ") ((string= code "0246") "ö") ((string= code "0247") "÷")
        ((string= code "0248") "ø") ((string= code "0249") "ù") ((string= code "0250") "ú")
        ((string= code "0251") "û") ((string= code "0252") "ü") ((string= code "0253") "ý")
        ((string= code "0254") "þ") ((string= code "0255") "ÿ")
        (t nil)))


;;;###autoload
(defun alt-codes-table ()
  "Print out the table of all Alt Code."
  (interactive)
  (switch-to-buffer-other-window "*Alt-Codes*")
  (read-only-mode -1)
  (erase-buffer)
  (save-excursion
    (insert "Alt-Codes.\nThe list of all Alt Codes for special characters and symbols.\n\n")
    (let ((index 1)
          (code-str nil)
          (reach-255 nil))
      (insert "| Code | Symbol |\n")
      (insert "|------+--------|\n")
      (while (< index 256)
        (setq code-str (number-to-string index))
        (when reach-255 (setq code-str (concat "0" code-str)))
        (setq code-str (alt-codes--get-alt-codes (number-to-string index)))
        (when code-str
          (insert (format "| %4d | %6s |\n" index code-str))
          (setq index (1+ index)))
        (when (and (not reach-255)
                   (= index 255))
          (insert "\n")
          (setq index 128)
          (setq reach-255 t)))))
  (view-mode)
  (special-mode))

;;;###autoload
(defun alt-codes-insert ()
  "Insert the alt-code by string."
  (interactive)
  (let* ((code (string-trim (read-string "Insert Alt-Code: ")))
         (alt-code (alt-codes--get-alt-codes code)))
    (if alt-code
        (insert alt-code)
      (user-error "Invalid Alt Code, please input the valid one"))))


(defun alt-codes--enable ()
  "Enable 'alt-codes-mode'."
  (add-hook 'pre-command-hook #'alt-codes--pre-command-hook nil t))

(defun alt-codes--disable ()
  "Disable 'alt-codes-mode'."
  (remove-hook 'pre-command-hook #'alt-codes--pre-command-hook t))


;;;###autoload
(define-minor-mode alt-codes-mode
  "Minor mode for inserting `alt-codes'."
  :lighter " alt-codes"
  :group alt-codes
  (if alt-codes-mode
      (alt-codes--enable)
    (alt-codes--disable)))

(defun alt-codes-turn-on-alt-codes-mode ()
  "Turn on the 'alt-codes-mode'."
  (alt-codes-mode 1))

;;;###autoload
(define-globalized-minor-mode global-alt-codes-mode
  alt-codes-mode alt-codes-turn-on-alt-codes-mode
  :require 'alt-codes)


(provide 'alt-codes)
;;; alt-codes.el ends here
