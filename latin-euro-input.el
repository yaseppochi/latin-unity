;;; latin-9-input.el --- Input method for Latin-9 (ISO 8859/15) -*- coding: iso-2022-jp -*-

;; Copyright (C) 2001, 2002 Free Software Foundation, Inc

;; Author: Dave Love
;; Adapted-by: Stephen J. Turnbull for XEmacs
;; Keywords: mule, input methods
;; Created: 2002 March 1
;; Last-modified: 2002 March 1

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Grabbed from latin-pre.el in the Emacs 21 distribution.  I believe this
;; is the method posted by Dave Love to gnu.emacs.sources.  The copyright in
;; latin-pre.el is bogus as Love's post was late 2001.

(require 'latin-unity-vars)		; for ISO 8859/15
(require 'quail)
(quail-define-package
 "latin-9-prefix" "Latin-9" "0>" t
 "Latin-9 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> ,ba(B
    grave    |   `    | `a -> ,b`(B
  circumflex |   ^    | ^a -> ,bb(B
  diaeresis  |   \"    | \"a -> ,bd(B, \"Y -> ,b>(B
    tilde    |   ~    | ~a -> ,bc(B
    caron    |   ~    | ~z -> ,b8(B
   cedilla   |   ~    | ~c -> ,bg(B
    misc     | \" ~ /  | \"s -> ,b_(B  ~d -> ,bp(B  ~t -> ,b~(B  /a -> ,be(B  /e -> ,bf(B  /o -> ,bx(B
             | \" ~ /  | /o -> ,b=(B
   symbol    |   ~    | ~> -> ,b;(B  ~< -> ,b+(B  ~! -> ,b!(B  ~? -> ,b?(B  ~~ -> ,b8(B
             |   ~    | ~s -> ,b'(B  ~e -> ,b$(B  ~. -> ,b7(B  ~$ -> ,b#(B  ~u -> ,b5(B
             |   ~    | ~- -> ,b-(B  ~= -> ,b/(B
   symbol    |  _ /   | _o -> ,b:(B  _a -> ,b*(B  // -> ,b0(B  /\\ -> ,bW(B  _y -> ,b%(B
             |  _ /   | _: -> ,bw(B  /c -> ,b"(B  ~p -> ,b6(B
             |  _ /   | /= -> ,b,(B
   symbol    |   ^    | ^r -> ,b.(B  ^c -> ,b)(B  ^1 -> ,b9(B  ^2 -> ,b2(B  ^3 -> ,b3(B  _a -> ,b*(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?,bA(B)
 ("'E" ?,bI(B)
 ("'I" ?,bM(B)
 ("'O" ?,bS(B)
 ("'U" ?,bZ(B)
 ("'Y" ?,b](B)
 ("'a" ?,ba(B)
 ("'e" ?,bi(B)
 ("'i" ?,bm(B)
 ("'o" ?,bs(B)
 ("'u" ?,bz(B)
 ("'y" ?,b}(B)
 ("' " ?')
 ("`A" ?,b@(B)
 ("`E" ?,bH(B)
 ("`I" ?,bL(B)
 ("`O" ?,bR(B)
 ("`U" ?,bY(B)
 ("`a" ?,b`(B)
 ("`e" ?,bh(B)
 ("`i" ?,bl(B)
 ("`o" ?,br(B)
 ("`u" ?,by(B)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?,bB(B)
 ("^E" ?,bJ(B)
 ("^I" ?,bN(B)
 ("^O" ?,bT(B)
 ("^U" ?,b[(B)
 ("^a" ?,bb(B)
 ("^e" ?,bj(B)
 ("^i" ?,bn(B)
 ("^o" ?,bt(B)
 ("^u" ?,b{(B)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?,bD(B)
 ("\"E" ?,bK(B)
 ("\"I" ?,bO(B)
 ("\"O" ?,bV(B)
 ("\"U" ?,b\(B)
 ("\"a" ?,bd(B)
 ("\"e" ?,bk(B)
 ("\"i" ?,bo(B)
 ("\"o" ?,bv(B)
 ("\"s" ?,b_(B)
 ("\"u" ?,b|(B)
 ("\"y" ?,b(B)
 ("\" " ?\")
 ("~A" ?,bC(B)
 ("~C" ?,bG(B)
 ("~D" ?,bP(B)
 ("~N" ?,bQ(B)
 ("~O" ?,bU(B)
 ("~S" ?,b&(B)
 ("~T" ?,b^(B)
 ("~Z" ?,b4(B)
 ("~a" ?,bc(B)
 ("~c" ?,bg(B)
 ("~d" ?,bp(B)
 ("~n" ?,bq(B)
 ("~o" ?,bu(B)
 ("~s" ?,b((B)
 ("~t" ?,b~(B)
 ("~z" ?,b8(B)
 ("~>" ?\,b;(B)
 ("~<" ?\,b+(B)
 ("~!" ?,b!(B)
 ("~?" ?,b?(B)
 ("~ " ?~)
 ("/A" ?,bE(B)
 ("/E" ?,bF(B)
 ("/O" ?,bX(B)
 ("/a" ?,be(B)
 ("/e" ?,bf(B)
 ("/o" ?,bx(B)
 ("//" ?,b0(B)
 ("/ " ?/)
 ("_o" ?,b:(B)
 ("_a" ?,b*(B)
 ("_+" ?,b1(B)
 ("_y" ?,b%(B)
 ("_:" ?,bw(B)
 ("/c" ?,b"(B)
 ("/\\" ?,bW(B)
 ("/o" ?,b=(B)		; clash with ,bx(B, but ,bf(B uses /
 ("/O" ?,b<(B)
 ("\"Y" ?,b>(B)
 ("~s" ?,b'(B)
 ("~p" ?,b6(B)
 ;; Is this the best option for Euro entry?
 ("~e" ?,b$(B)
 ("~." ?,b7(B)
 ("~$" ?,b#(B)
 ("~u" ?,b5(B)
 ("^r" ?,b.(B)
 ("^c" ?,b)(B)
 ("^1" ?,b9(B)
 ("^2" ?,b2(B)
 ("^3" ?,b3(B)
 ("~-" ?,b-(B)
 ("~=" ?,b/(B)
 ("/=" ?,b,(B))

