;;; latin-latin7-input.el --- Input method for Latin-7 (ISO 8859/13) -*- coding: iso-2022-jp -*-

;; Copyright (C) 2001, 2002 Free Software Foundation, Inc

;; Author: Stephen J. Turnbull
;; Keywords: mule, input methods
;; Added: 2002 August 31

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

(require 'latin-unity-vars)		; for ISO 8859/13
(require 'quail)
(quail-define-package
 "latin-7-prefix" "Latin-7" "0>" t
 "Latin-7 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
   ogonek    |   `    | `a
   macron    |   -    | -a
    acute    |   '    | 'a
  ring above |   %    | %a
   dot above |   %    | %e
  diaeresis  |   \"    | \"a
    tilde    |   ~    | ~o
    caron    |   ~    | ~s
   cedilla   |   ,    | ,g
    misc     |   /    | /s -> ,b_(B, /a -> ae
   symbol    |   _    | _\" -> ,Y!(B, _' -> ,Y(B, _* -> ,Y$(B, _+ -> ,Y1(B, _, -> ,Y%(B, _- -> ,Y-(B,
             |        | _. -> ,Y7(B, _/ -> ,Yw(B, _1 -> ,Y9(B, _2 -> ,Y2(B, _3 -> ,Y3(B, _< -> ,Y+(B,
             |        | _> -> ,Y;(B, _C -> ,Y)(B, _L -> ,Y#(B, _P -> ,Y6(B, _R -> ,Y.(B, _` -> ,Y4(B,
             |        | _c -> ,Y"(B, _h -> ,Y=(B, _n -> ,Y,(B, _o -> ,Y0(B, _q -> ,Y<(B, _s -> ,Y'(B,
             |        | _t -> ,Y>(B, _u -> ,Y5(B, _x -> ,YW(B, _| -> ,Y&(B
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("\"A" ?,YD(B)
 ("\"O" ?,YV(B)
 ("\"U" ?,Y\(B)
 ("\"a" ?,Yd(B)
 ("\"o" ?,Yv(B)
 ("\"u" ?,Y|(B)
 ("%A" ?,YE(B)
 ("%E" ?,YK(B)
 ("%Z" ?,Y](B)
 ("%a" ?,Ye(B)
 ("%e" ?,Yk(B)
 ("%z" ?,Y}(B)
 ("'C" ?,YC(B)
 ("'E" ?,YI(B)
 ("'N" ?,YQ(B)
 ("'O" ?,YS(B)
 ("'S" ?,YZ(B)
 ("'Z" ?,YJ(B)
 ("'c" ?,Yc(B)
 ("'e" ?,Yi(B)
 ("'n" ?,Yq(B)
 ("'o" ?,Ys(B)
 ("'s" ?,Yz(B)
 ("'z" ?,Yj(B)
 (",G" ?,YL(B)
 (",K" ?,YM(B)
 (",L" ?,YO(B)
 (",N" ?,YR(B)
 (",R" ?,Y*(B)
 (",g" ?,Yl(B)
 (",k" ?,Ym(B)
 (",l" ?,Yo(B)
 (",n" ?,Yr(B)
 (",r" ?,Y:(B)
 ("-A" ?,YB(B)
 ("-E" ?,YG(B)
 ("-I" ?,YN(B)
 ("-O" ?,YT(B)
 ("-U" ?,Y[(B)
 ("-a" ?,Yb(B)
 ("-e" ?,Yg(B)
 ("-i" ?,Yn(B)
 ("-o" ?,Yt(B)
 ("-u" ?,Y{(B)
 ("/A" ?,Y/(B)
 ("/L" ?,YY(B)
 ("/O" ?,Y((B)
 ("/a" ?,Y?(B)
 ("/l" ?,Yy(B)
 ("/o" ?,Y8(B)
 ("/s" ?,Y_(B)
 ("`A" ?,Y@(B)
 ("`E" ?,YF(B)
 ("`I" ?,YA(B)
 ("`U" ?,YX(B)
 ("`a" ?,Y`(B)
 ("`e" ?,Yf(B)
 ("`i" ?,Ya(B)
 ("`u" ?,Yx(B)
 ("~C" ?,YH(B)
 ("~O" ?,YU(B)
 ("~S" ?,YP(B)
 ("~Z" ?,Y^(B)
 ("~c" ?,Yh(B)
 ("~o" ?,Yu(B)
 ("~s" ?,Yp(B)
 ("~z" ?,Y~(B)
 ("_\"" ?,Y!(B)
 ("_'" ?,Y(B)
 ("_*" ?,Y$(B)
 ("_+" ?,Y1(B)
 ("_," ?,Y%(B)
 ("_-" ?,Y-(B)
 ("_." ?,Y7(B)
 ("_/" ?,Yw(B)
 ("_1" ?,Y9(B)
 ("_2" ?,Y2(B)
 ("_3" ?,Y3(B)
 ("_<" ?,Y+(B)
 ("_>" ?,Y;(B)
 ("_C" ?,Y)(B)
 ("_L" ?,Y#(B)
 ("_P" ?,Y6(B)
 ("_R" ?,Y.(B)
 ("_`" ?,Y4(B)
 ("_c" ?,Y"(B)
 ("_h" ?,Y=(B)
 ("_n" ?,Y,(B)
 ("_o" ?,Y0(B)
 ("_q" ?,Y<(B)
 ("_s" ?,Y'(B)
 ("_t" ?,Y>(B)
 ("_u" ?,Y5(B)
 ("_x" ?,YW(B)
 ("_|" ?,Y&(B)
)

(provide 'latin-latin7-input)

;; end of latin-latin7-input.el
